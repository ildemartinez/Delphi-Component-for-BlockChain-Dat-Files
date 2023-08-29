unit BlocksUnit;

interface

uses
  System.Classes,
  System.Contnrs,
  Generics.Collections,

  SeSHA256;

type
  TCrypto = (tcBitcoin);
  TNet = (tnMainNet, tnTestNet);

  TBlockChainFiles = class;

  TBlockFile = class(TObject)
    parent: TBlockChainFiles;
    aFileName: string;
    aBlockNumber: integer;
    afs: TBufferedFileStream;
  end;

  TBlockChainFiles = class(TList<TBlockFile>)

  end;

  TBlockHeader = record
    versionNumber: UInt32;
    aPreviousBlockHash: T32; // reverse please
    aMerkleRoot: T32; // reverse please
    time: UInt32; // UnixTime
    DifficultyTarget: UInt32;
    nonce: UInt32;
  end;

  TInput = class(TObject)
    aTXID: T32;
    aVOUT: UInt32;
    CoinBaseLength: uint64;
    CoinBase: PByte;

    destructor Destroy; override;
  end;

  TOutput = class(TObject)
    nValue: uint64;
    OutputScriptLength: uint64;
    OutputScript: PByte;

    destructor Destroy; override;
  end;

  TInputs = class(TObjectList)
  private
    function GetInput(index: integer): TInput;
  public
    function NewInput: TInput;
    property items[index: integer]: TInput read GetInput; default;
  end;

  TOutputs = class(TObjectList)
  private
    function GetOutput(index: integer): TOutput;
  public
    function NewOutput: TOutput;
    property items[index: integer]: TOutput read GetOutput; default;
  end;

  TTransaction = class(TObject)
    version: UInt32;
    inputs: TInputs;
    outputs: TOutputs;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBlockTransactions = class(TObjectList)
  private
    function GetTransaction(index: integer): TTransaction;
  public
    function NewTransaction: TTransaction;
    property items[index: integer]: TTransaction read GetTransaction; default;
  end;

  TBlockRecord = class(TObject)
    nblock: uint64;
    blocktype: TCrypto;
    network: TNet;

    headerLenght: UInt32;
    hash: string;
    header: TBlockHeader;
    transactions: TBlockTransactions;

    ninputs, noutputs: uint64;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TStartFileBlockFoundNotify = procedure(const aBlockFiles: TList<String>) of object;
  TFoundFileBlockNotify = procedure(const aBlockFile: TBlockFile; var next: boolean) of object;
  TEndFilesBlockFoundNotify = procedure(const aBlockFiles: tstringlist) of object;

  TFoundBlockNotify = procedure(const aBlock: TBlockRecord; var findnext: boolean) of object;
  TBlockProcessStepNotify = procedure(const aPos, asize: int64) of object;

  TBlocks = class(tthread)
  private
    // File block events
    fOnStartProcessFiles: TStartFileBlockFoundNotify;
    fOnAfterFileBlockProcessed: TFoundFileBlockNotify;

    fOnMagicBlockFound: TFoundBlockNotify;
    fBlockProcessStep: TBlockProcessStepNotify;
   // fEndProcessBlockFile: TEndProcessBlockFile;
    fOnEndProc: TNotifyEvent;
    fOnStartProc: TNotifyEvent;
    fOnBeforeFileBlockProcess: TFoundFileBlockNotify;

    procedure InternalProcessBlock(const aBlockFile: TBlockFile);
  public
    aBlockChainFiles: TBlockChainFiles;
    BlocksDir: string;
    aBlocksDirectory: string;

    procedure Execute; override;

    // Inicio y fin del parseo
    property OnStartingParsingBlockfiles: TNotifyEvent read fOnStartProc write fOnStartProc;
    property OnFinishedParsingBlockFiles: TNotifyEvent read fOnEndProc write fOnEndProc;

    // Start process all files
    property OnStartProcessFiles: TStartFileBlockFoundNotify read fOnStartProcessFiles write fOnStartProcessFiles;

    // before process a file
    property OnBeforeFileBlockProcess: TFoundFileBlockNotify read fOnBeforeFileBlockProcess write fOnBeforeFileBlockProcess;

    // after a processed file
    property OnAfterFileBlockProcessed: TFoundFileBlockNotify read fOnAfterFileBlockProcessed write fOnAfterFileBlockProcessed;

    // Block found
    property OnMagicBlockFound: TFoundBlockNotify read fOnMagicBlockFound write fOnMagicBlockFound;

    property OnBlockProcessStep: TBlockProcessStepNotify read fBlockProcessStep write fBlockProcessStep;
  end;

implementation

uses
  WinApi.Windows,
  SysUtils, dialogs, dateutils,
  MainFormUnit, System.hash,
  inifiles;

procedure TBlocks.Execute;
var
  searchResult: TSearchRec;
  aBlockFile: TBlockFile;

  aBlockFiles: TList<string>;
  aBlockFil: string;

  next: boolean;
  files: string;
begin

  aBlockChainFiles := TBlockChainFiles.Create;

  // Start parsing component
  if Assigned(OnStartingParsingBlockfiles) then
    Synchronize(
      procedure
      begin
        OnStartingParsingBlockfiles(self);
      end);

  SetCurrentDir(aBlocksDirectory);

  files := 'blk?????.dat';
  // files := 'blk00074.dat';

  if findfirst(files, faAnyFile, searchResult) = 0 then
  begin
    aBlockFiles := TList<String>.Create;

    repeat
      aBlockFiles.Add(aBlocksDirectory + '\' + searchResult.Name);
    until findnext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    System.SysUtils.FindClose(searchResult);

    // Start parsing all files
    if Assigned(OnStartProcessFiles) then
      Synchronize(
        procedure
        begin
          OnStartProcessFiles(aBlockFiles);
        end);

    for aBlockFil in aBlockFiles do
    begin

      aBlockFile := TBlockFile.Create;
      aBlockFile.parent := aBlockChainFiles;
      aBlockChainFiles.Add(aBlockFile);
      aBlockFile.aFileName := aBlockFil;
      aBlockFile.aBlockNumber := aBlockFiles.IndexOf(aBlockFil) + 1;

      // Start parsing a file
      if Assigned(fOnBeforeFileBlockProcess) then
        Synchronize(
          procedure
          begin
            fOnBeforeFileBlockProcess(aBlockFile, next);
          end);

      aBlockFile.afs := TBufferedFileStream.Create(aBlockFile.aFileName, fmOpenRead);
      try
        InternalProcessBlock(aBlockFile);
      finally
        aBlockFile.afs.Free;
      end;

      if Assigned(OnAfterFileBlockProcessed) then
        Synchronize(
          procedure
          begin
            OnAfterFileBlockProcessed(aBlockFile, next);
          end);

      aBlockFile.Free;

      if next = false then
        break;
    end;

    aBlockFiles.Free;
  end;

  if Assigned(OnFinishedParsingBlockFiles) then
    Synchronize(
      procedure
      begin
        OnFinishedParsingBlockFiles(self);
      end);
end;

procedure TBlocks.InternalProcessBlock(const aBlockFile: TBlockFile);
var
  state, car: byte;
  aBlock: TBlockRecord;
  cont: boolean;
  aTransaction: TTransaction;
  aseq: longword;
  aInput: TInput;
  aOutput: TOutput;

  tb: THeader;
var
  alocktime: UInt32;
  txCount, k: uint64;

  function ReadVarValue: uint64;
  var
    atxCount: UInt8;
    atxCount2: UInt16;
    atxCount4: UInt32;
  begin
    aBlockFile.afs.Read(atxCount, 1);
    if atxCount < $FD then
      result := atxCount
    else
      case atxCount of
        $FD:
          begin
            aBlockFile.afs.Read(atxCount2, 2);
            result := atxCount2;
          end;
        $FE:
          begin
            aBlockFile.afs.Read(atxCount4, 4);
            result := atxCount4;
          end;
        $FF:
          aBlockFile.afs.Read(result, 8);
      end;

  end;

begin
  state := 0;
  cont := true;

  while (cont = true) and (aBlockFile.afs.Read(car, 1) = 1) do
  begin

    case state of
      0:
        if car = $F9 then
          inc(state);
      1:
        if car = $BE then
          inc(state)
        else
          state := 0;
      2:
        if car = $B4 then
          inc(state)
        else
          state := 0;
      3:
        if car = $D9 then
        begin

          if Assigned(OnBlockProcessStep) then
            Synchronize(
              procedure
              begin
                OnBlockProcessStep(aBlockFile.afs.Position, aBlockFile.afs.Size);
              end);

          aBlock := TBlockRecord.Create;

          aBlockFile.afs.Read(aBlock.headerLenght, 4);

          // Read the header fields
          aBlockFile.afs.Read(aBlock.header, HEADERSIZE);

          // Re-read the header to calculate hash
          aBlockFile.afs.Seek(-HEADERSIZE, soCurrent);
          aBlockFile.afs.Read(tb, HEADERSIZE);

          // double header hash
          aBlock.hash := reversehash(SHA256ToStr(CalcSHA256(SHA256ToBinaryStr(CalcHeaderSHA256(tb)))));

          // tx count
          txCount := ReadVarValue;

          while (txCount > 0) do
          begin
            aTransaction := aBlock.transactions.NewTransaction;

            // Read the transaction version
            aBlockFile.afs.Read(aTransaction.version, 4);
            // Read the inputs
            aBlock.ninputs := ReadVarValue;

            if aBlock.ninputs > 0 then
              for k := 0 to aBlock.ninputs - 1 do
              begin
                aInput := aTransaction.inputs.NewInput;

                aBlockFile.afs.Read(aInput.aTXID, 32);
                aBlockFile.afs.Read(aInput.aVOUT, 4);

                aInput.CoinBaseLength := ReadVarValue;

                GetMem(aInput.CoinBase, aInput.CoinBaseLength);
                aBlockFile.afs.Read(aInput.CoinBase^, aInput.CoinBaseLength);

                // No need store the seq
                aBlockFile.afs.Read(aseq, 4);
              end;

            // tx out count
            aBlock.noutputs := ReadVarValue;

            if aBlock.noutputs > 0 then
              for k := 0 to aBlock.noutputs - 1 do
              begin
                aOutput := aTransaction.outputs.NewOutput;

                aBlockFile.afs.Read(aOutput.nValue, 8); // in satoshis

                aOutput.OutputScriptLength := ReadVarValue;

                GetMem(aOutput.OutputScript, aOutput.OutputScriptLength);
                aBlockFile.afs.Read(aOutput.OutputScript^, aOutput.OutputScriptLength);
              end;

            aBlockFile.afs.Read(alocktime, 4);
            dec(txCount);
          end;

          // Fire the block found event
          if Assigned(OnMagicBlockFound) then
            Synchronize(
              procedure
              begin
                OnMagicBlockFound(aBlock, cont);
              end);

          // Free the block so user must copy or use it and forget
          aBlock.Free;

          state := 0;
        end;

    end;

  end;

end;

{ TBlockRecord }

constructor TBlockRecord.Create;
begin
  transactions := TBlockTransactions.Create;
end;

destructor TBlockRecord.Destroy;
begin
  transactions.Free;

  inherited;
end;

{ TBlockTransactions }

function TBlockTransactions.GetTransaction(index: integer): TTransaction;
begin
  result := inherited items[index] as TTransaction;
end;

function TBlockTransactions.NewTransaction: TTransaction;
begin
  result := TTransaction.Create;
  self.Add(result);
end;

{ TTransaction }

constructor TTransaction.Create;
begin
  inputs := TInputs.Create;
  outputs := TOutputs.Create;
end;

destructor TTransaction.Destroy;
begin
  inputs.Free;
  outputs.Free;
end;

{ TOutputs }

function TOutputs.GetOutput(index: integer): TOutput;
begin
  result := TOutput(inherited items[index]);
end;

function TOutputs.NewOutput: TOutput;
begin
  result := TOutput.Create;
  self.Add(result);
end;

{ TInputs }

function TInputs.GetInput(index: integer): TInput;
begin
  result := TInput(inherited items[index]);
end;

function TInputs.NewInput: TInput;
begin
  result := TInput.Create;
  self.Add(result);
end;

destructor TInput.Destroy;
begin
  FreeMem(CoinBase);

  inherited;
end;

destructor TOutput.Destroy;
begin
  FreeMem(OutputScript);

  inherited;
end;

end.
