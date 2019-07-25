unit BlocksUnit;

interface

uses
  System.Classes,
  System.Contnrs,

  SeSHA256;

type
  TCrypto = (tcBitcoin);
  TNet = (tnMainNet, tnTestNet);

  TBlockFile = class(TObject)
    aFileName: string;
    afs: TBufferedFileStream;
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

  TStartFileBlockFoundNotify = procedure(const aBlockFiles: tstringlist)
    of object;
  TFoundFileBlockNotify = procedure(const aBlockFile: TBlockFile;
    const actualFileBlockNumber, totalBlockFiles: integer; var next: boolean)
    of object;
  TEndFilesBlockFoundNotify = procedure(const aBlockFiles: tstringlist)
    of object;

  TFoundBlockNotify = procedure(const aBlock: TBlockRecord;
    var findnext: boolean) of object;
  TBlockProcessStepNotify = procedure(const aPos, asize: int64) of object;
  TEndProcessBlockFile = procedure(const aBlockFile: TBlockFile) of object;

  TBlocks = class(TObject)
  private
    // File block events
    fOnStartProcessFiles: TStartFileBlockFoundNotify;
    fOnAfterFileBlockProcessed: TFoundFileBlockNotify;

    fOnMagicBlockFound: TFoundBlockNotify;
    fBlockProcessStep: TBlockProcessStepNotify;
    fEndProcessBlockFile: TEndProcessBlockFile;
    fOnEndProc: TNotifyEvent;
    fOnStartProc: TNotifyEvent;
    fOnBeforeFileBlockProcess: TFoundFileBlockNotify;

    procedure InternalProcessBlock(const aBlockFile: TBlockFile);
  public
    BlocksDir: string;

    constructor Create;

    procedure ParseBlockFiles(const aBlocksDirectory: string);
    procedure ProcessBlock(const aBlockFile: TBlockFile);

    // Inicio y fin del parseo
    property OnStartParsing: TNotifyEvent read fOnStartProc write fOnStartProc;
    property OnEndParsing: TNotifyEvent read fOnEndProc write fOnEndProc;

    // Start process all files
    property OnStartProcessFiles: TStartFileBlockFoundNotify
      read fOnStartProcessFiles write fOnStartProcessFiles;

    // before process a file
    property OnBeforeFileBlockProcess: TFoundFileBlockNotify
      read fOnBeforeFileBlockProcess write fOnBeforeFileBlockProcess;

    // after a processed file
    property OnAfterFileBlockProcessed: TFoundFileBlockNotify
      read fOnAfterFileBlockProcessed write fOnAfterFileBlockProcessed;

    // Block found
    property OnMagicBlockFound: TFoundBlockNotify read fOnMagicBlockFound
      write fOnMagicBlockFound;

    property OnBlockProcessStep: TBlockProcessStepNotify read fBlockProcessStep
      write fBlockProcessStep;
    property OnEndProcessBlockFile: TEndProcessBlockFile
      read fEndProcessBlockFile write fEndProcessBlockFile;
  end;

implementation

uses
  WinApi.Windows,
  SysUtils, dialogs, dateutils,
  MainFormUnit, System.hash,
  inifiles;

constructor TBlocks.Create;
begin
end;

procedure TBlocks.ParseBlockFiles(const aBlocksDirectory: string);
var
  searchResult: TSearchRec;
  aBlockFile: TBlockFile;
  aBlockFiles: tstringlist;
  k: integer;
  next: boolean;
  files : string;
begin
  if Assigned(OnStartParsing) then
    OnStartParsing(self);

  SetCurrentDir(aBlocksDirectory);


  files := 'blk?????.dat';
  files := 'blk00196.dat';

  if findfirst(files, faAnyFile, searchResult) = 0 then
  begin
    aBlockFiles := tstringlist.Create;

    repeat
      aBlockFiles.Add(aBlocksDirectory + '\' + searchResult.Name);
    until findnext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);

    if Assigned(OnStartProcessFiles) then
      OnStartProcessFiles(aBlockFiles);

    for k := 0 to aBlockFiles.Count - 1 do
    begin
      aBlockFile := TBlockFile.Create;
      aBlockFile.aFileName := aBlockFiles[k];

      if Assigned(fOnBeforeFileBlockProcess) then
        fOnBeforeFileBlockProcess(aBlockFile, k + 1, aBlockFiles.Count, next);

      ProcessBlock(aBlockFile);

      // Block found
      if Assigned(OnAfterFileBlockProcessed) then
        OnAfterFileBlockProcessed(aBlockFile, k + 1, aBlockFiles.Count, next);

      aBlockFile.Free;

      if next = false then
        break;
    end;

  end;

  if Assigned(OnEndParsing) then
    OnEndParsing(self);
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
            OnBlockProcessStep(aBlockFile.afs.Position, aBlockFile.afs.Size);

          aBlock := TBlockRecord.Create;

          aBlockFile.afs.Read(aBlock.headerLenght, 4);

          // Read the header fields
          aBlockFile.afs.Read(aBlock.header, HEADERSIZE);

          // Re-read the header to calculate hash
          aBlockFile.afs.Seek(-HEADERSIZE, soCurrent);
          aBlockFile.afs.Read(tb, HEADERSIZE);

          // double header hash
          aBlock.hash :=
            reversehash
            (SHA256ToStr(CalcSHA256(SHA256ToBinaryStr(CalcHeaderSHA256(tb)))));

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
                aBlockFile.afs.Read(aOutput.OutputScript^,
                  aOutput.OutputScriptLength);
              end;

            aBlockFile.afs.Read(alocktime, 4);
            dec(txCount);
          end;

          // Fire the block found event
          if Assigned(OnMagicBlockFound) then
            OnMagicBlockFound(aBlock, cont);

          // Free the block so user must copy or use it and forget
          aBlock.Free;

          state := 0;
        end;

    end;

  end;

  if (cont = true) then
    if Assigned(OnEndProcessBlockFile) then
      OnEndProcessBlockFile(aBlockFile);
end;

procedure TBlocks.ProcessBlock(const aBlockFile: TBlockFile);
begin
  aBlockFile.afs := TBufferedFileStream.Create(aBlockFile.aFileName,
    fmOpenRead);

  try
    InternalProcessBlock(aBlockFile);
  finally
    aBlockFile.afs.Free;
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
