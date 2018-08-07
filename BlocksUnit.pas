unit BlocksUnit;

interface

uses
  System.Classes,
  System.Contnrs;

type
  T32 = array [0 .. 31] of byte;

  TCrypto = (tcBitcoin);
  TNet = (tnMainNet, tnTestNet);

  TBlockFile = class(TObject)
    aFileName: string;
    afs: TBufferedFileStream;
  end;

  TBlockHeader = record
    headerLenght: UInt32;
    versionNumber: UInt32;
    aPreviousBlockHash: T32; // reverse please
    aMerkleRoot: T32; // reverse please
    time: UInt32; // UnixTime
    bits: UInt32;
    nonce: UInt32;
  end;

  TInput = class(TObject)
    aTXID: T32;
    aVOUT: UInt32;
    CoinBase : PByte;

    destructor Destroy; override;
  end;

  TOutput = class(TObject)
    nValue: uint64;
    OutputScript : PByte;

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

    header: TBlockHeader;
    transactions: TBlockTransactions;

    ninputs, noutputs: uint64;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TStartFileBlockFoundNotify = procedure(const aBlockFiles: tstringlist)
    of object;
  TFoundFileBlockNotify = procedure(const aBlockFile: TBlockFile) of object;
  TEndFileBlockFoundNotify = procedure(const aBlockFiles: tstringlist)
    of object;

  TFoundBlockNotify = procedure(const aBlock: TBlockRecord;
    var findnext: boolean) of object;
  TBlockProcessStepNotify = procedure(const aPos, asize: int64) of object;
  TEndProcessBlockFile = procedure(const aBlockFile: TBlockFile) of object;

  TBlocks = class(TObject)
  private
    // File block events
    fOnStartFileBlockFoundNotify: TStartFileBlockFoundNotify;
    fOnFoundBlock: TFoundFileBlockNotify;
    fOnEndFileBlockFoundNotify: TEndFileBlockFoundNotify;

    fOnMagicBlockFound: TFoundBlockNotify;
    fBlockProcessStep: TBlockProcessStepNotify;
    fEndProcessBlockFile: TEndProcessBlockFile;

    procedure InternalProcessBlock(const aBlockFile: TBlockFile);
  public
    BlocksDir: string;

    constructor Create;

    procedure FindBlocks(const aBlocksDirectory: string);
    procedure ProcessBlock(const aBlockFile: TBlockFile);

    property OnStartFileBlockFound: TStartFileBlockFoundNotify
      read fOnStartFileBlockFoundNotify write fOnStartFileBlockFoundNotify;
    property OnFoundBlock: TFoundFileBlockNotify read fOnFoundBlock
      write fOnFoundBlock;
    property OnEndFileBlockFound: TEndFileBlockFoundNotify
      read fOnEndFileBlockFoundNotify write fOnEndFileBlockFoundNotify;

    property OnMagicBlockFound: TFoundBlockNotify read fOnMagicBlockFound
      write fOnMagicBlockFound;

    property OnBlockProcessStep: TBlockProcessStepNotify read fBlockProcessStep
      write fBlockProcessStep;
    property OnEndProcessBlockFile: TEndProcessBlockFile
      read fEndProcessBlockFile write fEndProcessBlockFile;
  end;

function T32ToString(const at32: T32): string;

implementation

uses
  WinApi.Windows,
  SysUtils, dialogs, dateutils;

constructor TBlocks.Create;
begin

end;

procedure TBlocks.FindBlocks(const aBlocksDirectory: string);
var
  searchResult: TSearchRec;
  aBlockFile: TBlockFile;
  aBlockFiles: tstringlist;
  k: integer;
begin
  SetCurrentDir(aBlocksDirectory);

  if findfirst('blk?????.dat', faAnyFile, searchResult) = 0 then
  begin
    aBlockFiles := tstringlist.Create;

    repeat
      aBlockFiles.Add(aBlocksDirectory + '\' + searchResult.Name);
    until findnext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);

    if assigned(OnStartFileBlockFound) then
      OnStartFileBlockFound(aBlockFiles);
    k := 0;
    // for k := 0 to aBlockFiles.Count - 1 do
    begin
      aBlockFile := TBlockFile.Create;
      aBlockFile.aFileName := aBlockFiles[k];

      if assigned(fOnFoundBlock) then
        fOnFoundBlock(aBlockFile);
    end;

    if assigned(OnEndFileBlockFound) then
      OnEndFileBlockFound(aBlockFiles);
  end;
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

  alocktime: UInt32;

  txCount, k, aScriptSigSize: uint64;

  function ReadVarValue: uint64;
  var
    atxCount: UInt8;
    atxCount2: UInt16;
    atxCount4: longword;
  begin
    aBlockFile.afs.Read(atxCount, 1);
    if atxCount < $FD then
      result := atxCount
    else if atxCount = $FD then
    begin
      aBlockFile.afs.Read(atxCount2, 2);
      result := atxCount2;
    end
    else if atxCount = $FE then
    begin
      aBlockFile.afs.Read(atxCount4, 4);
      result := atxCount4;
    end
    else if atxCount = $FF then
    begin
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

          if assigned(OnBlockProcessStep) then
            OnBlockProcessStep(aBlockFile.afs.Position, aBlockFile.afs.Size);

          aBlock := TBlockRecord.Create;

          aBlockFile.afs.Read(aBlock.header, 84);


          // tx count
          txCount := ReadVarValue;
          if txCount > 1 then
            txCount := txCount;

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

                aScriptSigSize := ReadVarValue;

                GetMem(aInput.CoinBase, aScriptSigSize);
                aBlockFile.afs.Read(aInput.CoinBase^, aScriptSigSize);

                // No need store the seq
                aBlockFile.afs.Read(aseq, 4);
              end;

            // tx out count
            aBlock.noutputs := ReadVarValue;

            if aBlock.noutputs > 0 then
              for k := 0 to aBlock.noutputs - 1 do
              begin
                aOutput := aTransaction.outputs.NewOutput;

                aBlockFile.afs.Read(aOutput.nValue, 8);

                aScriptSigSize := ReadVarValue;
                GetMem(aOutput.OutputScript, aScriptSigSize);
                aBlockFile.afs.Read(aOutput.OutputScript^, aScriptSigSize);
              end;
            aBlockFile.afs.Read(alocktime, 4);
            dec(txCount);
          end;

          // Fire the block found event
          if assigned(OnMagicBlockFound) then
            OnMagicBlockFound(aBlock, cont);

          // Free the block so user must copy or use it and forget
          aBlock.Free;

          state := 0;

        end;

    end;

  end;

  if (cont = true) then
    if assigned(OnEndProcessBlockFile) then
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

function T32ToString(const at32: T32): string;
var
  k: integer;
begin
  result := '';
  for k := 0 to 31 do
  begin
    result := IntToHex(byte(at32[k])) + result;
  end;
end;

{ TInput }

destructor TInput.Destroy;
begin
  FreeMem(CoinBase);

  inherited;
end;

{ TOutput }

destructor TOutput.Destroy;
begin
  FreeMem(OutputScript);

  inherited;
end;

end.
