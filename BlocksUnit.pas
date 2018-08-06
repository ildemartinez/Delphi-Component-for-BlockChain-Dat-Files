unit BlocksUnit;

interface

uses
  System.Classes;

type
  T32 = string[32];

  TCrypto = (tcBitcoin);
  TNet = (tnMainNet, tnTestNet);

  TBlockFile = class(TObject)
    aFileName: string;
    afs: TBufferedFileStream;
  end;

  TBlockRecord = record
    n: uint64;
    blocktype: TCrypto;
    network: TNet;

    headerLenght: UInt32;
    aMagic, asize, bits, nonce, versionNumber: UInt32;
    aPreviousBlockHash: T32;
    aMerkleRoot: T32;
    time: TDateTime;

    ninputs, noutputs: uint64;
    nValue: uint64;
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
  k: Integer;
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

    for k := 0 to aBlockFiles.Count - 1 do
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

  aVOUT, atxVersion, aseq: longword;

  aTXID: T32;
  temp: array [0 .. 10240] of byte;
  aTime: cardinal;
  memStart: PAnsiChar;

  alocktime: cardinal;

  varintvalue, k, aScriptSigSize: uint64;

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

          // Header Lenght
          aBlockFile.afs.Read(aBlock.headerLenght, 4);

          // Version number
          aBlockFile.afs.Read(aBlock.versionNumber, 4);

          // Previous block
          aBlockFile.afs.Read(aBlock.aPreviousBlockHash, 32);

          // Merkle root
          aBlockFile.afs.Read(aBlock.aMerkleRoot, 32);

          // Time, bits and nonce
          aBlockFile.afs.Read(aTime, 4);
          aBlock.time := UnixToDateTime(aTime);

          aBlockFile.afs.Read(aBlock.bits, 4);
          aBlockFile.afs.Read(aBlock.nonce, 4);

          // tx count
          varintvalue := ReadVarValue;

          while (varintvalue > 0) do
          begin
            // Read the transaction version
            aBlockFile.afs.Read(atxVersion, 4);
            // Read the inputs
            aBlock.ninputs := ReadVarValue;

            if aBlock.ninputs > 0 then

              for k := 0 to aBlock.ninputs - 1 do
              begin

                aBlockFile.afs.Read(aTXID, 32);
                aBlockFile.afs.Read(aVOUT, 4);

                // aBlockFile.afs.Read(aScriptSigSize, 1);
                aScriptSigSize := ReadVarValue;
                memStart := AllocMem(aScriptSigSize);
                aBlockFile.afs.Read(temp, aScriptSigSize);
                FreeMem(memStart);
                aBlockFile.afs.Read(aseq, 4);
              end;

            // tx out count
            aBlock.noutputs := ReadVarValue;

            if aBlock.noutputs > 0 then
              for k := 0 to aBlock.noutputs - 1 do
              begin

                aBlockFile.afs.Read(aBlock.nValue, 8);
                aScriptSigSize := ReadVarValue;

                // memStart := AllocMem (aScriptSigSize);
                aBlockFile.afs.Read(temp, aScriptSigSize);
                // FreeMem(memstart);
              end;
            aBlockFile.afs.Read(alocktime, 4);
            dec(varintvalue);
          end;

          // Fire the block found event
          if assigned(OnMagicBlockFound) then
            OnMagicBlockFound(aBlock, cont);

          state := 0;

        end;

    end;

  end;
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

end.
