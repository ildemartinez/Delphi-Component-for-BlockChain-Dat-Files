unit BlocksUnit;

interface

uses
  System.Classes;

type
  T32 = string[32];

  TBlockRecord = record
    aMagic, asize, bits, nonce, headerLenght, versionNumber: cardinal;
    aPreviousBlockHash: T32;
    aMerkleRoot: T32;
    time: TDateTime;

    ninputs, noutputs: byte;
    nValue: UInt64;
  end;

  TFoundFileBlockNotify = procedure(Sender: TComponent; const FileName: string)
    of object;
  TFoundBlockNotify = procedure(Sender: TComponent; const aBlock: TBlockRecord;
    var findnext: boolean) of object;

  TEndFoundBlocksNotify = procedure(Sender: TComponent) of object;

  TBlocks = class(TComponent)
  private
    fOnFoundBlock: TFoundFileBlockNotify;
    fOnEndFoundBlocks: TEndFoundBlocksNotify;
    fOnMagicBlockFound: TFoundBlockNotify;

    procedure InternalProcessBlock(const afs: TBufferedFileStream);
  public
    BlocksDir: string;

    constructor Create(Owner: TComponent); override;

    procedure FindBlocks(const aBlocksDirectory: string);
    procedure ProcessBlock(const aBlockFileName: string);

    property OnFoundBlock: TFoundFileBlockNotify read fOnFoundBlock
      write fOnFoundBlock;
    property OnMagicBlockFound: TFoundBlockNotify read fOnMagicBlockFound
      write fOnMagicBlockFound;
    property OnEndFoundBlocks: TEndFoundBlocksNotify read fOnEndFoundBlocks
      write fOnEndFoundBlocks;
  end;

implementation

uses
  WinApi.Windows,
  SysUtils, dialogs, dateutils;

constructor TBlocks.Create(Owner: TComponent);
begin
  inherited;

end;

procedure TBlocks.FindBlocks(const aBlocksDirectory: string);
var
  searchResult: TSearchRec;
begin
  SetCurrentDir(aBlocksDirectory);

  if findfirst('blk?????.dat', faAnyFile, searchResult) = 0 then
  begin
    repeat
      if assigned(fOnFoundBlock) then
        fOnFoundBlock(self, aBlocksDirectory + '\' + searchResult.Name);

    until findnext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);

    if assigned(fOnEndFoundBlocks) then
      fOnEndFoundBlocks(self);
  end;
end;

procedure TBlocks.InternalProcessBlock(const afs: TBufferedFileStream);
var
  k: integer;
  state, car: byte;

  aBlock: TBlockRecord;
  cont: boolean;

  aMagic, asize, time, bits, nonce, headerLenght, versionNumber, aVOUT,
    atxVersion, aseq: cardinal;
  atxCount, inputCount, aScriptSigSize: byte;
  aTXID: T32;
  temp: string[255];
  aTime: cardinal;

  alocktime: cardinal;
begin
  state := 0;
  cont := true;

  while (cont = true) and (afs.Read(car, 1) = 1) do
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

          // Header Lenght
          afs.Read(headerLenght, 4);

          // Version number
          afs.Read(aBlock.versionNumber, 4);

          // Previous block
          afs.Read(aBlock.aPreviousBlockHash, 32);

          // Merkle root
          afs.Read(aBlock.aMerkleRoot, 32);

          // Time, bits and nonce
          afs.Read(aTime, 4);
          aBlock.time := UnixToDateTime(aTime);

          afs.Read(aBlock.bits, 4);
          afs.Read(aBlock.nonce, 4);
          //

          // tx count
          afs.Read(atxCount, 1);
          if atxCount <= $FC then
          begin
            // Read the transaction version
            afs.Read(atxVersion, 4);
            // Read the inputs
            afs.Read(aBlock.ninputs, 1);

            afs.Read(aTXID, 32);
            afs.Read(aVOUT, 4);

            afs.Read(aScriptSigSize, 1);
            afs.Read(temp, aScriptSigSize);
            afs.Read(aseq, 4);

            afs.Read(aBlock.noutputs, 1);
            afs.Read(aBlock.nValue, 8);
            afs.Read(aScriptSigSize, 1);
            afs.Read(temp, aScriptSigSize);
            afs.Read(alocktime, 4);
          end
          else
            showmessage('uno');

          // Fire the block found event
          if assigned(OnMagicBlockFound) then
            OnMagicBlockFound(self, aBlock, cont);

          // showmessage(inttostr(versionnumber));
          inc(state);
          state := 0;

        end;
      4:
        begin

        end;

    end;

  end;

end;

procedure TBlocks.ProcessBlock(const aBlockFileName: string);
var
  afs: TBufferedFileStream;
begin
  afs := TBufferedFileStream.Create(aBlockFileName, fmOpenRead);

  try
    InternalProcessBlock(afs);
  finally
    afs.Free;
  end;

end;

end.
