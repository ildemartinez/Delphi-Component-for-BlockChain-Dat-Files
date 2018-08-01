unit BlocksUnit;

interface

uses
  System.Classes;

type
  TBlockRecord = record
    aMagic, asize, time, bits, nonce, headerLenght, versionNumber: cardinal;
      aPreviousBlockHash: string[32];
  aMerkleRoot: string[32];
  end;

  TFoundFileBlockNotify = procedure(Sender: TComponent; const FileName: string)
    of object;
  TFoundBlockNotify = procedure(Sender: TComponent; const aBlock: TBlockRecord)
    of object;

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
  SysUtils, dialogs;

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

    until FindNext(searchResult) <> 0;

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

  atime: tdatetime;

  aBlock: TBlockRecord;

  aMagic, asize, time, bits, nonce, headerLenght, versionNumber: cardinal;
  atxCount: byte;
begin
  state := 0;

  while afs.Read(car, 1) = 1 do
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
          afs.Read(time, 4);
          afs.Read(bits, 4);
          afs.Read(nonce, 4);

          // tx count
          afs.Read(atxCount, 1);
          if atxCount <= $FC then
          begin

          end;

          // Fire the block found event
          if assigned(OnMagicBlockFound) then
            OnMagicBlockFound(self, aBlock);

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
  aFileHandle, aFileLength, iBytesRead: integer;
  Buffer: Pchar;
  car: byte;
  k: integer;
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
