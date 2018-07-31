unit BlocksUnit;

interface

uses
  System.Classes;

type
  TFoundBlockNotify = procedure(Sender: TComponent; const FileName: string)
    of object;
  TEndFoundBlocksNotify = procedure(Sender: TComponent) of object;

  TBlocks = class(TComponent)
  private
    fOnFoundBlock: TFoundBlockNotify;
    fOnEndFoundBlocks: TEndFoundBlocksNotify;
    fOnMagicBlockFound: TEndFoundBlocksNotify;

    procedure InternalProcessBlock(const afs: TBufferedFileStream);
  public
    BlocksDir: string;

    constructor Create(Owner: TComponent); override;

    procedure FindBlocks(const aBlocksDirectory: string);
    procedure ProcessBlock(const aBlockFileName: string);

    property OnFoundBlock: TFoundBlockNotify read fOnFoundBlock
      write fOnFoundBlock;
    property OnMagicBlockFound: TEndFoundBlocksNotify read fOnMagicBlockFound
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
  headerLenght, versionNumber : integer;
begin
  state := 0;

  while afs.Read(car, 1) = 1 do
  begin

    case state of
      0:
        if car = $f9 then
          inc(state);
      1:
        if car = $Be then
          inc(state)
        else
          state := 0;
      2:
        if car = $B4 then
          inc(state)
        else
          state := 0;
      3:
        if car = $d9 then
        begin
          if assigned(OnMagicBlockFound) then
            OnMagicBlockFound(self);

          afs.Read(headerLenght, 4);
          headerLenght := system.Swap(headerLenght);
          afs.Read(versionNumber,4);

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
