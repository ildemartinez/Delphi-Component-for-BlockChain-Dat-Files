unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  BlocksUnit, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    aBlocks: TBlocks;
    n: uint64;
  protected
    procedure StartFoundFileBlock(const aBlockFiles : tstringlist);
    procedure EndFoundFileBlock(const aBlockFiles : tstringlist);
    procedure FoundBlock(const aBlockFile: TBlockFile);

    procedure FoundMagicBlock(const aBlock: TBlockRecord;
      var findnext: boolean);
    procedure BlockProcessStep(const aPos, aSize: int64);
    procedure EndProcessBlockFile(const aBlockFile: TBlockFile);

  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BlockProcessStep(const aPos, aSize: int64);
begin
  ProgressBar1.Max := aSize;
  ProgressBar1.Position := aPos;
end;

constructor TForm2.Create(Owner: TComponent);
begin
  inherited;

  aBlocks := TBlocks.Create;
  aBlocks.OnStartFileBlockFound := StartFoundFileBlock;
  aBlocks.OnFoundBlock := FoundBlock;
  aBlocks.OnEndFileBlockFound := EndFoundFileBlock;

  aBlocks.OnMagicBlockFound := FoundMagicBlock;

  aBlocks.OnBlockProcessStep := BlockProcessStep;
  aBlocks.OnEndProcessBlockFile := EndProcessBlockFile;

  n := 0;
end;

procedure TForm2.EndFoundFileBlock(const aBlockFiles : tstringlist);
begin
  aBlockFiles.Free;
end;

procedure TForm2.EndProcessBlockFile(const aBlockFile: TBlockFile);
begin

end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  aBlocks.FindBlocks('C:\Users\ilde\AppData\Roaming\Bitcoin\blocks');
end;



procedure TForm2.FoundBlock(const aBlockFile: TBlockFile);
begin
  // Memo1.Lines.Add(FileName);
  aBlocks.ProcessBlock(aBlockFile);

  aBlockFile.Free;
end;

procedure TForm2.FoundMagicBlock(const aBlock: TBlockRecord;
  var findnext: boolean);
{var
  st: string;
  k: Integer; }

begin
  { st := inttostr(aBlock.ninputs) + ' ' + inttostr(aBlock.noutputs) + ' ' +
    datetimetostr(aBlock.time) + ' ' + IntToHex(aBlock.bits) + ' ' +
    inttostr(aBlock.nonce) + ' <' + inttostr(aBlock.nValue) + '> ';
    for k := 0 to 31 do
    begin
    st := st + IntToHex(byte(aBlock.aPreviousBlockHash[k]));
    end;

    Memo1.Lines.Add(st);
  }
  //Memo1.Lines.Add(inttostr(n));
  inc(n);

  Application.ProcessMessages;
  // findnext := false;
end;

procedure TForm2.StartFoundFileBlock(const aBlockFiles : tstringlist);
begin

end;

end.
