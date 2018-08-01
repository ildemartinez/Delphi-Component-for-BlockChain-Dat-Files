unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  BlocksUnit, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    aBlocks: TBlocks;

  protected
    procedure FoundBlock(Sender: TComponent; const FileName: string);
    procedure FoundAllBlocks(Sender: TComponent);
    procedure FoundMagicBlock(Sender: TComponent; const aBlock: TBlockRecord;
      var findnext: boolean);

  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

constructor TForm2.Create(Owner: TComponent);
begin
  inherited;

  aBlocks := TBlocks.Create(self);
  aBlocks.OnFoundBlock := FoundBlock;
  aBlocks.OnEndFoundBlocks := FoundAllBlocks;
  aBlocks.OnMagicBlockFound := FoundMagicBlock;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin

  aBlocks.FindBlocks('C:\Users\ilde\AppData\Roaming\Bitcoin\blocks');
end;

procedure TForm2.FoundAllBlocks(Sender: TComponent);
begin

end;

procedure TForm2.FoundBlock(Sender: TComponent; const FileName: string);
begin
  Memo1.Lines.Add(FileName);
  aBlocks.ProcessBlock(FileName);
end;

procedure TForm2.FoundMagicBlock(Sender: TComponent; const aBlock: TBlockRecord;
  var findnext: boolean);
var
  st: string;
  k: Integer;
begin
  st := inttostr(aBlock.ninputs) + ' ' + inttostr(aBlock.noutputs) + ' ' +
    datetimetostr(aBlock.time) + ' ' + IntToHex(aBlock.bits) + ' ' +
    inttostr(aBlock.nonce) + ' <' + inttostr(aBlock.nValue) + '> ';
  for k := 0 to 31 do
  begin
    st := st + IntToHex(byte(aBlock.aPreviousBlockHash[k]));
  end;

  Memo1.Lines.Add(st);

  Application.ProcessMessages;
  //findnext := false;
end;

end.
