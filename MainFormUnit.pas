unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  BlocksUnit, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    aBlocks : TBlocks;

  protected
    procedure FoundBlock(Sender : TComponent; const FileName : string);
    procedure FoundAllBlocks(Sender : TComponent);
    procedure FoundMagicBlock(Sender: TComponent; const aBlock: TBlockRecord);

  public
    { Public declarations }
    constructor Create(Owner : TComponent); override;
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
  memo1.Lines.Add(FileName);
  aBlocks.ProcessBlock(Filename);
end;

procedure TForm2.FoundMagicBlock(Sender: TComponent; const aBlock: TBlockRecord);
begin
  Memo1.Lines.Add(inttostr(aBlock.versionNumber));
end;

end.
