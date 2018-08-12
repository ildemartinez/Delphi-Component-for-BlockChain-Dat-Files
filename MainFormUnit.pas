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
    pbFiles: TProgressBar;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    aBlocks: TBlocks;
    // n: uint64;
  protected
    procedure StartFoundFileBlock(const aBlockFiles: tstringlist);
    procedure EndFoundFileBlock(const aBlockFiles: tstringlist);
    procedure FoundBlock(const aBlockFile: TBlockFile; var next: boolean);

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

uses
  dateutils;
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
end;

procedure TForm2.EndFoundFileBlock(const aBlockFiles: tstringlist);
begin
  aBlockFiles.Free;
end;

procedure TForm2.EndProcessBlockFile(const aBlockFile: TBlockFile);
begin
  Memo1.Lines.Add('End processing ' + aBlockFile.aFileName);
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  aBlocks.FindBlocks('C:\Users\ilde\AppData\Roaming\Bitcoin\blocks');
end;

procedure TForm2.FoundBlock(const aBlockFile: TBlockFile; var next: boolean);
begin
  Memo1.Lines.Add(aBlockFile.aFileName);
  pbFiles.StepIt;

  aBlocks.ProcessBlock(aBlockFile);
  aBlockFile.Free;

  next := true;
end;

procedure TForm2.FoundMagicBlock(const aBlock: TBlockRecord;
  var findnext: boolean);
var
  k, j, i: Integer;
  t: ansistring;
begin
                      findnext := false;
                      exit;

  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Add(datetimetostr(Unixtodatetime(aBlock.header.time)) + ' Bits: '
    + aBlock.header.DifficultyTarget.ToString + ' nonce: ' + aBlock.header.nonce.ToString);
  Memo1.Lines.Add(' Prev. block: ' +
    T32ToString(aBlock.header.aPreviousBlockHash));
  Memo1.Lines.Add(' MerkleRoot: ' + T32ToString(aBlock.header.aMerkleRoot));

  Memo1.Lines.Add(' Transactions ' + aBlock.transactions.Count.ToString);

  for k := 0 to aBlock.transactions.Count - 1 do
  begin
    Memo1.Lines.Add(' version ' + aBlock.transactions[k].version.ToString);

    if aBlock.transactions[k].inputs.Count > 0 then
      for j := 0 to aBlock.transactions[k].inputs.Count - 1 do
      begin
        Memo1.Lines.Add('  input ' + T32ToString(aBlock.transactions[k].inputs
          [j].aTXID) + ' ' + aBlock.transactions[k].inputs[j].aVOUT.ToString);

        t := '';
        for i := 0 to aBlock.transactions[k].inputs[j].CoinBaseLength - 1 do
        begin
          t := t + IntToHex(aBlock.transactions[k].inputs[j].CoinBase[i]);
        end;
        Memo1.Lines.Add(' Coinbase: '+t);

      end;

    if aBlock.transactions[k].outputs.Count > 0 then
      for j := 0 to aBlock.transactions[k].outputs.Count - 1 do
      begin
        Memo1.Lines.Add('  output ' + aBlock.transactions[k].outputs[j]
          .nValue.ToString);

           t := '';
        for i := 0 to aBlock.transactions[k].outputs[j].OutputScriptLength - 1 do
        begin
          t := t + IntToHex(aBlock.transactions[k].outputs[j].OutputScript[i]);
        end;
        Memo1.Lines.Add(' Outputscript: '+t);
      end;
  end;

  Memo1.Lines.EndUpdate;

  Application.ProcessMessages;
  findnext := false;
end;

procedure TForm2.StartFoundFileBlock(const aBlockFiles: tstringlist);
begin
  pbFiles.Min := 0;
  pbFiles.Max := aBlockFiles.Count;
  pbFiles.Step := 1;
end;

end.
