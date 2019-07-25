unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls,

  System.Diagnostics, System.TimeSpan,

  BlocksUnit;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    pbFiles: TProgressBar;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblpblocks: TLabel;
    lblpblock: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    fBlockFilePath: string;

    aBlocks: TBlocks;
    ContinueProcess: boolean;
    nblocks: uint64;

    Stopwatch: TStopwatch;
    TimeSpan: TTimeSpan;

  protected
    procedure StartProc(Sender: TObject);
    procedure EndProc(Sender: TObject);

    procedure StartProcessFiles(const aBlockFiles: tstringlist);
    procedure EndFoundFileBlock(const aBlockFiles: tstringlist);

    procedure BeforeProcessAFile(const aBlockFile: TBlockFile;
      const actualFileBlock, TotalFiles: integer; var next: boolean);

    procedure AfterProcessAFile(const aBlockFile: TBlockFile;
      const actualFileBlock, TotalFiles: integer; var next: boolean);

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
  datamodule, inifiles, dateutils, SeSHA256;

{$R *.dfm}

procedure TForm2.BeforeProcessAFile(const aBlockFile: TBlockFile;
  const actualFileBlock, TotalFiles: integer; var next: boolean);
begin
  Memo1.Lines.Add(format('Start process  %s file %d of %d',
    [aBlockFile.aFileName, actualFileBlock, TotalFiles]));
end;

procedure TForm2.BlockProcessStep(const aPos, aSize: int64);
begin
  if aPos mod 1000 = 0 then
  begin
    ProgressBar1.Max := aSize;
    ProgressBar1.Position := aPos;

    lblpblock.Caption := aPos.ToString + ' / ' + aSize.ToString;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ContinueProcess := false;
end;

constructor TForm2.Create(Owner: TComponent);
var
  aINIFile: TIniFile;
begin

  inherited;

  ContinueProcess := true;

  aBlocks := TBlocks.Create;
  aBlocks.OnStartParsing := StartProc;
  aBlocks.OnEndParsing := EndProc;

  aBlocks.OnStartProcessFiles := StartProcessFiles;
  aBlocks.OnEndProcessBlockFile := EndProcessBlockFile;

  aBlocks.OnBeforeFileBlockProcess := BeforeProcessAFile;
  aBlocks.OnAfterFileBlockProcessed := AfterProcessAFile;

  aBlocks.OnMagicBlockFound := FoundMagicBlock;

  aBlocks.OnBlockProcessStep := BlockProcessStep;

  // Load INI file
  aINIFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\' +
    'config.ini ');
  fBlockFilePath := aINIFile.ReadString('File', 'BlockFilePath', '');

  dm.dbconnection.Connected := true;
end;

procedure TForm2.EndFoundFileBlock(const aBlockFiles: tstringlist);
var
  nbsec: double;
begin
  TimeSpan := Stopwatch.Elapsed;
  nbsec := nblocks / TimeSpan.TotalSeconds;
  Memo1.Lines.Add(nbsec.ToString);

  aBlockFiles.Free;
end;

procedure TForm2.EndProc(Sender: TObject);
begin
  Memo1.Lines.Add('End parsing');
end;

procedure TForm2.EndProcessBlockFile(const aBlockFile: TBlockFile);
begin
  Memo1.Lines.Add('End processing ' + aBlockFile.aFileName);
  ProgressBar1.Position := ProgressBar1.Max;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  aBlocks.ParseBlockFiles(fBlockFilePath);
end;

procedure TForm2.AfterProcessAFile(const aBlockFile: TBlockFile;
  const actualFileBlock, TotalFiles: integer; var next: boolean);
begin
  Memo1.Lines.Add('Processed ' + aBlockFile.aFileName);
  pbFiles.StepIt;

  lblpblocks.Caption := format('%d / %d', [actualFileBlock, TotalFiles]);

  // next := true;
  next := false;
end;

procedure TForm2.FoundMagicBlock(const aBlock: TBlockRecord;
  var findnext: boolean);
var
  k, j, i: integer;
  t: string;
begin
  // Performance
  inc(nblocks);

  // dm.InsertBlock(aBlock);

  // Memo1.Lines.BeginUpdate;
  // Memo1.Lines.Add(' Hash: ' + aBlock.hash);

  Memo1.Lines.Add(' ');
  Memo1.Lines.Add(datetimetostr(Unixtodatetime(aBlock.header.time)) + ' Bits: '
    + aBlock.header.DifficultyTarget.ToString + ' nonce: ' +
    aBlock.header.nonce.ToString);
  Memo1.Lines.Add(' Hash: ' + aBlock.hash);
  Memo1.Lines.Add(' Prev. block: ' +
    T32ToString(aBlock.header.aPreviousBlockHash));
  Memo1.Lines.Add(' MerkleRoot: ' + T32ToString(aBlock.header.aMerkleRoot));
  Memo1.Lines.Add(' Transactions ' + aBlock.transactions.Count.ToString);
  Memo1.Lines.Add(' ');

  for k := 0 to aBlock.transactions.Count - 1 do
  begin
    Memo1.Lines.Add(' Transacción ' + inttostr(k));
    Memo1.Lines.Add(' version ' + aBlock.transactions[k].version.ToString);

    if aBlock.transactions[k].inputs.Count > 0 then
      for j := 0 to aBlock.transactions[k].inputs.Count - 1 do
      begin
        Memo1.Lines.Add(format('  input %s',
          [T32ToString(aBlock.transactions[k].inputs[j].aTXID)]));
        Memo1.Lines.Add(format('  %.8f BTC',
          [(aBlock.transactions[k].inputs[j].aVOUT / 100000000)]));

        t := '';
        for i := 0 to aBlock.transactions[k].inputs[j].CoinBaseLength - 1 do
        begin
          t := t + IntToHex(aBlock.transactions[k].inputs[j].CoinBase[i]);
        end;
        Memo1.Lines.Add('  coinbase: ' + t);
        Memo1.Lines.Add(' ');
      end;

    if aBlock.transactions[k].outputs.Count > 0 then
      for j := 0 to aBlock.transactions[k].outputs.Count - 1 do
      begin
        Memo1.Lines.Add(format('  output %.8f BTC',
          [aBlock.transactions[k].outputs[j].nValue / 100000000]));

        t := '';
        for i := 0 to aBlock.transactions[k].outputs[j]
          .OutputScriptLength - 1 do
        begin
          t := t + IntToHex(aBlock.transactions[k].outputs[j].OutputScript[i]);
        end;
        Memo1.Lines.Add('  outputscript: ' + t);
        Memo1.Lines.Add(' ');
      end;

    Memo1.Lines.Add(' ');
  end;

  // Memo1.Lines.EndUpdate;

  Application.ProcessMessages;
  findnext := ContinueProcess;

  findnext := false;
end;

procedure TForm2.StartProc(Sender: TObject);
begin
  Memo1.Lines.Add('Start parsing');
  lblpblock.Caption := '';
  lblpblocks.Caption := '';
end;

procedure TForm2.StartProcessFiles(const aBlockFiles: tstringlist);
begin
  Memo1.Lines.Add('Block files found to process ' + aBlockFiles.Count.ToString);
  nblocks := 0;

  Stopwatch := TStopwatch.StartNew;

  pbFiles.Min := 0;
  pbFiles.Max := aBlockFiles.Count;
  pbFiles.Step := 1;
end;

end.
