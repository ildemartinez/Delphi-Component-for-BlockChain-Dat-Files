unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls,

  System.Diagnostics, System.TimeSpan,

  BlocksUnit, System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.ExtCtrls,
  CryptoVirtualTreeUnit;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    fCryptoVirtualTree: TCryptoVirtualTree;

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

    procedure BeforeProcessAFile(const aBlockFile: TBlockFile; const actualFileBlock, TotalFiles: integer; var next: boolean);

    procedure AfterProcessAFile(const aBlockFile: TBlockFile; const actualFileBlock, TotalFiles: integer; var next: boolean);

    procedure FoundMagicBlock(const aBlock: TBlockRecord; var findnext: boolean);
    procedure BlockProcessStep(const aPos, aSize: int64);
    procedure EndProcessBlockFile(const aBlockFile: TBlockFile);

  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  datamodule, inifiles, dateutils, SeSHA256;

{$R *.dfm}

procedure TMainForm.BeforeProcessAFile(const aBlockFile: TBlockFile; const actualFileBlock, TotalFiles: integer; var next: boolean);
begin
  Memo1.Lines.Add(format('Start process  %s file %d of %d', [aBlockFile.aFileName, actualFileBlock, TotalFiles]));
end;

procedure TMainForm.BlockProcessStep(const aPos, aSize: int64);
begin
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ContinueProcess := false;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  aBlocks.ParseBlockFiles(fBlockFilePath);
end;

constructor TMainForm.Create(Owner: TComponent);
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
end;

procedure TMainForm.EndFoundFileBlock(const aBlockFiles: tstringlist);
var
  nbsec: double;
begin
  TimeSpan := Stopwatch.Elapsed;
  nbsec := nblocks / TimeSpan.TotalSeconds;
  Memo1.Lines.Add(nbsec.ToString);

  aBlockFiles.Free;
end;

procedure TMainForm.EndProc(Sender: TObject);
begin
  Memo1.Lines.Add('End parsing');

end;

procedure TMainForm.EndProcessBlockFile(const aBlockFile: TBlockFile);
begin
  Memo1.Lines.Add('End processing ' + aBlockFile.aFileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  aFileDir: string;
  aINIFile: TIniFile;

begin
  // Create data directories
  aFileDir := ExtractFilePath(Application.ExeName);

  if DirectoryExists(aFileDir + '\data\') = false then
    CreateDir(aFileDir + '\data\');

  if DirectoryExists(aFileDir + '\btc') = false then
    CreateDir(aFileDir + '\btc');

  // Load settings from IniFile
  aINIFile := TIniFile.Create(aFileDir + '\config.ini');
  fBlockFilePath := aINIFile.ReadString('File', 'BlockFilePath', '');
  aINIFile.Free;
end;

procedure TMainForm.AfterProcessAFile(const aBlockFile: TBlockFile; const actualFileBlock, TotalFiles: integer; var next: boolean);
begin
  Memo1.Lines.Add('Processed ' + aBlockFile.aFileName);

  next := true;
  // next := false;
end;

procedure TMainForm.FoundMagicBlock(const aBlock: TBlockRecord; var findnext: boolean);
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
  Memo1.Lines.Add(datetimetostr(Unixtodatetime(aBlock.header.time)) + ' Bits: ' + aBlock.header.DifficultyTarget.ToString + ' nonce: ' + aBlock.header.nonce.ToString);
  Memo1.Lines.Add(' Hash: ' + aBlock.hash);
  Memo1.Lines.Add(' Prev. block: ' + T32ToString(aBlock.header.aPreviousBlockHash));
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
        Memo1.Lines.Add(format('  input %s', [T32ToString(aBlock.transactions[k].inputs[j].aTXID)]));
        Memo1.Lines.Add(format('  %.8f BTC', [(aBlock.transactions[k].inputs[j].aVOUT / 100000000)]));

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
        Memo1.Lines.Add(format('  output %.8f BTC', [aBlock.transactions[k].outputs[j].nValue / 100000000]));

        t := '';
        for i := 0 to aBlock.transactions[k].outputs[j].OutputScriptLength - 1 do
        begin
          t := t + IntToHex(aBlock.transactions[k].outputs[j].OutputScript[i]);
        end;
        Memo1.Lines.Add('  outputscript: ' + t);
        Memo1.Lines.Add(' ');
      end;

    Memo1.Lines.Add(' ');
  end;

  // Memo1.Lines.EndUpdate;

  findnext := ContinueProcess;
 // findnext := false;

end;

procedure TMainForm.StartProc(Sender: TObject);
begin
  Memo1.Lines.Add('Start parsing');
end;

procedure TMainForm.StartProcessFiles(const aBlockFiles: tstringlist);
begin
  Memo1.Lines.Add('Block files found to process ' + aBlockFiles.Count.ToString);
  nblocks := 0;

  Stopwatch := TStopwatch.StartNew;

end;

end.
