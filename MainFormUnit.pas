unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Generics.Collections,

  System.Diagnostics, System.TimeSpan,

  BlocksUnit, System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.ExtCtrls,
  CryptoVirtualTreeUnit;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fBlockFilePath: string;

    aBlocks: TBlocks;
    nblocks: uint64;

    Stopwatch: TStopwatch;
    TimeSpan: TTimeSpan;

  protected
    procedure StartingParsingBlockFiles(Sender: TObject);
    procedure FinishedParsingBlockFiles(Sender: TObject);

    procedure StartProcessFiles(const aBlockFiles: TList<String>);
    // procedure EndFoundFileBlock(const aBlockFiles: TList<String>);

    procedure BeforeProcessAFile(const aBlockFile: TBlockFile; var next: boolean);

    procedure AfterProcessAFile(const aBlockFile: TBlockFile; var next: boolean);

    procedure FoundMagicBlock(const aBlock: TBlockRecord; var findnext: boolean);
    procedure BlockProcessStep(const aPos, aSize: int64);
    procedure EndProcessBlockFile(const aBlockFile: TBlockFile);

  public
    constructor Create(Owner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  datamodule, inifiles, dateutils, SeSHA256, System.Threading;

{$R *.dfm}

procedure TMainForm.BeforeProcessAFile(const aBlockFile: TBlockFile; var next: boolean);
begin
  Memo1.Lines.Add(format('  Start process %s file %d of %d', [aBlockFile.aFileName, aBlockFile.aBlockNumber, aBlockFile.parent.Count]));
end;

procedure TMainForm.BlockProcessStep(const aPos, aSize: int64);
begin
  if (aPos mod 25) = 0 then
    Memo1.Lines.Add(IntToStr(aPos * 100 div aSize) + '%');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  aBlocks.aBlocksDirectory := fBlockFilePath;
  aBlocks.start;
end;

constructor TMainForm.Create(Owner: TComponent);
begin
  inherited;

  aBlocks := TBlocks.Create(true);
  aBlocks.FreeOnTerminate := true;

  // Starts and ends processing all .dat files
  aBlocks.OnStartingParsingBlockfiles := StartingParsingBlockFiles;
  aBlocks.OnFinishedParsingBlockFiles := FinishedParsingBlockFiles;

  // Starts and ends process each .dat file
  aBlocks.OnStartProcessFiles := StartProcessFiles;
  // aBlocks.OnEndProcessBlockFile := EndProcessBlockFile;

  // Stars and ends block
  aBlocks.OnBeforeFileBlockProcess := BeforeProcessAFile;
  aBlocks.OnAfterFileBlockProcessed := AfterProcessAFile;

  aBlocks.OnMagicBlockFound := FoundMagicBlock;

  aBlocks.OnBlockProcessStep := BlockProcessStep;
end;

{ procedure TMainForm.EndFoundFileBlock(const aBlockFiles: TList<String>);
  var
  nbsec: double;
  begin
  TimeSpan := Stopwatch.Elapsed;
  nbsec := nblocks / TimeSpan.TotalSeconds;
  Memo1.Lines.Add(nbsec.ToString);

  aBlockFiles.Free;
  end;
}

procedure TMainForm.FinishedParsingBlockFiles(Sender: TObject);
begin
  Memo1.Lines.Add(' End parsing all files');
end;

procedure TMainForm.EndProcessBlockFile(const aBlockFile: TBlockFile);
begin
  Memo1.Lines.Add('  End processing ' + aBlockFile.aFileName);
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

procedure TMainForm.AfterProcessAFile(const aBlockFile: TBlockFile; var next: boolean);
begin
  Memo1.Lines.Add('  Processed ' + aBlockFile.aFileName);

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


  // exit;

  if (nblocks <> 12 ) then exit;

  Memo1.Lines.Add(' Hash: ' + aBlock.hash);
  Memo1.Lines.Add(' ');
  Memo1.Lines.Add(datetimetostr(Unixtodatetime(aBlock.header.time)) + ' UTC ' + ' Bits: ' + aBlock.header.DifficultyTarget.ToString + ' nonce: ' + aBlock.header.nonce.ToString);
  Memo1.Lines.Add(' Hash: ' + aBlock.hash);
  Memo1.Lines.Add(' Prev. block: ' + T32ToString(aBlock.header.aPreviousBlockHash));
  Memo1.Lines.Add(' MerkleRoot: ' + T32ToString(aBlock.header.aMerkleRoot));
  Memo1.Lines.Add(' Transactions ' + aBlock.transactions.Count.ToString);
  Memo1.Lines.Add(' ');

  for k := 0 to aBlock.transactions.Count - 1 do
  begin
    Memo1.Lines.Add(' Transacción ' + IntToStr(k));
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

end;

procedure TMainForm.StartingParsingBlockFiles(Sender: TObject);
begin
  Memo1.Lines.Add('Start parsing component');
end;

procedure TMainForm.StartProcessFiles(const aBlockFiles: TList<String>);
begin
  Memo1.Lines.Add(' Block files found to process ' + aBlockFiles.Count.ToString);
  nblocks := 0;

  Stopwatch := TStopwatch.StartNew;

end;

end.
