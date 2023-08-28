program BitcoinBlockChainExplorer;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  BlocksUnit in 'BlocksUnit.pas',
  SeSHA256 in 'SeSHA256.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
