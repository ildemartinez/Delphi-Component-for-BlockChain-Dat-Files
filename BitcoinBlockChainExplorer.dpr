program BitcoinBlockChainExplorer;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form2},
  BlocksUnit in 'BlocksUnit.pas',
  SeSHA256 in 'SeSHA256.pas',
  datamodule in 'datamodule.pas' {dm: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
