unit datamodule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG,
  FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  BlocksUnit, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet;

type
  Tdm = class(TDataModule)
    dbconnection: TFDConnection;
    aQuery: TFDQuery;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
  private
    { Private declarations }
  public
    { Public declarations }
    function InsertBlock(aBlock: TBlockRecord): boolean;
  end;

var
  dm: Tdm;

implementation

uses
  System.DateUtils,
  SeSHA256;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ Tdm }

function Tdm.InsertBlock(aBlock: TBlockRecord): boolean;
var
  aPreviousBlock, aMerkleRoot, aNextBlock: string;
  aTime: TDateTime;
begin
  aQuery.SQL.Clear;

  aPreviousBlock := T32ToString(aBlock.header.aPreviousBlockHash);
             {
  if aPreviousBlock = 0 then
  begin
    aPreviousId := 0;
  end
  else
  begin

  end;        }

  aMerkleRoot := T32ToString(aBlock.header.aMerkleRoot);
  aTime := UnixToDateTime(aBlock.header.time);

  aQuery.SQL.Add
    (format('insert into blocks (hash,amerkleroot,time) values (''%s'',''%s'',''%s'')',
    [aBlock.hash, aMerkleRoot, DateTimeToStr(aTime)]));

  aQuery.ExecSQL;
  aQuery.Close;
end;

end.
