object dm: Tdm
  OldCreateOrder = False
  Height = 183
  Width = 265
  object dbconnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=btcblockchaindb')
    LoginPrompt = False
    Left = 57
    Top = 20
  end
  object aQuery: TFDQuery
    Connection = dbconnection
    SQL.Strings = (
      '')
    Left = 144
    Top = 88
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 56
    Top = 111
  end
end
