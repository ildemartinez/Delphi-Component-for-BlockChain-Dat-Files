object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 344
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 277
    Width = 107
    Height = 13
    Caption = 'Processed blocks files:'
  end
  object Label2: TLabel
    Left = 40
    Top = 300
    Width = 102
    Height = 13
    Caption = 'Processed blocks file:'
  end
  object lblpblocks: TLabel
    Left = 520
    Top = 277
    Width = 45
    Height = 13
    Caption = 'lblpblocks'
  end
  object lblpblock: TLabel
    Left = 520
    Top = 300
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Memo1: TMemo
    Left = 40
    Top = 24
    Width = 609
    Height = 177
    TabOrder = 0
  end
  object ProgressBar1: TProgressBar
    Left = 153
    Top = 296
    Width = 352
    Height = 17
    Position = 45
    TabOrder = 1
  end
  object pbFiles: TProgressBar
    Left = 153
    Top = 273
    Width = 352
    Height = 17
    Position = 45
    TabOrder = 2
  end
  object Button1: TButton
    Left = 40
    Top = 207
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
