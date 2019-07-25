object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 479
  ClientWidth = 1114
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
    Left = 16
    Top = 429
    Width = 107
    Height = 13
    Caption = 'Processed blocks files:'
  end
  object Label2: TLabel
    Left = 16
    Top = 452
    Width = 102
    Height = 13
    Caption = 'Processed blocks file:'
  end
  object lblpblocks: TLabel
    Left = 496
    Top = 429
    Width = 45
    Height = 13
    Caption = 'lblpblocks'
  end
  object lblpblock: TLabel
    Left = 496
    Top = 452
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 1114
    Height = 337
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitWidth = 1154
  end
  object ProgressBar1: TProgressBar
    Left = 129
    Top = 448
    Width = 352
    Height = 17
    Max = 0
    TabOrder = 1
  end
  object pbFiles: TProgressBar
    Left = 129
    Top = 425
    Width = 352
    Height = 17
    Max = 0
    TabOrder = 2
  end
  object Button1: TButton
    Left = 16
    Top = 359
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
