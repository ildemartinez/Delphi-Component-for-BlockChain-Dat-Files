object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 434
  ClientWidth = 967
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
  object Memo1: TMemo
    Left = 40
    Top = 24
    Width = 905
    Height = 177
    TabOrder = 0
  end
  object ProgressBar1: TProgressBar
    Left = 40
    Top = 400
    Width = 529
    Height = 17
    Position = 45
    TabOrder = 1
  end
  object pbFiles: TProgressBar
    Left = 40
    Top = 232
    Width = 529
    Height = 17
    Position = 45
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
