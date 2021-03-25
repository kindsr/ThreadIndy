object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 441
    Height = 299
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
  end
  object BTN_ServerStart: TButton
    Left = 447
    Top = 8
    Width = 180
    Height = 49
    Caption = 'BTN_ServerStart'
    TabOrder = 1
    OnClick = BTN_ServerStartClick
  end
  object BTN_ServerStop: TButton
    Left = 447
    Top = 63
    Width = 180
    Height = 49
    Caption = 'BTN_ServerStop'
    TabOrder = 2
    OnClick = BTN_ServerStopClick
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 51001
    OnExecute = IdTCPServer1Execute
    Left = 40
    Top = 32
  end
  object IdAntiFreeze1: TIdAntiFreeze
    Left = 120
    Top = 40
  end
end
