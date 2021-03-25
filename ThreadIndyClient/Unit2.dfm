object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 416
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 369
    Height = 416
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 375
    Top = 8
    Width = 135
    Height = 41
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 375
    Top = 55
    Width = 135
    Height = 41
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Host = '127.0.0.1'
    IPVersion = Id_IPv4
    Port = 10151
    ReadTimeout = 0
    Left = 80
    Top = 48
  end
end
