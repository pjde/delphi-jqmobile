object ServerForm: TServerForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = ' JQM Component Tester'
  ClientHeight = 453
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 6
    Top = 4
    Width = 62
    Height = 13
    Caption = 'Browser Port'
  end
  object Label1: TLabel
    Left = 160
    Top = 4
    Width = 80
    Height = 13
    Caption = 'Web Socket Port'
  end
  object Memo1: TMemo
    Left = 0
    Top = 91
    Width = 469
    Height = 343
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnDblClick = Memo1DblClick
  end
  object Status1: TStatusBar
    Left = 0
    Top = 434
    Width = 469
    Height = 19
    Panels = <
      item
        Text = 'Success...'
        Width = 50
      end>
  end
  object LinkPortTxt: TEdit
    Left = 6
    Top = 17
    Width = 85
    Height = 21
    TabOrder = 2
    OnKeyPress = PortTxtKeyPress
  end
  object Button2: TButton
    Left = 102
    Top = 17
    Width = 43
    Height = 21
    Caption = 'Set'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 6
    Top = 48
    Width = 79
    Height = 27
    Caption = 'Sessions'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 214
    Top = 48
    Width = 105
    Height = 27
    Caption = 'WS Send'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 97
    Top = 48
    Width = 106
    Height = 27
    Caption = 'Mon Enable'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 256
    Top = 17
    Width = 43
    Height = 21
    Caption = 'Set'
    TabOrder = 7
    OnClick = Button5Click
  end
  object WSPortTxt: TEdit
    Left = 160
    Top = 17
    Width = 85
    Height = 21
    TabOrder = 8
    OnKeyPress = PortTxtKeyPress
  end
  object Button6: TButton
    Left = 350
    Top = 14
    Width = 91
    Height = 25
    Caption = 'Misc'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Bar1: TTrackBar
    Left = 328
    Top = 52
    Width = 133
    Height = 21
    Max = 100
    TabOrder = 10
    OnChange = Bar1Change
  end
end
