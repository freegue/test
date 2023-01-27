object formIPPortValueEditor: TformIPPortValueEditor
  Left = 219
  Top = 111
  BorderStyle = bsDialog
  Caption = 'IP'#44050' '#51077#47141
  ClientHeight = 345
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonInitial: TButton
    Left = 7
    Top = 273
    Width = 75
    Height = 25
    Caption = #52488#44592#54868
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
    OnClick = ButtonInitialClick
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 7
    Top = 5
    Width = 387
    Height = 260
    Margins.Left = 7
    Margins.Top = 5
    Margins.Right = 7
    Margins.Bottom = 5
    ActivePage = TabSheet
    Align = alTop
    TabOrder = 1
    TabStop = False
    object TabSheet: TTabSheet
      Caption = #44050' '#51221#48372
      object Label1: TLabel
        Left = 15
        Top = 25
        Width = 55
        Height = 13
        AutoSize = False
        Caption = #49884#51089' '#44050' : '
      end
      object Label2: TLabel
        Left = 15
        Top = 50
        Width = 55
        Height = 13
        AutoSize = False
        Caption = #51333#47308' '#44050' : '
      end
      object Bevel2: TBevel
        Left = 81
        Top = 10
        Width = 295
        Height = 5
        Shape = bsTopLine
      end
      object Bevel3: TBevel
        Left = 76
        Top = 93
        Width = 300
        Height = 10
        Shape = bsTopLine
      end
      object EditFrom: TEdit
        Left = 75
        Top = 21
        Width = 300
        Height = 21
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 1
        OnEnter = EditFromEnter
      end
      object RadioButtonMulti: TRadioButton
        Left = 5
        Top = 86
        Width = 70
        Height = 17
        Caption = #45796#51473#51077#47141
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = #47569#51008' '#44256#46357
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = RadioButtonMultiClick
      end
      object RadioButtonRange: TRadioButton
        Left = 5
        Top = 3
        Width = 70
        Height = 17
        Caption = #45800#51068#48276#50948
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonRangeClick
      end
      object EditTo: TEdit
        Left = 75
        Top = 49
        Width = 300
        Height = 21
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 2
        OnEnter = EditToEnter
      end
      object ListBoxMulti: TListBox
        Left = 15
        Top = 109
        Width = 299
        Height = 109
        TabStop = False
        ImeName = 'Microsoft Office IME 2007'
        ItemHeight = 13
        TabOrder = 4
        OnKeyDown = ListBoxMultiKeyDown
      end
      object ButtonAdd: TButton
        Left = 320
        Top = 109
        Width = 55
        Height = 25
        Caption = #45800#51068#52628#44032
        TabOrder = 5
        OnClick = ButtonAddClick
      end
      object ButtonDel: TButton
        Left = 320
        Top = 169
        Width = 55
        Height = 25
        Caption = #49440#53469#49325#51228
        TabOrder = 6
        OnClick = ButtonDelClick
      end
      object ButtonClear: TButton
        Left = 320
        Top = 193
        Width = 55
        Height = 25
        Caption = #51204#52404#49325#51228
        TabOrder = 7
        OnClick = ButtonClearClick
      end
      object ButtonAddRange: TButton
        Left = 320
        Top = 133
        Width = 55
        Height = 25
        Caption = #48276#50948#52628#44032
        TabOrder = 8
        OnClick = ButtonAddRangeClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 305
    Width = 401
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 401
      Height = 10
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 274
    end
    object ButtonOK: TButton
      Left = 210
      Top = 8
      Width = 88
      Height = 26
      Caption = #54869#51064
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 306
      Top = 8
      Width = 88
      Height = 26
      Cancel = True
      Caption = #52712#49548
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
end
