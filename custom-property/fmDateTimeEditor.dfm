object formDateTimeEditor: TformDateTimeEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #45216#51676' '#49440#53469
  ClientHeight = 295
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxStart: TGroupBox
    Left = 5
    Top = 5
    Width = 230
    Height = 200
    Caption = #49884#51089#51068
    TabOrder = 0
    object CalendarFrom: TMonthCalendar
      AlignWithMargins = True
      Left = 5
      Top = 15
      Width = 220
      Height = 155
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alTop
      CalColors.BackColor = clBtnFace
      CalColors.TitleBackColor = 8404992
      CalColors.MonthBackColor = clBtnFace
      CalColors.TrailingTextColor = clGray
      Date = 40134.716344386570000000
      ImeName = 'Microsoft Office IME 2007'
      ParentShowHint = False
      ShowHint = True
      ShowToday = False
      TabOrder = 0
      OnClick = CalendarFromClick
    end
    object DateTimeFrom: TDateTimePicker
      AlignWithMargins = True
      Left = 5
      Top = 174
      Width = 220
      Height = 21
      Align = alBottom
      Date = 39911.863420254630000000
      Time = 39911.863420254630000000
      DateFormat = dfLong
      ImeName = 'Microsoft Office IME 2007'
      Kind = dtkTime
      TabOrder = 1
      OnChange = CalendarFromClick
    end
  end
  object GroupBoxEnd: TGroupBox
    Left = 239
    Top = 5
    Width = 230
    Height = 200
    Caption = #51333#47308#51068' '
    TabOrder = 1
    object CalendarTo: TMonthCalendar
      AlignWithMargins = True
      Left = 5
      Top = 15
      Width = 220
      Height = 155
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alTop
      CalColors.BackColor = clBtnFace
      CalColors.TitleBackColor = 8404992
      CalColors.MonthBackColor = clBtnFace
      CalColors.TrailingTextColor = clGray
      Date = 40138.716344502320000000
      ImeName = 'Microsoft Office IME 2007'
      ParentShowHint = False
      ShowHint = True
      ShowToday = False
      TabOrder = 0
      OnClick = CalendarToClick
    end
    object DateTimeTo: TDateTimePicker
      AlignWithMargins = True
      Left = 5
      Top = 174
      Width = 220
      Height = 21
      Align = alBottom
      Date = 39911.863420254630000000
      Time = 39911.863420254630000000
      DateFormat = dfLong
      ImeName = 'Microsoft Office IME 2007'
      Kind = dtkTime
      TabOrder = 1
      OnChange = CalendarFromClick
    end
  end
  object RadioGroupRange: TRadioGroup
    AlignWithMargins = True
    Left = 5
    Top = 209
    Width = 464
    Height = 41
    Margins.Left = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Caption = #48276#50948#51648#51221
    Columns = 4
    ItemIndex = 3
    Items.Strings = (
      #50724#45720
      #51060#48264#51452
      #51060#48264#45804
      #51076#51032#51648#51221)
    TabOrder = 3
    OnClick = RadioGroupRangeClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 255
    Width = 474
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 474
      Height = 10
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 274
    end
    object ButtonOK: TButton
      Left = 133
      Top = 8
      Width = 100
      Height = 25
      Caption = #54869#51064
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 239
      Top = 8
      Width = 100
      Height = 25
      Cancel = True
      Caption = #52712#49548
      ModalResult = 2
      TabOrder = 1
    end
  end
end
