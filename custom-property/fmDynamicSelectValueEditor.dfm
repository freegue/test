object formDynamicSelectValueEditor: TformDynamicSelectValueEditor
  Left = 219
  Top = 111
  BorderStyle = bsDialog
  Caption = #52264#45800' '#48169#48277
  ClientHeight = 312
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 7
    Top = 5
    Width = 284
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
      Caption = #49440#53469' '#45824#49345
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 272
    Width = 298
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 298
      Height = 10
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 274
    end
    object ButtonOK: TButton
      Left = 55
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
      Left = 151
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
