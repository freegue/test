object FormPropertyPopup: TFormPropertyPopup
  Left = 201
  Top = 132
  BorderStyle = bsNone
  Caption = ' '
  ClientHeight = 71
  ClientWidth = 220
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Pitch = fpVariable
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButton: TPanel
    Left = 0
    Top = 36
    Width = 220
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    Visible = False
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 0
      Top = 3
      Width = 220
      Height = 10
      Margins.Left = 0
      Margins.Right = 0
      Align = alTop
      Shape = bsTopLine
      ExplicitTop = 0
      ExplicitWidth = 274
    end
    object ButtonOK: TButton
      Left = 57
      Top = 9
      Width = 50
      Height = 21
      Caption = #54869#51064
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 113
      Top = 9
      Width = 50
      Height = 21
      Caption = #52712#49548
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
end
