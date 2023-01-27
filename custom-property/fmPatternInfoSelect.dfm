object fmPatternSelect: TfmPatternSelect
  Left = 0
  Top = 0
  Caption = #53456#51648' '#51221#52293' '#49440#53469
  ClientHeight = 598
  ClientWidth = 882
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelCondition: TPanel
    Left = 0
    Top = 321
    Width = 882
    Height = 236
    Align = alClient
    Caption = #51312#44148' '#44288#44228' '#54364#49884
    TabOrder = 0
  end
  object PanelSoureData: TPanel
    Left = 0
    Top = 0
    Width = 882
    Height = 321
    Align = alTop
    BevelOuter = bvNone
    Caption = #51312#44148#51004#47196' '#49440#53469#46112' '#51221#48372
    TabOrder = 1
    object TreeView1: TTreeView
      Left = 0
      Top = 0
      Width = 200
      Height = 280
      Align = alLeft
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnDblClick = TreeView1DblClick
      OnMouseDown = TreeView1MouseDown
      Items.NodeData = {
        0301000000260000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00050000000104D0D0C0C915C845CC280000000000000000000000FFFFFFFFFF
        FFFFFF00000000000000000000000001051CC144BEA4C270AC80BD2800000000
        00000000000000FFFFFFFFFFFFFFFF00000000000000000000000001051CC144
        BEA4C2F5ACA9AC260000000000000000000000FFFFFFFFFFFFFFFF0000000000
        00000000000000010428D334D114BEEDB7260000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000000000000010415C8F4BC18C2D1C92E00000000
        00000000000000FFFFFFFFFFFFFFFF000000000000000000000000010804D55C
        B8A0D15CCF2000E8CD7DC510C8}
    end
    object mGrid: TAdvStringGrid
      Left = 200
      Top = 0
      Width = 682
      Height = 280
      Cursor = crDefault
      Align = alClient
      ColCount = 4
      DrawingStyle = gdsClassic
      RowCount = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      OnDblClick = mGridDblClick
      OnMouseDown = mGridMouseDown
      HoverRowCells = [hcNormal, hcSelected]
      ActiveCellFont.Charset = DEFAULT_CHARSET
      ActiveCellFont.Color = clWindowText
      ActiveCellFont.Height = -11
      ActiveCellFont.Name = 'Tahoma'
      ActiveCellFont.Style = [fsBold]
      ColumnHeaders.Strings = (
        'NO'
        #44277#44201#50976#54805
        #44277#44201#53076#46300
        #44277#44201#47749)
      ControlLook.FixedGradientHoverFrom = clGray
      ControlLook.FixedGradientHoverTo = clWhite
      ControlLook.FixedGradientDownFrom = clGray
      ControlLook.FixedGradientDownTo = clSilver
      ControlLook.DropDownHeader.Font.Charset = DEFAULT_CHARSET
      ControlLook.DropDownHeader.Font.Color = clWindowText
      ControlLook.DropDownHeader.Font.Height = -11
      ControlLook.DropDownHeader.Font.Name = 'Tahoma'
      ControlLook.DropDownHeader.Font.Style = []
      ControlLook.DropDownHeader.Visible = True
      ControlLook.DropDownHeader.Buttons = <>
      ControlLook.DropDownFooter.Font.Charset = DEFAULT_CHARSET
      ControlLook.DropDownFooter.Font.Color = clWindowText
      ControlLook.DropDownFooter.Font.Height = -11
      ControlLook.DropDownFooter.Font.Name = 'Tahoma'
      ControlLook.DropDownFooter.Font.Style = []
      ControlLook.DropDownFooter.Visible = True
      ControlLook.DropDownFooter.Buttons = <>
      Filter = <>
      FilterDropDown.Font.Charset = DEFAULT_CHARSET
      FilterDropDown.Font.Color = clWindowText
      FilterDropDown.Font.Height = -11
      FilterDropDown.Font.Name = 'Tahoma'
      FilterDropDown.Font.Style = []
      FilterDropDown.TextChecked = 'Checked'
      FilterDropDown.TextUnChecked = 'Unchecked'
      FilterDropDownClear = '(All)'
      FilterEdit.TypeNames.Strings = (
        'Starts with'
        'Ends with'
        'Contains'
        'Not contains'
        'Equal'
        'Not equal'
        'Larger than'
        'Smaller than'
        'Clear')
      FixedRowHeight = 22
      FixedFont.Charset = DEFAULT_CHARSET
      FixedFont.Color = clWindowText
      FixedFont.Height = -11
      FixedFont.Name = 'Tahoma'
      FixedFont.Style = [fsBold]
      FloatFormat = '%.2f'
      HoverButtons.Buttons = <>
      HoverButtons.Position = hbLeftFromColumnLeft
      PrintSettings.DateFormat = 'dd/mm/yyyy'
      PrintSettings.Font.Charset = DEFAULT_CHARSET
      PrintSettings.Font.Color = clWindowText
      PrintSettings.Font.Height = -11
      PrintSettings.Font.Name = 'Tahoma'
      PrintSettings.Font.Style = []
      PrintSettings.FixedFont.Charset = DEFAULT_CHARSET
      PrintSettings.FixedFont.Color = clWindowText
      PrintSettings.FixedFont.Height = -11
      PrintSettings.FixedFont.Name = 'Tahoma'
      PrintSettings.FixedFont.Style = []
      PrintSettings.HeaderFont.Charset = DEFAULT_CHARSET
      PrintSettings.HeaderFont.Color = clWindowText
      PrintSettings.HeaderFont.Height = -11
      PrintSettings.HeaderFont.Name = 'Tahoma'
      PrintSettings.HeaderFont.Style = []
      PrintSettings.FooterFont.Charset = DEFAULT_CHARSET
      PrintSettings.FooterFont.Color = clWindowText
      PrintSettings.FooterFont.Height = -11
      PrintSettings.FooterFont.Name = 'Tahoma'
      PrintSettings.FooterFont.Style = []
      PrintSettings.PageNumSep = '/'
      SearchFooter.FindNextCaption = 'Find &next'
      SearchFooter.FindPrevCaption = 'Find &previous'
      SearchFooter.Font.Charset = DEFAULT_CHARSET
      SearchFooter.Font.Color = clWindowText
      SearchFooter.Font.Height = -11
      SearchFooter.Font.Name = 'Tahoma'
      SearchFooter.Font.Style = []
      SearchFooter.HighLightCaption = 'Highlight'
      SearchFooter.HintClose = 'Close'
      SearchFooter.HintFindNext = 'Find next occurrence'
      SearchFooter.HintFindPrev = 'Find previous occurrence'
      SearchFooter.HintHighlight = 'Highlight occurrences'
      SearchFooter.MatchCaseCaption = 'Match case'
      SortSettings.DefaultFormat = ssAutomatic
      Version = '7.7.3.0'
    end
    object Panel1: TPanel
      Left = 0
      Top = 280
      Width = 882
      Height = 41
      Align = alBottom
      Caption = 'Panel1'
      TabOrder = 2
      object btnCategory: TButton
        Left = 32
        Top = 6
        Width = 150
        Height = 25
        Caption = #52852#53580#44256#47532' '#51312#44148' '#52628#44032
        TabOrder = 0
        OnClick = btnCategoryClick
      end
      object btnAttackCode: TButton
        Left = 206
        Top = 6
        Width = 150
        Height = 25
        Caption = #44277#44201#53076#46300' '#51312#44148' '#52628#44032
        TabOrder = 1
        OnClick = btnAttackCodeClick
      end
      object btnAttackName: TButton
        Left = 392
        Top = 6
        Width = 150
        Height = 25
        Caption = #44277#44201#47749' '#51312#44148' '#52628#44032
        TabOrder = 2
        OnClick = btnAttackNameClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 557
    Width = 882
    Height = 41
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      Left = 711
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 792
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
