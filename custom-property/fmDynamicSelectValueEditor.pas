{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : Inspector 에서 사용하는 동적 목록 정보 선택 대화상자
-------------------------------------------------------------------------------}

unit fmDynamicSelectValueEditor;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Commctrl, Consts,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin,
  DefineTypeUnit, uFunctionUnit, Vcl.CheckLst;


type
  TformDynamicSelectValueEditor = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Bevel1: TBevel;
    PageControl: TPageControl;
    TabSheet: TTabSheet;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FMultiSelected: boolean;
    ListBox: TCustomListBox;
    FValueList: TStringList;
    function GetSelectValue: String;
    procedure SetSelectValue(const Value: String);
    procedure LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure LBClick(Sender: TObject);
  public
    constructor CreateForm(AOwner: TComponent; AValueList: TStringList; AMultiSelect : boolean = false);
    property MultiSelected : boolean read FMultiSelected write FMultiSelected;
    property SelectValue : String read GetSelectValue write SetSelectValue;
    property ValueList : TStringList read FValueList write FValueList;
  end;

implementation

{$R *.DFM}


procedure TformDynamicSelectValueEditor.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

constructor TformDynamicSelectValueEditor.CreateForm(AOwner: TComponent; AValueList: TStringList; AMultiSelect : boolean);
begin
  inherited Create(AOwner);
  FMultiSelected := AMultiSelect;

  if FMultiSelected then
  begin
    ListBox := TCheckListBox.Create(TabSheet);
    TCheckListBox(ListBox).OnDrawItem := LBDrawItem;
    TCheckListBox(ListBox).OnClick := LBClick;
    TCheckListBox(ListBox).Style := lbOwnerDrawFixed;
  end
  else
  begin
    ListBox := TListBox.Create(TabSheet);
    TListBox(ListBox).OnDrawItem := LBDrawItem;
    TListBox(ListBox).OnClick := LBClick;
    TListBox(ListBox).Style := lbOwnerDrawFixed;
  end;

  ListBox.Parent := TabSheet;
  ListBox.Align := alClient;
  ListBox.AlignWithMargins := True;
  ListBox.Items := AValueList;
end;

function TformDynamicSelectValueEditor.GetSelectValue: String;
var
  i : integer;
  AList : TStringList;
begin
  AList := TStringList.Create;
  try
    if FMultiSelected then
    begin
      for i := 0 to ListBox.Count - 1 do
        if TCheckListBox(ListBox).Checked[i] then
          AList.Add(TCheckListBox(ListBox).Items[i]);
    end
    else
    begin
      for i := 0 to ListBox.Count - 1 do
      begin
        if ListBox.Selected[i] then
        begin
          AList.Add(ListBox.Items[i]);
          break;
        end;
      end;
    end;
    Result := AList.CommaText;
  finally
    AList.Free;
  end;
end;

procedure TformDynamicSelectValueEditor.LBClick(Sender: TObject);
begin
  if not Boolean(ListBox.Items.Objects[ListBox.ItemIndex]) then
  begin
    Application.MessageBox('해당 값은 사용 할 수 없습니다.',
      PChar(Application.Title), MB_OK + MB_ICONINFORMATION);

    if ListBox is TCheckListBox then
    begin
      TCheckListBox(ListBox).Checked[ListBox.ItemIndex] := false;
      TCheckListBox(ListBox).Selected[ListBox.ItemIndex] := false;
    end
    else
    begin
      TListBox(ListBox).Selected[ListBox.ItemIndex] := false;
    end
  end;
end;

procedure TformDynamicSelectValueEditor.LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  LRect: TRect;
  ItemState: TOwnerDrawState;
  AEnable: Boolean;
begin
  LRect := Rect;
  AEnable := Boolean(ListBox.Items.Objects[Index]);
  ItemState := State;
  if not AEnable then //사용 할 수 없는 값은 상태값을 변경.
    ItemState := ItemState + [odDisabled];

  with ListBox.Canvas do
  begin
    FillRect( LRect );
    Font.Color := clBlue;
    if odSelected in ItemState then
    begin
      Font.Color := clWhite;
      if odDisabled in ItemState then
        Font.Color := clGrayText;
      TextOut( LRect.left + 2, LRect.top, ListBox.Items[index]);
    end
    else
    begin
      if odDisabled in ItemState then
        Font.Color := clGrayText;
      TextOut( LRect.left + 2, LRect.top, ListBox.Items[index]);
    end;
  end;

end;

procedure TformDynamicSelectValueEditor.SetSelectValue(const Value: String);
var
  i, idx : integer;
  AList : TStringList;
begin
  AList := TStringList.Create;
  try
    AList.CommaText := Value;
    for i := 0 to AList.Count - 1 do
    begin
      idx := ListBox.Items.IndexOf(AList[i]);
      if idx >= 0 then
      begin
        if FMultiSelected then
        begin
          if Boolean(ListBox.Items.Objects[idx]) then
            TCheckListBox(ListBox).Checked[idx] := True;
        end
        else //1개 선택하는 경우는 선택가능한 1개가 선택되면 빠지기
        begin
          if Boolean(ListBox.Items.Objects[idx]) then
          begin
            ListBox.Selected[idx] := True;
            break;
          end;
        end;;
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TformDynamicSelectValueEditor.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



end.

