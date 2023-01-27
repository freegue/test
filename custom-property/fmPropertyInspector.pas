{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-07
  Comment  : 조회조건 인스펙터 UI
-------------------------------------------------------------------------------}

unit fmPropertyInspector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, System.Types,
  ComCtrls, Dialogs, ExtCtrls, StdCtrls, Buttons, TypInfo,
  ToolWin, ImgList, System.ImageList,
  fmPropertyPopup,
  uPropertyDesignIntf, uPropertyDesignEditors, uPropertyResource, uDefineType;

const
  DEFAULT_PATH = 'Config\QueryCondition';

type
  TformPropertyInspector = class(TForm)
    PopupMenu1: TPopupMenu;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    PanelBackGround: TPanel;
    ScrollBox: TScrollBox;
    PaintBox: TPaintBox;
    Edit: TEdit;
    PanelButton: TPanel;
    EditButton: TSpeedButton;
    PanelCombo: TPanel;
    ComboButton: TSpeedButton;
    PanelDescription: TScrollBox;
    Splitter1: TSplitter;
    LabelProperty: TLabel;
    LabelDescription: TLabel;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    Bevel1: TBevel;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    ImageList1: TImageList;
    ToolBarSaveLoad: TToolBar;
    ToolButtonLoad: TToolButton;
    ToolButtonSave: TToolButton;
    TimerDesc: TTimer;
    ToolButtonClear: TToolButton;
    ToolButton2: TToolButton;
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditButtonClick(Sender: TObject);
    procedure ComboButtonClick(Sender: TObject);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure ComboButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure TabChange(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure ToolButtonLoadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerDescTimer(Sender: TObject);
    procedure ToolButtonClearClick(Sender: TObject);
  private
    { Private declarations }
    FDisableDblClick: Boolean;
    FDisableUpdate: Boolean;
    FDown: Boolean;
    FHintWindow: THintWindow;
    FItemIndex: Integer;
    FLastPosition: String;
    FList: TSniperPropertyList;
    FPopupForm: TFormPropertyPopup;
    FPopupLB: TListBox;
    FPopupLBVisible: Boolean;
    FPropertyList: TSniperPropertyList;
    FPanel: TPanel;
    FRowHeight: Integer;
    FSplitterPos: Integer;
    FTempBMP: TBitmap;
    FTempList: TList;
    FTickCount: UInt;
    FUpdatingObjectsCB: Boolean;
    FUpdatingPB: Boolean;
    FOnSelectionChanged: TNotifyEvent;
    FOnModify: TNotifyEvent;
    FJustOneObjects : TList;
    FCurrentObjects: TComponent;
    FLogQueryProductGroupID: string;
    FOnObjectInitial : TNotifyEvent;
    FOnDataValidate: TDataValidateEvent;
    FPropertyTitle: string;

    function Count: Integer;
    function GetItem(Index: Integer): TSniperPropertyItem;
    function GetName(Index: Integer): String;
    function GetOffset(Index: Integer): Integer;
    function GetType(Index: Integer): TSniperPropertyAttributes;
    function GetValue(Index: Integer): String;
    procedure AdjustControls;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure DrawOneLine(i: Integer; Selected: Boolean);
    procedure DoModify;
    function DoValidationEvent(const AValue : string) : boolean;
    procedure SetObjects(Value: TList);
    procedure SetItemIndex(Value: Integer);
    procedure SetSelectedObjects(Value: TList);
    procedure SetValue(Index: Integer; Value: String);
    procedure LBClick(Sender: TObject);
    function GetSplitter1Pos: Integer;
    procedure SetSplitter1Pos(const Value: Integer);
    procedure SetCurrentObjects(const Value: TComponent);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetSelectItem : TSniperPropertyItem;
    procedure DisableUpdate;
    procedure EnableUpdate;
    procedure Inspect(AObjects: array of TPersistent);
    procedure SetColor(Color: TColor);
    procedure UpdateProperties;

    property PropertyTitle : string read FPropertyTitle write FPropertyTitle;
    property Objects: TList write SetObjects;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property SplitterPos: Integer read FSplitterPos write FSplitterPos;
    property Splitter1Pos: Integer read GetSplitter1Pos write SetSplitter1Pos;
    property CurrentObjects: TComponent read FCurrentObjects write SetCurrentObjects;

    property OnModify: TNotifyEvent read FOnModify write FOnModify;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnObjectInitial : TNotifyEvent read FOnObjectInitial write FOnObjectInitial;
    property OnDataValidate : TDataValidateEvent read FOnDataValidate write FOnDataValidate;
  end;

implementation

{$R *.DFM}

type
  TInspPanel = class(TPanel)
  protected
    procedure WMEraseBackground(var Message: TMessage); message WM_ERASEBKGND;
    procedure Paint; override;
  end;

  THackWinControl = class(TWinControl);


procedure SaveToFilePropertyData(AFileName : string; AObject : TComponent);
var
  FileStream:TFileStream;
  BinStream :TMemoryStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate);
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(AObject);
    BinStream.Seek(0, soFromBeginning);
    ObjectBinaryToText(BinStream, FileStream);
  finally
    FileStream.Free;
    BinStream.Free;
  end;
end;

// RegisterClass 등록된 경우에 읽는 방식
function LoadFromFileComponentForRegisteedClass(AFileName: string): TComponent;
var
  StrStream:TStringStream;
  BinStream:TMemoryStream;
begin
  result := nil;
  if FileExists(AFileName) then
  begin
    try
      StrStream := TStringStream.Create;
      StrStream.LoadFromFile(AFileName);
      try
        BinStream := TMemoryStream.Create;
        try
          ObjectTextToBinary(StrStream, BinStream);
          BinStream.Seek(0, soFromBeginning);
          Result := BinStream.ReadComponent(nil);
        finally
          BinStream.Free;
        end;
      finally
        StrStream.Free;
      end;
    except
    end;
  end;
end;

function LoadFromFilePropertyData(AFileName : string; AObject : TComponent) : boolean;
var
  FileStream: TFileStream;
  BinStream: TMemoryStream;
begin
  Result := false;
  if FileExists(AFileName) then
  begin
    try
      FileStream := TFileStream.Create(AFileName, fmOpenRead);
      FileStream.Position := 0;
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(FileStream, BinStream);
        BinStream.Seek(0, soFromBeginning);
        BinStream.ReadComponent(AObject); //인자를 nil로 넣으면 사용될 컴포넌트가 RegisterClass(TSniperLogQueryBase); 등록이 되어있어야함.
        Result := True;
      finally
        FileStream.Free;
        BinStream.Free;
      end;
    except
      on e : Exception do
      begin
        raise Exception.Create('Invalid property info' + #13 + e.Message);
      end;
    end;
  end;
end;


procedure ErrorMsg(const Title, Text: String);
begin
  Application.MessageBox(PChar(Text), PChar(Title), MB_OK + MB_ICONERROR);
end;

procedure WarningMsg(const Title, Text: String);
begin
  Application.MessageBox(PChar(Text), PChar(Title), MB_OK + MB_ICONWARNING);
end;

function ConfirmMsg(const Title, Text: String; Buttons: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Title), MB_ICONQUESTION + Buttons);
end;

procedure InfoMsg(const Title, Text: String);
begin
  Application.MessageBox(PChar(Text), PChar(Title), MB_OK + MB_ICONINFORMATION);
end;


{ TInspPanel }

procedure TInspPanel.WMEraseBackground(var Message: TMessage);
begin
// empty method
end;

procedure TInspPanel.Paint;
begin
// empty method
end;


{ TObjectInspector }

constructor TformPropertyInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPropertyTitle := self.Caption;

  FItemIndex := -1;
  FTempBMP := TBitmap.Create;
  FTempList := TList.Create;
  FHintWindow := THintWindow.Create(Self);
  FHintWindow.Color := clInfoBk;

  //FProductGroupList := TStringList.Create;
  //기본값
  //FProductGroupList.Add('ALL'); //전체 - 기본
  //FProductGroupList.Add('CM');  //자산운영 - 기본

  FPanel := TInspPanel.Create(Self);
  FPanel.DoubleBuffered := True;
  with FPanel do
  begin
    Parent := ScrollBox;
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;
  PaintBox.Parent := FPanel;
  PanelCombo.Parent := FPanel;
  PanelButton.Parent := FPanel;
  Edit.Parent := FPanel;
//{$IFDEF UseTabset}
  ScrollBox.BevelKind := bkFlat;
  PanelDescription.BevelKind := bkFlat;
//{$ELSE}
//  ScrollBox.BorderStyle := bsSingle;
//  PanelDescription.BorderStyle := bsSingle;
//{$IFDEF Delphi7}
//  ScrollBox.ControlStyle := ScrollBox.ControlStyle + [csNeedsBorderPaint];
//  PanelDescription.ControlStyle := PanelDescription.ControlStyle + [csNeedsBorderPaint];
//{$ENDIF}
//{$ENDIF}

  FRowHeight := PanelButton.Height + 0;// Canvas.TextHeight('Wg') + 5; //18 크기 되도록
  with ScrollBox.VertScrollBar do
  begin
    Increment := FRowHeight;
    Tracking := True;
  end;

  FSplitterPos := PaintBox.Width div 2;
  AutoScroll := False;

  FJustOneObjects := TList.Create;
  FLogQueryProductGroupID := '';

  FormResize(nil);
end;

destructor TformPropertyInspector.Destroy;
begin
  //FProductGroupList.Free;
  FTempBMP.Free;
  FTempList.Free;
  if FPropertyList <> nil then
    FPropertyList.Free;

  FJustOneObjects.Free;

  inherited;
end;

procedure TformPropertyInspector.UpdateProperties;
begin
  SetSelectedObjects(FJustOneObjects);
end;

procedure TformPropertyInspector.Inspect(AObjects: array of TPersistent);
var
  i: Integer;
begin
  FTempList.Clear;
  for i := Low(AObjects) to High(AObjects) do
    FTempList.Add(AObjects[i]);
  Objects := FTempList;
end;

function TformPropertyInspector.GetSelectItem: TSniperPropertyItem;
begin
  Result := GetItem(FItemIndex);
end;

function TformPropertyInspector.GetSplitter1Pos: Integer;
begin
  Result := PanelDescription.Height;
end;

procedure TformPropertyInspector.SetSplitter1Pos(const Value: Integer);
begin
  PanelDescription.Height := Value;
end;

procedure TformPropertyInspector.DisableUpdate;
begin
  FDisableUpdate := True;
end;

procedure TformPropertyInspector.EnableUpdate;
begin
  FDisableUpdate := False;
end;

procedure TformPropertyInspector.SetColor(Color: TColor);
begin
  ScrollBox.Color := Color;
  PaintBox.Repaint;
end;

procedure TformPropertyInspector.SetCurrentObjects(const Value: TComponent);
begin
  if Value = nil then exit;

  FCurrentObjects := Value;
  FJustOneObjects.Clear;
  FJustOneObjects.Add(Value);
  SetSelectedObjects(FJustOneObjects);
end;

procedure TformPropertyInspector.SetObjects(Value: TList);
var
  i: Integer;
  s: String;
begin
  for i := 0 to Value.Count - 1 do
  begin
    if TObject(Value[i]) is TComponent then
      s := TComponent(Value[i]).Name + ': ' + TComponent(Value[i]).ClassName else
      s := '';
  end;
end;

procedure TformPropertyInspector.SetSelectedObjects(Value: TList);
var
  i: Integer;
  s: String;

  procedure CreateLists;
  var
    i: Integer;
    p: TSniperPropertyItem;
    s: String;
  begin
    if FPropertyList <> nil then
      FPropertyList.Free;
    FPropertyList := CreatePropertyList(Value);
    if FPropertyList <> nil then
    begin
      i := 0;
      while i < FPropertyList.Count do
      begin
        p := FPropertyList[i];
        s := String(p.Editor.PropInfo.PropType^.Name);
        Inc(i);
      end;
    end;

    FList := FPropertyList;
  end;
begin
  CreateLists;

  FUpdatingObjectsCB := True;
  FUpdatingObjectsCB := False;

  FItemIndex := -1;
  FormResize(nil);
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
      if GetName(i) = FLastPosition then
      begin
        ItemIndex := i;
        Exit;
      end;
    s := FLastPosition;
    ItemIndex := 0;
    FLastPosition := s;
  end;
end;

function TformPropertyInspector.Count: Integer;

  function EnumProperties(p: TSniperPropertyList): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to p.Count - 1 do
    begin
      Inc(Result);
      if (p[i].SubProperty <> nil) and p[i].Expanded then
        Inc(Result, EnumProperties(p[i].SubProperty));
    end;
  end;

begin
  if FList <> nil then
    Result := EnumProperties(FList) else
    Result := 0;
end;

function TformPropertyInspector.GetItem(Index: Integer): TSniperPropertyItem;

  function EnumProperties(p: TSniperPropertyList; var Index: Integer): TSniperPropertyItem;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to p.Count - 1 do
    begin
      Dec(Index);
      if Index < 0 then
      begin
        Result := p[i];
        break;
      end;
      if (p[i].SubProperty <> nil) and p[i].Expanded then
        Result := EnumProperties(p[i].SubProperty, Index);
      if Index < 0 then
        break;
    end;
  end;

begin
  if (Index >= 0) and (Index < Count) then
    Result := EnumProperties(FList, Index) else
    Result := nil;
end;

function TformPropertyInspector.GetOffset(Index: Integer): Integer;
var
  p: TSniperPropertyList;
begin
  Result := 0;
  p := TSniperPropertyList(GetItem(Index).Collection);
  while p.Parent <> nil do
  begin
    Inc(Result);
    p := p.Parent;
  end;
end;

function TformPropertyInspector.GetName(Index: Integer): String;
begin
  Result := GetItem(Index).Editor.GetName;
end;

function TformPropertyInspector.GetType(Index: Integer): TSniperPropertyAttributes;
begin
  Result := GetItem(Index).Editor.GetAttributes;
end;

function TformPropertyInspector.GetValue(Index: Integer): String;
begin
  Result := GetItem(Index).Editor.Value;
end;

procedure TformPropertyInspector.DoModify;
begin
  if Assigned(FOnModify) then
    FOnModify(Self);
end;

function TformPropertyInspector.DoValidationEvent(const AValue : string) : boolean;
begin
  result := true;
  if Assigned(FOnDataValidate) then
    FOnDataValidate(GetSelectItem.Editor, AValue, Result);
end;

procedure TformPropertyInspector.SetItemIndex(Value: Integer);
var
  p: TSniperPropertyItem;
  s: String;
begin
  LabelProperty.Caption := '';
  LabelDescription.Caption := '';
  if Value > Count - 1 then
    Value := Count - 1;
  if Value < 0 then
    Value := -1;

  Edit.Visible := Count > 0;
  if Count = 0 then Exit;

  if FItemIndex <> -1 then
    if Edit.Modified then
    begin
      Edit.Modified := False;
      SetValue(FItemIndex, Edit.Text);
    end;
  FItemIndex := Value;

  if FItemIndex <> -1 then
  begin
    FLastPosition := GetName(FItemIndex);
    p := GetItem(FItemIndex);
    s := GetName(FItemIndex);
    //LabelProperty.Caption := ' ' + s;

    //숫자형 프로퍼티일때는 숫자만 입력하게....
    if UpperCase(p.Editor.ClassName) = 'TINTEGERPROPERTY' then
      SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE) or ES_NUMBER)
    else if UpperCase(p.Editor.ClassName) = 'TSTRINGPROPERTY' then
      SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE) and not ES_NUMBER);

    Edit.Hint := '';
    //if UpperCase(p.Editor.GetName) = 'COUNT' then
    //  Edit.Hint := '0 보다 큰 값일때만 조건으로 사용됩니다.';

    if p.Editor.PropInfo.PropType^.Kind = tkInteger then
      Edit.Hint := '0 보다 큰 값일때만 조건으로 사용됩니다.';


    //TODO : 프로퍼티의 설명을 어디선가 가져오게한다. 프로퍼티 이름과 매칭되게 적어둔 목록에서.
    if TSniperPropertyList(p.Collection).Component <> nil then
    begin
      //s := s + '.' + TPropertyList(p.Collection).Component.ClassName;
      LabelDescription.Caption := s + ' : 프로퍼티 설명을 표시합니다.';
    end;

  end;

  AdjustControls;
end;

procedure TformPropertyInspector.SetValue(Index: Integer; Value: String);
begin
  try
    //입력될 값을 먼저 유효성 체크이벤트가 있을시 체크하고. 정상이면 반영.
    if DoValidationEvent(Value) then
    begin
      GetItem(Index).Editor.Value := Value;
      DoModify;
      PaintBoxPaint(nil);
    end;
  except
    on E: Exception do
    begin
      LabelDescription.Caption := E.Message;
      WarningMsg(PropertyTitle, E.Message);
      Edit.Text := GetItem(Index).Editor.Value;
    end;
  end;
end;

procedure TformPropertyInspector.AdjustControls;
var
  PropType: TSniperPropertyAttributes;
  y, ww: Integer;
  bPanelCombo, bPanelEdit : Boolean;
  //Editor : TSniperPropertyEditor;
begin
  if (csDocking in ControlState) or FDisableUpdate then Exit;
  if FItemIndex = -1 then
  begin
    PanelButton.Visible := False;
    PanelCombo.Visible := False;
    Edit.Visible := False;
    FUpdatingPB := False;
    PaintBoxPaint(nil);
    Exit;
  end;

  FUpdatingPB := True;
  PropType := GetType(FItemIndex);

  bPanelEdit := paDialog in PropType;
  bPanelCombo := paValueList in PropType;
  Edit.ReadOnly := paReadOnly in PropType;
  //Editor := GetItem(FItemIndex).Editor;

  ww := PaintBox.Width - FSplitterPos - 2;
  y := FItemIndex * FRowHeight + 1;
  if bPanelEdit then
  begin
    PanelButton.SetBounds(PaintBox.Width - (PanelButton.Width), y - 2 , PanelButton.Width, FRowHeight + 1);
    EditButton.SetBounds(-1, -1, PanelButton.Width + 2, PanelButton.Height + 2);
    Dec(ww, PanelButton.Width - 1);
  end;
  PanelButton.Visible := bPanelEdit;

  if bPanelCombo then
  begin
    PanelCombo.SetBounds(PaintBox.Width - (PanelCombo.Width), y - 2, PanelCombo.Width, FRowHeight + 1);
    ComboButton.SetBounds(-1, -1, PanelCombo.Width + 2, PanelCombo.Height + 2);
    Dec(ww, PanelCombo.Width - 1);
  end;
  PanelCombo.Visible := bPanelCombo;

  Edit.Text := GetValue(FItemIndex);
  Edit.Modified := False;
  Edit.SetBounds(FSplitterPos + 2, y + 1, ww - 2, FRowHeight - 3);
  Edit.SelectAll;

  if y + FRowHeight > ScrollBox.VertScrollBar.Position + ScrollBox.ClientHeight then
    ScrollBox.VertScrollBar.Position := y - ScrollBox.ClientHeight + FRowHeight;
  if y < ScrollBox.VertScrollBar.Position then
    ScrollBox.VertScrollBar.Position := y - 1;

  FUpdatingPB := False;
  PaintBoxPaint(nil);
end;

procedure TformPropertyInspector.DrawOneLine(i: Integer; Selected: Boolean);
var
  R: TRect;
  s: String;
  p: TSniperPropertyItem;
  offs, add: Integer;

  procedure Line(x, y, dx, dy: Integer);
  begin
    FTempBMP.Canvas.MoveTo(x, y);
    FTempBMP.Canvas.LineTo(x + dx, y + dy);
  end;

  procedure DrawProperty;
  var
    x, y: Integer;
  begin
    x := offs + GetOffset(i) * (12 + add);
    y := i * FRowHeight + 2;

    with FTempBMP.Canvas do
    begin
      Pen.Color := clGray;
      Brush.Color := clWhite;

      if offs < 12 then
      begin
        Rectangle(x + 1, y + 2 + add, x + 10, y + 11 + add);
        Line(x + 3, y + 6 + add, 5, 0);
        if s[1] = '+' then
          Line(x + 5, y + 4 + add, 0, 5);

        s := Copy(s, 2, 255);
        Inc(x, 12 + add);
      end;
      Brush.Style := bsClear;

      S := GetPropertyNameTranslation(s);
      //LogQueryPoductGroupID 값이 있는것은 로그 조회조건 으로사용되는 인스펙터이고.
      //없은것은 이벤트 조회 조건의 인스펙터 이므로, GetPropertyName 사용에 활용한다.
      //if LogQueryProductGroupID <> '' then
      //  S := GetPropertyNameTranslation('LOG_'+LogQueryProductGroupID + '_' + s);

      TextRect(R, x+1, y, s);
    end;
  end;

begin
  if Count > 0 then
  with FTempBMP.Canvas do
  begin
    Pen.Color := clBtnShadow;
    Font.Assign(Self.Font);
    R := Rect(0, i * FRowHeight, FSplitterPos, i * FRowHeight + FRowHeight - 1);

    if Screen.PixelsPerInch > 96 then
      add := 2
    else
      add := 0;
    p := GetItem(i);
    s := GetName(i);
    if p.SubProperty <> nil then
    begin
      offs := 1 + add;
      if p.Expanded then
        s := '-' + s else
        s := '+' + s;
    end
    else
      offs := 13 + add;

    p.Editor.ItemHeight := FRowHeight;

    if Selected then
    begin
      Pen.Color := clBtnFace;
      Brush.Color := $00D4BFB7;// $00F5D3C9;
      FillRect(R);
      DrawProperty;
    end
    else
    begin
      Pen.Color := clBtnFace;
      Brush.Color := $00ECECED;
      FillRect(R);
      DrawProperty;
      Font.Color := clNavy;
      if paCustomDropDown in p.Editor.GetAttributes then
        p.Editor.OnDrawItem(FTempBMP.Canvas,
          Rect(FSplitterPos + 2, 1 + i * FRowHeight, Width, 1 + (i + 1) * FRowHeight))
      else
        TextOut(FSplitterPos + 2, 1 + i * FRowHeight, GetValue(i));
    end;
    //항목별 가로라인..
    //Pen.Style := psDot;
    Pen.Color := $00C8D0D4;
    FTempBMP.Canvas.MoveTo(R.Left, R.Bottom);
    FTempBMP.Canvas.LineTo(R.Left + Width, R.Bottom);
    //중간 세로라인
    Pen.Color := clBtnFace;
    Line(FSplitterPos - 1, 0 + i * FRowHeight, 0, FRowHeight);
  end;
end;


{ Form events }

procedure TformPropertyInspector.FormShow(Sender: TObject);
begin
  AdjustControls;
end;

procedure TformPropertyInspector.FormResize(Sender: TObject);
begin
  FPanel.Height := Count * FRowHeight;
  FPanel.Width := ScrollBox.ClientWidth;
  AdjustControls;
end;

procedure TformPropertyInspector.FormEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  FormResize(nil);
end;

procedure TformPropertyInspector.TabChange(Sender: TObject);
begin
  FItemIndex := -1;
  FormResize(nil);
end;

procedure TformPropertyInspector.TimerDescTimer(Sender: TObject);
begin
  LabelDescription.Caption := '';
  TimerDesc.Enabled := False;
end;

procedure TformPropertyInspector.ToolButtonClearClick(Sender: TObject);
begin
  if ConfirmMsg(PropertyTitle, '조건을 초기화 합니다. 진행하시겠습니까?', MB_YESNO) = mrYes then
  begin
    if Assigned(FOnObjectInitial) then FOnObjectInitial(nil);
    AdjustControls;
  end;
end;

procedure TformPropertyInspector.ToolButtonLoadClick(Sender: TObject);
begin
  OpenDialog.InitialDir :=  IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + DEFAULT_PATH;
  if OpenDialog.Execute then
  begin
    //다른 조건 열기전에 이전 조건 초기화 하도록 요청
    if Assigned(FOnObjectInitial) then FOnObjectInitial(nil);
    if LoadFromFilePropertyData(OpenDialog.FileName, FCurrentObjects) then
    begin
      SetCurrentObjects(FCurrentObjects);
      AdjustControls;
    end;
  end;
end;

procedure TformPropertyInspector.ToolButtonSaveClick(Sender: TObject);
begin
  SaveDialog.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + DEFAULT_PATH;
  if SaveDialog.Execute then
  begin
    SaveToFilePropertyData(SaveDialog.FileName, FCurrentObjects);
  end;
end;

procedure TformPropertyInspector.N11Click(Sender: TObject);
begin
  if Edit.Visible then
    Edit.CutToClipboard;
end;

procedure TformPropertyInspector.N21Click(Sender: TObject);
begin
  if Edit.Visible then
    Edit.PasteFromClipboard;
end;

procedure TformPropertyInspector.N31Click(Sender: TObject);
begin
  if Edit.Visible then
    Edit.CopyToClipboard;
end;

procedure TformPropertyInspector.FormCreate(Sender: TObject);
begin
  LoadFromFileofPropertyResource;
  self.DoubleBuffered := True;
  ScrollBox.DoubleBuffered := True;
end;

procedure TformPropertyInspector.FormDeactivate(Sender: TObject);
begin
  if FDisableUpdate then Exit;
  SetItemIndex(FItemIndex);
end;


procedure TformPropertyInspector.FormDestroy(Sender: TObject);
begin
end;

{ PB events }

procedure TformPropertyInspector.PaintBoxPaint(Sender: TObject);
var
  i: Integer;
  r: TRect;
begin
  if FUpdatingPB then Exit;

  PaintBox.Canvas.Lock;
  try
    r := PaintBox.BoundsRect;
    FTempBMP.Width := PaintBox.Width;
    FTempBMP.Height := PaintBox.Height;
    with FTempBMP.Canvas do
    begin
      Brush.Color := ScrollBox.Color;
      FillRect(r);
    end;

    if not FDisableUpdate then
    begin
      for i := 0 to Count - 1 do
        if i <> ItemIndex then
          DrawOneLine(i, False);
      if FItemIndex <> -1 then
        DrawOneLine(ItemIndex, True);
    end;
    PaintBox.Canvas.Draw(0, 0, FTempBMP);
  finally
    PaintBox.Canvas.Unlock;
  end;
end;

procedure TformPropertyInspector.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TSniperPropertyItem;
  n, x1: Integer;
begin
  FDisableDblClick := False;
  if Count = 0 then Exit;
  if PaintBox.Cursor = crHSplit then
    FDown := True
  else
  begin
    n := Y div FRowHeight;

    if (X > FSplitterPos) and (X < FSplitterPos + 15) and
       (n >= 0) and (n < Count) then
    begin
      p := GetItem(n);
      if TSniperPropertyEditor(p.Editor).Component.ClassType = TSniperBooleanProperty then
      //if UpperCase(p.Editor.ClassName) = 'TBOOLEANPROPERTY' then
      begin
        p.Editor.Edit;
        DoModify;
        PaintBoxPaint(nil);
        Exit;
      end;
    end;

    ItemIndex := n;
    Edit.SetFocus;
    FTickCount := GetTickCount;

    p := GetItem(ItemIndex);
    x1 := GetOffset(ItemIndex) * 12;
    if (X > x1) and (X < x1 + 13) and (p.SubProperty <> nil) then
    begin
      p.Expanded := not p.Expanded;
      FormResize(nil);
      FDisableDblClick := True;
    end;
  end;
end;

procedure TformPropertyInspector.PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
end;

procedure TformPropertyInspector.PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  n, OffsetX, MaxWidth: Integer;
  s: String;
  HideHint: Boolean;

  procedure ShowHint(const s: String; x, y: Integer);
  var
    HintRect: TRect;
    p: TPoint;
    value : string;
  begin
    value := s;
    if PaintBox.Canvas.TextWidth(s) > Screen.WorkAreaWidth then
      value := value;// ShortString(Canvas.Handle, S, 1000);

    p := PaintBox.ClientToScreen(Point(x - 2, y - 2));
    HintRect := FHintWindow.CalcHintRect(1000, value, nil);
    OffsetRect(HintRect, p.X, p.Y);
    FHintWindow.ActivateHint(HintRect, value);
    HideHint := False;
  end;

begin
  HideHint := True;

  if not FDown then
  begin
    if (X > FSplitterPos - 4) and (X < FSplitterPos + 2) then
      PaintBox.Cursor := crHSplit
    else
    begin
      PaintBox.Cursor := crDefault;

      { hint window }
      n := Y div FRowHeight;
      if (X > 12) and (n >= 0) and (n < Count) then
      begin
        if X <= FSplitterPos - 4 then
        begin
          OffsetX := (GetOffset(n) + 1) * 12;
          s := GetName(n);
          MaxWidth := FSplitterPos - OffsetX;
        end
        else
        begin
          OffsetX := FSplitterPos + 1;
          s := GetValue(n);
          MaxWidth := PaintBox.ClientWidth - FSplitterPos;
          if n = ItemIndex then
            MaxWidth := 1000;
        end;

        if PaintBox.Canvas.TextWidth(s) > MaxWidth then
          ShowHint(s, OffsetX, n * FRowHeight);
      end;
    end;
  end
  else
  begin
    if (x > 30) and (x < PaintBox.ClientWidth - 30) then
      FSplitterPos := X;
    AdjustControls;
  end;

  if HideHint then
    FHintWindow.ReleaseHandle;
end;

procedure TformPropertyInspector.PaintBoxDblClick(Sender: TObject);
var
  p: TSniperPropertyItem;
begin
  if (Count = 0) or FDisableDblClick then Exit;

  p := GetItem(ItemIndex);
  if (p <> nil) and (p.SubProperty <> nil) then
  begin
    p.Expanded := not p.Expanded;
    FormResize(nil);
  end;
end;


{ Edit events }

procedure TformPropertyInspector.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetTickCount - FTickCount < GetDoubleClickTime then
    EditButtonClick(nil);
end;

procedure TformPropertyInspector.EditExit(Sender: TObject);
begin
  if Edit.Modified then
  begin
    Edit.Modified := False;
    SetValue(ItemIndex, Edit.Text);
  end;
end;

procedure TformPropertyInspector.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if Count = 0 then Exit;
  if Key = vk_Escape then
  begin
    Edit.Perform(EM_UNDO, 0, 0);
    Edit.Modified := False;
  end;
  if Key = vk_Up then
  begin
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
    Key := 0;
  end
  else if Key = vk_Down then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1;
    Key := 0;
  end
  else if Key = vk_Prior then
  begin
    i := ScrollBox.Height div FRowHeight;
    i := ItemIndex - i;
    if i < 0 then
      i := 0;
    ItemIndex := i;
    Key := 0;
  end
  else if Key = vk_Next then
  begin
    i := ScrollBox.Height div FRowHeight;
    i := ItemIndex + i;
    ItemIndex := i;
    Key := 0;
  end;
end;

procedure TformPropertyInspector.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if paDialog in GetType(ItemIndex) then
      EditButtonClick(nil)
    else
    begin
      if Edit.Modified then
      begin
        Edit.Modified := False;
        SetValue(ItemIndex, Edit.Text);
      end;
    end;
    Edit.SelectAll;
    Key := #0;
  end;
end;


{ EditButton and ComboButton events }


{-------------------------------------------------------------------------------
  Procedure: TObjectInspector.EditButtonClick
  Author   : kyungmun
  DateTime : 2022.08.17
  Arguments: Sender: TObject
  Result   : None
  Comment  : [...]버튼클릭 및 입력컨트롤 더블클릭으로 다음 값의 선택 기능.
-------------------------------------------------------------------------------}
procedure TformPropertyInspector.EditButtonClick(Sender: TObject);
var
  p : TSniperPropertyItem;
begin
  //Enum 타입은 더블 클릭으로 선택 안되게..단, boolean 형은 값이 2개이므로 토글 되야 함.
  p := GetItem(ItemIndex);
  if (p.Editor.PropInfo.PropType^.Kind = tkEnumeration) and
     (UpperCase(String(p.Editor.PropInfo.PropType^.Name)) <> UpperCase('boolean')) then Exit;

  if p.Editor.Edit then
  begin
    ItemIndex := FItemIndex;
    DoModify;
  end;
end;

procedure TformPropertyInspector.ComboButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPopupLBVisible := GetTickCount - PopupFormCloseTime < 100;
end;

procedure TformPropertyInspector.ComboButtonClick(Sender: TObject);
var
  i, wItems, nItems: Integer;
  p: TPoint;
  AEditor : TSniperPropertyEditor;
begin
  if FPopupLBVisible then
    Edit.SetFocus
  else
  begin
    FPopupForm := TFormPropertyPopup.Create(Self);
    FPopupLB := TListBox.Create(FPopupForm);

    with FPopupLB do
    begin
      ItemHeight := FRowHeight;
      Parent := FPopupForm;
      Ctl3D := False;
      Align := alClient;
      AEditor := GetItem(FItemIndex).Editor;

      if paCustomDropDown in AEditor.GetAttributes then
      begin
        Style := lbOwnerDrawFixed;
        ItemHeight := FRowHeight - 8;
      end;

      OnClick := LBClick;
      OnDrawItem := AEditor.OnDrawLBItem;

      //제품군이면 현재 자산트리에서 사용중인 제품군을 확인해서 그 제품군값만 표시한다.
      //if (AEditor.PropInfo.PropType^.Kind = tkEnumeration) and
      //   (UpperCase(AEditor.PropInfo.PropType^.Name) = 'TPRODUCTGROUP') then
      //begin
      //  for I := 0 to FProductGroupList.Count - 1 do
      //    Items.Add(GetPropertyNameTranslation(FProductGroupList.Strings[i]));
      //  Sorted := True;
      //end
      //else
      begin
        AEditor.GetValues;
        Items.Assign(AEditor.Values);
      end;

      if Items.Count > 0 then
      begin
        ItemIndex := Items.IndexOf(GetValue(FItemIndex));
        wItems := 0;
        for i := 0 to Items.Count - 1 do
        begin
          if Canvas.TextWidth(Items[i]) > wItems then
            wItems := Canvas.TextWidth(Items[i]);
        end;

        Inc(wItems, 8);
        if paCustomDropDown in AEditor.GetAttributes then
          Inc(wItems, AEditor.GetExtraLBSize);

        nItems := Items.Count;
        if nItems > 8 then
        begin
          nItems := 8;
          Inc(wItems, GetSystemMetrics(SM_CXVSCROLL));
        end;

        p := Edit.ClientToScreen(Point(0, Edit.Height));

        if wItems < PaintBox.Width - FSplitterPos then
          FPopupForm.SetBounds(p.X - 3, p.Y,
                             PaintBox.Width - FSplitterPos + 1, nItems * ItemHeight + 2)
        else
          FPopupForm.SetBounds(p.X + (PaintBox.Width - FSplitterPos - wItems) - 2, p.Y,
                             wItems, nItems * ItemHeight + 2);

        if FPopupForm.Left < 0 then
          FPopupForm.Left := 0;

        if FPopupForm.Top + FPopupForm.Height > Screen.Height then
          FPopupForm.Top := Screen.Height - FPopupForm.Height;

        FDisableUpdate := True;
        FPopupForm.Show;
        FDisableUpdate := False;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TObjectInspector.LBClick
  Author   : kyungmun
  DateTime : 2022.05.15
  Arguments: Sender: TObject
  Result   : None
  Comment  : 콤보버튼 눌러 리스트박스의 값을 선택한 경우.
             Enum 타입으로 사용된 경우 한글로 값이 표기되는 처리 체크
-------------------------------------------------------------------------------}
procedure TformPropertyInspector.LBClick(Sender: TObject);
var
  p : TSniperPropertyItem;
  TypeInfo: PTypeInfo;
  sTypeName, sInputValue : string;
begin
  Edit.Text := FPopupLB.Items[FPopupLB.ItemIndex];
  FPopupForm.Hide;
  try
    p := GetItem(FItemIndex);
    if p = nil then exit;

    TypeInfo := p.Editor.PropInfo.PropType^;
    sTypeName := UpperCase(String(TypeInfo.Name));
    sInputValue := Edit.Text;
    if (sTypeName =  UpperCase('TBlockType')) then
      sInputValue := format('TBlockType_%s', [Edit.Text]);

    //Enum 타입으로 사용시 번역되서 표기되는 정보는 역번역 필요
    if (TypeInfo.Kind = tkEnumeration) and
       ( (sTypeName = UpperCase('TPRODUCTGROUP')) or  //제품군 코드
         (sTypeName = UpperCase('TDirection')) or  //방향
         (sTypeName = UpperCase('TBlockType')) or  //차단종류
         (sTypeName = UpperCase('TBlockReasonType')) or //차단사유
         (sTypeName = UpperCase('TDATETYPE')) ) then     //일간.주간.월간
      SetValue(ItemIndex, GetPropertyNameTranslation(sInputValue))
    else
      SetValue(ItemIndex, Edit.Text);

    Edit.SetFocus;
    Edit.SelectAll;
  except
  end;
end;

{ Mouse wheel }

procedure TformPropertyInspector.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with ScrollBox.VertScrollBar do
    Position := Position + FRowHeight;
end;

procedure TformPropertyInspector.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with ScrollBox.VertScrollBar do
    Position := Position - FRowHeight;
end;

procedure TformPropertyInspector.CMMouseLeave(var Msg: TMessage);
begin
  FHintWindow.ReleaseHandle;
  inherited;
end;

end.