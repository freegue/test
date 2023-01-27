(*
  조회조건 입력시 특정 정보의 값을 멀티로 입력하는 경우 해당 입력 조건의 Visualization 처리
  - 조건 삭제 버튼
  - 필드명, 자체연산자, 조건값
  - 조건건 관계연산자
*)
unit uConditionObject;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls, Vcl.Forms, Dialogs, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.TypInfo, System.UITypes,
  Generics.Collections, System.Generics.Defaults, System.Win.ComObj, Winapi.ActiveX;

const
  COLOR_RELATIONAL_AND = $FE8404; //파란계열    $3056FF; //찐주황계열
  COLOR_RELATIONAL_OR  = $22AFF7; //오랜지계열  $7EB336; //녹색계열

  COLOR_OPERATOR_GREEN   = $7EB336;
  COLOR_OPERATOR_BLUE    = $FF9933;
  COLOR_OPERATOR_ORANGE  = $22AFF7;
  COLOR_OPERATOR_PURPLE  = $FF4AC2;

  COLOR_FRAME_GRAY = $B3AEB2;

  COLOR_SELECTED_DARK_ORANGE = $3056FF; //찐오랜지계열
  COLOR_SELECTED_RED = $2014E5; //빨간계열
  COLOR_SELECTED_BLUE = $D77800;// $D47702; //파란계열

  MARGIN_TOP = 5;
  MARGIN_LEFT = 5;

  WIDTH_REMOVE_BUTTON = 20;      //삭제버튼
  WIDTH_FIELD_NAME_AREA = 100;   //필드명
  WIDTH_OPERATOR_AREA = 25;      //연산자
  WIDTH_RELATIONAL_AREA = 50;    //관계연산자
  HEIGHT_RELATIONAL_AREA = 20;
  WIDTH_CONDITION_CONTROL = 400; //조건식 전체크기
  HEIGHT_CONDITION_CONTROL = 25;

type
  //패턴 종류
  TPatternInfoType = (pitDetectBlock, pitIPPool, pitApplication, pitReputation, pitACL, pitRatelimitDynamic, pitRateLimitStatic, pitRateLimitVoIP);

  //데이터 종류
  TFieldValueType = (dtkUnknown, dtkString, dtkNumber, dtkIPv4, dtkIPv6, dekDate);
  TIPValueType = (ipkUnknown, ipkSingle, ipkPrefix, ipkRange);

  //값 연산자
  TOperatorOption = (opUnknown, opEQ, opGT, opGTE, opLT, opLTE, opNE, opNOT, opIN, opRange, opContains, opStartWith, opEndWith);
  TOperatorOptions = set of TOperatorOption;
  //관계 연산자
  TRelationalOperatorOption = (ropNONE, ropAND, ropOR);
  //조건의 작업 종류
  TConditionWorkMode = (wmEdit, wmRemove, wmOperatorMenu, wmSelected, wmUnSelected);
  //조건 표시 정보 종류 (삭제버튼, 조건정보, 관계연산)
  TConditionObjectType = (cotRemoveButton, cotConditionInfo, cotRelational);

  //콜백 이벤트 핸들
  TConditionWorkNotifyEvent = procedure(Sender: TObject; AWorkMode : TConditionWorkMode) of object;
  TConditionFieldValidationEvent = procedure(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue: String; var Valid: boolean) of object;

  TConditionRelationalOpLabel = class;
  TConditionItem = class;

  //조건 정보 삭제 버튼 객체
  TConditionRemoveButton = class(TSpeedButton)
  private
    FConditionItem: TConditionItem;
    FID: string;
    FConditionWorkEvent: TConditionWorkNotifyEvent;
    procedure OnClickEvent(Sender: TObject);
  public
    constructor Create(AOwner : TComponent; Condi : TConditionItem); reintroduce;
    destructor Destroy; override;

    property ID : string read FID write FID;
    property ConditionItem : TConditionItem read FConditionItem write FConditionItem;
    property OnWorkEvent : TConditionWorkNotifyEvent read FConditionWorkEvent write FConditionWorkEvent;
  end;

  //조건 정보 표시 객체
  TConditionInfoLabel = class(TCustomLabel)
  private
    FID: string;
    FSelected: Boolean;
    FConditionItem: TConditionItem;
    FOpertorAreaInMouse : boolean;
    FValueAreaInMouse : boolean;
    FConditionWorkEvent: TConditionWorkNotifyEvent;
    FSelectedColor: TColor;
    procedure SetSelected(const Value: boolean);
    procedure OnClickEvent(Sender: TObject);
    procedure OnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnDbClickEvent(Sender: TObject);
  public
    constructor Create(AOwner : TComponent; Condi : TConditionItem); reintroduce;
    destructor Destroy; override;
    procedure Paint; override;
    procedure InputFrameView;

    property ID : string read FID write FID;
    property Selected : boolean read FSelected write SetSelected;
    property SelectedColor : TColor read FSelectedColor write FSelectedColor;
    property ConditionItem : TConditionItem read FConditionItem write FConditionItem;
    property OnWorkEvent : TConditionWorkNotifyEvent read FConditionWorkEvent write FConditionWorkEvent;
  end;

  //관계연산 표시 객체
  TConditionRelationalOpLabel = class(TCustomLabel)
  private
    FConditionItem: TConditionItem;
    FID: string;
    procedure OnRelationalOpClickEvent(Sender: TObject);
  public
    constructor Create(AOwner : TComponent; Condi : TConditionItem); reintroduce;
    destructor Destroy; override;
    procedure Paint; override;

    property ID : string read FID write FID;
    property ConditionItem : TConditionItem read FConditionItem write FConditionItem;
  end;

  //조건정보 메인 객체
  TConditionItem = class(TCollectionItem)
  private
    FID : string;
    FPrevID: string;
    FNextID: string;
    FOrder : integer;
    FFieldName : string;
    FFieldValue : string;
    FIPValueKind: TIPValueType;
    FFieldValueDisplay: string;
    FDataTypeKind: TFieldValueType;
    FUseableOperatorOptions: TOperatorOptions;
    FOperatorOption: TOperatorOption; //연결된 이전조건이 있을경우 해당조건과의 관계연산임. <주의, 다음조건과의 관계가 아님, 최초 조건이 추가될시 다음 조건이 없으로, 이후로 추가될시 이전 조건과의 관계로 해석>
    FFieldNameDisplay: string;
    FOperatorColor: TColor;
    FRelationalOption: TRelationalOperatorOption;
    FMinValue: UInt64;
    FMaxValue: UInt64;
    FMinLength: integer;
    FMaxLength: integer;
    FInvalidMsg: string;
    FValidated: boolean;
    function GetDisplayFieldName: string;
    function GetDisplayFieldValue: string;
    function GetOperatorViewStr: string;
    function GetOperatorRealStr: string;
    function GetRelationalOperatorStr: string;
    function GetConditionRealValue: string;
    procedure SetPrevID(const Value: string);
    procedure SetFieldValue(const Value: string);
  public
    constructor Create(AOwner : TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TConditionItem); reintroduce;

    property Validated : boolean read FValidated write FValidated;
    property InvalidMsg : string read FInvalidMsg write FInvalidMsg;

  published
    property ID : string read FID write FID;
    property PrevID : string read FPrevID write SetPrevID;
    property NextID : string read FNextID write FNextID;
    property Order : integer read FOrder write FOrder;
    property MinLength : integer read FMinLength write FMinLength;
    property MaxLength : integer read FMaxLength write FMaxLength;
    property MinValue : UInt64 read FMinValue write FMinValue;
    property MaxValue : UInt64 read FMaxValue write FMaxValue;
    property FieldName : string read FFieldName write FFieldName;
    property FieldValue : string read FFieldValue write SetFieldValue;
    property DataTypeKind : TFieldValueType read FDataTypeKind write FDataTypeKind;
    property IPValueKind : TIPValueType read FIPValueKind write FIPValueKind;
    property OperatorOption : TOperatorOption read FOperatorOption write FOperatorOption;
    property UseableOperatorOptions : TOperatorOptions read FUseableOperatorOptions write FUseableOperatorOptions;
    property OperatorColor : TColor read FOperatorColor write FOperatorColor;
    property RelationalOption : TRelationalOperatorOption read FRelationalOption write FRelationalOption;
    property DisplayFieldName : string read GetDisplayFieldName write FFieldNameDisplay;
    property DisplayFieldValue : string read GetDisplayFieldValue write FFieldValueDisplay;
  end;

  //조건정보 객체 목록 관리용
  TConditionCollection = class(TCollection)
  private
    FOwner: TComponent;
    FPatternType : TPatternInfoType;
    FNoRecursiveUpdate: Boolean;
    FOnEditShow: TNotifyEvent;  //편집시 콜백하여 정보선택 화면구동 실행 하도록
    FOnConditionFieldValueValidation: TConditionFieldValidationEvent; //값 유효성 체크 콜백 이벤트
    function GetItem(id : string) : TConditionItem; overload;
    function GetItem(Index: integer): TConditionItem; overload;
    procedure SetItem(Index: integer; const Value: TConditionItem);
    procedure SetPrevCondition(curr, prev: TConditionItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent ; APatternType : TPatternInfoType); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); reintroduce;

    function isAllValidate : boolean;
    function GetLastOrder : integer;
    function ContainsKey(key : string) : boolean;
    function GetOwner: TPersistent; override;
    function Add(prev : TConditionItem): TConditionItem;
    function Insert(index: Integer): TConditionItem;
    function GetConditionSqlQuery : string;

    property Items[index: integer]: TConditionItem read GetItem write SetItem; default;
    property PatternType : TPatternInfoType read FPatternType write FPatternType;
    property OnEditShow : TNotifyEvent read FOnEditShow write FOnEditShow;
    property OnConditionFieldValueValidation : TConditionFieldValidationEvent read FOnConditionFieldValueValidation write FOnConditionFieldValueValidation;
  end;

  //조건정보 객체 표시 윈도우 컨트롤
  TConditionViewControl = class(TScrollBox)
  private
    FConditionItems : TConditionCollection;
    FCurrentCondition : TConditionItem;
    FViewObjects : TDictionary<string, TObject>; //각 조건을 표기할 View 컨트로 객체 목록
    FEditValue : TEdit;
    FESCKeyUse : Boolean;
    FInControl : boolean;

    procedure ViewCondition(index : integer; value : TConditionItem);
    function GetConditionViewObject(CondiID : String; oType : TConditionObjectType) : TObject;
    function GetLastOrder: integer;
    procedure SetPopupMenuActive;
    function GetOpertorMenuCaption(op : TOperatorOption) : string;

    function GetConditionItems: TConditionCollection;
    procedure SetConditionItems(const Value: TConditionCollection);

    procedure OnControlClick(Sender: TObject);
    procedure OnControlMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnControlMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnControlMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnControlMouseEnter(Sender: TObject);
    procedure OnControlMouseLeave(Sender: TObject);

    procedure OnConditionWork(Sender: TObject; AWorkMode : TConditionWorkMode);
    procedure OnConditionOPClick(Sender: TObject);

    procedure OnEditChange(Sender: TObject);
    procedure OnEditKeyPress(Sender: TObject; var Key: Char);
    procedure OnEditExit(Sender: TObject);

    function Delete(value : TConditionItem) : boolean;
    procedure DeleteViewObject(id : string);
    procedure CreatePopupMenu;
    procedure ClearPopupMenu;
    procedure ClearViewObject;
    procedure SelectedCondition(Condi : TConditionItem; AState : Boolean);
    procedure UpdateCondition(Condi : TConditionItem);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function GetLastObject : TConditionItem;
    function GetConditionQuery : string;
    function GetConditionViewText : string;
    procedure AllViewCondition;
  published
    property ConditionItems: TConditionCollection read GetConditionItems write SetConditionItems;
  end;

implementation

uses
  uFunctionUnit;

function ByteToHexStr(by: TBytes; bSwap: Boolean = False): String;
var
  i: integer;
begin
  Result := '';
  for i in by do
  begin
    if bSwap then
      Result := Lowercase(IntToHex(i, 2)) + Result
    else
      Result := Result + Lowercase(IntToHex(i, 2));
  end;
end;

function GenerateUUID : string;
var
  type4Uuid: TGUID;
  byte: TBytes;
begin
  Result := '';
  try
    SetLength(byte, 16);
    OleCheck(CoCreateGUID(type4Uuid));
    Move(type4Uuid.D1, byte[0], 16);
  except on E:Exception do
		raise Exception.Create(format('%s <%s>', ['GenerateUUID fail.', E.Message]));
  end;
  result := ByteToHexStr(byte);
end;

{ TConditionViewControl }
procedure TConditionViewControl.ClearPopupMenu;
begin
  if PopupMenu <> nil then
    PopupMenu.Items.Clear;
end;

procedure TConditionViewControl.ClearViewObject;
var
  obj : TObject;
begin
  if FViewObjects = nil then exit;

  for obj in FViewObjects.Values do
  begin
    if obj <> nil then
    begin
      if obj is TConditionRemoveButton then
        TConditionRemoveButton(obj).Free
      else if obj is TConditionInfoLabel then
        TConditionInfoLabel(obj).Free
      else if obj is TConditionRelationalOpLabel then
        TConditionRelationalOpLabel(obj).Free;
    end;
  end;

  FViewObjects.Clear;
end;

constructor TConditionViewControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Name := 'ConditionViewControl';

  Parent := TWinControl(AOwner);
  BorderStyle := bsNone;
  DoubleBuffered := True;
  //VertScrollBar.Increment := 15;
  OnClick := OnControlClick;
  //OnMouseWheel := OnControlMouseWheel;
  OnMouseWheelDown := OnControlMouseWheelDown;
  OnMouseWheelUp := OnControlMouseWheelUp;
  OnMouseEnter := OnControlMouseEnter;
  OnMouseLeave := OnControlMouseLeave;

  //조건정보 목록 편집용 객체
  FConditionItems := TConditionCollection.Create(AOwner, pitDetectBlock);

  //조건정보 화면표시 객체 목록
  FViewObjects := TDictionary<string, TObject>.Create;

  //값 변경시 입력받을 컨트롤
  FEditValue := TEdit.Create(self);
  FEditValue.Parent := TWinControl(self);
  FEditValue.BevelInner := bvNone;
  FEditValue.BevelOuter := bvNone;
  FEditValue.BorderStyle := bsNone;
  FEditValue.OnKeyPress := OnEditKeyPress;
  FEditValue.OnExit := OnEditExit;
  FEditValue.OnChange := OnEditChange;
  FEditValue.Visible := false;
  FEditValue.ShowHint := True;

  //자체 연산자 선택 팝업메뉴
  CreatePopupMenu;
end;

function TConditionViewControl.GetOpertorMenuCaption(op: TOperatorOption): string;
begin
  result := '';
  case op of
    opUnknown: result := '';
    opEQ: result := 'Equal';
    opGT: result := 'Greater than';
    opLT: result := 'Less than';
    opGTE: result := 'Greater than or equal';
    opLTE: result := 'Less than or equal';
    opNE: result := 'Not Equal';
    opNOT: result := 'Not ';
    opIN: result := 'IN';
    opRANGE: result := 'Range';
    opContains: result := 'Contains';
    opStartWith: result := 'StartWith';
    opEndWith: result := 'EndWith';
  end;
end;

procedure TConditionViewControl.CreatePopupMenu;
var
  opValue : TOperatorOption;
  item : TMenuItem;
begin
  if PopupMenu = nil then
    PopupMenu := TPopupMenu.Create(self);

  //연산자 종류 만큼 메뉴 추가
  for opValue := Low(TOperatorOption) to High(TOperatorOption) do
  begin
    if opValue = opUnknown then continue;
    item := TMenuItem.Create(self);
    item.Name := GetEnumName(TypeInfo(TOperatorOption), Ord(opValue));
    item.OnClick := OnConditionOPClick;
    item.Caption := GetOpertorMenuCaption(opValue);
    PopupMenu.Items.Add(item);
  end;
end;

function TConditionViewControl.Delete(value: TConditionItem) : boolean;
var
  NextObj, PrevObj : TConditionItem;
begin
  result := false;

  if ConditionItems.ContainsKey(value.id) then
  begin
    if value.NextID = '' then //다음이 없는 마지막 삭제면
    begin
      if value.PrevID <> '' then
      begin
        PrevObj := ConditionItems.GetItem(value.PrevID);
        if PrevObj <> nil then
          PrevObj.NextID := '';
      end;
    end
    else if (value.PrevID = '') then //이전이 없은 처음것 삭제면
    begin
      if value.NextID <> '' then
      begin
        NextObj := ConditionItems.GetItem(value.NextID);
        if NextObj <> nil then
          NextObj.PrevID := '';
      end;
    end
    else if (value.PrevID <> '') and (value.NextID <> '') then  //중간 삭제
    begin
      //삭제되는 값의
      //  이전객체의 다음객체를 현재 삭제되는 객체의 다음 객채로 변경
      //  다음객체의 이전객체를 현재 삭제되는 객체의 이전 객체로 변경
      if value.PrevID <> '' then
      begin
        PrevObj := ConditionItems.GetItem(value.PrevID);
        if PrevObj <> nil then
          PrevObj.NextID := value.NextID;
      end;

      if value.NextID <> '' then
      begin
        NextObj := ConditionItems.GetItem(value.NextID);
        if NextObj <> nil then
          NextObj.PrevID := value.PrevID;
      end;
    end;

    DeleteViewObject(value.ID);
    ConditionItems.Delete(value.Index);

    result := true;
  end;
end;

procedure TConditionViewControl.DeleteViewObject(id: string);
var
  obj : TObject;
begin
  obj := GetConditionViewObject(id, cotRemoveButton);
  if obj <> nil then
  begin
    FViewObjects.Remove(TConditionRemoveButton(obj).ID);
    TConditionRemoveButton(obj).Free;
  end;

  obj := GetConditionViewObject(id, cotConditionInfo);
  if obj <> nil then
  begin
    FViewObjects.Remove(TConditionInfoLabel(obj).ID);
    TConditionInfoLabel(obj).Free;
  end;

  obj := GetConditionViewObject(id, cotRelational);
  if obj <> nil then
  begin
    FViewObjects.Remove(TConditionRelationalOpLabel(obj).ID);
    TConditionRelationalOpLabel(obj).Free;
  end;
end;

destructor TConditionViewControl.Destroy;
begin
  ClearPopupMenu;
  PopupMenu.Free;

  ClearViewObject;
  FViewObjects.Free;

  FConditionItems.Clear;

  inherited Destroy;
end;

function TConditionViewControl.GetConditionItems: TConditionCollection;
begin
  Result := FConditionItems;
end;

//조건 입력값을 개별연산 및 관계연산 반영하여 Query 문법 으로 반환
function TConditionViewControl.GetConditionQuery: string;
var
  value, NextObj : TConditionItem;
  I: Integer;
  bStartBrace : integer;
  sValue : string;
begin
  result := '';
  bStartBrace := 0;
  //우선순위로 정렬한후 조건값으로 쿼리문 생성
  for I := 0 to ConditionItems.Count - 1 do
  begin
    value := ConditionItems.Items[i];
    if value = nil then continue;
    sValue := '';

    NextObj := ConditionItems.GetItem(value.NextID);
    //다음조건 있으면 관계연산자 AND 체크 하며 (  ) 처리
    if NextObj <> nil then
    begin
      //1. 괄호 열지 판단.
      //다음 조건이 있으면서 관계연산이 AND 라면 괄호 열기
      if (NextObj.RelationalOption = ropAND) and (value.RelationalOption <> ropAND) then
      begin
        sValue := '(';
        inc(bStartBrace);
      end;

      //2. 현재 조건 붙이기
      sValue := format('%s%s', [sValue, value.GetConditionRealValue]);

      //3. 다음 조건과 관계에 따라 괄호 닫을지 판다.
      if (NextObj.RelationalOption <> ropAND) then
      begin
        if bStartBrace > 0 then
        begin
          sValue := sValue + ')';
          dec(bStartBrace);
        end;
      end;

      //4. 다음 조건과의 관계 연산 붙이기
      sValue := format('%s %s', [sValue, NextObj.GetRelationalOperatorStr]);
    end
    else
    begin //마지막 조건인 경우 괄호가 열려있는경우만 괄호 닫기.
      //1. 현재 조건 붙이기
      sValue := format('%s%s', [sValue, value.GetConditionRealValue]);
      if bStartBrace > 0 then
      begin
        sValue := sValue + ')';
        dec(bStartBrace);
      end;
    end;

    //값 전체 붙이기
    result := format('%s %s', [result, sValue]);

  end;
end;

function TConditionViewControl.GetConditionViewObject(CondiID: String; oType: TConditionObjectType): TObject;
var
  obj : TObject;
begin
  result := nil;
  for obj in FViewObjects.Values do
  begin
    case oType of
      cotRemoveButton:
      begin
        if obj is TConditionRemoveButton then
        begin
          if TConditionRemoveButton(obj).ConditionItem.ID = CondiID then
          begin
            result := obj;
            break;
          end;
        end;
      end;
      cotConditionInfo:
      begin
        if obj is TConditionInfoLabel then
        begin
          if TConditionInfoLabel(obj).ConditionItem.ID = CondiID then
          begin
            result := obj;
            break;
          end;
        end;
      end;
      cotRelational:
      begin
        if obj is TConditionRelationalOpLabel then
        begin
          if TConditionRelationalOpLabel(obj).ConditionItem.ID = CondiID then
          begin
            result := obj;
            break;
          end;
        end;
      end;
    end;
  end;

end;

function TConditionViewControl.GetConditionViewText: string;
var
  value : TConditionItem;
  i : integer;
begin
  result := '';
  if ConditionItems.Count > 1 then
  begin
    result := '(Multiple Condition)';
  end
  else
  begin
    for i := 0 to ConditionItems.Count - 1 do
    begin
      value := ConditionItems.Items[i];
      result := format('%s : %s', [value.DisplayFieldName, value.DisplayFieldValue]);
    end;
  end;
end;


function TConditionViewControl.GetLastOrder: integer;
var
  value : TConditionItem;
  i : integer;
begin
  result := 0;
  if ConditionItems.Count <= 0 then exit;
  for i := 0 to ConditionItems.Count - 1 do
  begin
    value := ConditionItems.Items[i];
    if result < value.Order then
      result := value.Order;
  end;
end;

function TConditionViewControl.GetLastObject: TConditionItem;
var
  value : TConditionItem;
  i, index : integer;
begin
  result := nil;
  index := -1;
  if ConditionItems.Count <= 0 then exit;

  for i := 0 to ConditionItems.Count - 1 do
  begin
    value := ConditionItems.Items[i];
    if index < value.index then
    begin
      index := value.Index;
      result := value;
    end;
  end;
end;

procedure TConditionViewControl.OnControlClick(Sender: TObject);
begin
  Self.SetFocus;
  if FCurrentCondition <> nil then
  begin
    SelectedCondition(FCurrentCondition, false);
    FCurrentCondition := nil;
  end;
end;

//View용 객체 사용시 콜백받는 이벤트 (삭제버튼클릭, 조건정보선택(전체영역, 연산자영역), 값영역 더블클릭으로 편집시작)
procedure TConditionViewControl.OnConditionWork(Sender: TObject; AWorkMode: TConditionWorkMode);
var
  Condi : TConditionItem;
  obj : TObject;
  infolabel : TConditionInfoLabel;
  curpos : TPoint;
begin
  Self.SetFocus;

  if Sender = nil then exit;
  Condi := TConditionItem(Sender);

  //이전 선택된 값 선택 해제 상태
  if (FCurrentCondition <> nil) and (FCurrentCondition.ID <> Condi.ID) then
    SelectedCondition(FCurrentCondition, false);

  //값 편집상태가 아니면 이전 값 입력상태를 마무리
  if AWorkMode <> wmEdit then
  begin
    //값을 입력하는중 다른것을 선택시 기존 선택된 값에 값 반영 먼저.
    if (FEditValue.Visible) and (FCurrentCondition <> nil) then
    begin
      FCurrentCondition.FieldValue := FEditValue.Text;
      FCurrentCondition.DisplayFieldValue := FEditValue.Text;
      FEditValue.Visible := false;
      UpdateCondition(FCurrentCondition);
    end;
  end;

  //현재 조건 선택 상태
  SelectedCondition(condi, true);

  case AWorkMode of
    wmEdit :
    begin
      //값 편집시 값 표시위치에 Edit 컨트롤을 표기하여 값 입력 받는다.
      obj := GetConditionViewObject(condi.ID, cotConditionInfo);
      if obj <> nil then
      begin
        infolabel := TConditionInfoLabel(obj);
        FEditValue.Top := infolabel.Top + 6;
        FEditValue.Left := infolabel.left + WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA + 3;
        FEditValue.Width := WIDTH_CONDITION_CONTROL - WIDTH_FIELD_NAME_AREA - WIDTH_OPERATOR_AREA - 6;
        FEditValue.Height := HEIGHT_CONDITION_CONTROL - 7;
        FEditValue.Text := condi.FieldValue;
        FEditValue.Hint := '';
        FEditValue.Visible := true;
        FEditValue.SetFocus;
      end;
      //선택된 조건객체 변경
      FCurrentCondition := Condi;
    end;
    wmRemove :
    begin
      if MessageDlg('해당 조건을 삭제 하시겠습니까?', mtConfirmation, [mbYes, mbNo], 0 ) = mrYes then
      begin
        //조건 객체 삭제후 전체 조건 다시 표시
        if Delete(Condi) then
          AllViewCondition;
      end
      else
      begin
        //취소시 현재 조건 선택 해제상태
        SelectedCondition(condi, false);
        FCurrentCondition := nil;
      end;
    end;
    wmOperatorMenu:
    begin
      //선택된 조건객체 변경
      FCurrentCondition := Condi;
      //연산자 팝업메뉴 표시
      SetPopupMenuActive;
      GetCursorPos(curpos);
      PopupMenu.Popup(curpos.x, curpos.y);
    end;
    wmSelected:
    begin
      //선택된 조건객체 변경
      FCurrentCondition := Condi;
    end;
    wmUnSelected:
    begin
      SelectedCondition(condi, false);
      FCurrentCondition := nil;
    end;
  end;
end;

procedure TConditionViewControl.OnEditChange(Sender: TObject);
var
  bValid : boolean;
  AChar : Char;
  sValue : string;
begin
  if FCurrentCondition = nil then exit;

  //조회 클래스 에서 필드 유효성 처리하도록 콜백
  AChar := #0;

  sValue := FCurrentCondition.FieldValue;
  if FEditValue.Visible then
    sValue := FEditValue.Text;

  if Assigned(FConditionItems.OnConditionFieldValueValidation) then
  begin
    try
      //체크 클래스에서는 오류 메세지를 담은 예외를 발생시킨다.
      FConditionItems.OnConditionFieldValueValidation(FCurrentCondition, FConditionItems.PatternType, AChar, sValue, bValid);
      if bValid then
      begin
        FEditValue.Hint := '';
        FCurrentCondition.InvalidMsg := '';
        FCurrentCondition.Validated := True;
        UpdateCondition(FCurrentCondition);
      end;
    except on E : Exception do
      begin
        FEditValue.Hint := E.Message;
        FCurrentCondition.InvalidMsg := E.Message;
        FCurrentCondition.Validated := False;
        UpdateCondition(FCurrentCondition);
      end;
    end;
  end;

end;

procedure TConditionViewControl.OnEditExit(Sender: TObject);
begin
  if (FCurrentCondition = nil) or (FESCKeyUse) then
  begin
    FESCKeyUse := False;
    Exit;
  end;
  //Edit 포커스 벗어날시 입력값 반영
  FCurrentCondition.FieldValue := FEditValue.Text;
  FCurrentCondition.DisplayFieldValue := FEditValue.Text;
  FEditValue.Visible := false;
  UpdateCondition(FCurrentCondition);
end;

procedure TConditionViewControl.OnEditKeyPress(Sender: TObject; var Key: Char);
var
  bValid : boolean;
begin
  FESCKeyUse := False;
  case key of
    #27 : //Esc : 입력 취소
    begin
      FESCKeyUse := True;
      FEditValue.Visible := false;
      SelectedCondition(FCurrentCondition, false);
      FCurrentCondition := nil;
    end;
    #13 : //Enter : 입력 반영
    begin
      if FCurrentCondition = nil then exit;
      FCurrentCondition.FieldValue := FEditValue.Text;
      FCurrentCondition.DisplayFieldValue := FEditValue.Text;
      UpdateCondition(FCurrentCondition);
      FEditValue.Visible := false;
      SelectedCondition(FCurrentCondition, false);
      FCurrentCondition := nil;
    end;
    else // 키 입력시 체크해야할 유효성 체크 (연결된 콜백 이벤트 호출)
    begin
      if Assigned(FConditionItems.OnConditionFieldValueValidation) then
      begin
        try
          FConditionItems.OnConditionFieldValueValidation(FCurrentCondition, FConditionItems.PatternType, Key, FEditValue.Text, bValid);
          if not bValid then
            Key := #0;
        except
          Key := #0;
        end;
      end;
    end;
  end;
end;

procedure TConditionViewControl.OnControlMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if not FInControl then exit;

  with Self.VertScrollBar do
    Position := Position + HEIGHT_CONDITION_CONTROL;
end;

procedure TConditionViewControl.OnControlMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if not FInControl then exit;

  with Self.VertScrollBar do
    Position := Position - HEIGHT_CONDITION_CONTROL;
end;

procedure TConditionViewControl.OnControlMouseEnter(Sender: TObject);
begin
  Self.SetFocus;
  FInControl := True;
end;

procedure TConditionViewControl.OnControlMouseLeave(Sender: TObject);
var
  MousePos: TPoint;
  control : TWinControl;
begin
  GetCursorPos(MousePos);
  control := FindVCLWindow(MousePos);
  if (control <> nil) then
  begin
    if (not (control is TConditionViewControl)) then
      FInControl := False;
  end;
end;

procedure TConditionViewControl.OnControlMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  h : HWND;
begin
(*
  h := WindowFromPoint(MousePos);

  if WheelDelta >= 0 then
    SendMessage(h, WM_VSCROLL, SB_LINELEFT, 0)
  else
    SendMessage(h, WM_VSCROLL, SB_LINERIGHT, 0);
*)
  Handled := True;

end;

//선택상태를 변경하고, 테두리를 갱신한다.
procedure TConditionViewControl.SelectedCondition(Condi : TConditionItem; AState : Boolean);
var
  obj : TObject;
begin
  if (Condi <> nil) then
  begin
    obj := GetConditionViewObject(Condi.ID, cotConditionInfo);
    if obj <> nil then
      TConditionInfoLabel(obj).Selected := AState;
  end;
end;

//팝업메뉴 클릭해서 자체 연산자 변경시
procedure TConditionViewControl.OnConditionOPClick(Sender: TObject);
var
  opValue : TOperatorOption;
begin
  if FCurrentCondition = nil then exit;
  opValue := TOperatorOption(GetEnumValue(TypeInfo(TOperatorOption), TMenuItem(Sender).Name));
  FCurrentCondition.OperatorOption := opValue;

  //연산자 변경시에도 유효성 체크 수행
  OnEditChange(Self);
end;

procedure TConditionViewControl.SetConditionItems(const Value: TConditionCollection);
begin
  //조건객체 다시 할당시 이전에 사용하던 조건객체 정보표시용 객체(삭제버튼, 조건정보라벨, 관계연산라벨)  모두 제거
  ClearViewObject;
  //편집중인 조건목록을 View 컨트롤에 복제
  FConditionItems.Assign(value);
  //화면에서 사용되는 편집정보 관리용 객체에도 유효성 체크 콜백이벤트 매핑
  FConditionItems.OnConditionFieldValueValidation := value.OnConditionFieldValueValidation;
  //할당 받은 조건으로 그리기
  AllViewCondition;
end;

procedure TConditionViewControl.SetPopupMenuActive;
var
  opValue : TOperatorOption;
  mItem : TMenuItem;
begin
  if FCurrentCondition = nil then exit;

  //팝업 메뉴의 이름으로 연산자 enmu 값 확인하고. 사용가능한 연산자목록에 있면 활성화
  for mItem in PopupMenu.Items do
  begin
    opValue := TOperatorOption(GetEnumValue(TypeInfo(TOperatorOption), mItem.Name));
    mItem.Enabled := (opValue in FCurrentCondition.UseableOperatorOptions);
  end;
end;

//조건 정보가 변경되었을때 정보 부분 다시 그리기
procedure TConditionViewControl.UpdateCondition(Condi: TConditionItem);
var
  obj : TObject;
begin
  if (Condi <> nil) then
  begin
    obj := GetConditionViewObject(Condi.ID, cotConditionInfo);
    if obj <> nil then
    begin
      TConditionInfoLabel(obj).Repaint;
      if Condi.InvalidMsg <> ''  then
        TConditionInfoLabel(obj).Hint := Condi.InvalidMsg
      else
        TConditionInfoLabel(obj).Hint := format('%s : %s', [Condi.DisplayFieldName, Condi.DisplayFieldValue]);
    end;
  end;
end;

procedure TConditionViewControl.ViewCondition(index : integer; value: TConditionItem);
var
  iHeight, iValueTop : integer;
  PrevCondi : TConditionItem;
  ViewObj : TObject;
  RemoveButton : TConditionRemoveButton;
  InfoObj : TConditionInfoLabel;
  RelationalObj : TConditionRelationalOpLabel;
begin
  if value = nil then exit;

  //구분해야할 객체 단위로 라인을 분리시 Top 값을 조정해야함.
  iValueTop := MARGIN_TOP + (index * (HEIGHT_CONDITION_CONTROL + 1));

  // 각 조건객체별 화면에 표기되는 View용객체(삭제버튼, 조건정보, 관계연산)를 생성하고 위치를 설정한다.
  //1. 삭제버튼 (해당 객체 찾아 없으면 생성)
  RemoveButton := nil;
  ViewObj := GetConditionViewObject(value.ID, cotRemoveButton);
  if ViewObj <> nil then
    RemoveButton := TConditionRemoveButton(ViewObj);

  if RemoveButton = nil then
  begin
    RemoveButton := TConditionRemoveButton.Create(self, value);
    RemoveButton.Parent := TWinControl(self);
    RemoveButton.OnWorkEvent := OnConditionWork;
    RemoveButton.Cursor := crHandPoint;
    RemoveButton.Visible := False;
    RemoveButton.Caption := '×';
    //생성후 관리 목록에 추가
    FViewObjects.Add(RemoveButton.ID, RemoveButton);
  end;
  RemoveButton.Top := iValueTop - 1;
  RemoveButton.Left := MARGIN_LEFT;
  RemoveButton.Width := WIDTH_REMOVE_BUTTON + 2;
  RemoveButton.Height := HEIGHT_CONDITION_CONTROL + 2;
  RemoveButton.Visible := True;

  //2. 조건 정보 (해당 객체 찾아 없으면 생성)
  InfoObj := nil;
  ViewObj := GetConditionViewObject(value.ID, cotConditionInfo);
  if ViewObj <> nil then
    InfoObj := TConditionInfoLabel(ViewObj);

  if InfoObj = nil then
  begin
    InfoObj := TConditionInfoLabel.Create(self, value);
    InfoObj.Parent := TWinControl(self);
    InfoObj.OnWorkEvent := OnConditionWork;
    InfoObj.ShowHint := True;
    InfoObj.Hint := format('%s : %s', [value.DisplayFieldName, value.DisplayFieldValue]);
    //생성후 관리 목록에 추가
    FViewObjects.Add(InfoObj.ID, InfoObj);
  end;
  InfoObj.Top := iValueTop;
  InfoObj.Left := RemoveButton.Left + RemoveButton.Width;
  InfoObj.Width := WIDTH_CONDITION_CONTROL;
  InfoObj.Height := HEIGHT_CONDITION_CONTROL;
  InfoObj.Visible := true;

  //3. 관계연산 (해당 객체 찾아 없으면 생성)
  PrevCondi := ConditionItems.GetItem(value.PrevID);
  RelationalObj := nil;
  ViewObj := GetConditionViewObject(value.ID, cotRelational);
  if ViewObj <> nil then
    RelationalObj := TConditionRelationalOpLabel(ViewObj);

  //관계연산 객체 위치 설정 및 표시 (위치 및 보기/감추기) - 지금 표시되는 객체 이전 조건도 있고 관계 연산이 설정된 경우 위치 조정
  if (PrevCondi <> nil) and (value.RelationalOption <> ropNONE) then
  begin
    if RelationalObj = nil then
    begin
      RelationalObj := TConditionRelationalOpLabel.Create(self, value);
      RelationalObj.Parent := TWinControl(self);
      RelationalObj.ShowHint := True;
      //생성후 관리 목록에 추가
      FViewObjects.Add(RelationalObj.ID, RelationalObj);
    end;
    //현재 조건에 있는 관계연산객체를 이전과 현재 사이에 표시
    RelationalObj.Top     := InfoObj.Top - (InfoObj.Height div 2) + 3;
    RelationalObj.Left    := InfoObj.Left + InfoObj.Width + 1;
    RelationalObj.Width   := WIDTH_RELATIONAL_AREA;
    RelationalObj.Height  := HEIGHT_RELATIONAL_AREA;
    RelationalObj.Visible := True;
  end;

  //이전조건이 없다면 관계연산 미표시 처리
  if (RelationalObj <> nil) and (PrevCondi = nil) then
    RelationalObj.Visible := False;

  //스크롤 위치 조정
  iHeight := InfoObj.Top + InfoObj.Height + 5;
  VertScrollBar.Range := iHeight;
end;

procedure TConditionViewControl.AllViewCondition;
var
  i : integer;
  value : TConditionItem;
begin
  if ConditionItems = nil then exit;

  VertScrollBar.Position := 0;
  for i := 0 to ConditionItems.Count - 1 do
  begin
    value := ConditionItems.Items[i];
    if value = nil then continue;
    ViewCondition(i, value);
  end;
  VertScrollBar.Position := VertScrollBar.Range;
end;

{ TConditionRelationalOpLabel }

constructor TConditionRelationalOpLabel.Create(AOwner: TComponent; Condi : TConditionItem);
begin
  inherited Create(AOwner);

  Self.Parent := TWinControl(AOwner);
  Self.ShowHint := True;
  Self.OnClick := OnRelationalOpClickEvent;
  Self.Cursor := crHandPoint;
  Self.Visible := False;
  Self.ConditionItem := Condi;

  FID := GenerateUUID;
end;

destructor TConditionRelationalOpLabel.Destroy;
begin

  inherited;
end;

procedure TConditionRelationalOpLabel.OnRelationalOpClickEvent(Sender: TObject);
begin
  if FConditionItem.RelationalOption = ropAND then
    FConditionItem.RelationalOption := ropOR
  else
    FConditionItem.RelationalOption := ropAND;

  Paint;
end;

procedure TConditionRelationalOpLabel.Paint;
var
  ARect : TRect;
  sOperator : string;
begin
  inherited;

  Canvas.Font.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Size := 8;

  if Self.Visible then
  begin
    ARect := Rect(0, 0, Width, Height);
    case ConditionItem.RelationalOption of
      ropAND:
      begin
        sOperator := 'AND';
        Canvas.Brush.Color := COLOR_RELATIONAL_AND;
        Canvas.Pen.Color   := COLOR_RELATIONAL_AND;
        Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 4, 4);
        Canvas.TextRect(ARect, sOperator, [tfSingleLine, tfCenter, tfVerticalCenter]);
      end;
      ropOR:
      begin
        sOperator := 'OR';
        Canvas.Brush.Color := COLOR_RELATIONAL_OR;
        Canvas.Pen.Color   := COLOR_RELATIONAL_OR;
        Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 4, 4);
        Canvas.TextRect(ARect, sOperator, [tfSingleLine, tfCenter, tfVerticalCenter]);
      end;
    end;
  end;
end;


{ TConditionCollection }

function TConditionCollection.Add(prev : TConditionItem): TConditionItem;
begin
  Result := TConditionItem(inherited Add);
  if prev <> nil then
    SetPrevCondition(result, prev);
end;

procedure TConditionCollection.Assign(Source: TPersistent);
var
  I : Integer;
  prev : TConditionItem;
begin
  if Source is TCollection then
  begin
    BeginUpdate;
    try
      // Item 삭제후 원본으로 할당.
      while Count > 0 do
        TCollectionItem(Items[Count - 1]).DisposeOf;

      prev := nil;
      for I := 0 to TCollection(Source).Count - 1 do
      begin
        if i > 0 then
          prev := TConditionItem(TCollection(Source).Items[I-1]);

        Add(prev).Assign(TConditionItem(TCollection(Source).Items[I]));
      end;

      //그 외 프로퍼티 할당
      PatternType := TConditionCollection(Source).PatternType;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TConditionCollection.Clear;
begin
  inherited Clear;
end;

function TConditionCollection.ContainsKey(key: string): boolean;
var
  i : integer;
  item : TConditionItem;
begin
  result := false;
  for I := 0 to self.Count - 1 do
  begin
    item := Items[i];
    if item.id = key then
    begin
      result := true;
      break;
    end;
  end;
end;

constructor TConditionCollection.Create(AOwner: TComponent; APatternType : TPatternInfoType);
begin
  inherited Create(TConditionItem);
  FOwner := AOwner;
  FPatternType := APatternType;
  FNoRecursiveUpdate := False;
end;

destructor TConditionCollection.Destroy;
begin
  Clear;
  inherited;
end;

function TConditionCollection.GetConditionSqlQuery: string;
var
  value, NextObj : TConditionItem;
  I: Integer;
  bStartBrace : integer;
  sValue : string;
begin
  result := '';
  bStartBrace := 0;
  //우선순위로 정렬한후 조건값으로 쿼리문 생성
  for I := 0 to Count - 1 do
  begin
    value := Items[i];
    if value = nil then continue;
    sValue := '';

    NextObj := GetItem(value.NextID);
    //다음조건 있으면 관계연산자 AND 체크 하며 (  ) 처리
    if NextObj <> nil then
    begin
      //1. 괄호 열지 판단.
      //다음 조건이 있으면서 관계연산이 AND 라면 괄호 열기
      if (NextObj.RelationalOption = ropAND) and (value.RelationalOption <> ropAND) then
      begin
        sValue := '(';
        inc(bStartBrace);
      end;

      //2. 현재 조건 붙이기
      sValue := format('%s%s', [sValue, value.GetConditionRealValue]);

      //3. 다음 조건과 관계에 따라 괄호 닫을지 판다.
      if (NextObj.RelationalOption <> ropAND) then
      begin
        if bStartBrace > 0 then
        begin
          sValue := sValue + ')';
          dec(bStartBrace);
        end;
      end;

      //4. 다음 조건과의 관계 연산 붙이기
      sValue := format('%s %s', [sValue, NextObj.GetRelationalOperatorStr]);
    end
    else
    begin //마지막 조건인 경우 괄호가 열려있는경우만 괄호 닫기.
      //1. 현재 조건 붙이기
      sValue := format('%s%s', [sValue, value.GetConditionRealValue]);
      if bStartBrace > 0 then
      begin
        sValue := sValue + ')';
        dec(bStartBrace);
      end;
    end;

    //값 전체 붙이기
    result := format('%s %s', [result, sValue]);

  end;
end;

function TConditionCollection.GetItem(Index: integer): TConditionItem;
begin
  Result := TConditionItem(inherited GetItem(Index));
end;

function TConditionCollection.GetItem(id: string): TConditionItem;
var
  i : integer;
  item : TConditionItem;
begin
  result := nil;
  for I := 0 to self.Count - 1 do
  begin
    item := Items[i];
    if item.ID = id then
    begin
      result := item;
      break;
    end;
  end;
end;

function TConditionCollection.GetLastOrder: integer;
var
  i : integer;
  item : TConditionItem;
begin
  result := 0;
  for I := 0 to self.Count - 1 do
  begin
    item := Items[i];
    if item.Order > result then
      result := item.Order;
  end;
end;

function TConditionCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TConditionCollection.Insert(index: Integer): TConditionItem;
var
  prev, next : TConditionItem;
begin
  //삽입 위치 앞뒤 객체 찾아두고
  prev := nil;
  if index > 0 then
    prev := Items[index - 1];

  next := Items[index];

  //삽입하고
  Result := TConditionItem(inherited Insert(Index));

  //앞/뒤 객체와 ID 연결
  if prev <> nil then
    Result.PrevID := prev.ID;

  if next <> nil then
  begin
    result.NextID := next.ID;
    next.PrevID := result.ID;
  end;
end;

function TConditionCollection.isAllValidate: boolean;
var
  condi : TConditionItem;
  i : integer;
begin
  result := true;
  for i := 0 to self.Count - 1 do
  begin
    condi := Items[i];
    if (condi <> nil) and (not condi.Validated) then
    begin
      result := false;
      break;
    end;
  end;
end;

(*
procedure TConditionCollection.OnDataValidationEvent(Sender: TObject; var AChar : Char; const AValue: String; var Valid: boolean);
begin
  if Assigned(FOnConditionValidationEvent) then
    FOnConditionValidationEvent(Sender, AChar, AValue, Valid);
end;
*)

//현재ID의 객체를 찾아서 이전ID 연결해주고, 이전객체의 다음ID를 현재ID를 넣어줌
procedure TConditionCollection.SetPrevCondition(curr, prev: TConditionItem);
begin
  curr.PrevID := prev.ID;
  prev.NextID := curr.ID;
end;

procedure TConditionCollection.SetItem(index: integer; const Value: TConditionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TConditionCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

{ TConditionItem }

procedure TConditionItem.Assign(Source: TConditionItem);
begin
  ID := Source.id;
  Order := Source.Order;
  PrevID := Source.PrevID;
  NextID := Source.NextID;
  MinLength := Source.MinLength;
  MaxLength := Source.MaxLength;
  MinValue := Source.MinValue;
  MaxValue := Source.MaxValue;
  FieldName := Source.FieldName;
  FieldValue := Source.FieldValue;
  DataTypeKind := Source.DataTypeKind;
  IPValueKind := Source.IPValueKind;
  OperatorColor := Source.OperatorColor;
  OperatorOption := Source.OperatorOption;
  UseableOperatorOptions := Source.UseableOperatorOptions;
  RelationalOption := Source.RelationalOption;
  DisplayFieldName := Source.DisplayFieldName;
  DisplayFieldValue := Source.DisplayFieldValue;
end;

constructor TConditionItem.Create(AOwner: TCollection);
begin
  inherited;

  FID := GenerateUUID;
  FPrevID := '';
  FNextID := '';
  FOrder := 0;
  FMinLength := 0;
  FMaxLength := 256;
  FMinValue := 0;
  FMaxValue := High(FMaxValue);
  FFieldName := '';
  FFieldValue := '';
  FIPValueKind := ipkUnknown;
  FDataTypeKind := dtkString;
  FFieldValueDisplay := '';
  FFieldNameDisplay := '';
  FOperatorColor := COLOR_OPERATOR_GREEN;
  FOperatorOption := opEQ;
  FUseableOperatorOptions := [opEQ];
  FRelationalOption := ropNONE;
  FValidated := True;
  FInvalidMsg := '';
end;

destructor TConditionItem.Destroy;
begin

  inherited;
end;

function TConditionItem.GetConditionRealValue: string;
var
  value, value1, value2 : string;
  sList : TStringList;
begin
  value := FieldValue;
  case FOperatorOption of
    opUnknown: result := '';
    opEQ:
    begin
      case DataTypeKind of
       dtkUnknown,
       dtkString : result := format('(%s = ''%s'')', [FieldName, value]);
       dtkNumber : result := format('(%s = %s)', [FieldName, value]);
       dtkIPv4 :;
       dtkIPv6 :;
       dekDate :;
      end;
    end;
    opGT:
    begin
      result := format('(%s > %s)', [FieldName, value]);
    end;
    opLT:
    begin
      result := format('(%s < %s)', [FieldName, value]);
    end;
    opGTE:
    begin
      result := format('(%s >= %s)', [FieldName, value]);
    end;
    opLTE:
    begin
      result := format('(%s <= %s)', [FieldName, value]);
    end;
    opNE:
    begin
      result := format('(%s != %s)', [FieldName, value]);
      if DataTypeKind = dtkString then
       result := format('(%s != ''%s'')', [FieldName, value]);
    end;
    opIN:
    begin
       result := format('(%s IN (%s))', [FieldName, value]);
    end;
    opRange:
    begin
      //comma 기준으로 분리해서 앞,뒤 값 사용
      sList := TStringList.Create;
      try
        sList.CommaText := value;
        value1 := sList[0];
        value2 := sList[1];
        result := format('((%s >= %s) AND (%s <= %s))', [FieldName, value1, FieldName, value2]);
      finally
        sList.Free;
      end;
    end;
    opContains: result := format('(%s LIKE ''%%%s%%'')', [FieldName, value]);
    opStartWith: result := format('(%s LIKE ''%s%%'')', [FieldName, value]);
    opEndWith: result := format('(%s LIKE ''%%%s'')', [FieldName, value]);
  end;

end;

function TConditionItem.GetDisplayFieldName: string;
begin
  Result := FFieldNameDisplay;
end;

function TConditionItem.GetDisplayFieldValue: string;
var
  sList : TStringList;
begin
  Result := FFieldValueDisplay;

  if OperatorOption = opRange then
  begin
    sList := TStringList.Create;
    try
      sList.CommaText := FFieldValueDisplay;
      if sList.Count = 1 then
        result := format('%s ~ %s', [sList[0], sList[0]])
      else if sList.Count = 2 then
        result := format('%s ~ %s', [sList[0], sList[1]]);
    finally
      sList.Free;
    end;
  end;
end;

//실제 쿼리에 사용하는 연산자
function TConditionItem.GetOperatorRealStr: string;
begin
  case FOperatorOption of
    opUnknown: result := '';
    opEQ: result := '=';
    opGT: result := '>';
    opLT: result := '<';
    opGTE: result := '>=';
    opLTE: result := '<=';
    opNE: result := '<>';
    opNOT: result := '!';
    opIN: result := 'IN';
    opRange: result := '';
    opContains: result := 'LIKE';
    opStartWith: result := 'LIKE';
    opEndWith: result := 'LIKE';
  end;
end;

//화면에 표기되는 연산자
function TConditionItem.GetOperatorViewStr : string;
begin
  case FOperatorOption of
    opUnknown: result := '';
    opEQ: result := '=';
    opGT: result := '>';
    opLT: result := '<';
    opGTE: result := '>=';
    opLTE: result := '<=';
    opNE: result := '<>';
    opNOT: result := '!';
    opIN: result := 'IN';
    opRange: result := '~';
    opContains: result := '%%';
    opStartWith: result := '~%';
    opEndWith: result := '%~';
  end;
end;

function TConditionItem.GetRelationalOperatorStr: string;
begin
  result := '';
  case RelationalOption of
    ropAND: result := 'AND';
    ropOR: result := 'OR';
  end;
end;

procedure TConditionItem.SetFieldValue(const Value: string);
begin
  FFieldValue := Value;
end;

procedure TConditionItem.SetPrevID(const Value: string);
begin
  FPrevID := Value;
end;

{ TConditionInfoLabel }

constructor TConditionInfoLabel.Create(AOwner: TComponent; Condi : TConditionItem);
begin
  inherited Create(AOwner);

  Self.Parent := TWinControl(AOwner);
  Self.ShowHint := True;
  Self.OnClick := OnClickEvent;
  Self.OnMouseMove := OnMouseMoveEvent;
  Self.OnDblClick := OnDbClickEvent;
  Self.Visible := False;
  Self.ConditionItem := Condi;
  Self.SelectedColor := COLOR_SELECTED_BLUE;//COLOR_SELECTED_RED;

  FID := GenerateUUID;
end;

destructor TConditionInfoLabel.Destroy;
begin

  inherited;
end;

procedure TConditionInfoLabel.InputFrameView;
var
  ARect : TRect;
begin
  // 조건 값 입력 영역 테두리 색상 변경
  ARect := Rect(WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA, 0, self.width, self.Height);
  Canvas.Brush.Color := COLOR_SELECTED_BLUE;
  Canvas.FrameRect(ARect);
end;

procedure TConditionInfoLabel.OnDbClickEvent(Sender: TObject);
begin
  if ConditionItem = nil then exit;
  if not FValueAreaInMouse then exit;

  if Assigned(FConditionWorkEvent) then
    FConditionWorkEvent(ConditionItem, wmEdit);
end;

procedure TConditionInfoLabel.OnClickEvent(Sender: TObject);
begin
  if ConditionItem = nil then exit;

  //연산자 영역 or 그 외 클릭시
  if Assigned(FConditionWorkEvent) then
  begin
    if FOpertorAreaInMouse then
      FConditionWorkEvent(ConditionItem, wmOperatorMenu) //연산자 영역 팝업메뉴 표시 처리
    else
      FConditionWorkEvent(ConditionItem, wmSelected); //객체 선택 상태 처리 (이전 선택 객체 UnSelected 처리를 하기위해 콜백 보내야 함)
  end;
end;


procedure TConditionInfoLabel.OnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  opRect : TRect;
  pt : TPoint;
begin
  inherited;
  pt.X := x;
  pt.Y := y;
  self.Cursor := crDefault;

  //연산자 영역
  opRect := Rect(WIDTH_FIELD_NAME_AREA, 0, WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA, self.Height);
  FOpertorAreaInMouse := PtInRect(opRect, pt);
  if FOpertorAreaInMouse then
    self.Cursor := crHandPoint;

  //값 영역
  opRect := Rect(WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA + 1, 1, self.Width - 1, self.Height - 1);
  FValueAreaInMouse := PtInRect(opRect, pt);
  if FValueAreaInMouse then
    self.Cursor := crIBeam;
end;

procedure TConditionInfoLabel.Paint;
var
  ARect : TRect;
  sOperator, sValue : string;
begin
  if ConditionItem = nil then exit;

  inherited;

  Canvas.Font.Style := [];
  Canvas.Font.Size := 8;

  //1. 필드명 표시 영역
  ARect := Rect(0, 0, WIDTH_FIELD_NAME_AREA, Height);
  //1-1. 배경색
  Canvas.Brush.Color := cl3DLight;
  Canvas.Font.Color := clBlack;
  Canvas.FillRect(ARect);
  //1-2. 테두리
  Canvas.Brush.Color := COLOR_FRAME_GRAY;
  Canvas.FrameRect(ARect);
  //1-3. 필드명
  Canvas.Brush.Color := cl3DLight;
  ARect.Left := ARect.Left + 3;
  sValue := ConditionItem.DisplayFieldName;
  Canvas.TextRect(ARect, sValue, [tfSingleLine, tfLeft, tfVerticalCenter]);

  //2. 필드 값 정보 영역
  //2-1. 테두리
  ARect := Rect(WIDTH_FIELD_NAME_AREA, 0, Width, Height);
  Canvas.Brush.Color := ConditionItem.OperatorColor;
  //유효성 오류시 빨간 테두리
  if not ConditionItem.Validated then
    Canvas.Brush.Color := COLOR_SELECTED_RED;
  Canvas.FrameRect(ARect);

  //2-2. 자체 연산자 표시 영역
  ARect := Rect(WIDTH_FIELD_NAME_AREA, 0, WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA, Height);
  Canvas.Brush.Color := ConditionItem.OperatorColor;
  //유효성 오류시 빨간 배경
  if not ConditionItem.Validated then
    Canvas.Brush.Color := COLOR_SELECTED_RED;
  Canvas.Font.Color := clWhite;
  Canvas.FillRect(ARect);
  sOperator := ConditionItem.GetOperatorViewStr;
  Canvas.TextRect(ARect, sOperator, [tfSingleLine, tfCenter, tfVerticalCenter]);

  //2-4. 조건값 표시 영역
  ARect := Rect(WIDTH_FIELD_NAME_AREA + WIDTH_OPERATOR_AREA + 1, 1, Width - 1, Height - 1);
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ARect);
  Canvas.Font.Color := clBlack;
  sValue := ConditionItem.DisplayFieldValue;
  ARect.Left := ARect.Left + 2;
  ARect.Width := ARect.Width - 3;
  Canvas.TextRect(ARect, sValue, [tfSingleLine, tfLeft, tfVerticalCenter]);

  if FSelected then
    SetSelected(True);
end;

procedure TConditionInfoLabel.SetSelected(const Value: boolean);
var
  ARect : TRect;
  sValue : string;
begin
  FSelected := Value;

  // 선택 색상 변경
  if FSelected then
  begin
    //1. 필드명 표시 영역
    ARect := Rect(0, 0, WIDTH_FIELD_NAME_AREA, Height);
    //1-1. 배경색
    Canvas.Brush.Color := SelectedColor;
    Canvas.Font.Color := clWhite;
    Canvas.FillRect(ARect);

    //1-2. 필드명
    ARect.Left := ARect.Left + 3;
    sValue := ConditionItem.DisplayFieldName;
    Canvas.TextRect(ARect, sValue, [tfSingleLine, tfLeft, tfVerticalCenter]);

    (*
    //2. 전체 테두리
    Canvas.Brush.Color := SelectedColor;
    ARect := Rect(0, 0, self.Width, self.Height);
    Canvas.Brush.Color := SelectedColor;
    Canvas.FrameRect(ARect);

    //2-1 (유효성 오류시 연산자 부분 부터 뒤쪽 값 부분까지 빨간테두리)
    if not ConditionItem.Validated then
    begin
      ARect := Rect(WIDTH_FIELD_NAME_AREA - 1, 0, Width, Height);
      Canvas.Brush.Color := COLOR_SELECTED_RED;
      Canvas.FrameRect(ARect);
    end;
    *)
  end
  else
    Repaint;
end;

{ TConditionRemoveButton }

constructor TConditionRemoveButton.Create(AOwner: TComponent; Condi : TConditionItem);
begin
  inherited Create(AOwner);

  Self.Parent := TWinControl(AOwner);
  Self.ShowHint := True;
  Self.OnClick := OnClickEvent;
  Self.Cursor := crHandPoint;
  Self.Visible := False;
  Self.ConditionItem := Condi;

  FID := GenerateUUID;
end;

destructor TConditionRemoveButton.Destroy;
begin

  inherited;
end;

procedure TConditionRemoveButton.OnClickEvent(Sender: TObject);
begin
  if ConditionItem = nil then exit;

  if Assigned(FConditionWorkEvent) then
    FConditionWorkEvent(ConditionItem, wmRemove);
end;



initialization

finalization

end.
