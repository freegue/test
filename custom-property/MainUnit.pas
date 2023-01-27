unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, acHeaderControl,
  Vcl.ToolWin, Vcl.Samples.Spin, System.JSON, System.Generics.Collections,
  fmPropertyInspector, uDefineType, uConditionObject,
  uPropertyConditionCore, uPropertyConditionDetect, uPropertyConditionBlock, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.Menus, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, AdvCGrid;

type
  TForm1 = class(TForm)
    TopPanel: TPanel;
    RightPanel: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Memo1: TMemo;
    ButtonQuery: TButton;
    Panel2: TPanel;
    ToolBarSaveLoad: TToolBar;
    ToolButtonDetect: TToolButton;
    ToolButtonBlock: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ActionDetect: TAction;
    ActionBlock: TAction;
    ActionTrafficAll: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionEventFilter: TAction;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    Button1: TButton;
    Splitter2: TSplitter;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Button3: TButton;
    Edit1: TEdit;
    procedure ButtonQueryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionDetectExecute(Sender: TObject);
    procedure ActionBlockExecute(Sender: TObject);
    procedure ActionTrafficAllExecute(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Exit(Sender: TObject);
  private
    FInspector: TformPropertyInspector;
    FLogQueryCondition : TPropertyConditionCore;

    FConditionObjectMng : TConditionViewControl;
    FConditionItems : TConditionCollection;

    procedure CreateInspector;
    procedure OnConditionIntial(Sender: TObject);
    procedure OnConditionModifyEvent(Sender: TObject; AModifyMode : TConditionWorkMode);
    procedure OnPatternConditionValidate(Sender: TObject; APatternType : TPatternInfoType; var AChar: Char; const AValue: String; var Valid: boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses fmPatternInfoSelect;


procedure TForm1.ActionBlockExecute(Sender: TObject);
begin
  memo1.Lines.Add('차단 로그조회');

  FreeAndNil(FLogQueryCondition);

  //block log
  FLogQueryCondition := TBlockLogCondition.Create(nil);
  FLogQueryCondition.Name := 'BlockLogCondition';

  FInspector.CurrentObjects := FLogQueryCondition;
  FInspector.OnDataValidate := FLogQueryCondition.OnDataValidate;
  FInspector.PropertyTitle := '차단로그조회';
end;

procedure TForm1.ActionDetectExecute(Sender: TObject);
begin
  memo1.Lines.Add('탐지 로그조회');

  FreeAndNil(FLogQueryCondition);

  //detect log
  FLogQueryCondition := TDetectLogCondition.Create(Panel1);
  FLogQueryCondition.Name := 'DetectLogCondition';

  //유효성 설정 예시
  //TDetectLogCondition(FLogQueryCondition).desc.MinLength := 1;
  //TDetectLogCondition(FLogQueryCondition).desc.MaxLength := 5;
  //TDetectLogCondition(FLogQueryCondition).desc.AllowCharSet := ['0' .. '9'];
  //TDetectLogCondition(FLogQueryCondition).desc.DenyCharSet := ['0' .. '9'];

  FInspector.CurrentObjects := FLogQueryCondition;
  FInspector.OnDataValidate := FLogQueryCondition.OnDataValidate;
  FInspector.PropertyTitle := '탐지로그조회';
end;

procedure TForm1.ActionTrafficAllExecute(Sender: TObject);
begin
  memo1.Lines.Add('전체트레픽 로그조회');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FConditionObjectMng.ConditionItems.Clear;
  FConditionObjectMng.AllViewCondition;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if fmPatternSelect = nil then
    fmPatternSelect := TfmPatternSelect.Create(self);

  if fmPatternSelect.ShowModal = mrOK then
  begin
    memo1.Lines.Add(fmPatternSelect.ConditionObjectMng.GetConditionQuery);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : integer;
  item : TConditionItem;
begin
  if FLogQueryCondition = nil then exit;

  for i := 0 to TDetectLogCondition(FLogQueryCondition).pattern_condition.Count - 1 do
  begin
    item := TDetectLogCondition(FLogQueryCondition).pattern_condition.Items[i];
    memo1.Lines.Add( item.id );
    memo1.Lines.Add( item.FieldName );
    memo1.Lines.Add( item.FieldValue );
  end;

end;

procedure TForm1.ButtonQueryClick(Sender: TObject);
var
  JsonResult : TJSONObject;
begin
  memo1.Lines.Add('조회 조건을 쿼리문으로 변환후 반환 필요');
  JsonResult := FLogQueryCondition.ConditionToJson;
  memo1.Lines.Add(JsonResult.ToJSON);
end;

procedure TForm1.CreateInspector;
begin
  if FInspector = nil then
  begin
    FInspector := TformPropertyInspector.Create(Self);
    FInspector.Parent := RightPanel;
    FInspector.BorderStyle := bsNone;
    FInspector.Align := alClient;
    FInspector.OnObjectInitial := OnConditionIntial;
    FInspector.Show;
  end;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  memo1.lines.add('exit');
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  memo1.lines.add('Edit1KeyPress ' + key);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateInspector;
  FConditionObjectMng := TConditionViewControl.Create(Panel1);
  FConditionObjectMng.Align := alClient;

  FConditionItems := TConditionCollection.Create(FConditionObjectMng, pitDetectBlock);
  FConditionItems.OnConditionFieldValueValidation := OnPatternConditionValidate;
  FConditionObjectMng.ConditionItems := FConditionItems;

end;

procedure TForm1.OnPatternConditionValidate(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue: String; var Valid: boolean);
var
  condi : TConditionItem;
begin
  Valid := True;
  if Sender = nil then exit;
  condi := TConditionItem(Sender);
  if AChar <> #0 then
  begin
    //1. 입력 문자 허용/불허 체크
    Valid := CharInSet(AChar, ['0'..'9', #8, #16]);
  end
  else
  begin
    //2. 값 길이 체크
    if (length(AValue) < condi.MinLength) or (length(AValue) > condi.MaxLength) then
    begin
      Valid := false;
      raise Exception.Create('invaild length error');
    end;

    //3. 값 범위 체크
    if (length(AValue) < condi.MinLength) or (length(AValue) > condi.MaxLength) then
    begin
      Valid := false;
      raise Exception.Create('invaild length error');
    end;

  end;
end;

procedure TForm1.OnConditionModifyEvent(Sender: TObject; AModifyMode : TConditionWorkMode);
var
  Condi : TConditionItem;
begin
  Condi := TConditionItem(Sender);
  showmessage(Condi.FieldName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FConditionItems.Free;
  FConditionObjectMng.Free;
end;

procedure TForm1.OnConditionIntial(Sender: TObject);
begin
  FreeAndNil(FLogQueryCondition);
  if ToolButtonDetect.Down then
  begin
    FLogQueryCondition := TDetectLogCondition.Create(nil);
    FLogQueryCondition.Name := 'DetectLogCondition';

    //유효성 설정 예시
    //TDetectLogCondition(FLogQueryCondition).desc.MinLength := 1;
    //TDetectLogCondition(FLogQueryCondition).desc.MaxLength := 5;
    //TDetectLogCondition(FLogQueryCondition).desc.AllowCharSet := ['0' .. '9'];
    //TDetectLogCondition(FLogQueryCondition).desc.DenyCharSet := ['0' .. '9'];

    //FLogQueryCondition.IPAddressType := itIPv6;
    FInspector.CurrentObjects := FLogQueryCondition;
    FInspector.PropertyTitle := '탐지로그조회';

  end
  else if ToolButtonBlock.Down then
  begin
    FLogQueryCondition := TBlockLogCondition.Create(nil);
    FLogQueryCondition.Name := 'BlockLogCondition';
    FInspector.CurrentObjects := FLogQueryCondition;
    FInspector.PropertyTitle := '차단로그조회';
  end;
end;


procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  memo1.Lines.Add('입력값 쿼리문 :' + FConditionObjectMng.GetConditionQuery);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
var
  Condi, lastObj : TConditionItem;
  coupleid : string;
begin
  coupleid := '';

  lastObj := FConditionObjectMng.GetLastObject;

  Condi := FConditionObjectMng.ConditionItems.Add(lastObj);
  Condi.FieldName := 'category';
  Condi.FieldValue := '1';
  Condi.DataTypeKind := dtkNumber;
  Condi.OperatorOption := opEQ;
  Condi.MinValue := 1;
  Condi.MaxValue := 65535;
  Condi.UseableOperatorOptions := [opEQ, opGT, opLT, opGTE, opLTE];
  Condi.DisplayFieldName := '공격유형';
  Condi.DisplayFieldValue := '서비스거부';

  lastObj := FConditionObjectMng.GetLastObject;
  Condi := FConditionObjectMng.ConditionItems.Add(lastObj);
  Condi.FieldName := 'attack_name';
  Condi.FieldValue := 'TEST NAME';
  Condi.DataTypeKind := dtkString;
  Condi.OperatorOption := opContains;
  Condi.UseableOperatorOptions := [opEQ, opNE, opContains, opStartWith, opEndWith];
  Condi.RelationalOption := ropOR;
  Condi.DisplayFieldName := '공격명';
  Condi.DisplayFieldValue := Condi.FieldValue;
  Condi.MaxLength := 32;

  lastObj := FConditionObjectMng.GetLastObject;
  Condi := FConditionObjectMng.ConditionItems.Add(lastObj);
  Condi.FieldName := 'code';
  Condi.FieldValue := '100002';
  Condi.DataTypeKind := dtkNumber;
  Condi.MaxLength := 6;
  Condi.MaxValue := 999999;
  Condi.OperatorOption := opEQ;
  Condi.UseableOperatorOptions := [opEQ, opGT, opLT, opGTE, opLTE, opIN, opRange];
  Condi.RelationalOption := ropOR;
  Condi.DisplayFieldName := '공격코드';
  Condi.DisplayFieldValue := Condi.FieldValue;

  lastObj := FConditionObjectMng.GetLastObject;
  Condi := FConditionObjectMng.ConditionItems.Add(lastObj);
  Condi.FieldName := 'code';
  Condi.FieldValue := '100002,100003,100004';
  Condi.DataTypeKind := dtkNumber;
  Condi.MaxLength := 6;
  Condi.MaxValue := 999999;
  Condi.OperatorOption := opIN;
  Condi.UseableOperatorOptions := [opEQ, opGT, opLT, opGTE, opLTE, opIN, opRange];
  Condi.RelationalOption := ropAND;
  Condi.DisplayFieldName := '공격코드';
  Condi.DisplayFieldValue := Condi.FieldValue;

  Condi := FConditionObjectMng.ConditionItems.Insert(Condi.Index);
  Condi.FieldName := 'code';
  Condi.FieldValue := '100002,100003,100004';
  Condi.DataTypeKind := dtkNumber;
  Condi.MaxLength := 6;
  Condi.MaxValue := 999999;
  Condi.OperatorOption := opIN;
  Condi.UseableOperatorOptions := [opEQ, opGT, opLT, opGTE, opLTE, opIN, opRange];
  Condi.RelationalOption := ropAND;
  Condi.DisplayFieldName := '맨뒤 앞에 삽입';
  Condi.DisplayFieldValue := Condi.FieldValue;

  FConditionObjectMng.AllViewCondition;
end;


end.
