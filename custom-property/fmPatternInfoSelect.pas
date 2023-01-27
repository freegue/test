unit fmPatternInfoSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uConditionObject, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid, Vcl.ComCtrls;

type
  TfmPatternSelect = class(TForm)
    PanelCondition: TPanel;
    PanelSoureData: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    TreeView1: TTreeView;
    mGrid: TAdvStringGrid;
    Panel1: TPanel;
    btnCategory: TButton;
    btnAttackCode: TButton;
    btnAttackName: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mGridDblClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure btnCategoryClick(Sender: TObject);
    procedure btnAttackCodeClick(Sender: TObject);
    procedure btnAttackNameClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FConditionObjectMng : TConditionViewControl;
    procedure ConditionMngDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ConditionMngDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure AddCondition(fieldname, fieldViewName, RealValue, RealViewValue: string; DataType : TFieldValueType);
  public
    property ConditionObjectMng : TConditionViewControl read FConditionObjectMng write FConditionObjectMng;
  end;

var
  fmPatternSelect: TfmPatternSelect;

implementation

{$R *.dfm}

procedure TfmPatternSelect.mGridDblClick(Sender: TObject);
begin
  AddCondition('category', '공격유형', intTostr(mGrid.Row), mGrid.Cells[1, mGrid.Row], dtkString);
  AddCondition('code', '공격코드', mGrid.Cells[2, mGrid.Row], mGrid.Cells[2, mGrid.Row], dtkNumber);
  AddCondition('attack_name', '공격명', mGrid.Cells[3, mGrid.Row], mGrid.Cells[3, mGrid.Row], dtkString);
end;

procedure TfmPatternSelect.mGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if button = mbleft then
    (sender as TAdvStringGrid).BeginDrag(false);

end;

procedure TfmPatternSelect.TreeView1DblClick(Sender: TObject);
begin
  AddCondition('category', '공격유형', intTostr(TreeView1.Selected.Index), TreeView1.Selected.Text, dtkString);
end;

procedure TfmPatternSelect.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbleft then
    (sender as TTreeView).BeginDrag(false);
end;

procedure TfmPatternSelect.btnAttackCodeClick(Sender: TObject);
begin
  //AddCondition('category', '공격유형', intTostr(mGrid.Row), mGrid.Cells[1, mGrid.Row]);
  AddCondition('code', '공격코드', mGrid.Cells[2, mGrid.Row], mGrid.Cells[2, mGrid.Row], dtkNumber);
end;

procedure TfmPatternSelect.btnAttackNameClick(Sender: TObject);
begin
  //AddCondition('category', '공격유형', intTostr(mGrid.Row), mGrid.Cells[1, mGrid.Row]);
  AddCondition('attack_name', '공격명', mGrid.Cells[3, mGrid.Row], mGrid.Cells[3, mGrid.Row], dtkString);
end;

procedure TfmPatternSelect.btnCategoryClick(Sender: TObject);
begin
  AddCondition('category', '공격유형', intTostr(TreeView1.Selected.Index), TreeView1.Selected.Text, dtkString);
end;

procedure TfmPatternSelect.ConditionMngDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source is TTreeView then
  begin
    AddCondition('category', '공격유형', intTostr(TTreeView(Source).Selected.Index), TTreeView(Source).Selected.Text, dtkString);
  end
  else
  if Source is TAdvStringGrid then
  begin
    AddCondition('category', '공격유형', intTostr(TAdvStringGrid(Source).Row), TAdvStringGrid(Source).Cells[1, TAdvStringGrid(Source).Row], dtkString);
    AddCondition('code', '공격코드', TAdvStringGrid(Source).Cells[2, TAdvStringGrid(Source).Row], TAdvStringGrid(Source).Cells[2, TAdvStringGrid(Source).Row], dtkNumber);
    AddCondition('attack_name', '공격명', TAdvStringGrid(Source).Cells[3, TAdvStringGrid(Source).Row], TAdvStringGrid(Source).Cells[3, TAdvStringGrid(Source).Row], dtkString);
  end;

end;

procedure TfmPatternSelect.ConditionMngDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := false;
  if (Source is TAdvStringGrid) or (Source is TTreeView) then
    Accept := True;
end;

procedure TfmPatternSelect.AddCondition(fieldname, fieldViewName, RealValue, RealViewValue : string; DataType : TFieldValueType);
var
  Condi, prev : TConditionItem;
begin
  prev := FConditionObjectMng.GetLastObject;
  Condi := FConditionObjectMng.ConditionItems.Add(prev);
  Condi.FieldName := fieldname;
  Condi.FieldValue := RealValue;
  Condi.DataTypeKind := DataType;
  Condi.OperatorOption := opEQ;
  Condi.UseableOperatorOptions := [opEQ, opNE, opNOT, opRange, opContains, opStartWith, opEndWith];
  Condi.RelationalOption := ropAND;
  Condi.DisplayFieldName := fieldViewName;
  Condi.DisplayFieldValue := RealViewValue;

  FConditionObjectMng.AllViewCondition;
end;

procedure TfmPatternSelect.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not ConditionObjectMng.ConditionItems.isAllValidate then
  begin
    MessageDlg('유효성에 맞지 않는 조건이 있습니다.' + #13 + '조건 값을 확인하십시오.', mtWarning, [mbOK], 0);
    ModalResult := mrCancel;
    CanClose := false;
  end;

end;

procedure TfmPatternSelect.FormCreate(Sender: TObject);
begin
  FConditionObjectMng := TConditionViewControl.Create(PanelCondition);
  FConditionObjectMng.OnDragDrop := ConditionMngDragDrop;
  FConditionObjectMng.OnDragOver := ConditionMngDragOver;
  FConditionObjectMng.Align := alClient;

  mGrid.Cells[1,1] := '서비스거부';
  mGrid.Cells[1,2] := '서비스공격';
  mGrid.Cells[1,3] := '정보수집';
  mGrid.Cells[1,4] := '패턴블럭';
  mGrid.Cells[1,5] := '프로토콜 취약점';

  mGrid.Cells[2,1] := '100001';
  mGrid.Cells[2,2] := '100002';
  mGrid.Cells[2,3] := '100003';
  mGrid.Cells[2,4] := '100004';
  mGrid.Cells[2,5] := '100005';

  mGrid.Cells[3,1] := 'TCP SYN Flooding';
  mGrid.Cells[3,2] := 'UDP Flooding';
  mGrid.Cells[3,3] := 'ICMP Smurf';
  mGrid.Cells[3,4] := 'Ping Flooding';
  mGrid.Cells[3,5] := 'Ping Sweep';

  mGrid.AutoSizeColumns(True);

  FConditionObjectMng.AllViewCondition;
end;

procedure TfmPatternSelect.FormDestroy(Sender: TObject);
begin
  FConditionObjectMng.Free;
end;

end.
