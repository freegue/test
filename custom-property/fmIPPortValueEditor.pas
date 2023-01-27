{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : Inspector 에서 사용하는 IP/Port값 입력 대화상자
-------------------------------------------------------------------------------}

unit fmIPPortValueEditor;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Commctrl, Consts,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin,
  uDefineType, uFunctionUnit;


type
  TformIPPortValueEditor = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Bevel1: TBevel;
    PageControl: TPageControl;
    TabSheet: TTabSheet;
    EditFrom: TEdit;
    RadioButtonMulti: TRadioButton;
    RadioButtonRange: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    EditTo: TEdit;
    ButtonInitial: TButton;
    Bevel2: TBevel;
    ListBoxMulti: TListBox;
    ButtonAdd: TButton;
    ButtonDel: TButton;
    ButtonClear: TButton;
    Bevel3: TBevel;
    ButtonAddRange: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure RadioButtonRangeClick(Sender: TObject);
    procedure RadioButtonMultiClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure ListBoxMultiKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonInitialClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure EditFromEnter(Sender: TObject);
    procedure EditToEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAddRangeClick(Sender: TObject);
  private
    FDefaultFont : TFont;
    HandleFromIP, HandleToIP : THandle;
    FromIPInt, ToIPInt : Int64;
    FFromValue, FToValue : string;
    FValueType: TNetworkValueType;
    FDataType : TNetworkDataType;
    FInitialButtonUse: Boolean;
    PrevWndProc: TWndMethod;

    procedure RangeEnable(Value : Boolean);
    procedure MultiEnable(Value : Boolean);
    procedure SetFromValue(const Value: string);
    procedure SetToValue(const Value: string);
    function GetMultiValue: string;
    procedure SetValueType(const Value: TNetworkValueType);
    procedure SetMultiValue(const Value: string);
    procedure CreateIPv4EditControl;
  public
    constructor CreateForm(AOwner: TComponent; ADataType : TNetworkDataType);
    property InitialButtonUse : Boolean read FInitialButtonUse write FInitialButtonUse;
    property DataType : TNetworkDataType read FDataType write FDataType;
    property FromValue : string read FFromValue write SetFromValue;
    property ToValue : string read FToValue write SetToValue;
    property ValueType : TNetworkValueType read FValueType write SetValueType;
    property MultiValue : string read GetMultiValue write SetMultiValue;
  end;

implementation

{$R *.DFM}

const
  IP_ADDRESS_ID: Longword = $0100;

procedure TformIPPortValueEditor.SetToValue(const Value: string);
var
  E, E2 : integer;
begin
  case DataType of
    dtIPv4:
      begin
        if value <> '' then
        begin
          ToIPInt := IP4ToInt64(Value);
          if ToIPInt <= -1 then
          begin
            raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');
            exit;
          end;
          if HandleAllocated then
          begin
            SendMessage(HandleToIP, IPM_SETADDRESS, 0, ToIPInt);
            SendMessage(HandleToIP, IPM_SETFOCUS, 0, 0);
          end;
        end;
      end;
    dtIPv6:
      begin
        EditTo.Text := value;
      end;
    dtPort:
      begin
        if value <> '' then
        begin
          Val(Value, E2, E);
          if (E <> 0) or (E > 65535) then
          begin
            raise EConvertError.Create(Value + ' 값은 네트웍 포트 형식과 맞지 않습니다.');
            exit;
          end;
          EditTo.Text := IntTostr(E2);//Value;
        end;
      end;
  end;
  FToValue := Value;
end;

procedure TformIPPortValueEditor.SetFromValue(const Value: string);
var
  E, E2 : integer;
begin
  case DataType of
    dtIPv4:
      begin
        if Value <> '' then
        begin
          FromIPInt := IP4ToInt64(Value);
          if FromIPInt <= -1 then
          begin
            raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');
            exit;
          end;
          if HandleAllocated then
          begin
            SendMessage(HandleFromIP, IPM_SETADDRESS, 0, FromIPInt);
            SendMessage(HandleFromIP, IPM_SETFOCUS, 0, 0);
          end;
        end;
      end;
    dtIPv6:
      begin
        EditFrom.Text := value;
      end;
    dtPort:
      begin
        if value <> '' then
        begin
          Val(Value, E2, E);
          if (E <> 0) or (E > 65535) then
          begin
            raise EConvertError.Create(Value + ' 값은 네트웍 포트 형식과 맞지 않습니다.');
            exit;
          end;
          EditFrom.Text := IntTostr(E2);// Value;
        end;
      end;
  end;
  FFromValue := Value;
end;


procedure TformIPPortValueEditor.SetValueType(const Value: TNetworkValueType);
begin
  FValueType := Value;
  case Value of
    vtRange:
      begin
        RadioButtonRange.Checked := True;
        RadioButtonRange.OnClick(nil);
      end;
    vtMulti:
      begin
        RadioButtonMulti.Checked := True;
        RadioButtonMulti.OnClick(nil);
      end;
  end;
end;

procedure TformIPPortValueEditor.SetMultiValue(const Value: string);
begin
  ListBoxMulti.Clear;
  ListBoxMulti.Items.Delimiter := ',';
  ListBoxMulti.Items.DelimitedText := Value;
end;

procedure TformIPPortValueEditor.ButtonOKClick(Sender: TObject);
var
  ValueInt : integer;
//  ValueStr : string;
begin
  if ValueType = vtMulti then
  begin
    if ListBoxMulti.Items.Count = 0 then
    begin
      if Application.MessageBox('다중입력을 선택하신경우 1개 이상의 값을 입력하셔야합니다.'
        + #13#10#13#10 + '값을 입력하시려면 [예]를 선택하세요.' + #13#10 +
        '[아니오]를 선택하면 범위지정으로 바뀌고 기본값으로 설정됩니다.',
        PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION) = IDYES then
      begin
        Exit;
      end
      else
      begin
        RadioButtonRange.Checked := True;
        case DataType of
          dtIPv4: begin
                    EditFrom.Text := '0.0.0.0';
                    EditTo.Text := '255.255.255.255';
                  end;
          dtPort: begin
                    EditFrom.Text := '0';
                    EditTo.Text := '65535';
                  end;
        end;
      end;
    end;
  end
  else //단일
  begin
    case DataType of
      dtIPv4:
      begin
        SendMessage(HandleFromIP, IPM_GETADDRESS, 0, longint(@FromIPInt));
        SendMessage(HandleToIP, IPM_GETADDRESS, 0, longint(@ToIPInt));

        if (FromIPInt = 0) then
        begin
          Application.MessageBox('시작 IP 값을 확인하십시오.',
            PChar(Application.Title), MB_OK + MB_ICONWARNING);
          SendMessage(HandleFromIP, IPM_SETFOCUS, 0, 0);
          Exit;
        end;

        if (ToIPInt = 0) then
        begin
          Application.MessageBox('종료 IP 값을 확인하십시오.',
            PChar(Application.Title), MB_OK + MB_ICONWARNING);
          SendMessage(HandleToIP, IPM_SETFOCUS, 0, 0);
          Exit;
        end;

        FromValue := Format('%d.%d.%d.%d',
          [FIRST_IPADDRESS(FromIPInt),SECOND_IPADDRESS(FromIPInt),
          THIRD_IPADDRESS(FromIPInt),FOURTH_IPADDRESS(FromIPInt)]);

        ToValue := Format('%d.%d.%d.%d',
          [FIRST_IPADDRESS(ToIPInt),SECOND_IPADDRESS(ToIPInt),
          THIRD_IPADDRESS(ToIPInt),FOURTH_IPADDRESS(ToIPInt)]);
      end;
      dtIPv6:
      begin
        FromValue := EditFrom.Text;
        ToValue := EditTo.Text;
      end;
      dtPort:
      begin
        ValueInt := StrToIntDef(EditFrom.Text, -1);
        if (ValueInt > 65535) or (ValueInt = -1) then
        begin
          Application.MessageBox('시작값에 잘못된 Port 값을 입력하셨습니다.',
            PChar(Application.Title), MB_OK + MB_ICONWARNING);
          EditFrom.SetFocus;
          Exit;
        end;
        FromValue := IntToStr(ValueInt);

        ValueInt := StrToIntDef(EditTo.Text, -1);
        if (ValueInt > 65535) or (ValueInt = -1) then
        begin
          Application.MessageBox('종료값에 잘못된 Port 값을 입력하셨습니다.',
            PChar(Application.Title), MB_OK + MB_ICONWARNING);
          EditTo.SetFocus;
          Exit;
        end;
        ToValue := IntToStr(ValueInt);
      end;
    end;
  end;
  ModalResult := mrOk;
end;

constructor TformIPPortValueEditor.CreateForm(AOwner: TComponent; ADataType : TNetworkDataType);
begin
  inherited Create(AOwner);
  FDataType := ADataType;
end;

procedure TformIPPortValueEditor.CreateIPv4EditControl;
var
  lpInitCtrls: TInitCommonControlsEx;
begin
  if UseRightToLeftAlignment then FlipChildren(True);

  lpInitCtrls.dwSize := SizeOf(TInitCommonControlsEx);
  lpInitCtrls.dwICC  := ICC_INTERNET_CLASSES;
  if InitCommonControlsEx(lpInitCtrls) then
  begin
    //PrevWndProc := WindowProc;
    //WindowProc  := NewWindowProc;

    FDefaultFont := TFont(GetStockObject(DEFAULT_GUI_FONT));
    //Create FROM_IP
    HandleFromIP := CreateWindow(WC_IPADDRESS,
            PChar('FROM_IP'),
            WS_CHILD or WS_VISIBLE or WS_TABSTOP,
            TabSheet.Left + EditFrom.Left, TabSheet.Top + EditFrom.Top, EditFrom.Width, EditFrom.Height,
            PageControl.Handle,
            0,
            0,
            nil);

    //Create TO_IP
    HandleToIP := CreateWindow(WC_IPADDRESS,
            PChar('TO_IP'),
            WS_CHILD or WS_VISIBLE or WS_TABSTOP,
            TabSheet.Left + EditTo.Left, TabSheet.Top + EditTo.Top, EditTo.Width, EditTo.Height,
            PageControl.Handle,
            0,
            0,
            nil);

    SendMessage(HandleFromIP, WM_SETFONT, Integer(FDefaultFont), 1);
    SendMessage(HandleToIP, WM_SETFONT, Integer(FDefaultFont), 1);

    SetWindowPos(EditFrom.Handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    SetWindowPos(EditTo.Handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

    SetWindowPos(HandleFromIP, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    SetWindowPos(HandleToIP, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TformIPPortValueEditor.EditFromEnter(Sender: TObject);
begin
  case DataType of
    dtIPv4: SendMessage(HandleFromIP, IPM_SETFOCUS, 0, 0);
  end;
end;

procedure TformIPPortValueEditor.EditToEnter(Sender: TObject);
begin
  case DataType of
    dtIPv4: SendMessage(HandleToIP, IPM_SETFOCUS, 0, 0);
  end;
end;

procedure TformIPPortValueEditor.FormCreate(Sender: TObject);
begin
  case DataType of
    dtIPv4:
    begin
      Caption := 'IPv4 입력';
      //EditFrom.Width := 150;
      //EditTo.Width := 150;
      CreateIPv4EditControl;
    end;
    dtIPv6:
    begin
      Caption := 'IPv6 입력';
    end;
    dtPort:
    begin
      Caption := 'PORT 입력';
      SetWindowLong(EditFrom.Handle, GWL_STYLE, GetWindowLong(EditFrom.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);
      SetWindowLong(EditTo.Handle, GWL_STYLE, GetWindowLong(EditTo.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);
    end;
  end;
end;

procedure TformIPPortValueEditor.FormShow(Sender: TObject);
begin
  Self.Height := 337;
  if FInitialButtonUse then Self.Height := 370;
  ButtonInitial.Visible := FInitialButtonUse;
  EditFrom.SetFocus;
  if HandleFromIP > 0 then
    SendMessage(HandleFromIP, IPM_SETFOCUS, 0, 0);
end;

procedure TformIPPortValueEditor.ButtonAddClick(Sender: TObject);
var
  AValue : string;
  ValueInt : integer;
begin
  case DataType of
    dtIPv4:
    begin
      AValue := '';
      if InputIPv4Query('단일 IPv4 추가', 'IPv4 값을 입력하십시오.', AValue) then
      begin
        if Trim(AValue) = '' then Exit;
        if ListBoxMulti.Items.IndexOf(AValue) < 0 then
          ListBoxMulti.Items.Add(AValue);
      end;
    end;
    dtIPv6:
    begin
      AValue := '';
      if InputTextQuery('단일 IPv6 추가', 'IPv6 값을 입력하십시오', AValue) then
      begin
        if Trim(AValue) = '' then Exit;
        if ListBoxMulti.Items.IndexOf(AValue) < 0 then
          ListBoxMulti.Items.Add(AValue);
      end;
    end;
    dtPort:
    begin
      ValueInt := 0;
      if InputNumberQuery('단일 Port 추가', 'Port 값을 입력하십시오.', ValueInt, 0, 65535) then
      begin
        AValue := IntToStr(ValueInt);
        if ListBoxMulti.Items.IndexOf(AValue) < 0 then
          ListBoxMulti.Items.Add(AValue);
      end;
    end;
  end;
end;

procedure TformIPPortValueEditor.ButtonAddRangeClick(Sender: TObject);
var
  AValue : string;
begin
  case DataType of
    dtIPv4:
    begin
      AValue := '0.0.0.0~255.255.255.255';
      if InputIPv4QueryRange('범위 IPv4 추가', 'IPv4 값을 입력하십시오.', AValue) then
      begin
        if Trim(AValue) = '' then Exit;
        if ListBoxMulti.Items.IndexOf(AValue) < 0 then
          ListBoxMulti.Items.Add(AValue);
      end;
    end;
    dtIPv6:
    begin

    end;
    dtPort:
    begin
      AValue := '0';
      if InputNumberQueryRange('범위 Port 추가', 'Port 값을 입력하십시오.', AValue, 0, 65535) then
      begin
        if ListBoxMulti.Items.IndexOf(AValue) < 0 then
          ListBoxMulti.Items.Add(AValue);
      end;
    end;
  end;
end;

procedure TformIPPortValueEditor.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TformIPPortValueEditor.ButtonClearClick(Sender: TObject);
begin
  ListBoxMulti.Clear;
end;

procedure TformIPPortValueEditor.ButtonDelClick(Sender: TObject);
begin
  ListBoxMulti.DeleteSelected;
end;

procedure TformIPPortValueEditor.ButtonInitialClick(Sender: TObject);
begin
  FFromValue := '';
  FToValue := '';
  ListBoxMulti.Clear;
  ModalResult := mrOk;
end;

function TformIPPortValueEditor.GetMultiValue: string;
begin
  Result := ListBoxMulti.Items.CommaText;
end;

procedure TformIPPortValueEditor.ListBoxMultiKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    ListBoxMulti.DeleteSelected;
end;

procedure TformIPPortValueEditor.RadioButtonMultiClick(Sender: TObject);
begin
  RangeEnable(False);
  MultiEnable(True);
  FValueType := vtMulti;
end;

procedure TformIPPortValueEditor.RadioButtonRangeClick(Sender: TObject);
begin
  RangeEnable(True);
  MultiEnable(False);
  FValueType := vtRange;
  if HandleFromIP > 0 then
    SendMessage(HandleFromIP, IPM_SETFOCUS, 0, 0);
end;

procedure TformIPPortValueEditor.MultiEnable(Value: Boolean);
begin
  ListBoxMulti.Enabled := Value;
  ButtonAdd.Enabled := Value;
  ButtonAddRange.Enabled := Value;
  ButtonDel.Enabled := Value;
  ButtonClear.Enabled := Value;
end;

procedure TformIPPortValueEditor.RangeEnable(Value: Boolean);
begin
  EnableWindow(HandleFromIP, Value);
  EnableWindow(HandleToIP, Value);
  EnableWindow(EditFrom.Handle, Value);
  EnableWindow(EditTo.Handle, Value);
end;


end.

