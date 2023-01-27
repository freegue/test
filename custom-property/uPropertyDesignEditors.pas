{-------------------------------------------------------------------------------
  Copyright (C) 2022 WINS
  Project  :
  Author   : kyungmun
  Date     : 2022-07-07
  Comment  : 특정 데이터 타입의 프로퍼티 에디터
-------------------------------------------------------------------------------}

unit uPropertyDesignEditors;

interface

uses
  Windows, Classes, SysUtils, Graphics, Controls, StdCtrls, Forms, Menus, Variants, Generics.Collections,
  Dialogs, TypInfo, uPropertyDesignIntf, uDefineType, uConditionObject,
  {fmIPPortValueEditor,} fmDateTimeEditor;


{$M+}

type
  //IP, Port 정보 : 관리 클래스
  TIPPortValue = class(TPersistent)
  private
    FFromValue: String;
    FToValue: string;
    FValueType : TNetworkValueType;
    FDataType : TNetworkDataType;
    FMultiValue : string;
    FOnEditEvent: TNotifyEvent;

    procedure SetFromValue(const Value: string);
    procedure SetToValue(const Value: string);
    procedure SetMultiValue(const Value: string);
    procedure SetDataType(const Value: TNetworkDataType);
  public
    constructor Create(DataType : TNetworkDataType = dtIPv4);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnEditEvent : TNotifyEvent read FOnEditEvent write FOnEditEvent;
  published
    property ValueType : TNetworkValueType read FValueType write FValueType;
    property DataType : TNetworkDataType read FDataType write SetDataType;
    property MultiValue : string read FMultiValue write SetMultiValue;
    property FromValue: string read FFromValue write SetFromValue;
    property ToValue: string read FToValue write SetToValue;
  end;

  //IP, Port 정보 : 프로퍼티 매핑 클래스
  TIPPortValueProperty = class(TSniperClassProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
    function GetObject: TIPPortValue;
    function ValueType : TNetworkValueType;
    function DataType : TNetworkDataType;
  end;

  //여러 값중 선택 되야 하는데, 동적으로 변경이 되는 값일때 사용.
  TDynamicSelectValue = class(TPersistent)
  private
    FValueList: TStringList;
    FSelectValue: string;
    FOnEditEvent: TNotifyEvent;
    FFormTitle: string;
    FMultiSelected: boolean;
    procedure SetSelectValue(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FormTitle : string read FFormTitle write FFormTitle;
    property MultiSelected : boolean read FMultiSelected write FMultiSelected;
    property ValueList : TStringList read FValueList write FValueList;
    property OnEditEvent : TNotifyEvent read FOnEditEvent write FOnEditEvent;
  published
    property SelectValue: string read FSelectValue write SetSelectValue;
  end;

  //동적 선택형 목록 정보 : 프로퍼티 매핑 클래스
  TDynamicSelectValueProperty = class(TSniperClassProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
    function GetRealValue: String;
    function GetObject: TDynamicSelectValue;
  end;

  //날짜/시간 정보 : 관리 클래스
  TDateTimeValue = class(TPersistent)
  private
    FFrom: string;
    FTo: string;
    FRange: Integer;
    FDateSelectType: TDateSelectType;
    FSelectLimitDay: integer;
    procedure SetFromDateTime(const Value: string);
    procedure SetToDateTime(const Value: string);
    procedure SetDateSelectType(const Value: TDateSelectType);
  public
    constructor Create(ADateSelectType : TDateSelectType);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DateSelectType : TDateSelectType read FDateSelectType write SetDateSelectType;
    property SelectLimitDay : integer read FSelectLimitDay write FSelectLimitDay;
    property DateRange : Integer read FRange write FRange;
  published
    property FromDate: string read FFrom write SetFromDateTime;
    property ToDate: string read FTo write SetToDateTime;
  end;

  //날짜/시간 정보 : 프로퍼티 매핑 클래스
  TSniperDateTimeProperty = class(TSniperClassProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
    function FromDate : string;
    function ToDate : string;
  end;

  //읽기전용 문자 정보 : 관리 클래스
  TReadOnlyStringValue = class(TPersistent)
  private
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: string read FValue write FValue;
  end;

  //읽기전용 문자 정보 : 프로퍼티 매핑 클래스
  TStringReadOnlyProperty = class(TSniperPropertyEditor)
  private
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
    procedure GetValues; override;
  end;

  //유효성 처리 가능한 문자 정보 : 관리 클래스
  TValidationStringValue = class(TPersistent)
  private
    FValue: string;
    FMinLength: integer;
    FMaxLength: integer;
    FAllowCharSet: TSysCharSet;
    FDenyCharSet: TSysCharSet;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CharSetToStr(const CharSet: TSysCharSet): string;
    property AllowCharSet : TSysCharSet read FAllowCharSet write FAllowCharSet;
    property DenyCharSet : TSysCharSet read FDenyCharSet write FDenyCharSet;
  published
    property Value: string read FValue write FValue;
    property MaxLength : integer read FMaxLength write FMaxLength;
    property MinLength : integer read FMinLength write FMinLength;
  end;

  //유효성 처리 가능항 문자 정보 : 프로퍼티 매핑 클래스
  TValidationStringProperty = class(TSniperPropertyEditor)
  private
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  //패턴 개별 정보 : 관리 클래스 (탐지정책, IPPool, Applicaton,,,등)
  TPatternInfoValue = class(TPersistent)
  private
    FCategoryCode: TReadOnlyStringValue; //패턴 선택화면으로 3가지 정보를 선택하고, 선택된 값을 표기하지만, 개별로 입력할 수 없도록 읽기전용 String 타입으로 사용
    FPatternCodes: TReadOnlyStringValue;
    FPatternNames: TReadOnlyStringValue;
    FConditionView: TReadOnlyStringValue;
    FConditionReal: TReadOnlyStringValue;
    FOnEditEvent: TNotifyEvent;
    FPatternList : TDictionary<string, Pointer>;
    FPatternInfoKind : TPatternInfoType;
    FTestValue: TValidationStringValue;
  public
    constructor Create(AKind : TPatternInfoType);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property PatternList : TDictionary<string, Pointer> read FPatternList write FPatternList;
    property OnEditEvent : TNotifyEvent read FOnEditEvent write FOnEditEvent;
  published
    property CategoryCode: TReadOnlyStringValue read FCategoryCode write FCategoryCode;
    property PatternCodes: TReadOnlyStringValue read FPatternCodes write FPatternCodes;
    property PatternNames: TReadOnlyStringValue read FPatternNames write FPatternNames;
    property ConditionView: TReadOnlyStringValue read FConditionView write FConditionView;
    property ConditionReal: TReadOnlyStringValue read FConditionReal write FConditionReal;
    property TestValue: TValidationStringValue read FTestValue write FTestValue;
    property PatternInfoKind : TPatternInfoType read FPatternInfoKind write FPatternInfoKind;
  end;

  //패턴 개별 정보 : 프로퍼티 매핑 클래스
  TPatternInfoValueProperty = class(TSniperClassProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
  end;

  //TConditionCollection 각종 패턴들 멀티조건 처리 가능한 정보 클래스의 프로퍼티 매핑 클래스
  TPatternInfoConditionProperty = class(TSniperClassProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function Edit: Boolean; override;
    function GetValue: String; override;
    function GetConditionSqlQuery: String;
  end;


implementation

{ TIPPortValue }

procedure TIPPortValue.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TIPPortValue.Create(DataType : TNetworkDataType);
begin
  FValueType := vtRange;
  FDataType := DataType;
  case DataType of
    dtIPv4:
    begin
      FromValue:= '';//0.0.0.0';
      ToValue := '';//255.255.255.255';
    end;
    dtIPv6:
    begin
      FromValue := '';//0::0';
      ToValue := '';//ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff';
    end;
    dtPort:
    begin
      FromValue:= '';//0';
      ToValue := '';//65535';
    end;
  end;
  FMultiValue := '';

  HideProperties(TIPPortValue, 'MultiValue;FromValue;ToValue;DataType;ValueType');
end;

destructor TIPPortValue.Destroy;
begin
  inherited;
end;

procedure TIPPortValue.SetDataType(const Value: TNetworkDataType);
begin
  FDataType := Value;
  case FDataType of
    dtIPv4:
    begin
      FromValue:= '0.0.0.0';
      ToValue := '255.255.255.255';
    end;
    dtIPv6:
    begin
      FromValue := '0::0';
      ToValue := 'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff';
    end;
  end;
end;

procedure TIPPortValue.SetFromValue(const Value: string);
var
  E, E2 : integer;
  I, Idx, N: Integer;
  RealValue, Str: String;
begin
  RealValue := '';
  if Value <> '' then
  begin
    case FDataType of
      dtIPv4:
      begin
        Str := Value;
        try
          for I := 0 to 3 do
          begin
            Idx := Pos('.', Str);
            if Idx <= 0 then
              Idx := Length(Str) + 1
            else if Idx = 1 then
              raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');

            N := StrToInt(Copy(Str, 1, Idx - 1));
            if (N < 0) or (N > 255) then
              raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');

            Delete(Str, 1, Idx);
          end;
        except
          raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');
          Exit;
        end;
        RealValue := Value;
      end;
      dtIPv6:
      begin
        RealValue := Value;
      end;
      dtPort:
      begin
        str := Value;
        Val(str, E2, E);
        if E <> 0 then
        begin
          raise EConvertError.Create(Value + ' 값은 네트웍 포트 형식과 맞지 않습니다.');
          Exit;
        end;
        RealValue := IntTostr(E2);
      end;
    end;
  end;
  FFromValue := RealValue;
end;

procedure TIPPortValue.SetMultiValue(const Value: string);
begin
  FMultiValue := Value;
  //멀티를 선택했지만 값이 없으면 범위값으로 변경.
  if (Trim(Value) = '') then
    ValueType := vtRange;
end;

procedure TIPPortValue.SetToValue(const Value: string);
var
  E, E2 : integer;
  I, Idx, N: Integer;
  RealValue, Str: String;
begin
  RealValue := '';
  if Value <> '' then
  begin
    case FDataType of
      dtIPv4:
      begin
        Str := Value;
        try
          for I := 0 to 3 do
          begin
            Idx := Pos('.', Str);
            if Idx <= 0 then
              Idx := Length(Str) + 1
            else if Idx = 1 then
              raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');

            N := StrToInt(Copy(Str, 1, Idx - 1));
            if (N < 0) or (N > 255) then
              raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');

            Delete(Str, 1, Idx);
          end;
        except
          raise EConvertError.Create(Value + ' 값은 IPv4 형식과 맞지 않습니다.');
          Exit;
        end;
        RealValue := Value;
      end;
      dtIPv6:
      begin
        RealValue := Value;
      end;
      dtPort:
      begin
        str := Value;
        Val(str, E2, E);
        if E <> 0 then
        begin
          raise EConvertError.Create(Value + ' 값은 네트웍 포트 형식과 맞지 않습니다.');
          exit;
        end;
        RealValue := IntTostr(E2);
      end;
    end;
  end;
  FToValue := RealValue;
end;

{ TDateTimeValue }

procedure TDateTimeValue.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TDateTimeValue.Create(ADateSelectType : TDateSelectType);
begin
  FromDate  := FormatDateTime('yyyy-mm-dd 00:00:00', now);
  ToDate    := FormatDateTime('yyyy-mm-dd 23:59:59', now);
  DateRange := 0;
  FSelectLimitDay := 180; //1Day 사용시 과거로 선택 제한 기본 값
  FDateSelectType := ADateSelectType;

  //HideProperties(TDateTimeValue, 'DateRange'); //미표시 프로퍼티
end;

destructor TDateTimeValue.Destroy;
begin
  inherited;
end;

procedure TDateTimeValue.SetFromDateTime(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := VarToDateTime(Value);
  FFrom := FormatDateTime('yyyy-mm-dd hh:mm:ss', DT);
  DateRange := 3;
end;

procedure TDateTimeValue.SetDateSelectType(const Value: TDateSelectType);
begin
  FDateSelectType := Value;
end;

procedure TDateTimeValue.SetToDateTime(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := VarToDateTime(Value);
  FTo := FormatDateTime('yyyy-mm-dd hh:mm:ss', DT);
  DateRange := 3;
end;

{ TPattrenInfoSelectValue }

procedure TPatternInfoValue.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TPatternInfoValue.Create(AKind : TPatternInfoType);
begin
  FPatternInfoKind := AKind;

  FCategoryCode := TReadOnlyStringValue.Create;
  FPatternCodes := TReadOnlyStringValue.Create;
  FPatternNames := TReadOnlyStringValue.Create;
  FConditionView := TReadOnlyStringValue.Create;
  FConditionReal := TReadOnlyStringValue.Create;

  FTestValue := TValidationStringValue.Create;
  FTestValue.Value := 'test';
  FTestValue.MinLength := 1;
  FTestValue.MaxLength := 10;
  //FCategoryCode.Value := 'test1';
  //FPatternCodes.Value := 'test2';
  //FPatternNames.Value := 'test3';
  FConditionView.Value := '';
  FConditionReal.Value := '';

  FPatternList := TDictionary<string, Pointer>.Create;
  HideProperties(TPatternInfoValue, 'CategoryCode;PatternCodes;PatternNames;ConditionView;ConditionReal;PatternInfoKind');
end;

destructor TPatternInfoValue.Destroy;
begin
  FCategoryCode.Free;
  FPatternCodes.Free;
  FPatternNames.Free;
  FConditionView.Free;
  FConditionReal.Free;
  FPatternList.Clear;
  FPatternList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Procedure: TPattrenInfoSelectValue.SetValues
  Author   : kyungmun
  DateTime : 2009.04.10
  Arguments: const Value: string
  Result   : None
  Comment  : 허용 값은 콤마, 숫자만 가능
-------------------------------------------------------------------------------}
(*
procedure TPattrenInfoSelectValue.SetValues(const Value: string);
var
  E, E2, i : integer;
begin
  for I := 1 to Length(Value) - 1 do
  begin
    if (Char(Value[i]) <> #44) then
    begin
      Val(Value[i], E2, E);
      if E <> 0 then
      begin
        raise EConvertError.Create(Value[i] + ' 문자는 사용 할 수 없습니다.');
        Exit;
      end;
    end;
  end;
  FValues := Value;
end;
*)

{ TIPPortValueProperty }

function TIPPortValueProperty.DataType: TNetworkDataType;
begin
  result := GetObject.DataType;
end;

function TIPPortValueProperty.Edit: Boolean;
var
  DataValue : TIPPortValue;
begin
  Result:= False;
  DataValue := GetObject;

  //선택화면을 Core 에서 연결한 편집 이벤트에서 처리
  if Assigned(DataValue.OnEditEvent) then
  begin
    DataValue.OnEditEvent(DataValue);
    Result := True;
  end
end;

function TIPPortValueProperty.GetObject: TIPPortValue;
begin
  Result := TIPPortValue(GetOrdValue);
end;

function TIPPortValueProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;


{-------------------------------------------------------------------------------
  Procedure: TIPPortValueProperty.GetValue
  Author   : kyungmun
  DateTime : 2009.04.09
  Arguments: None
  Result   : String
  Comment  : vkRange, vkMulti 표시 처리.
-------------------------------------------------------------------------------}
function TIPPortValueProperty.GetValue: String;
var
  IPValue : TIPPortValue;
  ValueInStr : string;
begin
  Result := '';
  IPValue := TIPPortValue(GetOrdValue);
  ValueInStr := ' : Default';
  case IPValue.DataType of
    dtIPv4:
    begin
      case IPValue.ValueType of
        vtRange:
        begin
          if (IPValue.FromValue <> '0.0.0.0') or (IPValue.ToValue <> '255.255.255.255') then
            ValueInStr := ' : InData';
          if (IPValue.FromValue <> '') and (IPValue.ToValue <> '') then
            Result := format('%s~%s', [IPValue.FromValue, IPValue.ToValue]);
        end;
        vtMulti:
        begin
          ValueInStr := ' : Empty';
          if IPValue.MultiValue <> '' then
            ValueInStr := ' : InData';
          Result := IPValue.MultiValue;
        end;
      end;
    end;
    dtIPv6:
    begin
      case IPValue.ValueType of
        vtRange:
        begin
          if (IPValue.FromValue <> '0::0') or (IPValue.ToValue <> 'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff') then
            ValueInStr := ' : InData';
          if (IPValue.FromValue <> '') and (IPValue.ToValue <> '') then
            Result := format('%s~%s', [IPValue.FromValue, IPValue.ToValue]);
        end;
        vtMulti:
        begin
          ValueInStr := ' : Empty';
          if IPValue.MultiValue <> '' then
            ValueInStr := ' : InData';
          Result := IPValue.MultiValue;
        end;
      end;
    end;
    dtPort:
    begin
      case IPValue.ValueType of
        vtRange:
        begin
          if (IPValue.FromValue <> '0') or (IPValue.ToValue <> '65535') then
            ValueInStr := ' : InData';
          if (IPValue.FromValue <> '') and (IPValue.ToValue <> '') then
            Result := format('%s~%s', [IPValue.FromValue, IPValue.ToValue]);
        end;
        vtMulti:
        begin
          ValueInStr := ' : Empty';
          if IPValue.MultiValue <> '' then
            ValueInStr := ' : InData';
          Result := IPValue.MultiValue;
        end;
      end;
    end;
  end;
  //Result := '['+ GetEnumName(TypeInfo(TNetworkValueType), Ord(IPValue.ValueType)) + ']' + ValueInStr;
end;

function TIPPortValueProperty.ValueType: TNetworkValueType;
begin
  result := GetObject.ValueType;
end;

{ TDateTimeProperty }

function TSniperDateTimeProperty.Edit: Boolean;
var
  DateValue : TDateTimeValue;
  AForm : TFormDateTimeEditor;
begin
  AForm := nil;
  Result := False;

  DateValue := TDateTimeValue(GetOrdValue);
  case DateValue.DateSelectType of
    dstRange:
    begin
      AForm := TFormDateTimeEditor.CreateForm(nil, False);
    end;
    dstDaily:
    begin
      AForm := TFormDateTimeEditor.CreateForm(nil, False, True);
      AForm.SelectLimitDay := DateValue.SelectLimitDay; //오늘부터 해당일 이전은 선택불가
    end;
  end;

  try
    With AForm do
    begin
      ToDateTime   := DateValue.ToDate;
      FromDatetime := DateValue.FromDate;
      DateRange    := DateValue.DateRange;
      if ShowModal = mrOk then
      begin
        DateValue.FromDate  := FromDatetime;
        DateValue.ToDate    := ToDateTime;
        DateValue.DateRange := DateRange;
        Result := True;
      end;
    end;
  finally
    if AForm <> nil then AForm.Free;
  end;
end;

function TSniperDateTimeProperty.FromDate: string;
begin
  Result := TDateTimeValue(GetOrdValue).FromDate;
end;

function TSniperDateTimeProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := inherited GetAttributes + [paRevertable, paDialog];//, paSubProperties, paReadOnly];
end;

function TSniperDateTimeProperty.GetValue: String;
begin
  Result := FromDate + '~' + ToDate;
end;

function TSniperDateTimeProperty.ToDate: string;
begin
  Result := TDateTimeValue(GetOrdValue).ToDate;
end;

{ TReadOnlyStringValue }

procedure TReadOnlyStringValue.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TReadOnlyStringValue.Create;
begin
  FValue := '';
  HideProperties(TReadOnlyStringValue, 'Value');
end;

destructor TReadOnlyStringValue.Destroy;
begin
  inherited;
end;

{ TStringReadOnlyProperty }

function TStringReadOnlyProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paReadOnly];
end;

function TStringReadOnlyProperty.GetValue: String;
begin
  result := TReadOnlyStringValue(GetOrdValue).Value;
end;

procedure TStringReadOnlyProperty.GetValues;
begin
  //Values.Assign(Screen.Fonts);
end;

procedure TStringReadOnlyProperty.SetValue(const Value: String);
begin
  TReadOnlyStringValue(GetOrdValue).Value := Value;
end;


{ TValidationStringValue }

procedure TValidationStringValue.Assign(Source: TPersistent);
begin
  inherited;

end;

function TValidationStringValue.CharSetToStr(const CharSet: TSysCharSet): string;
var
  C, Prev: Char;
  InRange: Boolean;
begin
  Result := '';
  InRange := False;
  Prev := #0;
  for C in CharSet do
  begin
    if not (Ord(Prev) + 1 = Ord(C)) then
    begin
      if InRange then
      begin
        InRange := False;
        Result := Result + '''' + Prev + ''', ''' + C + '''';
      end else if Result <> '' then
        Result := Result + ', ''' + C + ''''
      else
        Result := Result + '''' + C + '''';
    end else if not InRange then
    begin
      InRange := True;
      Result := Result + '..';
    end;
    Prev := C;
  end;
  if InRange then
    Result := Result + '''' + Prev + '''';
  Result := '[' + Result + ']';
end;

constructor TValidationStringValue.Create;
begin
  FValue := '';
  HideProperties(TValidationStringValue, 'Value;MaxLength;MinLength');
end;

destructor TValidationStringValue.Destroy;
begin

  inherited;
end;

{ TValidationStringProperty }

function TValidationStringProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [];
end;

function TValidationStringProperty.GetValue: String;
begin
  result := TValidationStringValue(GetOrdValue).Value;
end;

procedure TValidationStringProperty.SetValue(const Value: String);
begin
  TValidationStringValue(GetOrdValue).Value := Value;
end;

{ TPatternInfoSelectProperty }

function TPatternInfoValueProperty.Edit: Boolean;
var
  DataValue : TPatternInfoValue;
begin
  Result := False;
  DataValue := TPatternInfoValue(GetOrdValue);

  //선택화면 처리를 Core 에서 연결한 편집 이벤트에서 처리
  if Assigned(DataValue.OnEditEvent) then
  begin
    DataValue.OnEditEvent(DataValue);
    Result := True;
  end
  else
  begin
    //패턴선택 화면 표시하고 선택값을 할당.
    DataValue.CategoryCode.Value := 'CategoryCode';
    DataValue.PatternCodes.Value := 'PatternCodes';
    DataValue.PatternNames.Value := 'PatternNames';
  end;

end;

function TPatternInfoValueProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paDialog, paSubProperties, paReadOnly];
end;

function TPatternInfoValueProperty.GetValue: String;
var
  PatternValue : TPatternInfoValue;
begin
  Result := '';
  PatternValue := TPatternInfoValue(GetOrdValue);

  if (PatternValue.ConditionView.Value <> '') then
  begin
    Result := PatternValue.ConditionView.Value
  end;

  if Trim(Result) = '' then
    Result := '(None)';
end;

{ TOneDayDateTimeProperty }
(*
{-------------------------------------------------------------------------------
  Procedure: TLogQueryDateTimeProperty.Edit
  Author   : kyungmun
  DateTime : 2010.03.10
  Arguments: None
  Result   : Boolean
  Comment  : 로그조회용 날짜선택, 하루안에서 시간만 변경가능하고, 180일 이전날짜 선택 안됨.
-------------------------------------------------------------------------------}
function T1DayDateTimeProperty.Edit: Boolean;
var
  DateValue : T1DayDateTimeValue;
begin
  with TFormDateTimeEditor.CreateForm(nil, False, True) do
  begin
    SelectLimitDay := 180;
    DateValue := T1DayDateTimeValue(GetOrdValue);
    ToDateTime   := DateValue.ToDate;
    FromDatetime := DateValue.FromDate;
    DateRange    := DateValue.DateRange;
    Result := ShowModal = mrOk;
    if Result then
    begin
      DateValue.FromDate  := FromDatetime;
      DateValue.ToDate    := ToDateTime;
      DateValue.DateRange := DateRange;
    end;
    Free;
  end;
end;

function T1DayDateTimeProperty.FromDate: string;
begin
  Result := T1DayDateTimeValue(GetOrdValue).FromDate;
end;

function T1DayDateTimeProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paDialog, paSubProperties, paReadOnly];
end;

function T1DayDateTimeProperty.GetValue: String;
begin
  Result := FromDate + '~' + ToDate;
end;

function T1DayDateTimeProperty.ToDate: string;
begin
  Result := T1DayDateTimeValue(GetOrdValue).ToDate;
end;


{ TOneDayDateTimeValue }

procedure T1DayDateTimeValue.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor T1DayDateTimeValue.Create;
begin
  FFrom := FormatDateTime('yyyy-mm-dd 00:00:00', now);
  FTo   := FormatDateTime('yyyy-mm-dd 23:59:59', now);
  DateRange := 0;
  HideProperties(T1DayDateTimeValue, 'DateRange;FromDate;ToDate');
end;

destructor T1DayDateTimeValue.Destroy;
begin
  inherited;
end;

procedure T1DayDateTimeValue.SetFromDateTime(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := VarToDateTime(Value);
  FFrom := FormatDateTime('yyyy-mm-dd hh:mm:ss', DT);
  DateRange := 3;
end;

procedure T1DayDateTimeValue.SetToDateTime(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := VarToDateTime(Value);
  FTo := FormatDateTime('yyyy-mm-dd hh:mm:ss', DT);
  DateRange := 3;
end;
*)


{ TPatternInfoListProperty }

function TPatternInfoConditionProperty.Edit: Boolean;
var
  DataValue : TConditionCollection;
begin
  Result := False;
  DataValue := TConditionCollection(GetOrdValue);

  //선택화면 처리를 Core 에서 연결한 편집 이벤트에서 처리
  if Assigned(DataValue.OnEditShow) then
  begin
    DataValue.OnEditShow(DataValue);
    Result := True;
  end

end;

function TPatternInfoConditionProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paDialog, paSubProperties, paReadOnly];
end;

function TPatternInfoConditionProperty.GetConditionSqlQuery: String;
var
  DataValue : TConditionCollection;
begin
  Result := '';
  DataValue := TConditionCollection(GetOrdValue);
  Result := DataValue.GetConditionSqlQuery;
end;

function TPatternInfoConditionProperty.GetValue: String;
var
  DataValue : TConditionCollection;
  value : TConditionItem;
begin
  Result := '';
  DataValue := TConditionCollection(GetOrdValue);

  if DataValue.Count > 1 then
  begin
    result := '(Multiple Condition)';
  end
  else
  if (DataValue.Count = 1) then
  begin
    value := DataValue.Items[0];
    result := format('%s : %s', [value.DisplayFieldName, value.DisplayFieldValue]);
  end;

  if Trim(Result) = '' then
    Result := '(None)';
end;

{ TDynamicSelectValue }

procedure TDynamicSelectValue.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TDynamicSelectValue.Create;
begin
  FValueList := TStringList.Create;

  HideProperties(TDynamicSelectValue, 'SelectValue');
end;

destructor TDynamicSelectValue.Destroy;
begin
  FValueList.Clear;
  FValueList.Free;
  inherited;
end;

procedure TDynamicSelectValue.SetSelectValue(const Value: string);
begin
  FSelectValue := Value;
end;

{ TTDynamicSelectValueProperty }

function TDynamicSelectValueProperty.Edit: Boolean;
var
  DataValue : TDynamicSelectValue;
begin
  Result:= False;
  DataValue := GetObject;
  //선택화면을 Core 에서 연결한 편집 이벤트에서 처리
  if (DataValue <> nil) and (Assigned(DataValue.OnEditEvent)) then
  begin
    DataValue.OnEditEvent(DataValue);
    Result := True;
  end;
end;

function TDynamicSelectValueProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TDynamicSelectValueProperty.GetObject: TDynamicSelectValue;
begin
  Result := TDynamicSelectValue(GetOrdValue);
end;

function TDynamicSelectValueProperty.GetRealValue: String;
var
  sValue : string;
  AList : TStringList;
  I, idx: Integer;
begin
  result := '';
  AList := TStringList.Create;
  try
    AList.CommaText := GetValue;
    for I := 0 to AList.Count - 1 do
    begin
      idx := GetObject.ValueList.IndexOf(AList[i]);
      if idx >= 0 then
      begin
        sValue := GetObject.ValueList[idx];
        if GetObject.ValueList.Objects[idx] <> nil then
          sValue := IntToStr(Integer(GetObject.ValueList.Objects[idx]));

        if result <> '' then result := result + ',';
        result := result + sValue;
      end;
    end;
  finally
    AList.Free;
  end;
end;

function TDynamicSelectValueProperty.GetValue: String;
var
  DataValue : TDynamicSelectValue;
begin
  Result := '';
  DataValue := TDynamicSelectValue(GetOrdValue);
  if (DataValue <> nil) and (DataValue.SelectValue <> '') then
  begin
    Result := DataValue.SelectValue;
  end;
end;

initialization
  //사용자정의 타입에 대한 데이터 처리 및 속섲 지정위한 프로퍼티 매핑
  PropertyEditors.Register(TypeInfo(TIPPortValue), nil, '', TIPPortValueProperty);
  PropertyEditors.Register(TypeInfo(TDateTimeValue), nil, '', TSniperDateTimeProperty);
  PropertyEditors.Register(TypeInfo(TPatternInfoValue), nil, '', TPatternInfoValueProperty);
  PropertyEditors.Register(TypeInfo(TReadOnlyStringValue), nil, '', TStringReadOnlyProperty);
  PropertyEditors.Register(TypeInfo(TValidationStringValue), nil, '', TValidationStringProperty);
  PropertyEditors.Register(TypeInfo(TConditionCollection), nil, '', TPatternInfoConditionProperty);
  PropertyEditors.Register(TypeInfo(TDynamicSelectValue), nil, '', TDynamicSelectValueProperty);

end.



