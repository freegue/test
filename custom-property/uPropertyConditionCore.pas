{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  : SNIPER
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : 인스팩터 에서 사용되는 각각 클래스(콤포넌트)를 설계한다.
             - 로그 조건 기본 공통 클래스
-------------------------------------------------------------------------------}

unit uPropertyConditionCore;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls,
  Menus, TypInfo, XMLIntf, System.JSON, advCGrid,
  uDefineType, uPropertyDesignIntf, uPropertyDesignEditors, uPropertyResource, uConditionObject,
  fmIPPortValueEditor, fmPatternInfoSelect, fmDynamicSelectValueEditor;

type

  //////////////////////////////////////////////////////////////////////////////
  //유형별 로그 조회 조건에서 많이 사용되는 일반적인 항목들 정의
  //그 외 특별한 조회시에만 사용하는 항목은 해당 클래스에서 정의해서 사용
  //////////////////////////////////////////////////////////////////////////////
  TPropertyConditionCore = class(TComponent)
  private
    FIPAddressType: TIPAddressType;
    FDateTimeValue: TDateTimeValue;
    FRisks : TRiskLevel3Types;
    FDirection : TDirections;
    FIPSource : TIPPortValue;
    FIPDest : TIPPortValue;
    FPortDest: TIPPortValue;
    FPortSource: TIPPortValue;
    FPatternConditionItems : TConditionCollection;
    FApplicationConditionItems: TConditionCollection;
    FIPPoolConditionItems: TConditionCollection;
    FReputationConditionItems: TConditionCollection;
    procedure SetIPAddressType(const Value: TIPAddressType);
    procedure TestCondition2;
    function ConditionFieldMinMaxCheck(condi : TConditionItem; AValue : string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure initialize;

    procedure OnConditionFieldValidationForPattern(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue : String; var Valid : boolean); virtual;
    procedure OnDataValidate(Sender: TSniperPropertyEditor; const AValue : String; var Valid : boolean); virtual;

    function ConditionToJSON : TJSONObject;

    //인스펙터에 보이지 않아야할 프로퍼티
    property IPAddressType : TIPAddressType read FIPAddressType write SetIPAddressType;

  published
    //인스팩터에 보여야할 프로퍼티 정의
    //모든 하위 클래스에서 공통으로 사용할 프로퍼티
    property createtime  : TDateTimeValue read FDateTimeValue write FDateTimeValue;

  protected
    //하위 클래스에서 선택적으로 사용할 프로퍼티 or 메소드
    procedure OnIPPortValueEditEvent(Sender : TObject);
    procedure OnEditShowPattern(Sender : TObject);
    procedure OnDynamicSelectValueEditEvent(Sender : TObject);

    function GetConditionToJSON : TJSONObject; virtual;
    function GetRisk3Levels: string;
    function GetDirections: string;
    function GetSourceIP: string;
    function GetSourcePort: string;
    function GetDestIP: string;
    function GetDestPort: string;

    //하위 클래스에서 선택적으로 사용할 프로퍼티
    property pattern_condition : TConditionCollection read FPatternConditionItems write FPatternConditionItems;
    property application_condition : TConditionCollection read FApplicationConditionItems write FApplicationConditionItems;
    property ippool_condition : TConditionCollection read FIPPoolConditionItems write FIPPoolConditionItems;
    property reputation_condition : TConditionCollection read FReputationConditionItems write FReputationConditionItems;

    property risk        : TRiskLevel3Types read FRisks write FRisks default [];
    property sourceip    : TIPPortValue read FIPSource write FIPSource;
    property sourceport  : TIPPortValue read FPortSource write FPortSource;
    property destip      : TIPPortValue read FIPDest write FIPDest;
    property destport    : TIPPortValue read FPortDest write FPortDest;
    property direction   : TDirections read FDirection write FDirection default [];
  end;

implementation


{ TPropertyConditionCore }

function TPropertyConditionCore.ConditionToJSON: TJSONObject;
begin
  result := GetConditionToJSON;
end;

constructor TPropertyConditionCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Initialize;

  HideProperties(TPropertyConditionCore, 'Name;Tag');
end;

destructor TPropertyConditionCore.Destroy;
begin
  FIPSource.Free;
  FIPDest.Free;
  FPortSource.Free;
  FPortDest.Free;
  FDateTimeValue.Free;

  FPatternConditionItems.Free;
  FApplicationConditionItems.Free;
  FIPPoolConditionItems.Free;
  FReputationConditionItems.Free;

  inherited;
end;

procedure TPropertyConditionCore.initialize;
begin
  FIPSource.Free;
  FIPDest.Free;
  FPortSource.Free;
  FPortDest.Free;
  FDateTimeValue.Free;

  FIPSource := TIPPortValue.Create(dtIPv4);
  FIPDest := TIPPortValue.Create(dtIPv6);
  FPortSource := TIPPortValue.Create(dtPort);
  FPortDest := TIPPortValue.Create(dtPort);
  FDateTimeValue := TDateTimeValue.Create(dstRange);

  FIPSource.OnEditEvent := OnIPPortValueEditEvent;
  FIPDest.OnEditEvent := OnIPPortValueEditEvent;
  FPortSource.OnEditEvent := OnIPPortValueEditEvent;
  FPortDest.OnEditEvent := OnIPPortValueEditEvent;

  //탐지정책
  FPatternConditionItems := TConditionCollection.Create(self, pitDetectBlock);
  FPatternConditionItems.OnEditShow := OnEditShowPattern;  //패턴정보 선택 화면 열기
  FPatternConditionItems.OnConditionFieldValueValidation := OnConditionFieldValidationForPattern; //패턴정보조건 값 입력시 유효성 체크

  //Application 정책
  FApplicationConditionItems := TConditionCollection.Create(self, pitApplication);
  FApplicationConditionItems.OnEditShow := OnEditShowPattern;
  FApplicationConditionItems.OnConditionFieldValueValidation := OnConditionFieldValidationForPattern;

  //IPPool 정책
  FIPPoolConditionItems := TConditionCollection.Create(self, pitIPPool);
  FIPPoolConditionItems.OnEditShow := OnEditShowPattern;
  FIPPoolConditionItems.OnConditionFieldValueValidation := OnConditionFieldValidationForPattern;

  //Reputation 정책
  FReputationConditionItems := TConditionCollection.Create(self, pitReputation);
  FReputationConditionItems.OnEditShow := OnEditShowPattern;
  FReputationConditionItems.OnConditionFieldValueValidation := OnConditionFieldValidationForPattern;

  TestCondition2;
end;

procedure TPropertyConditionCore.TestCondition2;
var
  Condi, prevCondi : TConditionItem;
begin
  Condi := FPatternConditionItems.Add(nil);
  Condi.FieldName := 'category';
  Condi.FieldValue := '1';
  Condi.DataTypeKind := dtkNumber;
  Condi.OperatorOption := opEQ;
  Condi.DisplayFieldName := '공격유형';
  Condi.DisplayFieldValue := '서비스거부';

  prevCondi := pattern_condition.Items[pattern_condition.Count - 1];
  Condi := FPatternConditionItems.Add(prevCondi);
  Condi.FieldName := 'attack_name';
  Condi.FieldValue := 'TEST NAME';
  Condi.DataTypeKind := dtkString;
  Condi.OperatorOption := opContains;
  Condi.RelationalOption := ropOR;
  Condi.DisplayFieldName := '공격명';
  Condi.DisplayFieldValue := Condi.FieldValue;
  Condi.MaxLength := 32;

  prevCondi := pattern_condition.Items[pattern_condition.Count - 1];
  Condi := FPatternConditionItems.Add(prevCondi);
  Condi.FieldName := 'code';
  Condi.FieldValue := '101010';
  Condi.DataTypeKind := dtkNumber;
  Condi.UseableOperatorOptions := [opEQ, opGT, opLT, opGTE, opLTE, opIN, opRange];
  Condi.OperatorOption := opEQ;
  Condi.RelationalOption := ropOR;
  Condi.DisplayFieldName := '공격코드';
  Condi.DisplayFieldValue := Condi.FieldValue;
  Condi.MinLength := 1;
  Condi.MaxLength := 64;
  Condi.MinValue := 1;
  Condi.MaxValue := 999999;
end;


//IP/PORT 입력 화면 표기하고 값 입력
procedure TPropertyConditionCore.OnIPPortValueEditEvent(Sender: TObject);
var
  AForm : TFormIPPortValueEditor;
begin
  AForm := nil;
  case TIPPortValue(Sender).DataType of
    dtIPv4: AForm := TFormIPPortValueEditor.CreateForm(nil, dtIPv4);
    dtIPv6: AForm := TFormIPPortValueEditor.CreateForm(nil, dtIPv6);
    dtPort: AForm := TFormIPPortValueEditor.CreateForm(nil, dtPort);
  end;

 if AForm = nil then Exit;
  try
    with AForm do
    begin
      FromValue := TIPPortValue(Sender).FromValue;
      ToValue   := TIPPortValue(Sender).ToValue;
      MultiValue := TIPPortValue(Sender).MultiValue;
      ValueType  := TIPPortValue(Sender).ValueType;
      if ShowModal = mrOk then
      begin
        TIPPortValue(Sender).FromValue := FromValue;
        TIPPortValue(Sender).ToValue := ToValue;
        TIPPortValue(Sender).MultiValue := MultiValue;
        TIPPortValue(Sender).ValueType := ValueType;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TPropertyConditionCore.OnEditShowPattern(Sender: TObject);
begin
  case TConditionCollection(Sender).PatternType of
    pitDetectBlock:
    begin
      if fmPatternSelect = nil then
        fmPatternSelect := TfmPatternSelect.Create(nil);

      fmPatternSelect.ConditionObjectMng.ConditionItems := FPatternConditionItems;
      fmPatternSelect.Caption := '탐지정책 패턴 선택';

      if fmPatternSelect.ShowModal = mrOK then
      begin
        FPatternConditionItems.Assign(fmPatternSelect.ConditionObjectMng.ConditionItems);
      end;
    end;
    pitIPPool:
    begin
      if fmPatternSelect = nil then
        fmPatternSelect := TfmPatternSelect.Create(nil);

      fmPatternSelect.ConditionObjectMng.ConditionItems := FIPPoolConditionItems;
      fmPatternSelect.Caption := 'IPPool 패턴 선택';

      if fmPatternSelect.ShowModal = mrOK then
      begin
        FIPPoolConditionItems.Assign(fmPatternSelect.ConditionObjectMng.ConditionItems);
      end;
    end;
    pitApplication:
    begin
      if fmPatternSelect = nil then
        fmPatternSelect := TfmPatternSelect.Create(nil);

      fmPatternSelect.ConditionObjectMng.ConditionItems := FApplicationConditionItems;
      fmPatternSelect.Caption := 'Application 패턴 선택';

      if fmPatternSelect.ShowModal = mrOK then
      begin
        FApplicationConditionItems.Assign(fmPatternSelect.ConditionObjectMng.ConditionItems);
      end;
    end;
    pitReputation:
    begin
      if fmPatternSelect = nil then
        fmPatternSelect := TfmPatternSelect.Create(nil);

      fmPatternSelect.ConditionObjectMng.ConditionItems := FReputationConditionItems;
      fmPatternSelect.Caption := 'Reputation 패턴 선택';

      if fmPatternSelect.ShowModal = mrOK then
      begin
        FReputationConditionItems.Assign(fmPatternSelect.ConditionObjectMng.ConditionItems);
      end;
    end;
    pitACL:;
    pitRatelimitDynamic:;
    pitRateLimitStatic:;
    pitRateLimitVoIP:;
  end;
end;

//프로퍼티 특정 필드(패턴조건)의 내부 필드 값에 대한 유효성 체크
//각 필드에서 사용/불허 문자 정의를 조건객체에 할당하는 것보다 여기 프로퍼티 객체에서 처리하는게 나음
//단, 허용/불가 문자셋을 공통 정의하여 사용한다면 내부조건객체 자체에서 유효성 처리해도 됨.
procedure TPropertyConditionCore.OnConditionFieldValidationForPattern(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char;
  const AValue: String; var Valid: boolean);
var
  condi : TConditionItem;
  i : integer;
  c : Char;
begin
  Valid := True;
  if Sender = nil then exit;
  condi := TConditionItem(Sender);

  //키 입력이 있는경우 입력된 Char 값만 확인
  if AChar <> #0 then
  begin
    //1. 숫자타입시 허용 문자 체크
    if condi.DataTypeKind = dtkNumber then
    begin
      if condi.OperatorOption in [opEQ, opGT, opLT, opGTE, opLTE] then
        Valid := CharInSet(AChar, ['0'..'9', #8, #16])  //숫자, 삭제, 백스페이스
      else if condi.OperatorOption in [opIN, opRange] then
        Valid := CharInSet(AChar, ['0'..'9', ',', #8, #16])  //숫자, 콤마, 삭제, 백스페이스
    end;
  end
  else
  begin
    //1. 값 전체에서 숫자타입시 허용 문자 체크
    if condi.DataTypeKind = dtkNumber then
    begin
      for i := 1 to length(AValue) - 1 do
      begin
        if condi.OperatorOption in [opEQ, opGT, opLT, opGTE, opLTE] then
          Valid := CharInSet(AValue.Chars[i], ['0'..'9', #8, #16])  //숫자, 삭제, 백스페이스
        else if condi.OperatorOption in [opIN, opRange] then
          Valid := CharInSet(AValue.Chars[i], ['0'..'9', ',', #8, #16]);  //숫자, 콤마, 삭제, 백스페이스

        if not Valid then
          raise Exception.Create(format('invalid char include : %s', [AValue.Chars[i]]));
      end;
    end;

    //2. 값전체 길이 체크
    if (length(AValue) < condi.MinLength) or (length(AValue) > condi.MaxLength) then
    begin
      Valid := False;
      raise Exception.Create(format('invalid length, ( 입력범위 : %d ~ %d)', [condi.MinLength, condi.MaxLength]));
    end;

    //3. 값 범위 체크
    if not ConditionFieldMinMaxCheck(condi, AValue) then
    begin
      Valid := False;
      raise Exception.Create(format('invalid value, (입력범위 : %u ~ %u)', [condi.MinValue, condi.MaxValue]));
    end;
  end;
end;

function TPropertyConditionCore.ConditionFieldMinMaxCheck(condi : TConditionItem; AValue : string)  : boolean;
var
  sStringList : TStringList;
  sValue : string;
  realValue : uint64;
  I, code: Integer;
begin
  result := true;
  if Trim(AValue) = '' then exit;

  //숫자 타입인 경우만 값 범위 체크 (최초~최대 값)
  sValue := AValue;
  case condi.DataTypeKind of
    dtkNumber: //숫자 타입은 연산자에 따라 콤마 가능 (range, in 인경우 값이 2개 이상임 )
    begin
      if condi.OperatorOption in [opIN, opRange] then
      begin
        sStringList := TStringList.Create;
        try
          //콤마로 구분해서 각 값이 최소~최대 범위내인지 체크
          sStringList.CommaText := sValue;
          for I := 0 to sStringList.Count - 1 do
          begin
            sValue := sStringList[i];
            if Trim(sValue) = '' then continue;

            Val(sValue, realValue, code);
            if (code <> 0) or (realValue < condi.MinValue) or (realValue > condi.MaxValue) then
            begin
              result := false;
              exit;
            end;
          end;
        finally
          sStringList.Free;
        end;
      end
      else //그 외 연산자는 값 1개
      begin
        if Trim(sValue) = '' then exit;
        Val(sValue, realValue, code);
        if (code <> 0) or (realValue < condi.MinValue) or (realValue > condi.MaxValue) then
        begin
          result := false;
          exit;
        end;
      end;
    end;
  end;
end;

//프로퍼티의 개별 필드에 대한 유효성 체크
procedure TPropertyConditionCore.OnDataValidate(Sender: TSniperPropertyEditor; const AValue : String; var Valid: boolean);
var
  i : integer;
  ErrMsg : string;
  ValidationStringValue : TValidationStringValue;
begin
  Valid := True;
  try
    //기본 필드 체크, 그 외 필드는 개별 객체에서 처리
    if not Valid then
    begin
      ErrMsg := format('오류 발생 : %s' , ['상세정보']);
      raise Exception.Create(ErrMsg);
    end;

    case TSniperPropertyEditor(Sender).PropTypeKind of
      tkString,
      tkUString:
      begin

      end;
      tkClass:
      begin
        //class 타입중 직접 입력하여 유효성 체크를 해야하는 것들 처리
        //유효성 문자 직접 입력 클래스 :
        // 1. 최소~최대 자릿수 체크
        if (TSniperPropertyEditor(Sender).ClassType = TValidationStringProperty) then
        begin
          ValidationStringValue := nil;
          if (TSniperPropertyEditor(Sender).SubComponent.ClassType = TValidationStringValue) then
            ValidationStringValue := TValidationStringValue(TSniperPropertyEditor(Sender).SubComponent);

          if ValidationStringValue <> nil then
          begin
            if (ValidationStringValue.MinLength > length(AValue)) or (ValidationStringValue.MaxLength < length(AValue)) then
            begin
              Valid := False;
              ErrMsg := format('입력 유효범위 오류' + #13#13 + '[%s] 입력 유효범위 : %d ~ %d 자' , [
                        GetPropertyNameTranslation(TValidationStringProperty(Sender).GetName),
                        ValidationStringValue.MinLength,
                        ValidationStringValue.MaxLength]);
              raise Exception.Create(ErrMsg);
            end;
          end;

          //2. 입력 불가문자 설정시 체크 <입력 불가 문자가 있다면 Error>
          if ValidationStringValue.DenyCharSet <> [] then
          begin
            for i := 1 to Length( AValue ) do
            begin
              if CharInSet( AValue[i], ValidationStringValue.DenyCharSet ) then
              begin
                ErrMsg := format('입력 유효범위 오류' + #13#13 + '[%s] 사용불가 문자 포함' + #13 + '%s', [
                          GetPropertyNameTranslation(TValidationStringProperty(Sender).GetName),
                          ValidationStringValue.CharSetToStr(ValidationStringValue.DenyCharSet)]);
                raise Exception.Create(ErrMsg);
                exit;
              end;
            end;
          end;

          //2. 입력 허용문자 설정시 체크 <입력 허용 문자가 아니면 Error>
          if ValidationStringValue.AllowCharSet <> [] then
          begin
            for i := 1 to Length( AValue ) do
            begin
              if not CharInSet( AValue[i], ValidationStringValue.AllowCharSet ) then
              begin
                ErrMsg := format('입력 유효범위 오류' + #13#13 + '[%s] 허용가능 문자 이 외 문자 포함' + #13 + '%s', [
                          GetPropertyNameTranslation(TValidationStringProperty(Sender).GetName),
                          ValidationStringValue.CharSetToStr(ValidationStringValue.AllowCharSet)]);
                raise Exception.Create(ErrMsg);
                exit;
              end;
            end;
          end;
        end;
      end;
    end;

  except on E : Exception do
    begin
      Valid := False;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TPropertyConditionCore.OnDynamicSelectValueEditEvent(Sender: TObject);
var
  AForm : TFormDynamicSelectValueEditor;
begin
  AForm := TFormDynamicSelectValueEditor.CreateForm(nil, TDynamicSelectValue(Sender).ValueList, TDynamicSelectValue(Sender).MultiSelected);
  try
    AForm.Caption := TDynamicSelectValue(Sender).FormTitle;
    AForm.SelectValue := TDynamicSelectValue(Sender).SelectValue;
    if AForm.ShowModal = mrOk then
    begin
      TDynamicSelectValue(Sender).SelectValue := AForm.SelectValue;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TPropertyConditionCore.SetIPAddressType(const Value: TIPAddressType);
begin
  FIPAddressType := Value;
  case FIPAddressType of
    itIPv4:
    begin
      FIPSource.DataType := dtIPv4;
      FIPDest.DataType := dtIPv4;
    end;
    itIPv6:
    begin
      FIPSource.DataType := dtIPv6;
      FIPDest.DataType := dtIPv6;
    end;
  end;
end;

// 낮음, 보통, 높음이 2,3,4 값으로 사용되기에 +2 한 값으로 처리
function TPropertyConditionCore.GetRisk3Levels: string;
var
  S: TIntegerSet;
  i : integer;
begin
  Integer(S) := GetOrdProp(Self, 'risk');
  Result := '';
  for i := 0 to 31 do
  begin
    if i in S then
    begin
      if Length(Result) > 0 then
        Result := Result + ',';
      Result := Result + IntToStr(i+2);
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

function TPropertyConditionCore.GetSourceIP: string;
begin

end;

function TPropertyConditionCore.GetSourcePort: string;
begin

end;

function TPropertyConditionCore.GetDestIP: string;
begin

end;

function TPropertyConditionCore.GetDestPort: string;
begin

end;

// 탐지로그 : 인바운트, 아웃바운드, 노바운드 0, 1, f  값으로 사용됨
// 차단로그 : 다름..
function TPropertyConditionCore.GetDirections: string;
var
  S: TIntegerSet;
  i : integer;
begin
  Integer(S) := GetOrdProp(Self, 'direction');
  Result := '';
  for i := 0 to 31 do
  begin
    if i in S then
    begin
      if Length(Result) > 0 then
        Result := Result + ',';
      //Result := Result + IntToStr(i);
      Result := Result + '''' + GetEnumName(TypeInfo(TDirection), i) + '''';
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

//포맷 정보와, 공통 시간 필드만 생성 함.
//주의) 그 외 필드는 개별 하위 클래스에서 처리 하기 바람 여기서 하지 않음
function TPropertyConditionCore.GetConditionToJSON : TJSONObject;
var
  i : integer;
  PropertyList : TSniperPropertyList;
  sTypeName, sFieldName, sValue : string;
  jSon, jControl, jCondition, jField : TJSONObject;
begin
  jSon := TJSONObject.Create;
  jSon.AddPair('version', TJSONString.Create(SNIPER_PROPERTY_JSON_FORMAT_VERSION));

  //----control-------
  jControl := TJSONObject.Create;
  jControl.AddPair('name', TJSONString.Create(Self.ClassName));
  if IPAddressType = itIPv4 then
    jControl.AddPair('ipv4', TJSONTrue.Create)
  else
    jControl.AddPair('ipv4', TJSONFalse.Create);

  jSon.AddPair('control', jControl);
  //------------------

  //공통 필드만 처리
  PropertyList := TSniperPropertyList.Create;
  try
    PropertyList.Component := self;
    try
      jCondition := TJSONObject.Create;
      for I := 0 to PropertyList.Count - 1 do
      begin
        sTypeName := UpperCase(PropertyList[i].Editor.PropTypeName);
        sFieldName     := PropertyList[i].Editor.GetName;
        sValue    := PropertyList[i].Editor.GetValue;

        //createtime -> common
        if (PropertyList[i].Editor is TSniperDateTimeProperty) and (LowerCase(sFieldName) = 'createtime') then
        begin
          jField := TJSONObject.Create;
          jField.AddPair('start_time', TJSONString.Create(TSniperDateTimeProperty(PropertyList[i].Editor).FromDate));
          jField.AddPair('end_time', TJSONString.Create(TSniperDateTimeProperty(PropertyList[i].Editor).ToDate));
          jCondition.AddPair('search_time', jField);
          jSon.AddPair('condition', jCondition);
        end;
      end;
    except
    end;
  finally
    PropertyList.Free;
  end;
  result := jSon;
end;

initialization


end.
