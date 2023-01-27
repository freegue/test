{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  : SNIPER
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : 인스팩터 에서 사용되는 각각 클래스(콤포넌트)를 설계한다.
             - 차단로그 조건 클래스
-------------------------------------------------------------------------------}

unit uPropertyConditionBlock;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls,
  Menus, TypInfo, System.JSON,
  uDefineType, uPropertyDesignIntf, uPropertyDesignEditors, uPropertyResource, uConditionObject,
  uPropertyConditionCore;

type
  //[차단로그] 로그 조회, 필터링시 사용
  TBlockLogCondition = class(TPropertyConditionCore)
  private
    FBlockMethodTypes : TBlockMethodTypes;
    FBlockReasonTypes : TBlockReasonTypes;
    FTrustIPCheck: boolean;
    Fcreatetime: TDateTimeValue;
    FBlolckMethod: TDynamicSelectValue;
    function GetBlockMethods: string;
    function GetBlockReasons: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnConditionFieldValidationForPattern(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue : String; var Valid : boolean); override;
    procedure OnDataValidate(Sender: TSniperPropertyEditor; const AValue : string; var Valid : boolean ); override;

    function GetConditionToJSON: TJSONObject; override;

    //인스펙터에 보이지 않아야 할 프로퍼티
    property IPAddressType;

  published
    //core에 정의된 기본 필드중 사용 하는 값 <순서대로 프로퍼티에 표시>
    property createtime : TDateTimeValue read Fcreatetime write Fcreatetime;
    property pattern_condition;
    property sourceip;
    property destip;
    property destport;
    property ippool_condition;
    property direction;

    //차단로그에만 있는 추가 필드
    //property block_method : TBlockMethodTypes read FBlockMethodTypes write FBlockMethodTypes; //필드명 method
    property block_method: TDynamicSelectValue read FBlolckMethod write FBlolckMethod; //필드명 method '차단방법'은 라이센스에 따라서 선택 대상이 달라지기에 Enum 타입이 아닌 TDynamicSelectValue 클래스로 사용
    property block_reason : TBlockReasonTypes read FBlockReasonTypes write FBlockReasonTypes;  //필드명 blk_flag
    property trustip_check : boolean read FTrustIPCheck write FTrustIPCheck;
  end;

implementation


{ TBlockLogCondition }

constructor TBlockLogCondition.Create(AOwner: TComponent);
begin
  inherited;

  createtime := TDateTimeValue.Create(dstDaily); //일단위로 선택
  createtime.SelectLimitDay := 30;

  //차단방법은 라이센스에 따라서 사용될 값이 동적으로 처리해야 함.
  FBlolckMethod := TDynamicSelectValue.Create;
  FBlolckMethod.OnEditEvent := OnDynamicSelectValueEditEvent;

  FBlolckMethod.ValueList.Clear;
  FBlolckMethod.ValueList.AddObject('SN_SRC_IP', TObject(1));
  FBlolckMethod.ValueList.AddObject('SN_DST_IP', TObject(2));
  FBlolckMethod.ValueList.AddObject('SN_AND_IP', TObject(8));
  FBlolckMethod.ValueList.AddObject('SN_SRC_PORT', TObject(0));
  FBlolckMethod.ValueList.AddObject('SN_DST_PORT', TObject(0));

  //기본 값, 여러개 선택 가능여부
  FBlolckMethod.SelectValue := '';
  FBlolckMethod.MultiSelected := true;
  FBlolckMethod.FormTitle := '차단방법'; //선택화면 타이틀
end;

destructor TBlockLogCondition.Destroy;
begin

  inherited;
end;

function TBlockLogCondition.GetBlockMethods: string;
var
  S: TIntegerSet;
  i : integer;
begin
  Integer(S) := GetOrdProp(Self, 'block_method');
  Result := '';
  for i := 0 to 31 do
  begin
    if i in S then
    begin
      if Length(Result) > 0 then
        Result := Result + ',';

      case TBlockMethodType(i) of
        SN_SRC_IP : Result := Result + '1';
        SN_DST_IP : Result := Result + '2';
        SN_AND_IP : Result := Result + '8';
        SN_SRC_SRVC : ;
        SN_DST_SRVC : ;
        SN_DNS_TYPE : ;
        SN_DHCP_MAC : ;
        SN_SIP_CALLER : ;
        SN_SIP_CALLEE : ;
        SN_SIP_CALLER_CALLEE : ;
        SN_SIP_CALLER_TYPE : ;
        SN_SIP_CALLEE_TYPE : ;
        SN_SIP_CALLER_CALLEE_TYPE : ;
      end;
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

function TBlockLogCondition.GetBlockReasons: string;
var
  S: TIntegerSet;
  i : integer;
begin
  Integer(S) := GetOrdProp(Self, 'block_reason');
  Result := '';
  for i := 0 to 31 do
  begin
    if i in S then
    begin
      if Length(Result) > 0 then
        Result := Result + ',';

      case TBlockReasonType(i) of
        brtSessionForceClose: result := result + '16';
        brtDetectBlock      : result := result + '32';
        brtSBL              : result := result + '48';
        brtPacketBaseBlock  : result := result + '64';
      end;
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';

end;

function TBlockLogCondition.GetConditionToJSON: TJSONObject;
var
  i, j : integer;
  bCondition : boolean;
  PropertyList : TSniperPropertyList;
  AStringList : TStringList;
  sTypeName, sFieldName, sValue : string;
  jSon, jCondition, jField, jSub : TJSONObject;
  jArr : TJSONArray;
begin
  jSon := inherited GetConditionToJSON;

 //조건객체 확인후 조건객체안에 필드 정보 넣어야 함
  bCondition := false;
  if jSon.Values['condition'] <> nil then
  begin
    jCondition := TJSONObject(jSon.Values['condition']);
    bCondition := True;
  end;

  if jCondition = nil then
    jCondition := TJSONObject.Create;

  PropertyList := TSniperPropertyList.Create;
  try
    PropertyList.Component := self;
    try
      for I := 0 to PropertyList.Count - 1 do
      begin
        //필드타입
        sTypeName := UpperCase(PropertyList[i].Editor.PropTypeName);
        //필드명
        sFieldName:= LowerCase(PropertyList[i].Editor.GetName);
        //값
        sValue := PropertyList[i].Editor.GetValue;

        //createtime 필드는 공통필드로 선 처리되었기에 여기서는 제외
        if sFieldName = 'createtime' then continue;

        if (sTypeName = UpperCase('TDirections')) then  //통신방향
        begin
          sValue := GetDirections;
          jCondition.AddPair(sFieldName, sValue);
          continue;
        end;

        //차단방법
        if (sTypeName = UpperCase('TDynamicSelectValue')) and (sFieldName = 'block_method') then
        begin
          if PropertyList[i].Editor is TDynamicSelectValueProperty then
          begin
            sFieldName := 'method';
            sValue := TDynamicSelectValueProperty(PropertyList[i].Editor).GetRealValue; //표기되는 값기준으로 실제 조회에 사용되는 값 얻기
            if sValue <> '' then
            begin
              sValue := format('(%s)', [sValue]);
              jCondition.AddPair(sFieldName, sValue);
            end;
            continue;
          end;
        end;

        if (sTypeName = UpperCase('TBlockMethodTypes')) then
        begin
          sFieldName := 'method';
          sValue := GetBlockMethods;
          jCondition.AddPair(sFieldName, sValue);
          continue;
        end;

        if (sTypeName = UpperCase('TBlockReasonTypes')) then  //차단사유
        begin
          sFieldName := 'blk_flag';
          sValue := GetBlockReasons;
          jCondition.AddPair(sFieldName, sValue);
          continue;
        end;

        if (sTypeName = UpperCase('TRiskLevel3Types')) then  //위험도
        begin
          sValue := GetRisk3Levels;
          jCondition.AddPair(sFieldName, sValue);
          continue;
        end;

        //ip, port
        if PropertyList[i].Editor is TIPPortValueProperty then
        begin
          if TIPPortValueProperty(PropertyList[i].Editor).ValueType = vtMulti then
          begin
            sValue := TIPPortValueProperty(PropertyList[i].Editor).GetValue;
            if sValue <> '' then
            begin
              AStringList := TStringList.Create;
              AStringList.CommaText := sValue;
              jArr := TJSONArray.Create;
              for j := 0 to AStringList.Count - 1 do
              begin
                jArr.Add(AStringList[j]);
              end;
              AStringList.Free;
              jCondition.AddPair(sFieldName, jArr);
            end;
          end
          else
          begin
            if sValue <> '' then
              jCondition.AddPair(sFieldName, sValue);
          end;
          continue;
        end;

        //패턴조건정보 (쿼리문으로 받는다)
        if PropertyList[i].Editor is TPatternInfoConditionProperty then
        begin
          sValue := TPatternInfoConditionProperty(PropertyList[i].Editor).GetConditionSqlQuery;
        end;

        //서브 처리아닌 값은 입력값 그대로 사용
        jCondition.AddPair(sFieldName, sValue);
      end;

      //조건 객체가 없었으면 추가
      if not bCondition then
        jSon.AddPair('condition', jCondition);
    except
    end;
  finally
    PropertyList.Free;
  end;

  result := jSon;
end;

procedure TBlockLogCondition.OnConditionFieldValidationForPattern(Sender: TObject; APatternType: TPatternInfoType;
  var AChar: Char; const AValue: String; var Valid: boolean);
begin
  inherited;

end;

procedure TBlockLogCondition.OnDataValidate(Sender: TSniperPropertyEditor; const AValue: string; var Valid: boolean);
var
  ErrMsg : string;
  ValidationStringValue : TValidationStringValue;
begin
  //공통 필드 Core에서 체크
  inherited OnDataValidate(Sender, AValue, Valid);

  //차단로그에만 있는 필드중 직접 입력하는 특정 필드 유효성 체크
  case TSniperPropertyEditor(Sender).PropTypeKind of
    tkString,
    tkUString:
    begin
    end;
    tkClass:
    begin
    end;
  end;
end;

initialization


end.
