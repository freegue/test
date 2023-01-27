{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  : SNIPER
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : 인스팩터 에서 사용되는 각각 클래스(콤포넌트)를 설계한다.
             - 탐지로그 조건 클래스
-------------------------------------------------------------------------------}

unit uPropertyConditionDetect;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls,
  Menus, TypInfo, System.JSON,
  uDefineType, uPropertyDesignIntf, uPropertyDesignEditors, uPropertyResource, uConditionObject,
  uPropertyConditionCore;

type
  //[탐지로그] 로그 조회, 필터링시 사용
  TDetectLogCondition = class(TPropertyConditionCore)
  private
    FBlockTypes: TBlockTypes;
    FEtcTypes: TDetectEtcTypes;
    FDesc: TValidationStringValue;

    function GetBlockTypes : string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnConditionFieldValidationForPattern(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue : String; var Valid : boolean); override;
    procedure OnDataValidate(Sender: TSniperPropertyEditor; const AValue : string; var Valid : boolean ); override;

    function GetConditionToJSON: TJSONObject; override;

    //인스펙터에 보이지 않아야 할 프로퍼티
    property IPAddressType;
  published
    //core 기본 필드 사용 <순서대로 프로퍼티에 표시>
    property createtime;
    property direction;
    property pattern_condition;
    property risk;
    property sourceip;
    property destip;
    property sourceport;
    property destport;
    property ippool_condition;
    property application_condition;

    //탐지로그에만 있는 추가 필드
    property block_type : TBlockTypes read FBlockTypes write FBlockTypes;
    property etc : TDetectEtcTypes read FEtcTypes write FEtcTypes;
    //property desc : TValidationStringValue read FDesc write FDesc;
  end;

implementation


{ TDetectLogCondition }

constructor TDetectLogCondition.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //desc := TValidationStringValue.Create;
  //desc.MinLength := 3;
  //desc.MaxLength := 10;
end;

destructor TDetectLogCondition.Destroy;
begin

  inherited;
end;

procedure TDetectLogCondition.OnDataValidate(Sender : TSniperPropertyEditor; const AValue : string; var Valid : boolean);
var
  ErrMsg : string;
  ValidationStringValue : TValidationStringValue;
begin
  //공통 필드 Core에서 체크
  inherited OnDataValidate(Sender, AValue, Valid);

  //탐지로그에만 있는 필드중 직접 입력하는 특정 필드 체크
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

//패턴조건정보내에서 각 필드(공격명, 공격코드, 카테고리 ,,) 에 대한 입력 유효성 체크
//Sender 에는 TConditionItem 객체가 담겨 온다.
procedure TDetectLogCondition.OnConditionFieldValidationForPattern(Sender: TObject; APatternType : TPatternInfoType; var AChar : Char; const AValue: String; var Valid: boolean);
begin
  inherited;

end;

function TDetectLogCondition.GetBlockTypes: string;
var
  S: TIntegerSet;
  i : integer;
begin
  Integer(S) := GetOrdProp(Self, 'block_type');
  Result := '';
  for i := 0 to 31 do
  begin
    if i in S then
    begin
      if Length(Result) > 0 then
        Result := Result + ',';

      case TBlockType(i) of
        btBlock : Result := Result + '2,3';
        btRedirectBlock : Result := Result + '10,11';
        btPacketBaseBlock : Result := Result + '16,17';
      end;
    end;
  end;
  if Result <> '' then
    Result := '(' + Result + ')';
end;

function TDetectLogCondition.GetConditionToJSON : TJSONObject;
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

        //차단종류 Enum 값
        if (sTypeName = UpperCase('TBlockTypes')) then
        begin
          sFieldName := 'action';
          sValue := GetBlockTypes;
          jCondition.AddPair(sFieldName, sValue);
          continue;
        end;

        if (sTypeName = UpperCase('TDetectEtcType')) then //탐지기타
        begin

        end;

        if (sTypeName = UpperCase('TDirections')) then  //통신방향
        begin
          sValue := GetDirections;
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

initialization


end.
