{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-12
  Comment  : 현재 통신중인 윈도우 핸들 목록 관리
             프로젝트에서 사용하는 공통 변수
-------------------------------------------------------------------------------}


unit uCommunityUnit;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, Contnrs, SyncObjs, DefineTypeUnit;

type
  PHandleInfo = ^THandleInfo;
  THandleInfo = packed record
    ID: string;
    Name: string;
    Handle : THandle;
  end;

  //윈도우 핸들 목록
  TWindowHandleList = class(TObjectList)
  private
    FCS: TCriticalSection;

    function Get(Index: Integer): PHandleInfo;
    procedure Delete(Index: Integer);
  public
    procedure Clear; override;

    procedure Add(AId, AName : string; AHandle : THandle);
    procedure DeleteToID(AId : string);
    procedure DeleteToName(AName : string);
    procedure DeleteToHandle(AHandle: THandle);
    function FindToId(AId : string) : PHandleInfo;
    function FindToName(AName : string) : PHandleInfo;
    function FindToHandle(AHandle: THandle): PHandleInfo;

    constructor Create;
    destructor Destroy; override;
    property Items[Index: Integer]: PHandleInfo read Get;
  end;


  procedure WindowHandleChangeofID(OldId : string; NewHandle : THandle);

var
  ESMInfo   : TESMInfo;
  LoginInfo : TLoginInfo;
  SiteInfo  : TSiteInfo;
  AlertLevelKind : array[1..3] of TAlertLevelValue; //경보레벨 상/중/하의 경보 발생 종류 정보
  CurrentMonitor : TMonitor;

  //RealEventManager : TRealEventManager;
  XML_LOG_VIEW_FORM_HANDLE : THandle;
  IP_TRACE_FORM_HANDLE : THandle;
  TESTXML_FORM_HANDLE : THandle;
  FMainFormHandle : THandle;
  FProgramClose : Boolean;
  FOtherForceLogin : Boolean; //다른 사람이 강제 로그인 해서 내가 끊긴경우.

  WindowHandleList : TWindowHandleList;                //데이터 요청한 화면의 핸들.
  RealTimeLogReceiveFormHandle : TWindowHandleList;    //실시간 로그를 받을 화면 핸들
  RealTimePerforReceiveFormHandle : TWindowHandleList; //실시간 성능정보를 종합 화면에서 받을때 각 성능정보 자식 화면 핸들
  RealTimePerforAssetIdList : TStringList; //실시간성능정보 보고있는 자산들 목록 (종합성능정보, 멀티성능정보 2곳에서 사용됨.)

  //BlackIPList : TIPRangeInfoManager;       //범위형 블랙 IP
  //InsideIPList : TIPRangeInfoManager;      //범위형 내부 IP
  //ITSMIPList : TIPSingleInfoManager;       //단일형 ITSM IP (only 한국지역정보개발원)

  FIntegrityInterval : integer; //콘솔 자체 무결성 검사 주기. (0 이면 비활성, 1이상이면 활성)
  FFlashInstallANDFusionChartUse : boolean;
  FRealEventFormCreateCount : integer; //실시간종합이벤트 화면 개수

  //FBarMgrStyle : TdxBarManagerStyle;

implementation

//uses
  //LogFileUnit;

{ TWindowHandleList }

{-------------------------------------------------------------------------------
  Procedure: TWindowHandleList.WindowHandleChangeofID
  Author   : kyungmun
  DateTime : 2009.10.28
  Arguments: AId: string; AHandle: THandle
  Result   : None
  Comment  : 이전에있던 Id의 값을 삭제하고 new Handle 값을 추가한다.
-------------------------------------------------------------------------------}
procedure WindowHandleChangeofID(OldId: string; NewHandle: THandle);
var
  Info : PHandleInfo;
  AName : string;
begin
  if OldId = '' then Exit;
  Info := WindowHandleList.FindToId(OldId);
  if Info <> nil then
  begin
    AName := Info.Name;
    WindowHandleList.DeleteToID(OldId);
    WindowHandleList.Add(OldId, AName, NewHandle);
  end;
end;

procedure TWindowHandleList.Clear;
var
  i : integer;
begin
  for i := Self.Count - 1 downto 0 do
    Delete(i);

  inherited Clear;
end;

constructor TWindowHandleList.Create;
begin
  inherited Create(False);

  FCS := TCriticalSection.Create;
end;

destructor TWindowHandleList.Destroy;
begin
  Clear;
  FCS.Release;
  FCS.Free;
  inherited Destroy;
end;

function TWindowHandleList.FindToID(AId: string): PHandleInfo;
var
  i : integer;
begin
  Result := nil;
  FCS.Enter;
  try
    for i := 0 to Self.Count - 1 do
    begin
      if Get(i)^.ID = AID then
      begin
        Result := Get(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TWindowHandleList.FindToName(AName: string): PHandleInfo;
var
  i : integer;
begin
  Result := nil;
  FCS.Enter;
  try
    for i := 0 to Self.Count - 1 do
    begin
      if Get(i)^.Name = AName then
      begin
        Result := Get(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TWindowHandleList.FindToHandle(AHandle: THandle): PHandleInfo;
var
  i : integer;
begin
  Result := nil;
  FCS.Enter;
  try
    for i := 0 to Self.Count - 1 do
    begin
      if Get(i)^.Handle = AHandle then
      begin
        Result := Get(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWindowHandleList.Add(AId, AName: string; AHandle: THandle);
var
  Info : PHandleInfo;
begin
  FCS.Enter;
  try
    New(Info);
    Info.ID := AId;
    Info.Name := AName;
    Info.Handle := AHandle;
    inherited Add(TObject(Info));
  finally
    FCS.Release;
  end;
end;

procedure TWindowHandleList.Delete(index: integer);
begin
  FCS.Enter;
  try
    if index >= 0 then
    begin
      Dispose(Get(index));
      inherited Delete(index);
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWindowHandleList.DeleteToID(AId: string);
var
  i : integer;
begin
  FCS.Enter;
  try
    for i := Self.Count - 1 downto 0 do
    begin
      if Get(i)^.ID = AId then
      begin
        Delete(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWindowHandleList.DeleteToName(AName: string);
var
  i : integer;
begin
  FCS.Enter;
  try
    for i := Self.Count - 1 downto 0 do
    begin
      if Get(i)^.Name = AName then
      begin
        Delete(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TWindowHandleList.DeleteToHandle(AHandle: THandle);
var
  i : integer;
begin
  FCS.Enter;
  try
    for i := Self.Count - 1 downto 0 do
    begin
      if Get(i)^.Handle = AHandle then
      begin
        Delete(i);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TWindowHandleList.Get(Index: Integer) : PHandleInfo;
begin
  Result := PHandleInfo(inherited Get(Index));
end;

initialization

finalization

end.
