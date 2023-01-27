{-------------------------------------------------------------------------------
  Copyright (C) 2008                               
  Project  :                                      
  Author   : kyungmun                                    
  Date     : 2009-02-18
  Comment  : 로컬의 환경설정값을 관리하는 클래스
             컴포넌트 저장/읽기 방식으로 클래스의 프로퍼티값을 저장하고 읽는다.
             값이 추가될때 적당한 섹션별로 클래스를 생성해서 사용하길 권장한다.
-------------------------------------------------------------------------------}

unit ProfileNormalConfigUnit;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, DefineTypeUnit, LogFileUnit, uFunctionUnit;

const
  PROFILE_NORMAL_CONFIG_FILENAME = 'Config\Profile\ProfileNormalConfig.cfg';
  PROFILE_NORMAL_VERSION = '2011.07.06';

  PROFILE_SERVER_CONFIG_FILENAME = 'Config\Profile\ProfileServerConfig.cfg';
  PROFILE_SERVER_VERSION = '2011.07.06';

type
  //서버정보
  TServerInfo = class(TComponent)
  private
    FVersion : string;
    FServerName: string;
    FServerIP: string;
    FServerPort: string;
    FDBIP: string;
    FDBPort: string;
    FDBSID: string; //SID
    FDBUSER: string; //계정
    FDBPASS: string; //비밀번호
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version : string read FVersion write FVersion;
    property ServerName : string read FServerName write FServerName;
    property ServerIP : string read FServerIP write FServerIP;
    property ServerPort : string read FServerPort write FServerPort;
    property DBIP : string read FDBIP write FDBIP;
    property DBPort : string read FDBPort write FDBPort;
    property DBSID  : string read FDBSID write FDBSID;
    property DBUSER : string read FDBUSER write FDBUSER;
    property DBPASS : string read FDBPASS write FDBPASS;
  end;

  TServerInfoManager = class(TObjectList)
  private
    FileName : string;

    function Get(Index: Integer): TServerInfo;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetObject : TServerInfoManager;

    function LoadConfigFile : boolean;
    procedure SaveConfigFile;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function FindInfo(AName, AIP, APort : string) : TServerInfo;
    
    property Items[Index: Integer]: TServerInfo read Get; default;
  end;

  //로그인 설정 정보
  TConnectSection = class(TPersistent)
  private
    FIP: string;
    FID: string;
    FPort: string;
    FServerName : string;
    FEVVW_Use: Boolean;
    FEVQR_Use: Boolean;
    FEVQR_Count: Integer;
    FIDSave: Boolean;
    FDBPORT: string;
    FDBIP: string;
    FDBSID: string;
    FDBUSER: string;
    FDBPASS: string;
  public
    constructor Create;
  published
    property IP: string read FIP write FIP;
    property Port: string read FPort write FPort;
    property ServerName: string read FServerName write FServerName;
    property ID: string read FID write FID;
    property IDSave: Boolean read FIDSave write FIDSave;
    property EVVW_Use: Boolean read FEVVW_Use write FEVVW_Use;
    property EVQR_Use: Boolean read FEVQR_Use write FEVQR_Use;
    property EVQR_Count: Integer read FEVQR_Count write FEVQR_Count;
    property DBIP: string read FDBIP write FDBIP;
    property DBPORT: string read FDBPORT write FDBPORT;
    property DBSID: string read FDBSID write FDBSID;
    property DBUSER: string read FDBUSER write FDBUSER;
    property DBPASS: string read FDBPASS write FDBPASS;
  end;

  //로그파일 정보
  TLogSection = class(TPersistent)
  private
    FLevels: TMessageTypes;
    FDebugWindowsView: Boolean;
    FFileSizeLimiteMB: Integer;
  public
    constructor Create;
  published
    property FileSizeLimiteMB : Integer read FFileSizeLimiteMB write FFileSizeLimiteMB;
    property Levels: TMessageTypes read FLevels write FLevels;
    property DebugWindowsView: Boolean read FDebugWindowsView write FDebugWindowsView;
  end;

  //자동 재접속 정보
  TAutoReLoginSection = class(TPersistent)
  private
    FTryTermMinute: Integer;
    FTryMaxCount: Integer;
    FRun: Boolean;
  public
    constructor Create;
  published
    property Run: Boolean read FRun write FRun;
    property TryTermMinute: Integer read FTryTermMinute write FTryTermMinute;
    property TryMaxCount: Integer read FTryMaxCount write FTryMaxCount;
  end;

  //프로그램 잠그기 정보
  TProgramLockSection = class(TPersistent)
  private
    FWaitMinute: Integer;
    FRun: Boolean;
  public
    constructor Create;
  published
    property Run: Boolean read FRun write FRun;
    property WaitMinute: Integer read FWaitMinute write FWaitMinute;
  end;

  //자산트리 정보
  TAssetTreeSection = class(TPersistent)
  private
    FLGroupUnfiledVisible: Boolean;
    FSGroupVirtualVisible: Boolean;
    FImageUses: boolean;
    FSubImageCascade: Boolean;
    FSeverityImageCascade: Boolean;
  public
    constructor Create;
  published
    property LGroupUnfiledVisible: Boolean read FLGroupUnfiledVisible write FLGroupUnfiledVisible;
    property SGroupVirtualVisible: Boolean read FSGroupVirtualVisible write FSGroupVirtualVisible;
    property ImageUses : boolean read FImageUses write FImageUses;
    property SubImageCascade : Boolean read FSubImageCascade write FSubImageCascade;
    property SeverityImageCascade : Boolean read FSeverityImageCascade write FSeverityImageCascade;
  end;

  PCountColorValue = ^TCountColorValue;
  TCountColorValue = packed record
    AStart : Integer;
    AEnd   : Integer;
    AColor : TColor;
  end;

  //이벤트 표시 정보
  TEventSection = class(TPersistent)
  private
    FRowLimit: integer;
    FLowColor: TColor;
    FMediumColor: TColor;
    FLowestColor: TColor;
    FHighColor: TColor;
    FHighestColor: TColor;
    FCountColor: string;
    FCountColorUses: boolean;
    FCountColorTable : array of TCountColorValue;
    FRowMin: integer;
    FRealTotalEventFormCount: integer;
    function GetItems(index: Integer): PCountColorValue;
  public
    constructor Create;
    property CountColorTable[index : Integer] : PCountColorValue read GetItems;
    function GetCountColorTableCount : integer;
  published
    property RowMin: integer read FRowMin write FRowMin;
    property RowLimit: integer read FRowLimit write FRowLimit;
    property HighestColor: TColor read FHighestColor write FHighestColor;
    property HighColor: TColor read FHighColor write FHighColor;
    property MediumColor: TColor read FMediumColor write FMediumColor;
    property LowColor: TColor read FLowColor write FLowColor;
    property LowestColor: TColor read FLowestColor write FLowestColor;
    property CountColor: string read FCountColor write FCountColor;
    property CountColorUses: boolean read FCountColorUses write FCountColorUses;
    property RealTotalEventFormCount : integer read FRealTotalEventFormCount write FRealTotalEventFormCount;
  end;

  //화면 자동 전환 정보
  TAutoFocusRelaySection = class(TPersistent)
  private
    FTermSec: Integer;
    FRun: Boolean;
  public
    constructor Create;
  published
    property Run: Boolean read FRun write FRun;
    property TermSec: Integer read FTermSec write FTermSec;
  end;


  //환경값 섹션 관리 클래스. (요넘을 저장/읽어 사용한다.)
  TNormalConfig = class(TComponent)
  private
    FFileName: String;
    FConnectValue: TConnectSection;
    FLogValue: TLogSection;
    FAutoReLoginValue : TAutoReLoginSection;
    FProgramLockValue : TProgramLockSection;
    FAssetTreeValue : TAssetTreeSection;
    FEventValue: TEventSection;
    FVersion: string;
    FLastLayoutUse : Boolean;
    FProfileUses : Boolean;
    FAutoFocusRelay: TAutoFocusRelaySection;
    FTotalSeverityType : byte;
    FTotalSeverityPassive: integer;
    FTotalSeverityType2: byte;
  public
    class function GetObject : TNormalConfig;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadFromFile : Boolean;
    procedure SaveToFile;
  published
    property Version : string read FVersion write FVersion;
    property LastLayoutUse : boolean read FLastLayoutUse write FLastLayoutUse;
    property ProfileUses: Boolean read FProfileUses write FProfileUses;
    property TotalSeverityType : byte read FTotalSeverityType write FTotalSeverityType;
    property TotalSeverityType2 : byte read FTotalSeverityType2 write FTotalSeverityType2;
    property TotalSeverityPassive : integer read FTotalSeverityPassive write FTotalSeverityPassive;
    property Connect : TConnectSection read FConnectValue write FConnectValue;
    property Log: TLogSection read FLogValue write FLogValue;
    property AutoReLogin: TAutoReLoginSection read FAutoReLoginValue write FAutoReLoginValue;
    property ProgramLock: TProgramLockSection read FProgramLockValue write FProgramLockValue;
    property AssetTree: TAssetTreeSection read FAssetTreeValue write FAssetTreeValue;
    property Event: TEventSection read FEventValue write FEventValue;
    property AutoFocusRelay : TAutoFocusRelaySection read FAutoFocusRelay write FAutoFocusRelay;
  end;

implementation


var
  NormalConfig : TNormalConfig = nil;
  ServerInfoMgr : TServerInfoManager = nil;

{ TConfigValue }

constructor TNormalConfig.Create;
begin
  inherited;
  Name := 'NormalConfig';
  Version := PROFILE_NORMAL_VERSION;
  FLastLayoutUse := False;
  FTotalSeverityType  := 1; // 1 : 게이지 타입,  2 : 바 타입
  FTotalSeverityType2 := 1; // 1 : 게이지 타입,  2 : 바 타입
  FTotalSeverityPassive := -1; // 0 : 정상, 1 : 주의, 2 : 경고, 3 : 위협
  FFileName := '';
  FConnectValue     := TConnectSection.Create;
  FLogValue         := TLogSection.Create;
  FAutoReLoginValue := TAutoReLoginSection.Create;
  FProgramLockValue := TProgramLockSection.Create;
  FAssetTreeValue   := TAssetTreeSection.Create;
  FEventValue       := TEventSection.Create;
  FAutoFocusRelay   := TAutoFocusRelaySection.Create;
end;

destructor TNormalConfig.Destroy;
begin
  SaveToFile;
  FConnectValue.Free;
  FLogValue.Free;
  FAutoReLoginValue.Free;
  FProgramLockValue.Free;
  FAssetTreeValue.Free;
  FEventValue.Free;
  FAutoFocusRelay.Free;
  
  NormalConfig := nil;
end;

function TNormalConfig.LoadFromFile : Boolean;
var
  Stream: TFileStream;
  BinStream: TMemoryStream;
begin
  Result := False;
  if FileExists(FFileName) then
  begin
    Result := True;
    try
      Stream := TFileStream.Create(FFileName, fmOpenRead);
      BinStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(Stream, BinStream);
        BinStream.Seek(0, soFromBeginning);
        BinStream.ReadComponent(Self);
      finally
        Stream.Free;
        BinStream.Free;
      end;
    except
      Result := False;
    end;
  end;
end;

procedure TNormalConfig.SaveToFile;
var
  Stream: TFileStream;
  BinStream: TMemoryStream;
  i, Count : integer;
  AValue : string;
begin
  //OutputDebugString(PChar('TNormalConfig SaveToFile 1 : ' + FormatFloat('0000000000000000', (GetTickCount))));
  //이벤트 건수별 색상을 " 시작;끝;색상,시작;끝;색상,,,,, " 형식으로 변환후저장.
  Count := length(Event.FCountColorTable);
  Event.CountColor := '';
  for i := 1 to Count - 1 do
  begin
    if Event.CountColor <> '' then
      Event.CountColor := Event.CountColor + ',';

    if i = 6 then Event.FCountColorTable[i].AEnd := 1000000;

    AValue := IntToStr(Event.FCountColorTable[i].AStart) + ';' +
              IntToStr(Event.FCountColorTable[i].AEnd) + ';' +
              IntToStr(Event.FCountColorTable[i].AColor);

    Event.CountColor := Event.CountColor + AValue;
  end;

  Stream := TFileStream.Create(FFileName, fmCreate);
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(Self);
    BinStream.Seek(0, soFromBeginning);
    ObjectBinaryToText(BinStream, Stream);
  finally
    Stream.Free;
    BinStream.Free;
  end;
  //OutputDebugString(PChar('TNormalConfig SaveToFile 1 : ' + FormatFloat('0000000000000000', (GetTickCount))));
end;

class function TNormalConfig.GetObject: TNormalConfig;
var
  ColorList1, ColorList2 : TStringList;
  i : Integer;
begin
  if NormalConfig = nil then
  begin
    NormalConfig := TNormalConfig.Create(nil);
    NormalConfig.FFileName := GetExePath + PROFILE_NORMAL_CONFIG_FILENAME;

    //파일 읽기실패나 버젼이 다르면 파일을 삭제하고 다시 생성한다.
    if (not NormalConfig.LoadFromFile) or (NormalConfig.Version <> PROFILE_NORMAL_VERSION) then
    begin
      TLogFile.GetObject.Write(mtDebug, Format('[프로파일 읽기 실패]기본 프로파일 읽기 실패 또는 버젼이 다릅니다. 현재 버전 : %s, 읽어들인 프로파일 버젼 : %s', [PROFILE_NORMAL_VERSION, NormalConfig.Version]));
      NormalConfig.Free;
      NormalConfig := TNormalConfig.Create(nil);
      NormalConfig.FFileName := GetExePath + PROFILE_NORMAL_CONFIG_FILENAME;
      DeleteFile(NormalConfig.FFileName);
    end;

    //이벤트 건수별 색상을 " 시작;끝;색상,시작;끝;색상,,,,, " 형식을 파싱해서 읽음.
    ColorList1 := TStringList.Create;
    ColorList1.StrictDelimiter := True; //공백, 텝들을 구분자로 인식하지 못하게..
    ColorList1.Delimiter := ',';
    ColorList1.DelimitedText := NormalConfig.Event.CountColor;
    try
      if ColorList1.Count > 1 then
      begin
        ColorList2 := TStringList.Create;
        ColorList2.StrictDelimiter := True; //공백, 텝들을 구분자로 인식하지 못하게..
        ColorList2.Delimiter := ';';
        try
          SetLength(NormalConfig.Event.FCountColorTable, ColorList1.Count+1);
          for i := 0 to ColorList1.Count - 1 do
          begin
            ColorList2.DelimitedText := ColorList1[i];
            if ColorList2.Count = 3 then
            begin
              NormalConfig.Event.FCountColorTable[i+1].AStart := StrToInt(ColorList2[0]);
              NormalConfig.Event.FCountColorTable[i+1].AEnd   := StrToInt(ColorList2[1]);
              NormalConfig.Event.FCountColorTable[i+1].AColor := StrToInt(ColorList2[2]);
            end;
          end;
        finally
          ColorList2.Free;
        end;
      end;
    finally
      ColorList1.Free;
    end;
  end;

  Result := NormalConfig;
end;

{ TProgramLockSection }

constructor TProgramLockSection.Create;
begin
  inherited Create;

  Run := False;
  FWaitMinute := 5;
end;

{ TAutoReLoginSection }

constructor TAutoReLoginSection.Create;
begin
  inherited Create;

  TryTermMinute := 5;
  TryMaxCount := 10;
  Run := False;
end;

{ TLogSection }

constructor TLogSection.Create;
begin
  inherited Create;

  Levels := [mtNotice, mtInfo];
  DebugWindowsView := False;
  FileSizeLimiteMB := 5;
end;

{ TConnectSection }

constructor TConnectSection.Create;
begin
  inherited Create;

  IP := '';
  Port := '7911';
  ServerName := 'No Name';
  ID := '';
  EVQR_Use := False;
  EVQR_Count := 100;
  EVVW_Use := False;
  IDSave := False;
  DBIP := '';
  DBPORT := '1521';
  DBSID  := '';
  DBUSER := '';
  DBPASS := '';
end;

{ TAssetTreeSection }

constructor TAssetTreeSection.Create;
begin
  inherited Create;

  FLGroupUnfiledVisible := True;
  FSGroupVirtualVisible := True;
  FImageUses := True;
  FSubImageCascade:= True;
  FSeverityImageCascade := True;
end;

{ TEventSection }

constructor TEventSection.Create;
begin
  inherited Create;

  FRowLimit := 2000;
  FRowMin := 1000;
  FRealTotalEventFormCount := 1;
  FCountColor := '5;9;16777181,'+
                 '10;99;16777088,'+
                 '100;199;8453888,'+
                 '200;299;16744448,'+
                 '300;399;4227327,'+
                 '400;1000000;255';
  FCountColorUses := False;
  
  //위험도별 색상.
  FHighestColor := clRed;
  FHighColor    := $004080FF;
  FMediumColor  := $00FF8000;
  FLowColor     := $0080FF00;
  FLowestColor  := $00FFFF80;
end;

function TEventSection.GetCountColorTableCount: integer;
begin
  Result := Length(FCountColorTable);
end;

function TEventSection.GetItems(index: Integer): PCountColorValue;
begin
  Result := @FCountColorTable[index];
end;

{ TAutoFocusRelaySection }

constructor TAutoFocusRelaySection.Create;
begin
  inherited Create;

  FTermSec := 10;
  FRun := False;
end;

{ TServerInfoManager }

procedure TServerInfoManager.Clear;
var
  i : integer;
begin
  try
    for i := Count - 1 downto 0 do
      Delete(i);
    inherited Clear;
  finally
  end;

  inherited Clear;
end;

constructor TServerInfoManager.Create;
begin
  inherited Create(False);
  FileName := GetExePath + PROFILE_SERVER_CONFIG_FILENAME;
  LoadConfigFile;
end;

procedure TServerInfoManager.Delete(Index: Integer);
begin
  try
    if Items[index] <> nil then
      TServerInfo(Items[Index]).Free;
    inherited Delete(index);
  finally
  end;
end;

destructor TServerInfoManager.Destroy;
begin
  //SaveConfigFile;
  Clear;
  inherited;
end;

function TServerInfoManager.FindInfo(AName, AIP, APort: string): TServerInfo;
var
  AInfo : TServerInfo;
  i : integer;
begin
  Result := nil;
  try
    for i := Count - 1 downto 0 do
    begin
      AInfo := Items[i];
      if (AInfo.ServerName = AName) and (AInfo.ServerIP = AIP) and (AInfo.ServerPort = APort) then
      begin
        Result := AInfo;
        Break;
      end;
    end;
  finally
  end;
end;

function TServerInfoManager.Get(Index: Integer): TServerInfo;
begin
  Result := nil;
  try
    if (Index >= 0) and (Count-1 >= Index) then
      Result := TServerInfo(inherited Get(Index));
  finally
  end;
end;

class function TServerInfoManager.GetObject: TServerInfoManager;
begin
  if ServerInfoMgr = nil then
    ServerInfoMgr := TServerInfoManager.Create;
  result := ServerInfoMgr;
end;

function CompareItem(Item1, item2: Pointer): Integer;
begin
  Result := CompareText( TServerInfo(Item1).ServerName,  TServerInfo(Item2).ServerName );
end;

function TServerInfoManager.LoadConfigFile: boolean;
var
  Stream: TFileStream;
  BinStream: TMemoryStream;
  AMember : TServerInfo;
begin
  if not FileExists(FileName) then
  begin
    Result := False;
    Exit;
  end;
  Clear;
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    BinStream := TMemoryStream.Create;
    try
      Stream.Position := 0;
      while Stream.Size > Stream.Position do //파일 크기만큼 돌면서 데이터 읽는다.
      begin
        BinStream.Clear;
        AMember := nil;
        ObjectTextToBinary(Stream, BinStream);
        try
          BinStream.Seek(0, soFromBeginning);
          AMember := TServerInfo(BinStream.ReadComponent(nil));
          if AMember.Version <> PROFILE_SERVER_VERSION then
          begin
            TLogFile.GetObject.Write(mtDebug, Format('[프로파일 읽기 실패]서버정보 프로파일 읽기 실패 또는 버젼이 다릅니다. 현재 사용 버전 : %s, 프로파일에서 읽어들인 버전 : %s', [PROFILE_SERVER_VERSION, AMember.Version]));
            AMember.Free;
            Continue;
          end;
        except
          on E: Exception do
          begin
            if AMember <> nil then
            begin
              TLogFile.GetObject.Write(mtExcept, 'LoadFromFile : ' + AMember.Name + '=>' + E.Message);
              AMember.Free;
            end;
            Result := False;
            Exit;
          end;
        end;
        AMember.ServerIP := {Seed128Decrypt(}AMember.ServerIP;
        AMember.DBIP     := {Seed128Decrypt(}AMember.DBIP;
        AMember.DBSID    := {Seed128Decrypt(}AMember.DBSID;
        AMember.DBUSER   := {Seed128Decrypt(}AMember.DBUSER;
        AMember.DBPASS   := {Seed128Decrypt(}AMember.DBPASS;
        inherited Add(TObject(AMember));
      end;
    finally
      Stream.Free;
      BinStream.Free;
      inherited Sort(CompareItem);
    end;
  except
    Result := False;
  end;
end;

procedure TServerInfoManager.SaveConfigFile;
var
  FStream: TFileStream;
  MStream: TMemoryStream;
  AMember : TServerInfo;
  i : integer;
begin
  if Trim(FileName) <> '' then
  begin
    if FileExists(FileName) then DeleteFile(PChar(FileName));
    FStream := TFileStream.Create(FileName, fmCreate);
    MStream := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
      begin
        MStream.Clear;
        AMember := Items[i];
        if AMember <> nil then
        begin
          AMember.ServerIP := AMember.ServerIP;
          AMember.DBIP     := AMember.DBIP;
          AMember.DBSID    := AMember.DBSID;
          AMember.DBUSER   := AMember.DBUSER;
          AMember.DBPASS   := AMember.DBPASS;
          MStream.WriteComponent(AMember);
          MStream.Seek(0, soFromBeginning);
          ObjectBinaryToText(MStream, FStream);
        end;
      end;
    finally
      FStream.Free;
      MStream.Free;
    end;
  end;
end;

{ TServerInfo }

constructor TServerInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Version := PROFILE_SERVER_VERSION;
  ServerName := '';
  ServerIP := '0.0.0.0';
  ServerPort := '7911';
  DBIP := '0.0.0.0';
  DBPort := '1521';
  DBSID := 'ESM';
  DBUSER := '';
  DBPASS := '';
end;

destructor TServerInfo.Destroy;
begin
  inherited;
end;

initialization
  RegisterClasses( [TServerInfo] );

finalization
  if ServerInfoMgr <> nil then ServerInfoMgr.Destroy;
  if NormalConfig <> nil then NormalConfig.Destroy;

  UnRegisterClasses( [TServerInfo] );
end.