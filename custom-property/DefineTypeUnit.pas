{-------------------------------------------------------------------------------
 Copyright (C) 2022
 Project  :
 Author   : kyungmun
 Date     : 2022-07-06
 Comment  : 프로그램에서 사용하는 각종 타입 지정
-------------------------------------------------------------------------------}


unit DefineTypeUnit;

interface

//{$I Define.inc}

uses
  SysUtils, Graphics, Classes, Controls, Messages, XMLIntf;
  //{$IFDEF ASN1024}, AsnLib1024 {$ENDIF}
  //{$IFDEF ASN512}, AsnLib512 {$ENDIF}
  //{$IFDEF ASN256}, AsnLib256 {$ENDIF};

const
  WM_FORMRESTORM = WM_USER + 100;
  USER_XML_DATA  = WM_USER + 2000;
  USER_XML_DATA_REPLY = WM_USER + 2010;
  USER_XML_DATA_SELF  = WM_USER + 2020;
  USER_WIN_HANDLE     = WM_USER + 3000;
  //USER_REALTIME_EVENT = WM_USER + 4000;
  //USER_REALTIME_LOG   = WM_USER + 5000;
  USER_QUERY_EVENT              = WM_USER + 6000;
  USER_CHILD_FORM_CLOSE         = WM_USER + 7000;
  USER_ALERT_EVENT_FORM_CREATE  = WM_USER + 8000;
  USER_SCAN_EVENT_FORM_CREATE   = WM_USER + 8100;
  USER_REALTIME_EVALAK          = WM_USER + 8200;
  USER_ASSET_UPDATE             = WM_USER + 8300;
  USER_LOG_QUERY_COMPLETE       = WM_USER + 8400;
  USER_CORR_EVENT_FORM_CREATE   = WM_USER + 8500;
  USER_SELF_INTEGRITY_INTERVAL  = WM_USER + 8600;

  ESM_XML_LOG_HANDLE = 20091110;
  ESM_XML_LOG_SEND = 200911101;
  ESM_XML_LOG_RECV = 200911102;

  EV_TERMINATE    = 1;
  EV_DATAINPUT    = 2;
  EV_MIN          = EV_TERMINATE;
  EV_MAX          = EV_DATAINPUT;

  XML_RESULT_SUCCESS = 'SUCCESS';
  XML_RESULT_FAIL = 'FAIL';

  //실시간 이벤트 종류.
  REAL_EVENT_NORMAL_NAME = 'PMLC';   //일반 보안제품 이벤트
  REAL_EVENT_CORRELATION_NAME = 'EVCR'; //연관 이벤트
  REAL_EVENT_SCANNING_NAME = 'EVSR'; //탐지 룰셋 이벤트
  REAL_EVENT_BLACkIP_NAME = 'EVBL'; //블랙리스트 이벤트 (블랙리스트 등록시 이벤트 발생 체크된 IP 값들..)

  //성능정보 종류.
  XML_CPU_ITEM        = 'CM00000001';
  XML_MEMORY_ITEM     = 'CM00000002';
  XML_DISK_ITEM       = 'CM00000003';
  XML_NETWORK_ITEM    = 'CM00000004';
  XML_SESSION_ITEM    = 'CM00000015';
  XMLCTRL_ASRLQR_RM   = -999;

  ONE_MEGA_BYTE = 1024 * 1024;

  ETC_GUBUN1 = '|';
  ETC_GUBUN2 = '=';

  //PKNU_AC_InsideIP_DLL = 'pknuacip.dll';  //부경대학교 내부IP 확인 dll
  PKNU_AC_InsideIP_DLL = 'pknuacinsideip.dll';  //부경대학교 내부IP 확인 dll

  TAG_LESSTHEN        = '&lt;';    //xml 내용중에서 > 문자 대체 문자.
  TAG_GREATTHEN       = '&gt;';    //xml 내용중에서 < 문자 대체 문자.
  TAG_AND             = '&amp;';   //xml 내용중에서 & 문자 대체 문자.
  TAG_QUOT            = '&quot;';  //xml 내용중에서 " 문자 대체 문자.
  TAG_APOS            = '&apos;';  //xml 내용중에서 ' 문자 대체 문자.


  {
  COLOR_SERIES_BUF: array[0..10] of TColor
                    = ($000080FF,
                       $00FF8080,
                       $00FF8000,
                       clSkyBlue,
                       clMoneyGreen,
                       $00E4CAFF,
                       $009DBDFF,
                       clNavy,
                       clGreen,
                       clFuchsia,
                       clRed);
  }
  {//google
  COLOR_SERIES_BUF: array[0..10] of TColor
                   = ($00DA906B,
                      $00A43C3C,
                      $00007300,
                      $003399FF,
                      $00B38600,
                      $0000C3FF,
                      $000000CC,
                      $00B3608A,
                      $000066FF,
                      $004817BC,
                      $00CC0000);
  }

  COLOR_ACTIVE  = $0025C21B; //녹색계열
  COLOR_STANDBY = $00DA3DE4; //위 색의 보색

  COLOR_SERIES_BUF: array[0..10] of TColor
                   = ($0063BF8A,
                      $005152e5,
                      $00e99251,
                      $00ac59ff,
                      $00ed84c0,
                      $004da3ff,
                      $00676767,
                      $00e9d093,
                      $001dd1f7,
                      $003155ba,
                      $0051a600);

  COLOR_SERIES_BUF_FUCION : array[1..5] of string
                            = ('B6D9F5',
                               '90BD0B',
                               '0790D6',
                               'A68BC0',
                               'F8624D'
                               );

type

  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;


  //제품군 타입
  TEsmProductGroup = (ALL, DB, CM, FW, ID, IP, VP, PK, VI, SO, SC, MA, EA, SP, LA, NM,
                      NB, SY, PM, WS, IM, PA, EM, US, WF, DP, NC, DS, ES, SN);

  //로그 출력 타입
  TMessageType = (mtDebug, mtInfo, mtNotice, mtWarning, mtExcept, mtError, mtFatal);
  TMessageTypes = set of TMessageType;

  //컨트롤 데이타(조회,정보), 실시간 데이타(event, log) socket 별도 운영.
  TSocketConnectType = (stControl, stRealTime);

  //입력 컨트롤 타입 (룰관리 등에서 사용되는 동적생성 컨트롤)
  TInputContolType = (ctNull, ctBasic, ctEdit, ctCombobox, ctComboboxCheck, ctEditNum, ctEditCheckBox,
                      ctIP, ctPort, ctIPRange, ctNumRange, ctAsset, ctProduct, ctComboProduct, ctPeriodCount);

  TEditType = (etEdit, etEditLong, etEditNum);

  TColumnDataType = (cdtAuto, cdtFloat, cdtInteger, cdtChar, cdtDateTime, cdtIP, cdtPort);

  //ColumnDataType 값이 dtInteger 인 컬럼들을 지정한다.
  TDataTypeIntegerKind = (dtkNone, dtkCount, dtkSeverity, dtkSourcePort, dtkDestPort);

  //수신, 분석스레드에서 처리할 데이터 종류.
  TProcessDataType = (dtAll, dtXml, dtReal, dtLog, dtEvent);

  //작업 타입
  TWorkType = (wtAdd, wtEdit, wtView, wtDelete, wtInsert, wtLockUnLock, wtMove, wtHeartbeat);

  //실시간 이벤트 작업(추가, 갱신, 삭제, 전체삭제, 작업시작, 작업완료)
  TRealTimeEventWorkType = (ewtAdd, ewtUpdate, ewtDelete, ewtClear, ewtStart, ewtEnd);

  //실시간 이벤트 종류 (전체, 경보, 자산운영, 연관, 블랙, 스캐닝)
  TRealEventType = (etAll, etAlert, etOperation, etCorrelation, etBlackList, etScanning);

  //조회 이벤트 종류 (조건별,  인지,     미인지, 연관,     경보,    스캐닝,   휴면계정,         프로세스감시,     무결성위반,          intrusion_event, 연관근거데이타)
  TQueryEventType = (qtEVAKQR, qtEVADQR, qtEVQR, qtEVECQR, qtAlert, qtEVSNQR, qtDormantAccount, qtProcessMonitor, qtIntgrityViolation, qtEVINQR, qtEVECQR_BASE);

  //위험도
  TEsmSeverity = (Highest, High, Medium, Low, Lowest);
  TEsmSeveritys = set of TEsmSeverity;

  //차단종류
  TEsmBlockKind = (Block, Redirect, PacketBaseBlock);
  TEsmBlockKinds = set of TEsmBlockKind;

  //방향
  TEsmDirection = (InBound, OutBound, NoBound);
  TEsmDirections = set of TEsmDirection;

  //대응여부
  TEsmAnnotation = (Before, Process, Complete);
  TEsmAnnotations = set of TEsmAnnotation;

  //수행결과
  TEsmResultState = (Fail, Success, NoData);
  TEsmResultStates = set of TEsmResultState;

  //경보종류
  TEsmAlertType = (SMS, EMail, Siren, PopupWindow);
  TEsmAlertTypes = set of TEsmAlertType;

  //경보등급 없음/상/중/하
  TEsmAlertLevelType = (altNone, altTop, altMiddle, altBottom);
  TEsmAlertLevelTypes = set of TEsmAlertLevelType;

  //성능정보 조회 종류
  //TEsmResourceDataType = (CPU, MEMORY, DISK, NETWORK);
  TEsmResourceDataType = (CPU, MEMORY, DISK, NETWORK, SESSION);

  TEsmResourceSearchType = (HOUR, DAY, MONTH);

  //관리자 작업/경보 이력 조회 구분 타입
  TAdminQueryType = (atWork, atAlert);

  //IP, Port 데이타 입력 종류
  TValueKind = (vkRange, vkMulti);

  //IP, Port 타입 종류
  TDataKind = (dkIP, dkPort);

  //일별/주별/월별
  TDateType = (dtDaily, dtWeekly, dtMonthly);

  //자산정보 관련 타입들..
  { Asset Node Type }
  TNodeLevelType = (nlNone, nlDomain, nlLargeGroup, nlSmallGroup, nlAsset, nlProduct);
  { Asset Agent Type }
  TAgentType = (atNone, atAgent, atAgentless);
  { Agent Status Type }
	TAgentStatus = (asNone, asAlive, asDead); // initValue, Agent Alive,  Agent Dead
	{ Product Status Type }
	TProductStatus = (psNone, psOn, psHangUp, psOff); // initValue, service On, service hang-up, service Off
  { Asset Data change Status Type}
  TDataStatus = (dsNone, dsAdd, dsEdit, dsDelete, dsAll, dsClear);
  TModifyDetaile = (mdNone, mdInfo, mdPosition, mdAlived, mdSeverity); // Detail status of Modify (mdInfo: 일반데이터, mdPosition: 위치변동, mdAlived: 연결상태, mdSeverity: 위험도변경)
  { Asset OS Type }
  TOSType = (osNothing, osUnknown, osSunOS, osHP_UX, osAIX, osLinux, osTru64, osWindows, osSGA_IRIX , osNOS);
  { Asset Tree Update State }
  TAssetTreeUpdateState = (usNone, usStart, usEnd);

  //룰 정보 관련 타입
  TRuleInfoType = (rntRoot, rntGroup, rntTemplate, rntRule);

  //룰 종류 (기본룰, 연관룰, 스캐닝룰) 
  TRuleKind = (rkNormal, rkCorr, rkScan);

  //룰 적용 상태값
  PRuleApplyData = ^TRuleApplyData;
  TRuleApplyData = packed record
    AType : TRuleInfoType;
    AValue : array[0..2] of Variant; //이름, 적용, 실행 3항목의 표시글자
    AId   : String;
    ImageIndex : integer;
    ApplyChecked : Boolean;
    RunChecked : Boolean;
    Changed: Boolean;
  end;

  //관리자 정보 관련 타입
  TAdminNodeType = (antRoot, antGroup, antAdmin);

  //성능정보 종류 관련
  TPerfDataType = (ptCPU, ptMemory, ptDISK, ptNetWork, ptSession);
  TNetDataType = (ntNone, ntRx, ntTx, ntCollision);

  //이미지종류
  TPictureType = (ptBitmap, ptJpeg);

  //싸이트 버젼
  TVersion = (verWINS         //기본
             );

  TRangeNum = packed record
    FStart : integer;
    FEnd : integer;
  end;

  TRangeIPv4 = packed record
    FStart : string[15];
    FEnd : string[15];
  end;

  //ESM 기본 정보
  TESMInfo = packed record
    ID,
    Level,
    Name: OleVariant;
    ParentId, ParentId2 : integer;
  end;

  //관리자 경보 설정 정보
  TAlertStatus = packed record
    SMSAlert : Boolean;
    EMail : Boolean;
    Siren : Boolean;
    Popup : Boolean;
    BalloonPopup : Boolean;
    SirenWaveFile : string;
    SirenPlayTime : integer;
  end;

  //경보레벨별 발생경보 정보
  TAlertLevelValue = packed record
    TypeID: integer;
    SMS   : Boolean;
    Email : Boolean;
    Siren : Boolean;
    Popup : Boolean;
  end;

  //로그인 사용자 정보
  TLoginInfo = packed record
    Session    : string;  //ps의 cmd에서 관리하는 세션ID
    Group      : integer; //사용자그룹ID
    AdminID    : integer; //숫자
    LoginID    : string;  //영문 ID
    RealName   : string;  //한글 이름
    ServerName : string;  //접속 서버 호스트명
    Logined    : Boolean; //로그인 성공여부
    Deleted    : Boolean; //계정삭제여부 (로그인된 상태에서 super-admin이 내 계정을 삭제했을때 값을 true로 변경)
    AlertStatus: TAlertStatus; //관리자의 경보설정 값
    PassWord : string;
    ServerIP : string;
    DBIP : string;
    DBPort : string;
    DBSID : string;
    DBUSER : string;
    DBPASS : string;
  end;

  //싸이트 정보
  TSiteInfo = packed record
    ResourceName : string;
    Caption : string;
    Version : TVersion;
    Cascade : Boolean;
    Session : Boolean;
    ScanRule: Boolean;
    MySqlUse: Boolean;
    IntrusionEvent : Boolean;
    SMSAlert : Boolean;
  end;

  //이벤트 값 정보 (ASN 구조 + 추가정보)
  PEventData = ^TEventData;
  TEventData = packed record
    //Value          : PEVENT_STR;
    AssetGroupName : WideString;
    AssetName      : WideString;
    ProductName    : WideString;
    LastUpdateTime : WideString;
    SeverityName   : WideString;
    Annotated      : WideString;
    AckTime        : WideString;
    AckAdminName   : WideString;
    Protocol       : WideString;
    AlertLevel     : WideString;
    AckAdminID     : Integer;
    SourceBlackIP  : Boolean;
    DestBlackIP    : Boolean;
    SourceBlackColor  : TColor;
    DestBlackColor    : TColor;
    EventID        : Integer;
    //CountryInfoSrc : TGeoIPCountry;
    CountryFlagSrc : TGraphic;
    //CountryInfoDest : TGeoIPCountry;
    CountryFlagDest : TGraphic;
    Number         : integer;
  end;

  //제품군별 로그 조회 및 기타 조회 화면의 리스트뷰 값
  PLogData = ^TLogData;
  TLogData = record
    Node : IXMLNode;
    CellText : array of string;
    //CountryInfoSrc : TGeoIPCountry;
    CountryFlagSrc : TGraphic;
    //CountryInfoDest : TGeoIPCountry;
    CountryFlagDest : TGraphic;
  end;

  //이벤트 위험도별 건수 보드판
  TSeverityCountBoard = packed record
    Total, Highest, High, Medium, Low, Lowest: Integer;
  end;

  //내보내기 데이터 타입.
  TExportType = (etExcel, etCSV);

  //자산의 실시간 정보 조회 (서비스포트정보, 프로세스정보, 사용자접속정보, 패치정보, 라우팅정보)
  TAssetRealEtcInfoType = (rtServicePortInfo, rtProcessInfo, rtUserConnectInfo, rtPatchInfo, rtRouteInfo);

  TIPKind = (ipBlack, ipInternal);


implementation


end.