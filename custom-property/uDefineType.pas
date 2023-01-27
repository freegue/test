{-------------------------------------------------------------------------------
 Copyright (C) 2022
 Project  :
 Author   : kyungmun
 Date     : 2022-07-06
 Comment  : 프로그램에서 조회조건에 사용하는 타입 정의
-------------------------------------------------------------------------------}


unit uDefineType;

interface

uses
  SysUtils, Graphics, Classes, Controls, Messages, XMLIntf;

type
  TPatternInfo = class
    category : integer;
    code : integer;
    name : string;
  end;

  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

  //수신, 분석스레드에서 처리할 데이터 종류.
  TProcessDataType = (dtAll, dtXml, dtReal, dtLog, dtEvent);

  //로그 출력 타입
  TMessageType = (mtNotice, mtInfo, mtError, mtExcept, mtDebug);//, mtOff, mtAll);
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

  //작업 타입
  TWorkType = (wtAdd, wtEdit, wtView, wtDelete, wtInsert, wtLockUnLock, wtMove, wtHeartbeat);

  //위험도
  TRiskLevel3Type = (rtHigh, rtMedium, rtLow);  //높음/보통/낮음
  TRiskLevel3Types = set of TRiskLevel3Type;

  //방향
  TDirection = (InBound, OutBound, NoBound);
  TDirections = set of TDirection;

  //차단타입 (Block Action)
  TBlockType = (btBlock, btRedirectBlock, btPacketBaseBlock);
  TBlockTypes = set of TBlockType;

  //차단방법
  TBlockMethodType = (
                      SN_SRC_IP,
                      SN_DST_IP,
                      SN_AND_IP,
                      SN_SRC_SRVC,
                      SN_DST_SRVC,
                      SN_DNS_TYPE,
                      SN_DHCP_MAC,
                      SN_SIP_CALLER,
                      SN_SIP_CALLEE,
                      SN_SIP_CALLER_CALLEE,
                      SN_SIP_CALLER_TYPE,
                      SN_SIP_CALLEE_TYPE,
                      SN_SIP_CALLER_CALLEE_TYPE
                     );
  TBlockMethodTypes = set of TBlockMethodType;

  //차단사유타입
  TBlockReasonType = (brtSessionForceClose, brtDetectBlock, brtSBL, brtPacketBaseBlock);
  TBlockReasonTypes = set of TBlockReasonType;

  //탐지로그의 기타 정보 타입
  TDetectEtcType = (etAsset, etRaw, etTrustIP);
  TDetectEtcTypes = set of TDetectEtcType;

  //IP, Port 데이타 입력 종류
  TNetworkValueType = (vtRange, vtMulti, vtPrefix, vtSingle);

  //IP, Port 타입 종류
  TNetworkDataType = (dtIPv4, dtIPv6, dtPort);

  //IPv4 IPv6
  TIPAddressType = (itIPv4, itIPv6);

  //일별/주별/월별
  TDateType = (dtDaily, dtWeekly, dtMonthly);

  //날짜선택 타입 (범위형, 1일단위)
  TDateSelectType = (dstRange, dstDaily);

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

const
  SNIPER_PROPERTY_JSON_FORMAT_VERSION = '1.0';

implementation

end.
