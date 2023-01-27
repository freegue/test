unit uFunctionUnit;

interface

uses
  WinSock, Sysutils, Classes, XMLDoc, XMLIntf, xmldom, msxmldom, Typinfo, GIFImg,
  IniFiles, WinInet, Windows, Forms, Dialogs, ShellAPI, ComObj,
  StdCtrls, Consts, Controls, Graphics, StrUtils, CommCtrl, Variants, System.RegularExpressions,
  ExtCtrls, Messages, Math;//, RegExpr;
  //XmlCtrlUnit, mem_util, RegExpr, sea_base;

//const
  //SeedKeyValue : TSEABlock = ($49, $4E, $5A, $45, $4E, $53, $45, $43, $55, $50, $4C, $41, $54, $45, $53, $4D);

type
  //메모리정보 2GB 이상일때 TMemoryStatus 값으론 처리 못하는 문제.
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: Int64;
    ullAvailPhys: Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual: Int64;
    ullAvailVirtual: Int64;
    ullAvailExtendedVirtual: Int64;
  end;

  TVersionDescription = record
   CompanyName : string;
   FileDescription : string;
   FileVersion : string;
   InternalName : string;
   LegalCopyright : string;
   LegalTrademarks : string;
   OriginalFileName : string;
   ProductName : string;
   ProductVersion : string;
   Comments : string;
   SpecialBuild : string;
 end;

  function LoadFromIniFile(AFileName: string; APath: string = ''): TIniFile;
  function LoadFromConfigFile : TIniFile;
  function GetExePath : string;

  function GetType(AData: string; AType: Integer; Plus: Boolean=True): Integer;
  function GetText(ANode: IXMLNode; AText: string): OleVariant;
  function GetTextDef(ANode: IXMLNode; AText: string; DefValue: OleVariant): OleVariant;
  function GetAttr(ANode: IXMLNode; AAttr: string): OleVariant;
  function GetAttrInt(ANode: IXMLNode; AAttr: string): integer;
  function GetAttrDef(ANode: IXMLNode; AAttr: string; DefValue: OleVariant): OleVariant;
  function GetNode(ANode: IXMLNode; AText: string): IXMLNode;
  function GetFileList(const DirPath : string; AExt : string = '*.*' ) : TStringList;

  function isNumValue(AValue: string) : boolean;
  function isOnLine : Boolean;
  function isURLValue(AValue: string) : Boolean;
  function isEmailValue(AValue: string) : Boolean;
  function isMacAddresValue(AValue: string) : Boolean;
  function isIP4AddresValue(AValue: string) : Boolean;
  function isMobileValue(AValue: string) : Boolean;
  function IP4ToInt64(const Value: string) : Int64;
  function IntToIP4(const Value: DWORD) : string;
  function NumberCut(const Value : string) : Int64;
  function NumberCatchToNum(const Value : string) : Int64;
  function NumberCatchToStr(const Value : string) : string;
  function InCompareText(const ACompareValue, ASubValue : string; ADelimit : Char) : Boolean;
  function LikeCompareText(const ACompareValue, ASubValue : string; ADelimit : Char) : Boolean;
  function ByteToGB(Value: String): String;
  function MByteToGB(Value: String): String;
  function ByteTokbps(Value: Double): Double;
  function InputLimiteChar(Value : string; Alpha, Hangle : Boolean) : boolean;
  function StringToYYYY_MM_DD_HH_NN_SS(const Value : string) : string;
  function TrueFalse(AQuestion : Boolean; const TrueStr, FalseStr : string) : string;

  function RGBColorToXML(iColor: integer): string;
  function XMLColorToRGB(sColor: string): COLORREF;

  procedure WaitForTimer(MilliSeconds :  LongInt);
  function MessageBoxTimeOut(hWnd: HWND; lpText: PChar; lpCaption: PChar; uType: UINT; wLanguageId: WORD; dwMilliseconds: DWORD): Integer; stdcall; external user32 name 'MessageBoxTimeoutA';
  function InputNumberQuery(const ACaption, APrompt: string; var Value: string): Boolean; overload;
  function InputNumberQuery(const ACaption, APrompt: string; var Value: integer; const AMin, AMax : integer): Boolean; overload;
  function InputNumberQueryRange(const ACaption, APrompt: string; var Value: string; const AMin, AMax : integer): Boolean; overload;
  function InputIPv4Query(const ACaption, APrompt: string; var Value: string): Boolean;
  function InputIPv4QueryRange(const ACaption, APrompt: string; var Value: string): Boolean;
  function InputTextQuery(const ACaption, APrompt: string; var Value: string; AMaxLength : integer = 64): Boolean;
  function InputTextCustom(const ACaption, APrompt: string;
    var Value: string; AFormWidth, AControlHeight : integer; AMaxLength : integer = 64): Boolean;

  function WaitMsgFormCreate(AMsg : string; AOwner : TComponent) : TForm;
  procedure WaitMsgAlphaBlend(AMsg : string; Dest : TPaintBox; AlphaValue : byte = 230; AHeight : integer = 20;
            ABGColor : TColor = $00FFEFDF);
  procedure WaitMsgAlphaBlend2(AMsg : string; Dest : TCanvas;  ALeft, ATop : integer; AlphaValue : byte = 230; AHeight : integer = 20;
            ABGColor : TColor = $00FFEFDF);

  function DeleteDir(DirName : string): Boolean;
  function SecondsIdle: integer;
  function GetCommentValue(ATitle : string; AComment : string) : string;
  function GetWinVersionCaption : string;
  function GetWinVersionMajNumber : integer;
  function GetMemoryUseOfTotal : string;
  function GetFileVersion : string;
  function GetLastWriteTime : string;
  function GetFileVersionDescription(const FileName : string = '') : TVersionDescription;

  procedure SetControlViewStyle(ParentControl : TWinControl);
  function GetKoreanCharPos(letter: string; WishCnt: Integer): Integer;
  function CheckLengthOver(AControl : TWinControl; MaxLength : integer) : Boolean;

  function GetCurrentScreenNumber(AHandle : THandle) : integer;

  //function Seed128Encrypt(const Avalue : string): string;
  //function Seed128Decrypt(const Avalue : string): string;

  procedure IPTrace(AIP : string);

  function ShowDataLoadnig : TPanel;
  procedure ProcessWorkingSetSize;

  procedure PostRequest(const Url, Params: string);

  function minus_light(const cnum, inc_num : byte): integer;
  function minus_lightness(Value: Tcolor; inc_num: byte): Tcolor;
  function plus_light(const cnum, inc_num: byte): integer;
  function plus_lightness(Value: Tcolor; inc_num: byte): Tcolor;
  function get_complement_color(Value: Tcolor): Tcolor; //보색


implementation

uses
  uCommunityUnit, DefineTypeUnit;

procedure ProcessWorkingSetSize;
var
  MainHandle : THandle;
begin
  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, False, GetCurrentProcessID);
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
  except
  end;
end;

function GetCurrentScreenNumber(AHandle : THandle) : integer;
var
  i : integer;
  ARect : TRect;
begin
  Result := 0;
  //작업중인 화면 위치 확인
  GetWindowRect(AHandle, ARect);
  //작업중인 화면의 Top, Left 값이 각 스크린의 작업영역 안에있으면 해당 스크린 번호 리턴.
  for i := Screen.MonitorCount - 1 downto 0 do
  begin
    if (ARect.Left >= Screen.Monitors[i].Left) and (ARect.Top >= Screen.Monitors[i].Top) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function LoadFromIniFile(AFileName: string; APath: string = ''): TIniFile;
var
  Path : string;
begin
  Path := APath;
  if APath = '' then
    Path := GetExePath;
  Result := TIniFile.Create(Path + '\'+AFileName);
end;

function LoadFromConfigFile: TIniFile;
begin
  Result := TIniFile.Create(GetExePath + '\config.ini');
end;

function GetExePath : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function GetType(AData: string; AType: Integer; Plus: Boolean=True): Integer;
begin
  Result := 0 - 1;
  //case AType of
  //   1: Result := GetEnumValue(TypeInfo(TXMLCtrl),  AData);
  //end;
end;

function GetText(ANode: IXMLNode; AText: string): OleVariant;
var
  XMLNode: IXMLNode;
begin
  Result := '';
  if not Assigned(ANode) then exit;
  try
    XMLNode := ANode.ChildNodes.FindNode(AText);
    if Assigned(XMLNode) then
      Result := XMLNode.Text;
  except
  end;
end;

function GetTextDef(ANode: IXMLNode; AText: string; DefValue: OleVariant): OleVariant;
var
  XMLNode: IXMLNode;
begin
  Result := '';
  if not Assigned(ANode) then exit;
  try
    XMLNode := ANode.ChildNodes.FindNode(AText);
    if Assigned(XMLNode) then
      Result := XMLNode.Text;
  except
  end;
  if Trim(Result) = '' then
    Result := DefValue;
end;

function GetAttr(ANode: IXMLNode; AAttr: string): OleVariant;
begin
  Result := '';
  if not Assigned(ANode) then exit;
  try
    if ANode.HasAttribute(AAttr) then
      Result := ANode.Attributes[AAttr];
      //Result := Trim(string(ANode.Attributes[AAttr]));
  except
  end;
end;

function TrueFalse(AQuestion : Boolean; const TrueStr, FalseStr : string) : string;
begin
  if AQuestion then
    Result := TrueStr
  else
    Result := FalseStr;
end;

function ByteToGB(Value: String): String;
var
  iBytes: Double;
begin
  Result := Trim(Value);
  try
    if (Result <> '') and (isNumValue(Result)) then
    begin
      iBytes := StrToInt(Result);
      iBytes := (iBytes / 1024) / 1024;
      Result := FormatFloat('#,##0.0G', iBytes);
    end;
  except
    Result := '-';
  end;
end;

function MByteToGB(Value: String): String;
var
  iBytes: Double;
begin
  Result := Trim(Value);
  try
    if (Result <> '') and (isNumValue(Result)) then
    begin
      iBytes := StrToInt(Result);
      iBytes := (iBytes / 1024);
      Result := FormatFloat('#,##0.0G', iBytes);
    end;
  except
    Result := '-';
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: StringToYYYY_MM_DD_HH_NN_SS
  Author   : kyungmun
  DateTime : 2009.12.29
  Arguments: const Value : string
  Result   : string
  Comment  : 14자리 연결된 숫자를 yyyy-mm-dd hh:nn:ss 형식으로 만들기.
-------------------------------------------------------------------------------}
function StringToYYYY_MM_DD_HH_NN_SS(const Value : string) : string;
begin
  Result := '';
  if Trim(Value) = '' then Exit;

  Result := Copy(Value, 1, 4);
  Result := Result + '-' + Copy(Value, 5, 2);
  Result := Result + '-' + Copy(Value, 7, 2);
  Result := Result + ' ' + Copy(Value, 9, 2);
  Result := Result + ':' + Copy(Value, 11, 2);
  Result := Result + ':' + Copy(Value, 13, 2);
end;

function InputLimiteChar(Value : string; Alpha, Hangle : Boolean) : Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to length(Value) do
  begin
    case ByteType(Value, i) of
       mbSingleByte: {반각}
       begin
         if (not Alpha) and Hangle then  //한글만 사용할때는 반각 있으면 False
         begin
           Result := False;
           Exit;
         end;
       end;
       mbLeadByte: {전각의 1 바이트}
       begin
         if Alpha and (not Hangle) then
         begin
           Result := False;
           Exit;
         end;

         if Hangle then  //한글만 입력 받는데, 전각 1바이트가 한글 범위가 아니라면 False
           if not (Value[i] in [#176..#200]) then
           begin
             Result := False;
             Exit;
           end;
       end;
       mbTrailByte: {전각의 2 바이트}
       begin
         // 한글 전각 2바이트의 범위값 #161..#254]
       end;
    end;
  end;
end;

function ByteTokbps(Value: Double): Double;
var
  iBytes: Double;
begin
  Result := Value;
  try
    if (Result > 0) then
    begin
      iBytes := Result / 1024; // KBytes
      Result := iBytes * 8; // Kbps
    end;
  except
    Result := 0;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: InCompareText
  Author   : kyungmun
  DateTime : 2009.05.14
  Arguments: const ACompareValue, ASubValue : string; ADelimit : Char
  Result   : Boolean
  Comment  : 구분자로 구분된 문자중에 서브 문자와 같은게 있는지 체크.
-------------------------------------------------------------------------------}
function InCompareText(const ACompareValue, ASubValue : string; ADelimit : Char) : Boolean;
var
  AList : TStringList;
  i : integer;
  AStr : string;
begin
  Result := False;
  if Trim(ACompareValue) = '' then Exit;
  if Trim(ASubValue) = '' then Exit;

  AList := TStringList.Create;
  try
    AList.Delimiter := ADelimit;
    AList.StrictDelimiter := True;
    AList.DelimitedText := UpperCase(ACompareValue);
    AStr := UpperCase(ASubValue);
    for i := 0 to AList.Count - 1 do
    begin
      if AList.Strings[i] = AStr then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    AList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: LikeCompareText
  Author   : kyungmun
  DateTime : 2010.10.08
  Arguments: const ACompareValue, ASubValue : string; ADelimit : Char
  Result   : Boolean
  Comment  : 구분자로 구분된 문자 목록중에 서브문자에 포함된게 있는지 체크
-------------------------------------------------------------------------------}
function LikeCompareText(const ACompareValue, ASubValue : string; ADelimit : Char) : Boolean;
var
  AList : TStringList;
  i : integer;
  AStr : string;
begin
  Result := False;
  if Trim(ACompareValue) = '' then Exit;
  if Trim(ASubValue) = '' then Exit;

  AList := TStringList.Create;
  try
    AList.Delimiter := ADelimit;
    AList.StrictDelimiter := True;
    AList.DelimitedText := UpperCase(ACompareValue);
    AStr := UpperCase(ASubValue);
    for i := 0 to AList.Count - 1 do
    begin
      if Pos(AList.Strings[i], AStr) > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    AList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: NumberCut
  Author   : kyungmun
  DateTime : 2009.05.13
  Arguments: const Value : string
  Result   : Integer
  Comment  : 받은 문자열에서 처음부터 확인해, 문자 만날때까지 짤라서 숫자로 돌려준다.
-------------------------------------------------------------------------------}
function NumberCut(const Value : string) : Int64;
var
  NumValue : string;
  i, count : integer;
begin
  Result := 0;
  NumValue := '';
  count := Length(Value);
  for I := 1 to Count do
  begin
    if isNumValue(Value[i]) then
      NumValue := NumValue + Value[i]
    else
      Break;
  end;
  if Trim(NumValue) <> '' then Result := StrToInt64(NumValue);
end;

{-------------------------------------------------------------------------------
  Procedure: NumberCatch
  Author   : kyungmun
  DateTime : 2009.05.15
  Arguments: const Value : string
  Result   : Integer
  Comment  : 문자열 첨부터 끝까지중에서 숫자만 골라낸다.
-------------------------------------------------------------------------------}
function NumberCatchToNum(const Value : string) : Int64;
var
  NumValue : string;
  i, count : integer;
begin
  Result := 0;
  NumValue := '';
  count := Length(Value);
  for I := 1 to Count do
  begin
    if isNumValue(Value[i]) then
      NumValue := NumValue + Value[i];
  end;
  if Trim(NumValue) <> '' then Result := StrToInt64(NumValue);
end;

{-------------------------------------------------------------------------------
  Procedure: NumberCatch
  Author   : kyungmun
  DateTime : 2009.05.15
  Arguments: const Value : string
  Result   : Integer
  Comment  : 문자열 첨부터 끝까지중에서 숫자만 골라낸다.
-------------------------------------------------------------------------------}
function NumberCatchToStr(const Value : string) : string;
var
  NumValue : string;
  i, count : integer;
begin
  Result := '';
  NumValue := '';
  count := Length(Value);
  for I := 1 to Count do
  begin
    if isNumValue(Value[i]) then
      NumValue := NumValue + Value[i];
  end;
  Result := Trim(NumValue);
end;

{-------------------------------------------------------------------------------
  Procedure: GetAttrInt
  Author   : kyungmun
  DateTime : 2009.02.17
  Arguments: ANode: IXMLNode; AAttr: string
  Result   : integer
  Comment  : 숫자가 아니거나 값이 없는 경우 -999를 돌려준다.
             반드시 숫자값이어야 하는 경우에 이것으로 사용해서 처리한다.
-------------------------------------------------------------------------------}
function GetAttrInt(ANode: IXMLNode; AAttr: string): integer;
var
  value : string;
begin
  Result := -999;
  if not Assigned(ANode) then exit;
  try
    if ANode.HasAttribute(AAttr) then
    begin
      value := string(ANode.Attributes[AAttr]);
      if Trim(Value) <> '' then
        if isNumValue(value) then
          Result := Trunc(StrToCurr(Value));
    end;
  except
  end;
end;

function GetAttrDef(ANode: IXMLNode; AAttr: string; DefValue: OleVariant): OleVariant;
begin
  Result := '';
  try
    if ANode.HasAttribute(AAttr) then
      Result := ANode.Attributes[AAttr];
  except
  end;
  if Trim(Result) = '' then
    Result := DefValue;
end;

function GetNode(ANode: IXMLNode; AText: string): IXMLNode;
var
  XMLNode: IXMLNode;
begin
  Result := nil;
  if not Assigned(ANode) then exit;
  try
    XMLNode := ANode.ChildNodes.FindNode(AText);
    if Assigned(XMLNode) then
      Result := XMLNode;
  except
  end;
end;

function GetFileList(const DirPath : string; AExt : string = '*.*' ) : TStringList;
var
  I: Integer;
  SearchRec: TSearchRec;
begin
  Result := TStringList.Create;
  Result.Clear;
  try
    I := FindFirst(DirPath + '\' + AExt, 0, SearchRec);
    while I = 0 do
    begin
      Result.Add(SearchRec.Name);
      I := FindNext(SearchRec);
    end;
   except
     Result.Free;
     Result := nil;
   end;
end;

{-------------------------------------------------------------------------------
 Procedure: isNumValue
 Author   : kyungmun
 DateTime : 2008.11.03
 Arguments: AValue: string
 Result   : boolean
 Comment  : 값이 숫자인지 체크, 실수값도 숫자로 체크
-------------------------------------------------------------------------------}
function isNumValue(AValue: string) : boolean;
var
  E, E2 : integer;
  strData : string;
begin
  strData := Trim(AValue);
  if Pos(FormatSettings.DecimalSeparator , strData) > 0 then
    strData := copy(strData, 1, Pos(FormatSettings.DecimalSeparator , strData) - 1);

  Result := True;
  Val(strData, E2, E);
  if E <> 0 then Result := False;
end;
{-------------------------------------------------------------------------------
  Procedure: isOnLine
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: None
  Result   : Boolean
  Comment  : 네트웍 온라인인지 체크
-------------------------------------------------------------------------------}
function isOnLine : Boolean;
var
  dwConnectionTypes: DWORD;
begin
  Result := false;

  if InternetGetConnectedState(@dwConnectionTypes, 0) then // 정상적으로 검사됨
  begin
    if (dwConnectionTypes and INTERNET_CONNECTION_MODEM) <> 0 then
      Result := True;
    // 'Modem으로 인터넷 사용중...'

    if (dwConnectionTypes and INTERNET_CONNECTION_LAN) <> 0 then
      Result := True;
    // LAN으로 인터넷 사용중...'

    if (dwConnectionTypes and INTERNET_CONNECTION_PROXY) <> 0 then
      Result := True;
    // 'Proxy로 인터넷 사용중...'

    if (dwConnectionTypes and INTERNET_CONNECTION_MODEM_BUSY) <> 0 then
      Result := True;
    // 'Modem을 다른 용도로 사용중'
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: isURLValue
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: AValue: string
  Result   : boolean
  Comment  : 웹 주소 체크. 정규식 사용.
-------------------------------------------------------------------------------}
function isURLValue(AValue: string) : Boolean;
begin
  Result := False;
  try
    Result := TRegEx.IsMatch(AValue, '([Hh][Tt][Tt][Pp])://([_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+))((/[ _a-zA-Z\d\-\\\.]+)+)*');
    //if Result then
    //  Result := RegExpr.Match[0] = AValue; //처음 찾은 맞는형식이 입력된 값과 같지않으면 틀린형식으로 처리.
  finally
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: isEmailValue
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: AValue: string
  Result   : boolean
  Comment  : e-mail 주소 체크. 정규식 사용.
-------------------------------------------------------------------------------}
function isEmailValue(AValue: string) : Boolean;
begin
  Result := False;
  try
    Result := TRegEx.IsMatch(AValue, '[_a-zA-Z\d\-\.]+@([_a-zA-Z\d\-]+(\.[a-zA-Z]+)+)');
    //Result := RegExpr.Exec(AValue);
    //if Result then
    //  Result := RegExpr.Match[0] = AValue; //처음 찾은 맞는형식이 입력된 값과 같지않으면 틀린형식으로 처리.
  finally
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: isMacAddresValue
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: AValue: string
  Result   : boolean
  Comment  : MAC 주소 체크. 정규식 사용.
-------------------------------------------------------------------------------}
function isMacAddresValue(AValue: string) : Boolean;
begin
  Result := False;
  try
    Result := TRegEx.IsMatch(AValue, '([0-9a-fA-Z][0-9a-fA-Z][-:]){5}[0-9a-fA-Z][0-9a-fA-Z]');
    //Result := RegExpr.Exec(AValue);
    //if Result then
    //  Result := RegExpr.Match[0] = AValue; //처음 찾은 맞는형식이 입력된 값과 같지않으면 틀린형식으로 처리.
  finally
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: isIP4AddresValue
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: AValue: string
  Result   : boolean
  Comment  : IP4 주소 체크. 정규식 사용.
-------------------------------------------------------------------------------}
function isIP4AddresValue(AValue: string) : Boolean;
begin
  Result := False;
  try
    Result := TRegEx.IsMatch(AValue, '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.'+
                                     '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.'+
                                     '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.'+
                                     '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)');
    //Result := RegExpr.Exec(AValue);
    //if Result then
    //  Result := RegExpr.Match[0] = AValue; //처음 찾은 맞는형식이 입력된 값과 같지않으면 틀린형식으로 처리.
  finally
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: IP4ToInt64
  Author   : kyungmun
  DateTime : 2009.04.09
  Arguments: const Value: string
  Result   : Int64
  Comment  : IP값을 숫자값으로.
-------------------------------------------------------------------------------}
(*
function IP4ToInt64(const Value: string) : Int64;
var
  I, Idx, N: Integer;
  NewAddr: Cardinal;
  Str: String;
begin
  Str := Value;
  NewAddr := 0;
  try
    for I := 0 to 3 do
    begin
      Idx := Pos('.', Str);
      if (I = 3) and (Idx > 0) then
      begin
        Result := -1;
        Exit;
      end;

      if Idx <= 0 then
        Idx := Length(Str) + 1
      else if Idx = 1 then
      begin
        Result := -1;
        exit;
      end;

      N := StrToInt(Copy(Str, 1, Idx - 1));
      if (N < 0) or (N > 255) then
      begin
        Result := -1;
        exit;
      end;

      Delete(Str, 1, Idx);
      NewAddr := (NewAddr shl 8) or Byte(N);
    end;

    if Trim(Str) = '' then
      Result := NewAddr
    else
      Result := -1;

  except
    Result := -1;
  end;
end;
*)

function IP4ToInt64(const Value : string):Int64;
var
  str : TStringList;
  AFromValue, AToValue, AValue : string;
  ip1, ip2, ip3, ip4 : string;
  i : Integer;
begin
  Result := -1;
  if Trim(Value) = '' then Exit;

  if not isIP4AddresValue(Value) then Exit;
  str := TStringList.Create;
  try
    try
      str.Delimiter := '.';
      str.StrictDelimiter := True;
      str.DelimitedText := Value;

      ip1 := str.Strings[0];
      ip2 := str.Strings[1];
      ip3 := str.Strings[2];
      ip4 := str.Strings[3];

      Result := (strToint64(ip1)*256*256*256) +
                (strToint64(ip2)*256*256) +
                (strToint64(ip3)*256) +
                (strToint64(ip4));
    except
      Result := -1;
    end;
  finally
    str.free;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: IntToIP4
  Author   : kyungmun
  DateTime : 2009.04.15
  Arguments: const Value: DWORD
  Result   : string
  Comment  : 숫자값을 IP4 형태로 변환.
-------------------------------------------------------------------------------}
function IntToIP4(const Value: DWORD) : string;
type
  TIPv4 = record
     a,b,c,d: byte;
   end;
begin
 with TIPv4(Value) do
   Result := Format('%d.%d.%d.%d', [d, c, b, a]);
end;


{-------------------------------------------------------------------------------
  Procedure: isMobileValue
  Author   : kyungmun
  DateTime : 2008.11.12
  Arguments: AValue: string
  Result   : boolean
  Comment  : 핸드폰 번호 체크. 정규식 사용.
-------------------------------------------------------------------------------}
function isMobileValue(AValue: string) : Boolean;
begin
  Result := False;
  try
    Result := TRegEx.IsMatch(AValue, '(01[0,1,6,7,8,9])-(\d{3,4})-(\d{4})');
    //Result := RegExpr.Exec(AValue);
    //if Result then
    //  Result := RegExpr.Match[0] = AValue; //처음 찾은 맞는형식이 입력된 값과 같지않으면 틀린형식으로 처리.
  finally
  end;
end;

procedure WaitForTimer(MilliSeconds :  LongInt);
const
 _SECOND = -10000;
var
  lBusy : LongInt;
  hTimer : HWND;
  liDueTime : LARGE_INTEGER;
begin
  hTimer := CreateWaitableTimer(nil, True, 'WaitableTimer');

  if hTimer = 0 then Exit;

  liDueTime.QuadPart := _SECOND * MilliSeconds;
  SetWaitableTimer(hTimer, TLargeInteger(liDueTime), 0, nil, nil, False);

  repeat
    lBusy := MsgWaitForMultipleObjects(1, hTimer, False, INFINITE, QS_ALLINPUT);
    Application.ProcessMessages;
  until lBusy = WAIT_OBJECT_0;

  CloseHandle(hTimer);
end;

{-------------------------------------------------------------------------------
  Procedure: XMLColorToRGB
  Author   : kyungmun
  DateTime : 2009.03.13
  Arguments: sColor: string
  Result   : COLORREF
  Comment  : 0080FF 6자리값을 -> RBG 값으로
-------------------------------------------------------------------------------}
function XMLColorToRGB(sColor: string): COLORREF;
var
  cc: integer;
  r, g, b: Byte;
begin
  if sColor[1] <> '$' then
    sColor := '$'+sColor;

  cc := StrToInt(sColor);
  r := (cc and $FF0000) shr 16;
  g := (cc and $00FF00) shr 8;
  b := (cc and $0000FF);
  result := RGB(r, g, b);
end;

{-------------------------------------------------------------------------------
  Procedure: RGBColorToXML
  Author   : kyungmun
  DateTime : 2009.03.13
  Arguments: iColor: integer
  Result   : string
  Comment  : RGB 값을 -> 0080FF 6자리로.
-------------------------------------------------------------------------------}
function RGBColorToXML(iColor: integer): string;
begin
  result := format('%.2x%.2x%.2x', [(iColor and $0000FF)
                                 , (iColor and $00FF00) shr 8
                                 , (iColor and $FF0000) shr 16]);
end;


function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

{-------------------------------------------------------------------------------
  Procedure: InputNumberQuery
  Author   : kyungmun
  DateTime : 2009.04.09
  Arguments: const ACaption, APrompt: string; var Value: string
  Result   : Boolean
  Comment  : 숫자만 입력받는 inputquery
-------------------------------------------------------------------------------}
function InputNumberQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
const
  FormDefaultWidth = 180;

var
  Form: TForm;
  Panel: TPanel;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight, ButtonLeft1: Integer;
  PromptWidth : integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := 10;
        Top  := 10;
        WordWrap := True;
      end;
      PromptWidth := Canvas.TextWidth(Prompt.Caption);
      if PromptWidth < 180 then PromptWidth := FormDefaultWidth;
      Form.ClientWidth := PromptWidth + 50;
      Prompt.Constraints.MaxWidth := PromptWidth + 20;

      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := Form.ClientWidth - 20;
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);

      Panel := TPanel.Create(Form);
      With Panel do
      begin
        Parent := Form;
        Align  := alBottom;
        Height := 40;
        BevelEdges := [beTop];
        BevelKind := bkTile;
        BevelOuter := bvNone;
        TabStop := False;
      end;
      ButtonTop := 5;
      ButtonWidth := 70;
      ButtonHeight := 25;
      ButtonLeft1 := ((Form.ClientWidth - (ButtonWidth * 2)) div 2) - 4;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '확인';
        ModalResult := mrOk;
        Default := True;
        SetBounds(ButtonLeft1, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '취소';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(ButtonLeft1 + ButtonWidth + 7, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      Form.ClientHeight := 100;
      if ShowModal = mrOk then
      begin
        Value := Trim(Edit.Text);
        Result := Value <> '';
      end;
    finally
      Form.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: InputNumberQuery
  Author   : kyungmun
  DateTime : 2009.04.09
  Arguments: const ACaption, APrompt: string; var Value: string
  Result   : Boolean
  Comment  : 지정된 범위의(Min ~ Max) 숫자만 입력받는 inputquery
-------------------------------------------------------------------------------}
function InputNumberQuery(const ACaption, APrompt: string;
  var Value: integer; const AMin, AMax : integer): Boolean;
const
  FormDefaultWidth = 180;

var
  PromptWidth : integer;
  Form: TForm;
  Panel: TPanel;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight, ButtonLeft1: Integer;
  OkClick : Boolean;
begin
  OKClick := False;
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt + ' (' + intTostr(AMin) + '~' + intTostr(AMax) + ')';
        Left := 10;
        Top  := 10;
        WordWrap := True;
      end;
      PromptWidth := Canvas.TextWidth(Prompt.Caption);
      if PromptWidth < 180 then PromptWidth := FormDefaultWidth;
      Form.ClientWidth := PromptWidth + 50;
      Prompt.Constraints.MaxWidth := PromptWidth + 20;

      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := Form.ClientWidth - 20;
        MaxLength := 255;
        Text := intTostr(Value);
        SelectAll;
      end;
      SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);

      Panel := TPanel.Create(Form);
      With Panel do
      begin
        Parent := Form;
        Align  := alBottom;
        Height := 40;
        BevelEdges := [beTop];
        BevelKind := bkTile;
        BevelOuter := bvNone;
        TabStop := False;
      end;
      ButtonTop := 5;
      ButtonWidth := 70;
      ButtonHeight := 25;
      ButtonLeft1 := ((Form.ClientWidth - (ButtonWidth * 2)) div 2) - 4;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '확인';
        ModalResult := mrOk;
        Default := True;
        SetBounds(ButtonLeft1, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '취소';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(ButtonLeft1 + ButtonWidth + 7, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      Form.ClientHeight := 100;

      if ShowModal = mrOk then
      begin
        Value := strtoint(Trim(Edit.Text));
        if (Value < AMin) or (Value > AMax) then
        begin
          Application.MessageBox(PChar('(' + intTostr(AMin) + '~' + intTostr(AMax) + ') 범위 값에 맞게 입력하세요.'), '확인', MB_OK + MB_ICONWARNING);
          Result := False;
        end
        else
        begin
          Result := True;
        end;
        OkClick := True;
      end;
    finally
      Form.Free;
      if OKClick and not Result then
        Result := InputNumberQuery(ACaption, APrompt, Value, AMin, AMax);
    end;
  end;
end;

function InputNumberQueryRange(const ACaption, APrompt: string;
  var Value: string; const AMin, AMax : integer): Boolean;
const
  FormDefaultWidth = 180;

var
  PromptWidth : integer;
  Form: TForm;
  Panel: TPanel;
  Prompt, Prompt1, Prompt2: TLabel;
  Edit, Edit2: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight, ButtonLeft1: Integer;
  AFromValue, AToValue : Integer;
  OkClick : Boolean;
begin
  OKClick := False;
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt + ' (' + intTostr(AMin) + '~' + intTostr(AMax) + ')';
        Left := 10;
        Top  := 10;
        WordWrap := True;
      end;
      PromptWidth := Canvas.TextWidth(Prompt.Caption);
      if PromptWidth < 180 then PromptWidth := FormDefaultWidth;
      Form.ClientWidth := PromptWidth + 50;
      Prompt.Constraints.MaxWidth := PromptWidth + 20;

      Prompt1 := TLabel.Create(Form);
      with Prompt1 do
      begin
        Parent := Form;
        Caption := '시작 값 : ';
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 7;
        Constraints.MaxWidth := MulDiv(100, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Prompt2 := TLabel.Create(Form);
      with Prompt2 do
      begin
        Parent := Form;
        Caption := '종료 값 : ';
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 32;
        Constraints.MaxWidth := MulDiv(100, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left + 50;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := Form.ClientWidth - 75;
        MaxLength := 255;
        Text := IntToStr(AMin);
        SelectAll;
      end;

      Edit2 := TEdit.Create(Form);
      with Edit2 do
      begin
        Parent := Form;
        Left := Prompt.Left + 50;
        Top := Prompt.Top + Prompt.Height + 30;
        Width := Form.ClientWidth - 75;
        MaxLength := 255;
        Text := IntToStr(AMax);
        SelectAll;
      end;

      SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);
      SetWindowLong(Edit2.Handle, GWL_STYLE, GetWindowLong(Edit2.Handle, GWL_STYLE)or ES_NUMBER or ES_LEFT);

      Panel := TPanel.Create(Form);
      With Panel do
      begin
        Parent := Form;
        Align  := alBottom;
        Height := 40;
        BevelEdges := [beTop];
        BevelKind := bkTile;
        BevelOuter := bvNone;
        TabStop := False;
      end;
      ButtonTop := 5;
      ButtonWidth := 70;
      ButtonHeight := 25;
      ButtonLeft1 := ((Form.ClientWidth - (ButtonWidth * 2)) div 2) - 4;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '확인';
        ModalResult := mrOk;
        Default := True;
        SetBounds(ButtonLeft1, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := '취소';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(ButtonLeft1 + ButtonWidth + 7, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      Form.ClientHeight := 130;

      if ShowModal = mrOk then
      begin
        AFromValue := StrToInt64(Trim(Edit.Text));
        AToValue := StrToInt64(Trim(Edit2.Text));
        if (AFromValue < AMin) or (AToValue > AMax) then
        begin
          Application.MessageBox(PChar('(' + intTostr(AMin) + '~' + intTostr(AMax) + ') 범위 값에 맞게 입력하세요.'), '확인', MB_OK + MB_ICONWARNING);
          Result := False;
        end
        else
        if (AToValue < AFromValue) or (AToValue = AFromValue) then
        begin
          Application.MessageBox(PChar('시작/종료 값은 같지 않아야 하며, 종료 값이 더 커야 합니다.'), '확인', MB_OK + MB_ICONWARNING);
          Result := False;
        end
        else
        begin
          Value := IntTostr(AFromValue) + '~' + IntToStr(AToValue);
          Result := True;
        end;

        OkClick := True;
      end;
    finally
      Form.Free;
      if OKClick and (not Result) then
      begin
        Value := '0';
        Result := InputNumberQueryRange(ACaption, APrompt, Value, AMin, AMax);
      end;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: InputIPv4Query
  Author   : kyungmun
  DateTime : 2009.06.01
  Arguments: const ACaption, APrompt: string; var Value: string
  Result   : Boolean
  Comment  : 1개의 IPv4 형식 값을 입력받는 화면.
-------------------------------------------------------------------------------}
function InputIPv4Query(const ACaption, APrompt: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Panel: TPanel;
  Bevel: TBevel;
  Prompt: TLabel;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  HandleIP : THandle;
  FIPInt : Int64;
  FDefaultFont : TFont;
  EditLeft, EditTop, EditWidth, EditHight : integer;

  procedure CreateIPEditControl;
  var
    lpInitCtrls: TInitCommonControlsEx;
  begin
    lpInitCtrls.dwSize := SizeOf(TInitCommonControlsEx);
    lpInitCtrls.dwICC  := ICC_INTERNET_CLASSES;
    if InitCommonControlsEx(lpInitCtrls) then
    begin
      FDefaultFont := TFont(GetStockObject(DEFAULT_GUI_FONT));
      HandleIP := CreateWindow(WC_IPADDRESS,
              PChar('IP_VALUE'),
              WS_CHILD or WS_VISIBLE or WS_TABSTOP,
              EditLeft, EditTop, EditWidth, EditHight,
              Form.Handle,
              0,
              0,
              Form);
      SendMessage(HandleIP, WM_SETFONT, Integer(FDefaultFont), 1);
      SetWindowPos(HandleIP, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    end;
  end;

begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      EditLeft  := Prompt.Left;
      EditTop   := Prompt.Top + Prompt.Height + 5;
      EditWidth := MulDiv(164, DialogUnits.X, 4);
      EditHight := MulDiv(12, DialogUnits.Y, 8);

      CreateIPEditControl;
      FIPInt := 0;
      if Value <> '' then
        FIPInt := IP4ToInt64(Value);
      SendMessage(HandleIP, IPM_SETADDRESS, 0, FIPInt);
      SendMessage(HandleIP, IPM_SETFOCUS, 0, 0);

      Panel := TPanel.Create(Form);
      With Panel do
      begin
        Parent := Form;
        Align  := alBottom;
        Height := 40;
        BevelOuter := bvNone;
        TabStop := False;
      end;
      Bevel := TBevel.Create(Panel);
      with Bevel do
      begin
        Parent := Panel;
        Align := alTop;
        Shape := bsBottomLine;
        Height := 5;
      end;

      ButtonTop := 10;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      Form.ClientHeight := 100;
      if ShowModal = mrOk then
      begin
        SendMessage(HandleIP, IPM_GETADDRESS, 0, longint(@FIPInt));
        Value := Format('%d.%d.%d.%d',
          [FIRST_IPADDRESS(FIPInt),SECOND_IPADDRESS(FIPInt),
          THIRD_IPADDRESS(FIPInt),FOURTH_IPADDRESS(FIPInt)]);
        Result := True;
      end;
    finally
      Form.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: InputIPv4QueryRange
  Author   : kyungmun
  DateTime : 2009.06.01
  Arguments: const ACaption, APrompt: string; var Value: string
  Result   : Boolean
  Comment  : 1개의 IPv4 형식 값을 범위로 입력받는 화면.
-------------------------------------------------------------------------------}
function InputIPv4QueryRange(const ACaption, APrompt: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Panel: TPanel;
  Bevel: TBevel;
  Prompt, Prompt1, Prompt2: TLabel;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  HandleIP, HandleIP2 : THandle;
  FIPInt, FIPInt2 : Int64;
  FDefaultFont : TFont;
  EditLeft, EditTop, EditWidth, EditHight : integer;
  Edit2Left, Edit2Top, Edit2Width, Edit2Hight : integer;
  AFromValue, AToValue : string;

  procedure CreateIPEditControl;
  var
    lpInitCtrls: TInitCommonControlsEx;
  begin
    lpInitCtrls.dwSize := SizeOf(TInitCommonControlsEx);
    lpInitCtrls.dwICC  := ICC_INTERNET_CLASSES;
    if InitCommonControlsEx(lpInitCtrls) then
    begin
      FDefaultFont := TFont(GetStockObject(DEFAULT_GUI_FONT));
      HandleIP := CreateWindow(WC_IPADDRESS,
              PChar('IP_VALUE'),
              WS_CHILD or WS_VISIBLE or WS_TABSTOP,
              EditLeft, EditTop, EditWidth, EditHight,
              Form.Handle,
              0,
              0,
              nil);

      HandleIP2 := CreateWindow(WC_IPADDRESS,
              PChar('IP_VALUE2'),
              WS_CHILD or WS_VISIBLE or WS_TABSTOP,
              Edit2Left, Edit2Top, Edit2Width, Edit2Hight,
              Form.Handle,
              0,
              0,
              nil);

      SendMessage(HandleIP, WM_SETFONT, Integer(FDefaultFont), 1);
      SendMessage(HandleIP2, WM_SETFONT, Integer(FDefaultFont), 1);

      SetWindowPos(HandleIP, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      SetWindowPos(HandleIP2, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    end;
  end;

begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(225, DialogUnits.X, 4);
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Prompt1 := TLabel.Create(Form);
      with Prompt1 do
      begin
        Parent := Form;
        Caption := '시작 값 : ';
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 7;
        Constraints.MaxWidth := MulDiv(100, DialogUnits.X, 4);
        WordWrap := True;
      end;

      Prompt2 := TLabel.Create(Form);
      with Prompt2 do
      begin
        Parent := Form;
        Caption := '종료 값 : ';
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 32;
        Constraints.MaxWidth := MulDiv(100, DialogUnits.X, 4);
        WordWrap := True;
      end;

      EditLeft  := Prompt.Left + 50;
      EditTop   := Prompt.Top + Prompt.Height + 5;
      EditWidth := MulDiv(164, DialogUnits.X, 4);
      EditHight := MulDiv(12, DialogUnits.Y, 8);

      Edit2Left  := Prompt.Left + 50;
      Edit2Top   := Prompt.Top + Prompt.Height + 30;
      Edit2Width := MulDiv(164, DialogUnits.X, 4);
      Edit2Hight := MulDiv(12, DialogUnits.Y, 8);

      if (Value <> '') and (Pos('~', Value) > 0) then
      begin
        AFromValue := Copy(Value, 1, Pos('~', Value)-1);
        AToValue := Copy(Value, Pos('~', Value)+1, Length(Value));
      end;

      CreateIPEditControl;
      FIPInt := 0;
      if AFromValue <> '' then
        FIPInt := IP4ToInt64(AFromValue);
      SendMessage(HandleIP, IPM_SETADDRESS, 0, FIPInt);
      SendMessage(HandleIP, IPM_SETFOCUS, 0, 0);

      FIPInt2 := 0;
      if AToValue <> '' then
        FIPInt2 := IP4ToInt64(AToValue);
      SendMessage(HandleIP2, IPM_SETADDRESS, 0, FIPInt2);

      Panel := TPanel.Create(Form);
      With Panel do
      begin
        Parent := Form;
        Align  := alBottom;
        Height := 40;
        BevelOuter := bvNone;
        TabStop := False;
      end;
      Bevel := TBevel.Create(Panel);
      with Bevel do
      begin
        Parent := Panel;
        Align := alTop;
        Shape := bsBottomLine;
        Height := 5;
      end;

      ButtonTop := 10;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(61, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Panel) do
      begin
        TabStop := False;
        Parent := Panel;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(114, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      Form.ClientHeight := 130;
      if ShowModal = mrOk then
      begin
        SendMessage(HandleIP, IPM_GETADDRESS, 0, longint(@FIPInt));
        SendMessage(HandleIP2, IPM_GETADDRESS, 0, longint(@FIPInt2));
        Value := Format('%d.%d.%d.%d',
          [FIRST_IPADDRESS(FIPInt),SECOND_IPADDRESS(FIPInt),
          THIRD_IPADDRESS(FIPInt),FOURTH_IPADDRESS(FIPInt)]) + '~' +
          Format('%d.%d.%d.%d',
          [FIRST_IPADDRESS(FIPInt2),SECOND_IPADDRESS(FIPInt2),
          THIRD_IPADDRESS(FIPInt2),FOURTH_IPADDRESS(FIPInt2)]);
        Result := True;
      end;
    finally
      Form.Free;
    end;
  end;
end;

procedure WaitMsgAlphaBlend(AMsg : string; Dest : TPaintBox;
  AlphaValue : Byte = 230; AHeight : integer = 20; ABGColor : TColor = $00FFEFDF);
var
  AMsgWidth, AMsgHeight, TxtTopMargin : integer;
  Bitmap : TBitmap;
  bf : _BLENDFUNCTION;
begin
  Bitmap := TBitmap.Create;
  try
    AMsgWidth := Bitmap.Canvas.TextWidth(AMsg);
    AMsgHeight := Bitmap.Canvas.TextHeight(AMsg);
    TxtTopMargin := ((AHeight - AMsgHeight) div 2) + 1;
    Bitmap.Width := AMsgWidth + 50;
    Bitmap.Height := AHeight;

    Dest.Width := Bitmap.Width;
    Dest.Height := Bitmap.Height;

    //일단 배경을 투명하게.
    Bitmap.TransparentColor := clWhite;
    Bitmap.Transparent := True;

    Bitmap.Canvas.Pen.Width := 1;
    Bitmap.Canvas.Pen.Color := clBtnFace;
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    //테두리 그리고.
    Bitmap.Canvas.Pen.Color := clBtnFace;
    Bitmap.Canvas.Brush.Color := ABGColor;
    Bitmap.Canvas.RoundRect(0, 0, Bitmap.Width, Bitmap.Height, 7, 7);

    //뒷배경색칠하기.
    Bitmap.Canvas.FloodFill(10, 10, clBtnFace, fsSurface); // 10, 10 위치부터 clBtnFace 색을 만날때까지 brush color로 채운다. (영역 안쪽을 채울때..사용)

    //메세지 출력.
    Bitmap.Canvas.Font.Color := clBlack;
    //Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.TextOut(25, TxtTopMargin, AMsg);

    //반투명 속성
    bf.BlendOp := AC_SRC_OVER;              //출력옵션지정하되 현재 AC_SRC_OVER 만 가능
    bf.BlendFlags := 0;                     //미사용 맴버
    bf.SourceConstantAlpha := AlphaValue;   //투명도 (0: 투명; 255:불투명)
    bf.AlphaFormat := 0;                    //비트맵의 비트를 해석하는 방법
    Alphablend(Dest.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, bf);
                // dest,           rect(                  ),          source,            rect(                                ), 옵션
  finally
    Bitmap.Free;
  end;
end;

procedure WaitMsgAlphaBlend2(AMsg : string; Dest : TCanvas; ALeft, ATop : integer;
  AlphaValue : Byte = 230; AHeight : integer = 20; ABGColor : TColor = $00FFEFDF);
var
  AMsgWidth, AMsgHeight, TxtTopMargin : integer;
  Bitmap : TBitmap;
  bf : _BLENDFUNCTION;
begin
  Bitmap := TBitmap.Create;
  try
    AMsgWidth := Bitmap.Canvas.TextWidth(AMsg);
    AMsgHeight := Bitmap.Canvas.TextHeight(AMsg);
    TxtTopMargin := ((AHeight - AMsgHeight) div 2) + 1;
    Bitmap.Width := AMsgWidth + 50;
    Bitmap.Height := AHeight;

    //Dest.Width := Bitmap.Width;
    //Dest.Height := Bitmap.Height;

    //일단 배경을 투명하게.
    Bitmap.TransparentColor := clWhite;
    Bitmap.Transparent := True;

    Bitmap.Canvas.Pen.Width := 1;
    Bitmap.Canvas.Pen.Color := clBtnFace;
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    //테두리 그리고.
    Bitmap.Canvas.Pen.Color := clBtnFace;
    Bitmap.Canvas.Brush.Color := ABGColor;
    Bitmap.Canvas.RoundRect(0, 0, Bitmap.Width, Bitmap.Height, 7, 7);

    //뒷배경색칠하기.
    Bitmap.Canvas.FloodFill(10, 10, clBtnFace, fsSurface); // 10, 10 위치부터 clBtnFace 색을 만날때까지 brush color로 채운다. (영역 안쪽을 채울때..사용)

    //메세지 출력.
    Bitmap.Canvas.Font.Color := clBlack;
    //Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.TextOut(25, TxtTopMargin, AMsg);

    //반투명 속성
    bf.BlendOp := AC_SRC_OVER;              //출력옵션지정하되 현재 AC_SRC_OVER 만 가능
    bf.BlendFlags := 0;                     //미사용 맴버
    bf.SourceConstantAlpha := AlphaValue;   //투명도 (0: 투명; 255:불투명)
    bf.AlphaFormat := 0;                    //비트맵의 비트를 해석하는 방법
    Alphablend(Dest.Handle, ALeft, ATop, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, bf);
                // dest,           rect(                  ),          source,            rect(                                ), 옵션
  finally
    Bitmap.Free;
  end;
end;

function InputTextQuery(const ACaption, APrompt: string;
  var Value: string; AMaxLength : integer = 64): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := AMaxLength;
        Hint := Format('최대 %d byte 까지 입력할 수 있습니다.', [AMaxLength]);
        ShowHint := True;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '확인';
        Default := True;
        ModalResult := mrOk;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '취소';
        ModalResult := mrCancel;
        Cancel := True;
        //Default := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Trim(Edit.Text);
        Result := Value <> '';
      end;
    finally
      Form.Free;
    end;
  end;
end;


function InputTextCustom(const ACaption, APrompt: string;
  var Value: string; AFormWidth, AControlHeight : integer; AMaxLength : integer = 64): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
  begin
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      Constraints.MinWidth := MulDiv(180, DialogUnits.X, 4);
      Width := AFormWidth;

      Position := poOwnerFormCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TMemo.Create(Form);
      with Edit do
      begin
        Parent := Form;
        ScrollBars := ssVertical;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := Form.Width - ((Left * 2)+5);
        Height := AControlHeight;
        MaxLength := AMaxLength;
        Hint := Format('최대 %d byte 까지 입력할 수 있습니다.', [AMaxLength]);
        ShowHint := True;
        Lines.Text := Value;
        SelectAll;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '확인';
        ModalResult := mrOk;
        SetBounds(Max((AFormWidth div 2)- ButtonWidth - 6, 47), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '취소';
        ModalResult := mrCancel;
        Cancel := True;
        Default := True;
        SetBounds(Max((AFormWidth div 2) + 1, 115), Edit.Top + Edit.Height + 15, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Trim(Edit.Lines.Text);
        Result := Value <> '';
      end;
    finally
      Form.Free;
    end;
  end;
end;

function GetWinVersion(var ACaption: string; var AMajNumber: integer) : boolean;
var
  osVerInfo: TOSVersionInfo;
  majorVersion, minorVersion, BuildNumber : Integer;
begin
  Result := False;
  ACaption := 'Unknown';
  AMajNumber := 0;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
  if GetVersionEx(osVerInfo) then
  begin
    majorVersion := osVerInfo.dwMajorVersion;
    minorVersion := osVerInfo.dwMinorVersion;
    BuildNumber := osVerInfo.dwBuildNumber;
    //PlatformId := osVerInfo.dwPlatformId;

    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
      begin
        if (majorVersion = 4) and (minorVersion = 0) then
        begin
          ACaption := 'Microsoft Windows NT 4';
          AMajNumber := 4;
        end
        else if (majorVersion = 5) and (minorVersion = 0) then
        begin
          ACaption := 'Microsoft Windows 2000';
          AMajNumber := 5;
        end
        else if (majorVersion = 5) and (minorVersion = 1) then
        begin
          ACaption := 'Microsoft Windows XP';
          AMajNumber := 5;
        end
        else if (majorVersion = 5) and (minorVersion = 2) then
        begin
          ACaption := 'Microsoft Windows Server 2003';
          AMajNumber := 5;
        end
        else if (majorVersion = 6) and (minorVersion = 0) and (BuildNumber = 6000) then
        begin
          ACaption := 'Microsoft Windows Vista';
          AMajNumber := 6;
        end
        else if (majorVersion = 6) and (minorVersion = 0) and (BuildNumber = 6001) then
        begin
          ACaption := 'Microsoft Windows Server 2008';
          AMajNumber := 6;
        end
        else if (majorVersion = 6) and (minorVersion = 1) and (BuildNumber >= 7000) then
        begin
          ACaption := 'Microsoft Windows 7';
          AMajNumber := 6;
        end;
      end;
    end;
    ACaption := ACaption + #13 +
                Format('버젼 %d.%d (빌드 %d : %s)', [majorVersion, minorVersion, BuildNumber, osVerInfo.szCSDVersion]);
    Result := True;
  end;
end;

function GetWinVersionCaption : string;
var
  VerNum : integer;
begin
  GetWinVersion(Result, VerNum);
end;

function GetWinVersionMajNumber : integer;
var
  VerCaption : string;
begin
  GetWinVersion(VerCaption, Result);
end;

function GetGlobalMemoryRecord: TMemoryStatusEx;
type
  TGlobalMemoryStatusEx = procedure(var lpBuffer: TMemoryStatusEx); stdcall;
var
  ms : TMemoryStatus;
  h : THandle;
  gms : TGlobalMemoryStatusEx;
begin
  Result.dwLength := SizeOf(Result);
  if GetWinVersionMajNumber <= 4 then  //Win2000 이하..
  begin
    ms.dwLength := SizeOf(ms);
    GlobalMemoryStatus(ms);
    Result.dwMemoryLoad := ms.dwMemoryLoad;
    Result.ullTotalPhys := ms.dwTotalPhys;
    Result.ullAvailPhys := ms.dwAvailPhys;
    Result.ullTotalPageFile := ms.dwTotalPageFile;
    Result.ullAvailPageFile := ms.dwAvailPageFile;
    Result.ullTotalVirtual := ms.dwTotalVirtual;
    Result.ullAvailVirtual := ms.dwAvailVirtual;
  end
  else  //MajNumber 5이상 (Win2000 이상)
  begin
    h := LoadLibrary(kernel32);
    try
      if h <> 0 then
      begin
        @gms := GetProcAddress(h, 'GlobalMemoryStatusEx');
        if @gms <> nil then
          gms(Result);
      end;
    finally
      FreeLibrary(h);
    end;
  end;
end;

function GetMemoryUseOfTotal : string;
var
  MemoryStatusEx : TMemoryStatusEx;
begin
  Result := 'UnKnown';
  MemoryStatusEx := GetGlobalMemoryRecord;
  Result := FormatCurr('#,##0 MB', MemoryStatusEx.ullAvailPhys / ONE_MEGA_BYTE) + ' / ' +
            FormatCurr('#,##0 MB', MemoryStatusEx.ullTotalPhys / ONE_MEGA_BYTE);
end;

function GetFileVersion : string;
var
  Info:       PVSFixedFileInfo;
  InfoSize:   Cardinal;
  nHwnd:      DWORD;
  BufferSize: DWORD;
  Buffer: Pointer;
  Major, Minor,  Release, Build : word;
begin
  BufferSize := GetFileVersionInfoSize(PChar(ParamStr(0)), nHWnd);
  Result := 'Unknown';
  if BufferSize > 0 then
  begin
    GetMem( Buffer, BufferSize );
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), nHWnd, BufferSize, Buffer) then
      begin
        if VerQueryValue(Buffer, '\', Pointer(Info), InfoSize) then
        begin
          if Assigned(@Major) then Major := HiWord(Info^.dwFileVersionMS);
          if Assigned(@Minor) then Minor := LoWord(Info^.dwFileVersionMS);
          if Assigned(@Release) then Release := HiWord(Info^.dwFileVersionLS);
          if Assigned(@Build) then Build := LoWord(Info^.dwFileVersionLS);
          Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
        end;
      end;
    finally
      FreeMem(Buffer, BufferSize);
    end;
  end;
end;

function GetFileVersionDescription(const FileName : string) : TVersionDescription;
  function SwapLong(L : longint): longint;
  assembler;
  asm
    rol eax, 16;
  end;

var
  BuffSize, l, len : Dword;
  Buffer, P : Pointer;
  Language : string;
  AFileName : string;
begin
  Result.CompanyName      := '';
  Result.FileDescription  := '';
  Result.FileVersion      := '';
  Result.InternalName     := '';
  Result.LegalCopyright   := '';
  Result.LegalTrademarks  := '';
  Result.OriginalFileName := '';
  Result.ProductName      := '';
  Result.ProductVersion   := '';
  Result.Comments         := '';
  Result.SpecialBuild     := '';

  AFileName := FileName;
  if trim(AFileName) = '' then
    AFileName := ParamStr(0);

  BuffSize := GetFileVersionInfoSize(pchar(AFileName), l);
  if BuffSize> 0 then
  begin
    Getmem(Buffer, BuffSize);
    try
      if GetFileVersionInfo(pchar(AFileName), 0, BuffSize, Buffer) then
      begin
        if (BuffSize > 0) and (VerQueryValue(Buffer, '\VarFileInfo\Translation', P, Len)) then
          Language := format('%.8x', [SwapLong(Longint(P^))]);

        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\CompanyName'), P, Len)) then
          Result.CompanyName:= pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, PChar('\StringFileInfo\' + Language + '\FileDescription'), P, Len)) then
          Result.FileDescription := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, PChar('\StringFileInfo\' + Language + '\FileVersion'), P, Len)) then
          Result.FileVersion := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\InternalName'), P, Len)) then
          Result.InternalName := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\LegalCopyright'), P, Len)) then
          Result.LegalCopyright := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\LegalTrademarks'), P, Len)) then
          Result.LegalTrademarks := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\OriginalFileName'), P, Len)) then
          Result.OriginalFileName := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\ProductName'), P, Len)) then
          Result.ProductName := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\ProductVersion'), P, Len)) then
          Result.ProductVersion := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\Comments'), P, Len)) then
          Result.Comments := pchar(p);
        if (BuffSize > 0) and (VerQueryValue(Buffer, Pchar('\StringFileInfo\' + Language + '\SpecialBuild'), P, Len)) then
          Result.SpecialBuild := pchar(p);
      end;
    finally
      FreeMem(Buffer, BuffSize);
    end;
  end;
end;

function GetLastWriteTime : string;
var
  ftLocal : FILETIME;
  stCreate : SYSTEMTIME;
  fileAttData : _WIN32_FILE_ATTRIBUTE_DATA;
  ATime : Extended;
begin
  Result := 'Unknown';
  try
    GetFileAttributesEx(PChar(ParamStr(0)), GetFileExInfoStandard, @fileAttData);
    if not (FileTimeToLocalFileTime(fileAttData.ftLastWriteTime, ftLocal)) then exit;
    FileTimeToSystemTime(ftLocal, stCreate);
    ATime := EncodeDate(stCreate.wYear, stCreate.wMonth, stCreate.wDay);
    ATime := ATime + EncodeTime(stCreate.wHour, stCreate.wMinute, stCreate.wSecond, stCreate.wMilliseconds);
    Result := FormatDateTime('yyyy-mm-dd hh:mm:ss.zzz', ATime);
  except
  end;
end;

procedure SetControlViewStyle(ParentControl : TWinControl);
var
  i : integer;
begin
  for I := 0 to ParentControl.ControlCount - 1 do
  begin
    if ParentControl.Controls[i] is TEdit then
    begin
      TEdit(ParentControl.Controls[i]).Color := clBtnFace;
      TEdit(ParentControl.Controls[i]).BorderStyle := bsNone;
      TEdit(ParentControl.Controls[i]).BevelInner := bvNone;
      TEdit(ParentControl.Controls[i]).BevelKind := bkTile;
    end
    else
    if ParentControl.Controls[i] is TMemo then
    begin
      TMemo(ParentControl.Controls[i]).Color := clBtnFace;
      TMemo(ParentControl.Controls[i]).BorderStyle := bsNone;
      TMemo(ParentControl.Controls[i]).BevelInner := bvNone;
      TMemo(ParentControl.Controls[i]).BevelKind := bkTile;
    end
    else
    if ParentControl.Controls[i] is TComboBox then
    begin
      TComboBox(ParentControl.Controls[i]).Color := clBtnFace;
    end;
  end;
  //ParentControl.Enabled := False;
end;

function GetKoreanCharPos(letter: string; WishCnt: Integer): Integer;
var
  i, msb: integer;
begin
  if System.Length(letter) < WishCnt then
  begin
    Result := -1;
    System.Exit;
  end;

  msb := 0;
  for i := 1 to WishCnt do
    if (Integer(letter[i]) and Integer($80)) = Integer($80) then
      Inc(msb);

  if (msb mod 2) = 0 then {'$80'인것이 짝수개이면 완성된 한글문장}
    Result := WishCnt
  else
    Result := WishCnt - 1;
end;

function CheckLengthOver(AControl : TWinControl; MaxLength : integer) : Boolean;
begin
  Result := False;
  if TControl(AControl) is TCustomEdit then begin
    if Length(TCustomEdit(TControl(AControl)).Text) > MaxLength then
    begin
      Application.MessageBox(PChar('최대 입력 Byte수를 초과하였습니다.'), '확인', 0);

      TCustomEdit(TControl(AControl)).SelStart  := MaxLength;
      TCustomEdit(TControl(AControl)).SelLength := length(TCustomEdit(TControl(AControl)).Text);
      PostMessage(TCustomEdit(TControl(AControl)).Handle, WM_KEYDOWN, VK_BACK, 0);

      Result := True;
    end;
  end;
end;

(*
function Seed128Encrypt(const Avalue : string) : string;
var
  ctx: TSEAContext;
  InData, OutData: TSEABlock;
  Value, temp : string;
  i : integer;
begin
  Result := '';
  if Trim(Avalue) = '' then Exit;
  if SEA_Init(SeedKeyValue, 8*sizeof(SeedKeyValue), ctx) <> 0 then Exit;

  FillChar(InData, 16, #32);
  FillChar(OutData, 16, #32);
  temp := Avalue;
  for I := 0 to length(temp) - 1 do
    InData[i] := Byte(temp[i+1]);

  SEA_Encrypt(Ctx, InData, OutData);
  Value := '';
  for i := 0 to 15 do
    Value := Value + string(char(OutData[i]));

  Result := trim(Base64EncStr(Value));
end;

function Seed128Decrypt(const Avalue : string) : string;
var
  ctx: TSEAContext;
  InData, OutData: TSEABlock;
  Value, temp : string;
  i : integer;
begin
  Result := '';
  if Trim(Avalue) = '' then Exit;
  if SEA_Init(SeedKeyValue, 8*sizeof(SeedKeyValue), ctx) <> 0 then Exit;

  FillChar(InData, 16, #32);
  FillChar(OutData, 16, #32);
  temp := Base64DecStr(Avalue);
  for I := 0 to length(temp) - 1 do
    InData[i] := Byte(temp[i+1]);

  SEA_Decrypt(Ctx, InData, OutData);
  for i := 0 to 15 do
    Value := Value + string(char(OutData[i]));

  Result := Trim(Value);
end;
*)

procedure IPTrace(AIP : string);
type
  TData = record
    IPAddress: String[255];
  end;

var
  Data : Tdata;
  FileName : string;
  lWMsgHandle : THandle;
  DataStruct: TCopyDataStruct;
begin
  if Trim(AIP) <> '' then
  begin
    //이미 실행중??
    IP_TRACE_FORM_HANDLE := FindWindow('TFormTraceMain', nil);
    if IP_TRACE_FORM_HANDLE > 0 then
    begin
      lWMsgHandle := RegisterWindowMessage('IP Address');
      Data.IPAddress := AIP;
      DataStruct.dwData := lWMsgHandle;
      DataStruct.cbData := SizeOf(TData);
      DataStruct.lpData := @Data;
      SendMessage(IP_TRACE_FORM_HANDLE, WM_COPYDATA, wParam(Application.Handle), Longint(@DataStruct));
      SendMessage(IP_TRACE_FORM_HANDLE, WM_FORMRESTORM, 0, 0);
    end
    else
    begin
      //실행중 아니면 실행..param 값으로 넣어서
      FileName := GetExePath + '\IPTracer.exe';
      if FileExists(FileName) then
      begin
        ShellExecute(0, '', PChar(FileName), pchar(AIP), '', SW_SHOWNORMAL);
        IP_TRACE_FORM_HANDLE := 1;
      end
      else
      begin
        Application.MessageBox('IP Tracer 프로그램을 실행 할 수 없습니다.',
          PChar(Application.Title), MB_OK + MB_ICONWARNING);
      end;
    end;
  end;
end;

function ShowDataLoadnig : TPanel;
var
  Img : TImage;
  Gif : TGifImage;
begin
  Result := TPanel.Create(nil);
  Result.DoubleBuffered := True;
  Result.BorderStyle := bsNone;
  Result.BevelOuter := bvNone;
  Img := TImage.Create(Result);
  Img.Parent := Result;
  Img.Align := alClient;
  Img.Transparent := True;
  Gif := TGifImage.Create;
  Gif.LoadFromFile(GetExePath + '\loading.gif');
  Gif.AnimationSpeed := 400;
  Gif.Animate := True;
  Gif.AnimateLoop := glEnabled;
  Result.Width := Gif.Width;
  Result.Height := Gif.Height;
  Img.Picture.Assign(Gif);
end;

function WaitMsgFormCreate(AMsg : string; AOwner : TComponent) : TForm;
var
  Form: TForm;
  Img : TImage;
  Gif : TGifImage;
begin
  Result := nil;
  Form := TForm.Create(AOwner);
  with Form do
  begin
    DoubleBuffered := True;
    Canvas.Font := Font;
    BorderStyle := bsNone;
    Color := clWhite;// $00FFEFDF;//$00FFD1A4;
    TransparentColor := True;
    TransparentColorValue := clWhite;
  end;

  Img := TImage.Create(Form);
  Img.Parent := Form;
  Img.Align := alClient;
  Img.Transparent := True;

  Gif := TGifImage.Create;
  Gif.LoadFromFile(GetExePath + '\loading.gif');
  Gif.AnimationSpeed := 400;
  Gif.Animate := True;
  Gif.AnimateLoop := glEnabled;
  Form.Width := Gif.Width;
  Form.Height := Gif.Height;
  Img.Picture.Assign(Gif);
  Result := Form;
end;

{-------------------------------------------------------------------------------
  Procedure: GetCommentValue
  Author   : kyungmun
  DateTime : 2010.11.05
  Arguments: AType : byte; AComment : string
  Result   : string
  Comment  : AComment 값에 들어있는 Value 값을 확인해서 돌려준다.
-------------------------------------------------------------------------------}
function GetCommentValue(ATitle : string; AComment : string) : string;
var
  StringList1, StringList2 : TStringList;
  i, j : integer;
begin
  Result := '';
  StringList1 := TStringList.Create;
  StringList1.StrictDelimiter := True; //공백, 텝들을 구분자로 인식하지 못하게..
  StringList1.Delimiter := ETC_GUBUN1;
  StringList1.DelimitedText := Trim(AComment);

  StringList2 := TStringList.Create;
  StringList2.StrictDelimiter := True; //공백, 텝들을 구분자로 인식하지 못하게..
  StringList2.Delimiter := ETC_GUBUN2;
  try
    for I := 0 to StringList1.Count - 1 do
    begin
      AComment := StringList1.Strings[i];
      if Trim(AComment) = '' then Continue;
      StringList2.DelimitedText := AComment;
      AComment := '';
      for j := 1 to StringList2.Count - 1 do  //value에 "=" 값이 있어도 구분자로 인식하지 않고 그냥 값으로 처리하게 위함. 첫번째 "=" 값 이후로 값을 다 붙인다.
      begin
        if AComment <> '' then AComment := AComment + ETC_GUBUN2;
        AComment := AComment + StringList2.Strings[j];
      end;

      if UpperCase(StringList2.Strings[0]) = UpperCase(ATitle) then
      begin
        Result := AComment;
        Exit;
      end;
    end;
  finally
    StringList2.Free;
    StringList1.Free;
  end;
end;

// 2008-07-16 오전 11:52:06 입력장치 사용하지않는 시간을 계산해서 분으로 돌려준다.
function SecondsIdle: integer;
var
  liInfo: TLastInputInfo;
begin
  liInfo.cbSize := SizeOf(TLastInputInfo);
  GetLastInputInfo(liInfo);

  Result := ((GetTickCount - liInfo.dwTime) div 1000) div 60;
end;

function DeleteDir(DirName : string): Boolean;
var
  SHFileOpStruct : TSHFileOpStruct;
  DirBuf : array [0..255] of char;
begin
  try
    Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0);
    FillChar(DirBuf, Sizeof(DirBuf), 0 );
    StrPCopy(DirBuf, DirName);
    with SHFileOpStruct do
    begin
      Wnd := 0;
      pFrom := @DirBuf;
      wFunc := FO_DELETE;
      fFlags := FOF_ALLOWUNDO;
      fFlags := fFlags or FOF_NOCONFIRMATION;
      fFlags := fFlags or FOF_SILENT;
    end;
    Result := (SHFileOperation(SHFileOpStruct) = 0);
  except
    Result := False;
  end;
end;

//GET 방식의 파라메터값을 POST 방식으로 전달해서 웹페이지를 연다.
procedure PostRequest(const Url, Params: string);
var
  I: Integer;
  vUrl: OleVariant;
  vFlag: OleVariant;
  PostData: OleVariant;
  Headers: OleVariant;
  IE: Variant;
begin
  PostData := VarArrayCreate([0, Length(Params) - 1], varByte);
  for I := 1 to Length(Params) do  PostData[I-1] := Ord(Params[I]);
  Headers := 'Content-Type: application/x-www-form-urlencoded' + #10#13;
  try
    IE := CreateOleObject('InternetExplorer.Application');
  except
    Exit;
  end;
  IE.Visible := true;
  SetWindowPos(IE.HWND, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
  SetWindowPos(IE.HWND, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
  vUrl := Url;
  vFlag := EmptyParam;  //1 or 4 or 8;
  IE.Navigate2(vUrl, vFlag, EmptyParam, PostData, Headers);
  IE := Unassigned;
end;


function minus_light(const cnum, inc_num : byte): integer;
var
  num_buf : real ;
begin
  num_buf := abs(0 - cnum) ;
  num_buf := ((num_buf / 120 ) * inc_num) ;
  result  := cnum - (round(num_buf)) ;
end;

function minus_lightness(Value: Tcolor; inc_num: byte): Tcolor;
var
  r, g, b ,ir,ig,ib : byte ;
begin
  R := Byte(Value);
  G := Byte(Value shr 8);
  B := Byte(Value shr 16);
  // 붉은색의 명도의 감소
  if (r < 0) or (r > 255) then
  begin
    ir := r ;
  end
  else
  begin
    ir := minus_light(r, inc_num);
  end ;
  // 초록색의 명도의 감소
  if ( g < 0) or (g > 255) then
  begin
    ig := g ;
  end
  else
  begin
    ig := minus_light(g, inc_num);
  end  ;
  // 파랑색의 명도의 감소
  if ( b < 0) or (b > 255) then
  begin
    ib := b ;
  end
  else
  begin
    ib := minus_light(b, inc_num);
  end  ;
  result := rgb(ir,ig,ib) ;
end;

function plus_light(const cnum, inc_num: byte): integer;
var
  num_buf : real ;
begin
  num_buf := 255 - cnum ;
  num_buf := (num_buf / 120 ) * inc_num ;
  result  := round(num_buf) + cnum ;
end;


function plus_lightness(Value: Tcolor; inc_num: byte): Tcolor;
var
  r, g, b ,ir,ig,ib : byte ;
begin
  R := Byte(Value);
  G := Byte(Value shr 8);
  B := Byte(Value shr 16);
  // 붉은색의 명도의 증가
  if (r < 0) or (r > 255) then
  begin
    ir := r ;
  end
  else
  begin
    ir := plus_light(r, inc_num);
  end ;
  // 초록색의 명도의 증가
  if ( g < 0) or (g > 255) then
  begin
    ig := g ;
  end
  else
  begin
    ig := plus_light(g, inc_num);
  end  ;
  // 파랑색의 명도의 증가
  if ( b < 0) or (b > 255) then
  begin
    ib := b ;
  end
  else
  begin
    ib := plus_light(b, inc_num);
  end  ;
  result := rgb(ir,ig,ib) ;
end;

//보색
//보색 := $FFFFFF - Canvas.Brush.Color;
function get_complement_color(Value: Tcolor): Tcolor;
   var
   r,g,b,cr,cg,cb : byte ;
begin
   R := Byte(Value);
   G := Byte(Value shr 8);
   B := Byte(Value shr 16);
   // 색과 보색을 합하면 255  가 된다
   // cr + r = 255
   cr := 255 - r ;
   cg := 255 - g ;
   cb := 255 - b ;
   result := rgb(cr,cg,cb) ;
end;


end.
