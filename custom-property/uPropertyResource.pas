{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : 인스펙터의 프로퍼티명을 한글로 표시하기위한 유닛.
             파일에서 값을 읽어 목록으로 관리하고
             검색시 프로퍼티명에 해당하는 한글값을 찾아서 돌려준다.
-------------------------------------------------------------------------------}

unit uPropertyResource;

interface

uses
  Classes, SysUtils, RTLConsts;

resourcestring
  kyungmun = 'kyungmun-text';

const
  PROFILE_PROPERTYNAME_FILENAME = 'ProfilePropertyName_.cfg';

procedure LoadFromFileofPropertyResource;
function GetPropertyNameTranslation(ProName : string) : string;

implementation

var
  PropertyNameResource : TStringList;


procedure Add(Avalue : string);
begin
  PropertyNameResource.Add(Avalue);
end;

{-------------------------------------------------------------------------------
  Procedure: SetValue
  Author   : kyungmun
  DateTime : 2009.04.09
  Arguments: None
  Result   : None
  Comment  : 프로퍼티에 한글을 보이게 할때
             '프로퍼티명=한글' 형태로 아래 불러 들이는 화일에 입력해 줍니다.
-------------------------------------------------------------------------------}
procedure LoadFromFileofPropertyResource;
var
  AFileName : string;
begin
  PropertyNameResource.Clear;
  AFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + PROFILE_PROPERTYNAME_FILENAME;
  try
    if FileExists(AFileName) then
      PropertyNameResource.LoadFromFile(AFileName);
  except
//    raise EFCreateError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  {
  Add('LogType=로그유형');
  Add('Annotated=대응여부');
  Add('Severity=위험도');
  }
end;

function GetPropertyNameTranslation(ProName : string) : string;
begin
  Result := PropertyNameResource.Values[ProName];
  if Trim(Result) = '' then
    Result := ProName;
end;


initialization
  PropertyNameResource := TStringList.Create;
  LoadFromFileofPropertyResource;

finalization
  PropertyNameResource.Clear;
  PropertyNameResource.Free;

end.
