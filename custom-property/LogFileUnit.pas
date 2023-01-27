{-------------------------------------------------------------------------------
 Copyright (C)
 Author   : kyungmun
 Date     : 2008-08-08
 Comment  : 프로그램에서 발생하는 각종 메세지를 로그파일로 생성
-------------------------------------------------------------------------------}

unit LogFileUnit;

interface

uses
  Windows, classes, sysutils, Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.CheckLst, Vcl.ExtCtrls, DefineTypeUnit;

const
  STR_LINE = '{-------------------program start--------------------}';
  END_LINE = '{-------------------program end----------------------}';
  ONE_MEGA_BYTE = 1024 * 1024;
  FILE_SIZE_LIMITE_MB = 5;

type
  //메세지 표시 이벤트 함수
  TOnMessageView = procedure(AMsgType: TMessageType; AMsg : string) of object;

  TLogFile = class(TObject)
  private
    FFileName   : string;
    FFilePath   : string;
    FToDay      : string;
    FLevels     : TMessageTypes;
    FWriter     : TStreamWriter;
    FFileStream : TFileStream;
    FFileMaxSize : Integer; //실제 파일 크기 byte 단위로 계산된..

    function SetFileName(AIndex : Integer) : string;
    procedure FileCreate(FileSizeOver : Boolean = False);
    procedure SetFileMaxSize(const Value: integer);
    procedure SetLevels(const Value: TMessageTypes);

  public
    constructor Create;
    destructor Destroy; override;
    class function GetObject : TLogFile;

    procedure Write(AType : TMessageType; AMsg : string);
    procedure WriteDebug(AMsg : string);
    procedure WriteInfo(AMsg : string);
    procedure WriteNotice(AMsg : string);
    procedure WriteWarning(AMsg : string);
    procedure WriteExcept(AMsg: string);
    procedure WriteError(AMsg : string);
    procedure WriteFatal(AMsg : string);

    property FileMaxSize : integer read FFileMaxSize write SetFileMaxSize;
    property Levels : TMessageTypes read FLevels write SetLevels;
  end;

  TLogLevelSettingForm = class(TForm)
  private
    FBevel : TBevel;
    FCheckListBox : TCheckListBox;
    FButton : TButton;
    FLogLevels : TMessageTypes;

    procedure OnButtonClick(Sender : TObject);
    procedure OnFormShow(Sender : TObject);
    procedure OnFormKeyPress(Sender: TObject; var Key: Char);
    procedure SetLogLevels(const Value: TMessageTypes);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property Levels : TMessageTypes read FLogLevels write SetLogLevels;
  end;

  function GetMessageTypeToString(AType : TMessageType):string;
  function GetMessageTypeItems : string;


implementation

uses
  ProfileNormalConfigUnit, uCommunityUnit, uFunctionUnit;

resourcestring
  RES_STR_DEBUG   = '[DEBUG ]';
  RES_STR_INFO    = '[INFO  ]';
  RES_STR_NOTICE  = '[NOTI  ]';
  RES_STR_WARNING = '[WARN  ]';
  RES_STR_EXCEPT  = '[EXCEPT]';
  RES_STR_ERROR   = '[ERROR ]';
  RES_STR_FATAL   = '[FATAL ]';

var
  MyObject : TLogFile = nil;
  isFinally : Boolean;

{ TLogWrite }

function GetMessageTypeToString(AType : TMessageType):string;
begin
  case AType of
    mtDebug  : Result := RES_STR_DEBUG;
    mtInfo   : Result := RES_STR_INFO;
    mtNotice : Result := RES_STR_NOTICE;
    mtWarning: Result := RES_STR_WARNING;
    mtExcept : Result := RES_STR_EXCEPT;
    mtError  : Result := RES_STR_ERROR;
    mtFatal  : Result := RES_STR_FATAL;
  end;
end;

function GetMessageTypeItems : string;
var
  i : integer;
  sLevel, eLevel :TMessageType;
begin
  try
    sLevel := system.Low(TMessageType);
    eLevel := system.High(TMessageType);
    for I := Ord(sLevel) to Ord(eLevel) do
    begin
      Result := Result + GetMessageTypeToString(TMessageType(i));
      if i < Ord(eLevel) then
        Result := Result + ',';
    end;
  except
    Result := '[Off]';
  end;
end;

constructor TLogFile.Create;
begin
  //로그레벨 설정 :: 없을때는 아무 로그도 남기지 않는다.
  FLevels := [mtDebug, mtInfo, mtNotice, mtWarning, mtError, mtFatal];
  FFileName := '';
  FFileMaxSize := ONE_MEGA_BYTE * FILE_SIZE_LIMITE_MB;
  FileCreate;
end;

destructor TLogFile.Destroy;
begin
  if (FWriter <> nil)  then
  begin
    if FLevels <> [] then
      FWriter.WriteLine(END_LINE);

    FWriter.Free;
  end;

  if (FFileStream <> nil)  then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;

  FLevels := [];

  inherited Destroy;
end;

procedure TLogFile.FileCreate(FileSizeOver : Boolean);
var
  Index : Integer;
  OldFileName, sMsg : string;
begin
  if FLevels = [] then Exit;

  DateTimeToString(FToday, 'yyyymmdd', date);
  FFilePath := ExtractFilePath(Application.ExeName) + '\Log\' + FToday + '\';

  if FWriter <> nil then
  begin
    FWriter.Free;
    FWriter := nil;
  end;

  if FFileStream <> nil then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;

  if FFilePath <> '' then
  begin
    //디렉토리 검사
    if not DirectoryExists(FFilePath) then
      ForceDirectories(FFilePath);

    if FFileName <> '' then
      OldFileName := FFileName;

    //파일 제한 크기 초과해서 생성하는거면 파일명을 확인..
    if FileSizeOver then
    begin
      Index := 1;
      FFileName := FFilePath + SetFileName(Index);
      while FileExists(FFileName) do //파일이 있으면 계속 다음번호로..
      begin
        Inc(Index);
        FFileName := FFilePath + SetFileName(Index);
      end;
      //없는 번호 발견시. 현재의 파일을 이 이름으로 바꾸고..
      MoveFile(PChar(OldFileName), PChar(FFileName));
    end;

    //다시 0번호로 파일 생성.
    Index := 0;
    FFileName := FFilePath + SetFileName(Index);

    //파일이 없을때 생성
    if not FileExists(FFileName) then
    begin
      FFileStream := TFileStream.Create(FFileName, fmCreate or fmOpenReadWrite or fmShareDenyNone);

      FWriter := TStreamWriter.Create(FFileStream, TEncoding.UTF8);
      sMsg := 'Binary Last Build Date : ' + GetLastWriteTime;
      FWriter.WriteLine(sMsg);
      FWriter.WriteLine(STR_LINE);
    end
    else //파일이 있으면 열기.
    begin
      FFileStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
      FFileStream.Position := FFileStream.Size;
    //end;

    //if (FFileStream <> nil) and (FWriter = nil) then
    //begin
      FWriter := TStreamWriter.Create(FFileStream, TEncoding.UTF8);
      FWriter.WriteLine(STR_LINE);
    end;
  end;
end;

procedure TLogFile.SetFileMaxSize(const Value: integer);
begin
  FFileMaxSize := Value;
end;

function TLogFile.SetFileName(AIndex : Integer): string;
begin
  if AIndex = 0 then
    DateTimeToString(result, 'yyyymmdd', now)
  else
    DateTimeToString(result, 'yyyymmdd_'+intTostr(AIndex), now);

  result := result + '.log';
end;

procedure TLogFile.SetLevels(const Value: TMessageTypes);
begin
  FLevels := Value;
  if (FFileStream = nil) and (FWriter = nil) then
    FileCreate;
end;

procedure TLogFile.Write(AType : TMessageType; AMsg: string);
var
  cTime, Msg : string;
begin
  if not(AType in FLevels) then Exit;

  if (FFileStream = nil) or (FWriter = nil) then Exit;

  //날짜가 변경됐을때 새로생성
  DateTimeToString(cTime, 'yyyymmdd', date);
  if cTime <> FToday then
  begin
    FFileName := '';
    FileCreate;
  end;

  try
    DateTimeToString(cTime, 'yyyy-mm-dd hh:mm:ss.zzz', now);
    case AType of
      mtDebug   : Msg := Format('%s %s %s', [ctime, RES_STR_DEBUG, AMsg]);
      mtInfo    : Msg := Format('%s %s %s', [ctime, RES_STR_INFO, AMsg]);
      mtNotice  : Msg := Format('%s %s %s', [ctime, RES_STR_NOTICE, AMsg]);
      mtWarning : Msg := Format('%s %s %s', [ctime, RES_STR_WARNING, AMsg]);
      mtError   : Msg := Format('%s %s %s', [ctime, RES_STR_ERROR, AMsg]);
      mtFatal   : Msg := Format('%s %s %s', [ctime, RES_STR_FATAL, AMsg]);
    end;
    FWriter.WriteLine(Msg);

    //파일크기 제한 초과시 새로생성
    if FFileStream.Size >= FFileMaxSize  then
    begin
      FileCreate(True);
      Msg := Format('로그 파일 크기 제한(%dMB)을 초과해 다시 생성되었습니다.', [FILE_SIZE_LIMITE_MB]);
      FWriter.WriteLine(Msg);
    end;
  except
  end;
end;

procedure TLogFile.WriteDebug(AMsg: string);
begin
  Write(mtDebug, AMsg);
end;

procedure TLogFile.WriteExcept(AMsg: string);
begin
  Write(mtExcept, AMsg);
end;

procedure TLogFile.WriteError(AMsg: string);
begin
  Write(mtError, AMsg);
end;

procedure TLogFile.WriteFatal(AMsg: string);
begin
  Write(mtFatal, AMsg);
end;

procedure TLogFile.WriteInfo(AMsg: string);
begin
  Write(mtInfo, AMsg);
end;

procedure TLogFile.WriteNotice(AMsg: string);
begin
  Write(mtNotice, AMsg);
end;

procedure TLogFile.WriteWarning(AMsg: string);
begin
  Write(mtWarning, AMsg);
end;

class function TLogFile.GetObject: TLogFile;
begin
  if (not isFinally) and (MyObject = nil) then
    MyObject := TLogFile.Create;
  Result := MyObject;
end;

{ TLogLevelSettingForm }

constructor TLogLevelSettingForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  self.WindowState := wsNormal;
  self.FormStyle := fsNormal;
  self.BorderStyle := bsSingle;
  self.BorderIcons := [biSystemMenu];
  self.Position := poOwnerFormCenter;
  self.KeyPreview := True;
  self.Caption := 'Log Level Setting';
  self.Height := 250;
  self.Width := 200;

  self.OnShow := OnFormShow;
  self.OnKeyPress := OnFormKeyPress;

  FCheckListBox := TCheckListBox.Create(self);
  FCheckListBox.Align := alClient;
  FCheckListBox.AlignWithMargins := True;
  FCheckListBox.Parent := self;

  FBevel := TBevel.Create(self);
  FBevel.Align := alBottom;
  FBevel.Height := 10;
  FBevel.AlignWithMargins := True;
  FBevel.Shape := bsBottomLine;
  FBevel.Parent := self;

  FButton := TButton.Create(self);
  FButton.Align := alBottom;
  FButton.AlignWithMargins := True;
  FButton.Margins.Bottom := 10;
  FButton.Caption := '확인';
  FButton.ModalResult := mrOk;
  FButton.OnClick := OnButtonClick;
  FButton.Parent := self;

  FLogLevels := TLogFile.GetObject.Levels;
end;

destructor TLogLevelSettingForm.Destroy;
begin
  FreeAndNil(FCheckListBox);
  FreeAndNil(FBevel);
  FreeAndNil(FButton);

  inherited;
end;

procedure TLogLevelSettingForm.OnButtonClick(Sender: TObject);
var
  i : integer;
  Level : TMessageType;
begin
  FLogLevels := [];
  for I := 0 to FCheckListBox.Items.Count - 1 do
  begin
    if FCheckListBox.Checked[i] then
    begin
      Level := TMessageType(i);
      FLogLevels := FLogLevels + [Level];
    end;
  end;
end;

procedure TLogLevelSettingForm.OnFormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then ModalResult := mrClose;
end;

procedure TLogLevelSettingForm.OnFormShow(Sender: TObject);
var
  i : integer;
  Level : TMessageType;
begin
  for I := 0 to Ord(system.High(TMessageType)) do
  begin
    Level := TMessageType(i);
    FCheckListBox.Items.Add(GetMessageTypeToString(Level));
    if level in FLogLevels then
      FCheckListBox.Checked[i] := True;
  end;

  FBevel.Align := alBottom;
  FButton.Align := alBottom;

end;

procedure TLogLevelSettingForm.SetLogLevels(const Value: TMessageTypes);
begin
  FLogLevels := Value;
end;

initialization
  isFinally := False;

finalization
  isFinally := True;
  if MyObject <> nil then MyObject.Destroy;
end.
