{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : Inspector 에서 사용하는 날짜선택 대화상자
-------------------------------------------------------------------------------}

unit fmDateTimeEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, DateUtils;

type
  TformDateTimeEditor = class(TForm)
    GroupBoxStart: TGroupBox;
    CalendarFrom: TMonthCalendar;
    DateTimeFrom: TDateTimePicker;
    GroupBoxEnd: TGroupBox;
    CalendarTo: TMonthCalendar;
    DateTimeTo: TDateTimePicker;
    Panel1: TPanel;
    Bevel1: TBevel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    RadioGroupRange: TRadioGroup;
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioGroupRangeClick(Sender: TObject);
    procedure CalendarFromClick(Sender: TObject);
    procedure CalendarToClick(Sender: TObject);
  private
    FFromDateTimeStr: string;
    FToDateTimeStr : string;
    FDateRange: integer;
    FSingle : Boolean;
    F1DayQuery : Boolean;
    FSelectLimitDay : integer;

    procedure SetFormValue(const Value: string);
    procedure SetToValue(const Value: string);
    function GetFromDateTime: string;
    function GetToDateTime: string;
    procedure SetDateRange(const Value: integer);
    { Private declarations }
  public
    constructor CreateForm(AOwner: TComponent; ASingle : Boolean; A1DayQuery : Boolean = False);
    property SelectLimitDay : integer read FSelectLimitDay write FSelectLimitDay;
    property FromDateTime : string read GetFromDateTime write SetFormValue;
    property ToDateTime : string read GetToDateTime write SetToValue;
    property DateRange : integer read FDateRange write SetDateRange;
  end;

var
  formDateTimeEditor: TformDateTimeEditor;

implementation

{$R *.dfm}

{ TDateTimeEditorForm }

procedure TformDateTimeEditor.ButtonOKClick(Sender: TObject);
begin
  if (not FSingle) and (GetFromDateTime > GetToDateTime) then
  begin
    Application.MessageBox('시작일 값이 종료일 값 보다 클 수 없습니다.',
      PChar(Application.Title), MB_OK + MB_ICONWARNING);
    Exit;
  end;
  DateRange := RadioGroupRange.ItemIndex;
  ModalResult := mrOk;
end;

procedure TformDateTimeEditor.CalendarFromClick(Sender: TObject);
begin
  if CalendarFrom.Date > Now then
  begin
    Application.MessageBox('오늘 보다 큰 날짜는 선택 할 수 없습니다.',
      PChar(Application.Title), MB_OK + MB_ICONWARNING);
    CalendarFrom.Date := Now;
    Exit;
  end;

  //로그조회시는 설정한 제한값 이전값은 선택 안되고, From, To 서로 싱크 맞춘다.
  if F1DayQuery then
  begin
    if (Now - CalendarFrom.Date) > FSelectLimitDay then
    begin
      Application.MessageBox(Pchar(Format('최근 %d 일 이전 날짜는 선택 할 수 없습니다.', [FSelectLimitDay])),
        PChar(Application.Title), MB_OK + MB_ICONWARNING);
      CalendarFrom.Date := Now - FSelectLimitDay;
    end;
    CalendarTo.Date := CalendarFrom.Date;
  end;

  RadioGroupRange.ItemIndex := 3;
end;

procedure TformDateTimeEditor.CalendarToClick(Sender: TObject);
begin
  if CalendarTo.Date > Now then
  begin
    Application.MessageBox('오늘 보다 큰 날짜는 선택 할 수 없습니다.',
      PChar(Application.Title), MB_OK + MB_ICONWARNING);
    CalendarTo.Date := Now;
    Exit;
  end;

  //로그조회시는 설정한 제한값 이전값은 선택 안되고, From, To 서로 싱크 맞춘다.
  if F1DayQuery then
  begin
    if (Now - CalendarTo.Date) > FSelectLimitDay then
    begin
      Application.MessageBox(PChar(Format('최근 %d 일 이전 날짜는 선택 할 수 없습니다.', [FSelectLimitDay])),
        PChar(Application.Title), MB_OK + MB_ICONWARNING);
      CalendarTo.Date := Now - FSelectLimitDay;
    end;
    CalendarFrom.Date := CalendarTo.Date;
  end;

  RadioGroupRange.ItemIndex := 3;
end;

{-------------------------------------------------------------------------------
  Procedure: TFormEsmDateTimeEditor.CreateForm
  Author   : kyungmun
  DateTime : 2010.03.10
  Arguments: AOwner: TComponent; ASingle: Boolean; AOneDay : Boolean = False
  Result   : None
  Comment  : ASingle : 날짜선택 달력이 1개만 나오도록..
             ALogQuery : 로그 조회용 달력..(하루만 선택가능, 양쪽 모두 씽크 시키기...)
-------------------------------------------------------------------------------}
constructor TformDateTimeEditor.CreateForm(AOwner: TComponent; ASingle: Boolean; A1DayQuery : Boolean = False);
begin
  inherited Create(AOwner);

  FSingle := ASingle;
  F1DayQuery := A1DayQuery;

  if ASingle then
  begin
    GroupBoxStart.Caption := '';
    GroupBoxEnd.Visible := False;
    RadioGroupRange.Visible := False;
    Self.Height := 275;
    Self.Width  := 245;
    ButtonOK.Left := 17;
    ButtonCancel.Left := 123;
  end
  else
  begin
    Self.Height := 320;
    Self.Width  := 480;
  end;

  //로그조회용 달력이면
  if F1DayQuery then
  begin
    RadioGroupRange.Visible := False;
    Self.Height := 275;
    CalendarFrom.Hint := '선택한 하루 안에서 시간 값만 범위로 지정 가능합니다.';
    CalendarTo.Hint := '선택한 하루 안에서 시간 값만 범위로 지정 가능합니다.';
  end;
end;

function TformDateTimeEditor.GetFromDateTime: string;
begin
  Result := FormatDateTime('yyyy-mm-dd ', CalendarFrom.Date);
  Result := Result + FormatDateTime('hh:mm:ss', DateTimeFrom.Time);
end;

function TformDateTimeEditor.GetToDateTime: string;
begin
  Result := FormatDateTime('yyyy-mm-dd ', CalendarTo.Date);
  Result := Result + FormatDateTime('hh:mm:ss', DateTimeTo.Time);
end;

procedure TformDateTimeEditor.RadioGroupRangeClick(Sender: TObject);
begin
  case RadioGroupRange.ItemIndex of
    0 : begin
          SetFormValue(FormatDateTime('yyyy-mm-dd 00:00:00', Now));
          SetToValue(FormatDateTime('yyyy-mm-dd 23:59:59', Now));
        end;
    1 : begin
          SetFormValue(FormatDateTime('yyyy-mm-dd 00:00:00', StartOfTheWeek(Now)));
          SetToValue(FormatDateTime('yyyy-mm-dd 23:59:59', EndOfTheWeek(Now)));
        end;
    2 : begin
          SetFormValue(FormatDateTime('yyyy-mm-dd 00:00:00', StartOfTheMonth(Now)));
          SetToValue(FormatDateTime('yyyy-mm-dd 23:59:59', EndOfTheMonth(Now)));
        end;
  end;
end;

procedure TformDateTimeEditor.SetDateRange(const Value: integer);
begin
  FDateRange := Value;
  RadioGroupRange.ItemIndex := Value;
end;

procedure TformDateTimeEditor.SetFormValue(const Value: string);
begin
  FFromDateTimeStr := Value;
  CalendarFrom.Date := VarToDateTime(Value);
  DateTimeFrom.Time := VarToDateTime(Value);
end;

procedure TformDateTimeEditor.SetToValue(const Value: string);
begin
  FToDateTimeStr := Value;
  CalendarTo.Date := VarToDateTime(Value);
  DateTimeTo.Time := VarToDateTime(Value);
end;


end.