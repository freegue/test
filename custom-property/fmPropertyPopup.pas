{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-07
  Comment  : 인스펙터에서 사용하는 리스트박스 팝업화면
-------------------------------------------------------------------------------}

unit fmPropertyPopup;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormPropertyPopup = class(TForm)
    PanelButton: TPanel;
    Bevel1: TBevel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PopupFormCloseTime: UInt = 0;


implementation

{$R *.DFM}


procedure TFormPropertyPopup.FormDeactivate(Sender: TObject);
begin
  PopupFormCloseTime := GetTickCount;
  Close;
end;

procedure TFormPropertyPopup.FormResize(Sender: TObject);
var
  Center : Integer;
begin
  Center := self.Width div 2;
  ButtonOK.Left := Center - ButtonOK.Width - 3;
  ButtonCancel.Left := Center + 3;
end;

procedure TFormPropertyPopup.ButtonCancelClick(Sender: TObject);
begin
  close;
end;

procedure TFormPropertyPopup.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
