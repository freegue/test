program CustomProperty;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  uFunctionUnit in 'uFunctionUnit.pas',
  uCommunityUnit in 'uCommunityUnit.pas',
  uDefineType in 'uDefineType.pas',
  uPropertyDesignEditors in 'uPropertyDesignEditors.pas',
  uPropertyDesignIntf in 'uPropertyDesignIntf.pas',
  uPropertyResource in 'uPropertyResource.pas',
  uPropertyConditionCore in 'uPropertyConditionCore.pas',
  uPropertyConditionDetect in 'uPropertyConditionDetect.pas',
  uPropertyConditionBlock in 'uPropertyConditionBlock.pas',
  uConditionObject in 'uConditionObject.pas',
  fmPropertyInspector in 'fmPropertyInspector.pas' {formPropertyInspector},
  fmPatternInfoSelect in 'fmPatternInfoSelect.pas' {fmPatternSelect},
  fmIPPortValueEditor in 'fmIPPortValueEditor.pas' {formIPPortValueEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
