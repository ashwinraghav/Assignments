program Ouch;

uses
  ShareMem,
  Forms,
  UiOuchSkin in '..\..\..\common\1\src\UiOuchSkin.pas',
  FMain in 'src\FMain.pas' {OuchApp},
  UEngine in 'src\UEngine.pas',
  UiOuchClient in '..\..\..\common\1\src\UiOuchClient.pas',
  UmOuchCommon in '..\..\..\common\1\src\UmOuchCommon.pas',
  UUser in 'src\UUser.pas',
  UmOuchTools in '..\..\..\common\1\src\UmOuchTools.pas';

{$R *.RES}

begin
	Application.Initialize;
	Application.ShowMainForm := false;
	Application.CreateForm(TOuchApp, OuchApp);
  Application.Run;
end.
