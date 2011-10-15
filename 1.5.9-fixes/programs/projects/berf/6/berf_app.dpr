program berf_app;

{%File 'berf.xml'}

uses
  Forms,
  FoMain in 'FoMain.pas' {Form2},
  UmBerf in 'UmBerf.pas',
  UiBerf in 'UiBerf.pas',
  UmProxyInfo in 'UmProxyInfo.pas',
  UmServer in 'UmServer.pas',
  UmClient in 'UmClient.pas',
  UmGroupInfo in 'UmGroupInfo.pas';

{$R *.res}

begin
  Application.ShowMainForm := false;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
