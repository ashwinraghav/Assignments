program berf;

{%File 'berf.xml'}

uses
  Sil,
  SvcMgr,
  main in 'main.pas' {svBerf: TService},
  UiBerf in 'UiBerf.pas',
  UmBerf in 'UmBerf.pas',
  UmClient in 'UmClient.pas',
  UmGroupInfo in 'UmGroupInfo.pas',
  UmProxyInfo in 'UmProxyInfo.pas',
  UmServer in 'UmServer.pas';

{$R *.RES}

begin
  if Sil.OS.Process.PrevInstance then Exit;

  Application.Initialize;
  Application.CreateForm(TsvBerf, svBerf);
  Application.Run;
end.
