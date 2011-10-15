program logan;

uses
  SvcMgr,
  SmMain in 'SmMain.pas' {LoganService: TService},
  UiLogan in 'UiLogan.pas',
  UmLogan in 'UmLogan.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TLoganService, LoganService);
  Application.Run;
end.
