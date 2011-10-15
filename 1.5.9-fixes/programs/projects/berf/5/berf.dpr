program berf;

{%File 'berf.xml'}

uses
  Sil,
  SvcMgr,
  main in 'main.pas' {svBerf: TService},
  list in 'list.pas';

{$R *.RES}

begin
  if Sil.OS.Process.PrevInstance then Exit;

  Application.Initialize;
  Application.CreateForm(TsvBerf, svBerf);
  Application.Run;
end.
