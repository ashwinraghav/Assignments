program berf_app;

{%File 'berf.xml'}

uses
  Forms,
  FoMain in 'FoMain.pas' {Form2},
  list in 'list.pas';

{$R *.res}

begin
  Application.ShowMainForm := false;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
