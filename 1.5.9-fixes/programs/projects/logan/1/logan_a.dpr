program logan_a;

{%File 'logan.xml'}

uses
  Forms,
  FmMain in 'FmMain.pas' {Form1},
  UiLogan in 'UiLogan.pas',
  UmLogan in 'UmLogan.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
