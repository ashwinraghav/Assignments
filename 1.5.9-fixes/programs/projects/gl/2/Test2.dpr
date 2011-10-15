program Test2;

uses
  Forms,
  FmTest2 in 'FmTest2.pas' {Form1},
  SilSeGlTypes in 'lib\SilSeGlTypes.pas',
  SilSiGlEngine in 'lib\SilSiGlEngine.pas',
  SilSiGlTypes in 'lib\SilSiGlTypes.pas',
  SilSiGlData in 'lib\SilSiGlData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
