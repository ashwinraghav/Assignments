program http;

{%File 'rfc2616.txt'}
{%File 'http.txt'}
{%File 'rfc1808.txt'}

uses
  Forms,
  FmEvalMain in 'FmEvalMain.pas',
  FmEvalDisplay in 'FmEvalDisplay.pas' {FormDisplay},
  FmEvalCode in 'FmEvalCode.pas' {FormCode};

{$R *.res}

begin                                  
  Application.Initialize;
  Application.CreateForm(TFormTester, FormTester);
  Application.Run;
end.
