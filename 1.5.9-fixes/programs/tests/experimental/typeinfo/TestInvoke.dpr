program TestInvoke;

uses
  Forms,
  FmTestInvoke in 'FmTestInvoke.pas' {FormTestInvoke},
  UmTest in 'UmTest.pas',
  UiTest in 'UiTest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestInvoke, FormTestInvoke);
  Application.Run;
end.
