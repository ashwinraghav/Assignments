program TestStub;

uses
  Forms,
  FmTestStub in 'FmTestStub.pas' {Form1},
  UiTest in 'UiTest.pas',
  UmTest in 'UmTest.pas',
  UmStub in 'UmStub.pas',
  UiStub in 'UiStub.pas',
  UfStub in 'UfStub.pas',
  UhStub in 'UhStub.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
