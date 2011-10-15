program TestMotor;

uses
  Forms,
  FmTester in 'FmTester.pas' {Form1},
  SilSiHolors in 'SilSiHolors.pas',
  SilSiMotor in 'SilSiMotor.pas',
  SilSiLists in 'SilSiLists.pas',
  SilSiCalculus in 'SilSiCalculus.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
