program ping;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  UmIcmp in 'UmIcmp.pas',
  UiIcmp in 'UiIcmp.pas',
  UiIcmpEcho in 'UiIcmpEcho.pas',
  UmIcmpEcho in 'UmIcmpEcho.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
