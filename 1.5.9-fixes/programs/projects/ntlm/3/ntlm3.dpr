program ntlm3;

uses
  Forms,
  FmMain in 'FmMain.pas' {Form2},
  SilSeNtlm in 'SilSeNtlm.pas',
  SilSfNtlm in 'SilSfNtlm.pas',
  SilSiNtlm in 'SilSiNtlm.pas',
  SilSmNtlm in 'SilSmNtlm.pas',
  SilStNtlm in 'SilStNtlm.pas',
  SilLmTextStream in '..\..\..\lib\SilLmTextStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
