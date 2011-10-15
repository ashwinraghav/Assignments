program ntlm2;

uses
  Forms,
  FmMain in 'FmMain.pas' {Form1},
  SilStNtlm in 'SilStNtlm.pas',
  SilSeNtlm in 'SilSeNtlm.pas',
  SilSfNtlm in 'SilSfNtlm.pas',
  SilSiNtlm in 'SilSiNtlm.pas',
  SilSmNtlm in 'SilSmNtlm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
