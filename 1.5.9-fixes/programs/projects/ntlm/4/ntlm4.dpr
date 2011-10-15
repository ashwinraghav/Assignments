program ntlm4;

uses
  Forms,
  FmMain in 'FmMain.pas' {Form3},
  UiServer in 'UiServer.pas',
  UmServer in 'UmServer.pas',
  SilSeNtlm in 'SilSeNtlm.pas',
  SilSfNtlm in 'SilSfNtlm.pas',
  SilSiNtlm in 'SilSiNtlm.pas',
  SilSmNtlm in 'SilSmNtlm.pas',
  SilStNtlm in 'SilStNtlm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
