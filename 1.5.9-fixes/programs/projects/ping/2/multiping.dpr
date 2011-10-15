program multiping;

uses
  Forms,
  FmProbeMain in 'FmProbeMain.pas' {FormPingMain},
  SilSmIcmpProtocol in '..\..\..\..\source\tool\SilSmIcmpProtocol.pas',
  SilSiIcmpProtocol in '..\..\..\..\source\tool\SilSiIcmpProtocol.pas',
  SilSiIcmpEcho in '..\..\..\..\source\tool\SilSiIcmpEcho.pas',
  SilSmIcmpEcho in '..\..\..\..\source\tool\SilSmIcmpEcho.pas',
  SilIcmp in '..\..\..\..\source\tool\SilIcmp.pas',
  SilStIcmp in '..\..\..\..\source\tool\SilStIcmp.pas',
  FPassword in 'FPassword.pas' {PasswordDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Multi Ping Probe';
  Application.ShowMainForm := False;
  Application.CreateForm(TFormPingMain, FormPingMain);
  Application.Run;
end.
