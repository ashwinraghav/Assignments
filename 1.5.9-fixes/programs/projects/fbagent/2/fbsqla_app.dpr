program fbsqla_app;

uses
  Forms,
  FmService in 'FmService.pas' {Form1},
  DmAgentSource in 'DmAgentSource.pas' {daAgentSource: TDataModule},
  UiAgent in 'UiAgent.pas',
  UmAgent in 'UmAgent.pas',
  UmAgentDispatch in 'UmAgentDispatch.pas',
  UmSchedule in 'UmSchedule.pas',
  UtXml in 'UtXml.pas',
  DmAgentDispatch in 'DmAgentDispatch.pas' {daDispatch: TDataModule},
  UtLog in 'UtLog.pas',
  UtAgentNotify in 'UtAgentNotify.pas',
  UfConfig in 'UfConfig.pas',
  UiRecurrence in 'UiRecurrence.pas',
  UmRecurrence in 'UmRecurrence.pas',
  UiAgentProtocol in 'UiAgentProtocol.pas',
  UtCoder in 'UtCoder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
