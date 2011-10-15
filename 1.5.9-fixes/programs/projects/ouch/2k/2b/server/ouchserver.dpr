program ouchserver;

uses
  Sil,
  Forms,
  FmMain in 'FmMain.pas' {foMain},
  UiServer in 'UiServer.pas',
  UmServer in 'UmServer.pas',
  UiProtocol in '..\common\UiProtocol.pas',
  UmProtocolServer in '..\common\UmProtocolServer.pas',
  UtProtocol in '..\common\UtProtocol.pas',
  UtLog in '..\common\UtLog.pas',
  UmClient in 'UmClient.pas',
  UiClient in 'UiClient.pas',
  UmData in 'UmData.pas',
  UcOuch in '..\common\UcOuch.pas',
  UiUpdater in 'UiUpdater.pas',
  UmUpdater in 'UmUpdater.pas',
  UmClientList in 'UmClientList.pas';

{$R *.res}

begin
  if Sil.OS.Process.PrevInstance then Exit;

  Application.ShowMainForm := false;
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
