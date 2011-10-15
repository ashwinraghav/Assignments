library engine;

uses
  SilModule,
  UmData in 'UmData.pas',
  UrEngine in 'UrEngine.pas',
  UmClient in 'UmClient.pas',
  UiProtocol in '..\common\UiProtocol.pas',
  UiClient in '..\common\UiClient.pas',
  UmKeepAlive in '..\common\UmKeepAlive.pas',
  UtProtocol in '..\common\UtProtocol.pas',
  UtUpdate in '..\common\UtUpdate.pas';

{$E so}

{$LIBPREFIX 'ouch'}
{$LIBSUFFIX '2'}
{$LIBVERSION '2.0.0.0'}

begin
  UrEngine.Register;
end.
