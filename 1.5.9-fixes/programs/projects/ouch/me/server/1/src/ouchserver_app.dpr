program ouchserver_app;

{%File '..\doc\protocol.txt'}
{%File '..\..\..\common\1\src\UiOuchProtocol.pas'}
{%File '..\..\..\common\1\src\UmOuchProtocol.pas'}
{%File '..\..\..\common\1\src\UkOuchChannel.pas'}

uses
  Forms,
  amain in 'amain.pas' {Form3},
  UtData in 'UtData.pas',
  UiData in 'UiData.pas',
  UmData in 'UmData.pas',
  UmClient in 'UmClient.pas',
  UiClient in 'UiClient.pas',
  UmServer in 'UmServer.pas',
  UiServer in 'UiServer.pas',
  UiEventLog in 'UiEventLog.pas',
  UcClient in 'UcClient.pas',
  UmStatus in 'UmStatus.pas',
  UiStatus in 'UiStatus.pas',
  UmOuchProtocolPacker in '..\..\..\common\1\src\UmOuchProtocolPacker.pas',
  UcOuchProtocol in '..\..\..\common\1\src\UcOuchProtocol.pas',
  UiTables in 'UiTables.pas',
  UiNotify in 'UiNotify.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
