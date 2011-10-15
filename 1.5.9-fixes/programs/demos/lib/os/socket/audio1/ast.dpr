program ast;

uses
  Forms,
  FmMain in 'FmMain.pas' {foStreamer},
  UiStreamingProtocol in 'UiStreamingProtocol.pas',
  UmStreamingProtocol in 'UmStreamingProtocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoStreamer, foStreamer);
  Application.Run;
end.
