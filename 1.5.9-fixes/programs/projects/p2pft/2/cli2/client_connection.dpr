program client_connection;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SilSmLayerConnectionImate in '..\SilSmLayerConnectionImate.pas',
  SilSiLayerConnection in '..\SilSiLayerConnection.pas',
  SilSmKeepAlive in '..\SilSmKeepAlive.pas',
  SilSmLayerConnection in '..\SilSmLayerConnection.pas',
  SilSiKeepAlive in '..\SilSiKeepAlive.pas',
  SilSiLayerConnectionImate in '..\SilSiLayerConnectionImate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
