program TestToolsConfigReader;

uses
  Forms,
  FmTestToolsConfigReader in 'FmTestToolsConfigReader.pas' {Form1},
  SilStConfig in '..\..\..\..\..\source\tool\SilStConfig.pas',
  SilSmXmlConfig in '..\..\..\..\..\source\tool\SilSmXmlConfig.pas',
  SilSmRegistryConfig in '..\..\..\..\..\source\tool\SilSmRegistryConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
