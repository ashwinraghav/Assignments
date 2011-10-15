program TestBaseContainerInterface;

uses
  Forms,
  FmTestBaseContainerInterface in 'FmTestBaseContainerInterface.pas' {Form1},
  SilLmContainerInterfaces in '..\..\..\..\..\source\lib\SilLmContainerInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
