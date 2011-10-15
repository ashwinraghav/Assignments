program TestOsEnvironmentExpand;

uses
  Forms,
  FmTestOsEnvironmentExpand in 'FmTestOsEnvironmentExpand.pas' {FormTestEnvironmentExpand},
  SilOtEnvironment in '..\..\..\..\lib\os\win\SilOtEnvironment.pas',
  SilOjEnvironment in '..\..\..\..\lib\SilOjEnvironment.pas',
  SilOdEnvironment in '..\..\..\..\lib\lc\sp\SilOdEnvironment.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestEnvironmentExpand, FormTestEnvironmentExpand);
  Application.Run;
end.
