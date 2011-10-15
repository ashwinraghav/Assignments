program TestOsEnvironmentAccess;

uses
  Forms,
  FmTestOsEnvironmentAccess in 'FmTestOsEnvironmentAccess.pas' {FormTestEnvironmentAccess};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestEnvironmentAccess, FormTestEnvironmentAccess);
  Application.Run;
end.
