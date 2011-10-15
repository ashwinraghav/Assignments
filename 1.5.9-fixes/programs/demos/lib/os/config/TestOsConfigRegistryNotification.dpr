program TestOsConfigRegistryNotification;

uses
  Forms,
  FmTestOsConfigRegistryNotification in 'FmTestOsConfigRegistryNotification.pas' {FormTestRegistryNotification};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestRegistryNotification, FormTestRegistryNotification);
  Application.Run;
end.
