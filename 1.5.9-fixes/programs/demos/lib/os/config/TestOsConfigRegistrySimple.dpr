program TestOsConfigRegistrySimple;

uses
  Forms,
  FmTestOsConfigRegistrySimple in 'FmTestOsConfigRegistrySimple.pas' {FormTestSimpleRegistry};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestSimpleRegistry, FormTestSimpleRegistry);
  Application.Run;
end.
