program TestOsModules;

uses
  Forms,
  FmTestOsModules in 'FmTestOsModules.pas' {FormTestModules},
  FmModuleProperties in 'FmModuleProperties.pas' {FormModuleProperties};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestModules, FormTestModules);
  Application.Run;
end.
