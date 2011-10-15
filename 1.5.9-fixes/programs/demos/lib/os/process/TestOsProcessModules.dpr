program TestOsProcessModules;

uses
  Forms,
  FmTestOsProcessModules in 'FmTestOsProcessModules.pas' {FormTestOsProcessModules};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsProcessModules, FormTestOsProcessModules);
  Application.Run;
end.
