program TestBaseFrameworkLockers;

uses
  Forms,
  FmTestBaseFrameworkLockers in 'FmTestBaseFrameworkLockers.pas' {FormTestLockers};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestLockers, FormTestLockers);
  Application.Run;
end.
