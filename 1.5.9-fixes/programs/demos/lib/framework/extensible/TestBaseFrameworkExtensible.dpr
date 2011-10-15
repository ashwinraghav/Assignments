program TestBaseFrameworkExtensible;

uses
  Forms,
  FmTestBaseFrameworkExtensible in 'FmTestBaseFrameworkExtensible.pas' {FormTestExtensible};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestExtensible, FormTestExtensible);
  Application.Run;
end.
