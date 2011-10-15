program TestBaseFrameworkEnumeration;

uses
  Forms,
  FmTestBaseFrameworkEnumeration in 'FmTestBaseFrameworkEnumeration.pas' {FormTestEnumeration};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestEnumeration, FormTestEnumeration);
  Application.Run;
end.
