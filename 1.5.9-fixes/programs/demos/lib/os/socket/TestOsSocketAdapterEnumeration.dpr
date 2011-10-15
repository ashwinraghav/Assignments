program TestOsSocketAdapterEnumeration;

uses
  Forms,
  FmTestOsSocketAdapterEnumeration in 'FmTestOsSocketAdapterEnumeration.pas' {FormTestOsSocketAdapterEnumeration};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsSocketAdapterEnumeration, FormTestOsSocketAdapterEnumeration);
  Application.Run;
end.
