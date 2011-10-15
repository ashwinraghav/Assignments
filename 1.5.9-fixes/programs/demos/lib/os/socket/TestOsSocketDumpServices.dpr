program TestOsSocketDumpServices;

uses
  Forms,
  FmTestOsSocketDumpServices in 'FmTestOsSocketDumpServices.pas' {FormTestOsSocketDumpServices};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsSocketDumpServices, FormTestOsSocketDumpServices);
  Application.Run;
end.
