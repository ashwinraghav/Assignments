program TestOsSocketDumpProtocols;

uses
  Forms,
  FmTestOsSocketDumpProtocols in 'FmTestOsSocketDumpProtocols.pas' {FormTestOsSocketDumpProtocols};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsSocketDumpProtocols, FormTestOsSocketDumpProtocols);
  Application.Run;
end.
