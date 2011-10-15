program TestOsSocketResolveAddress;

uses
  Forms,
  FmTestOsSocketResolveAddress in 'FmTestOsSocketResolveAddress.pas' {FormTestOsSocketResolveAddress};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsSocketResolveAddress, FormTestOsSocketResolveAddress);
  Application.Run;
end.
