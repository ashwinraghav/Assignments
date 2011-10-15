program TestOsLibraryStringsClient;

uses
  SilShareMem,
  SilModule,
  SilExport,
  Forms,
  FmTestOsLibraryStringsClient in 'FmTestOsLibraryStringsClient.pas' {FormTestLibraryClient},
  UiTestStringsStub in 'UiTestStringsStub.pas',
  UmTestStringsStub in 'UmTestStringsStub.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestLibraryClient, FormTestLibraryClient);
  Application.Run;
end.
