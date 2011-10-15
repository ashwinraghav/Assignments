program TestOsFileListEvents;

uses
  Forms,
  FmTestOsFileListEvents in 'FmTestOsFileListEvents.pas' {FormTestFileListEvents};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileListEvents, FormTestFileListEvents);
  Application.Run;
end.
