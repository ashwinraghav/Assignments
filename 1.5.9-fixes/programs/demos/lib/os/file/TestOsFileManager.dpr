program TestOsFileManager;

uses
  Forms,
  FmTestOsFileManager in 'FmTestOsFileManager.pas' {FormTestFileManager};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileManager, FormTestFileManager);
  Application.Run;
end.
