program TestOsFileChangeNotification;

uses
  Forms,
  FmTestOsFileChangeNotification in 'FmTestOsFileChangeNotification.pas' {FormTestFileChangeNotification},
  SilOfWait in '..\..\..\..\lib\os\win\SilOfWait.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileChangeNotification, FormTestFileChangeNotification);
  Application.Run;
end.
