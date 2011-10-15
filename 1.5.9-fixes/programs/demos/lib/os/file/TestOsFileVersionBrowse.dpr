program TestOsFileVersionBrowse;

uses
  Forms,
  FmTestOsFileVersionBrowse in 'FmTestOsFileVersionBrowse.pas' {FormTestFileVersion};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileVersion, FormTestFileVersion);
  Application.Run;
end.
