program TestOsFileListEnumeration;

uses
  Forms,
  FmTestOsFileListEnumeration in 'FmTestOsFileListEnumeration.pas' {FormTestFileListEnumeration};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileListEnumeration, FormTestFileListEnumeration);
  Application.Run;
end.
