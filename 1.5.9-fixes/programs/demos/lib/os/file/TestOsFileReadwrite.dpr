program TestOsFileReadwrite;

uses
  Forms,
  FmTestOsFileReadwrite in 'FmTestOsFileReadwrite.pas' {FormTestFileReadwrite};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileReadwrite, FormTestFileReadwrite);
  Application.Run;
end.
