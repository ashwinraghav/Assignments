program TestOsFileTextFile;

uses
  Forms,
  FmTestOsFileTextFile in 'FmTestOsFileTextFile.pas' {FormTestTextFile};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestTextFile, FormTestTextFile);
  Application.Run;
end.
