program TestOsFileClone;

uses
  Forms,
  FmTestOsFileClone in 'FmTestOsFileClone.pas' {FormTestFileClone};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestFileClone, FormTestFileClone);
  Application.Run;
end.
