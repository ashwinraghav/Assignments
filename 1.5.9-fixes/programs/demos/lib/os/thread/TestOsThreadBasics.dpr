program TestOsThreadBasics;

uses
  Forms,
  FmTestOsThreadBasics in 'FmTestOsThreadBasics.pas' {FormTestThreadBasics};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestThreadBasics, FormTestThreadBasics);
  Application.Run;
end.
