program TestOsNamedPipes;

uses
  Forms,
  FmTestOsNamedPipes in 'FmTestOsNamedPipes.pas' {FormTestOsNamedPipes};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsNamedPipes, FormTestOsNamedPipes);
  Application.Run;
end.
