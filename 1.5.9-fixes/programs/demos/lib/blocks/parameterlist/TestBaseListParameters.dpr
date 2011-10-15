program TestBaseListParameters;

uses
  Forms,
  FmTestBaseListParameters in 'FmTestBaseListParameters.pas' {FormTestParameters};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestParameters, FormTestParameters);
  Application.Run;
end.
