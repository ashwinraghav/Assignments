program TestBaseStrToken;

uses
  Forms,
  FmTestBaseStrToken in 'FmTestBaseStrToken.pas' {FormTestToken};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestToken, FormTestToken);
  Application.Run;
end.
