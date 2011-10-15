program TestBaseContainer;

uses
  Forms,
  FmTestBaseContainer in 'FmTestBaseContainer.pas' {FormTestContainer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestContainer, FormTestContainer);
  Application.Run;
end.
