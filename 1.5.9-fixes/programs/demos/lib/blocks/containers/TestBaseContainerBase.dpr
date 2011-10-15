program TestBaseContainerBase;

uses
  Forms,
  FmTestBaseContainerBase in 'FmTestBaseContainerBase.pas' {FormTestBase};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestBase, FormTestBase);
  Application.Run;
end.
