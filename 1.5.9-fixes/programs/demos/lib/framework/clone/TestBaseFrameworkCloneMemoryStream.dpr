program TestBaseFrameworkCloneMemoryStream;

uses
  Forms,
  FmTestBaseFrameworkCloneMemoryStream in 'FmTestBaseFrameworkCloneMemoryStream.pas' {FormTestCloneMemory};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestCloneMemory, FormTestCloneMemory);
  Application.Run;
end.
