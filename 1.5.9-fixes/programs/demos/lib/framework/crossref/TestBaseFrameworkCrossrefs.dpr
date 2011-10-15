program TestBaseFrameworkCrossrefs;

uses
  Forms,
  FmTestBaseFrameworkCrossrefs in 'FmTestBaseFrameworkCrossrefs.pas' {FormTestCrossrefs};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestCrossrefs, FormTestCrossrefs);
  Application.Run;
end.
