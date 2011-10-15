program TestBaseContainerSorted;

uses
  Forms,
  FmTestBaseContainerSorted in 'FmTestBaseContainerSorted.pas' {FormTestSorted};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestSorted, FormTestSorted);
  Application.Run;
end.
