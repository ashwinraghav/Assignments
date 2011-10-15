program TestOsProcessList;

uses
  Forms,
  FmTestOsProcessList in 'FmTestOsProcessList.pas' {FormTestOsProcessList};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsProcessList, FormTestOsProcessList);
  Application.Run;
end.
