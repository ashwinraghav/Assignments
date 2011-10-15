program LogViewer;

uses
  SIL,
  Forms,
  Graphics,
  FMain in 'FMain.pas' {MainForm},
  FFilters in 'FFilters.pas' {FormFilters},
  UDef in 'UDef.pas',
  UGroup in 'UGroup.pas',
  UGroupList in 'UGroupList.pas',
  UFilters in 'UFilters.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
