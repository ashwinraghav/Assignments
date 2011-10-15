program TestOsProcessBasics;

uses
  Forms,
  FmTestOsProcessBasics in 'FmTestOsProcessBasics.pas' {FormTestOsProcessBasics},
  SilOjThread in '..\..\..\..\..\source\lib\SilOjThread.pas',
  SilLiTraits in '..\..\..\..\..\source\lib\SilLiTraits.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsProcessBasics, FormTestOsProcessBasics);
  Application.Run;
end.
