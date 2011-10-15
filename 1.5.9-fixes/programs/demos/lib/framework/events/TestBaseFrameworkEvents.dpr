program TestBaseFrameworkEvents;

uses
  Forms,
  FmTestBaseFrameworkEvents in 'FmTestBaseFrameworkEvents.pas' {FormTestEvents};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestEvents, FormTestEvents);
  Application.Run;
end.
