program TestOsTimers;

uses
  Forms,
  FmTestOsTimers in 'FmTestOsTimers.pas' {FormTestOsTimers};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsTimers, FormTestOsTimers);
  Application.Run;
end.
