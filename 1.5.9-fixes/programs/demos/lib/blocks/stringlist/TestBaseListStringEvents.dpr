program TestBaseListStringEvents;

uses
  Sil,
  Forms,
  FmTestBaseListStringEvents in 'FmTestBaseListStringEvents.pas' {FormTestEvents};

{$R *.RES}

begin
  if Sil.OS.Process.PrevInstance then Exit;

  Application.Initialize;
  Application.CreateForm(TFormTestEvents, FormTestEvents);
  Application.Run;
end.
