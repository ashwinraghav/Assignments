program TestBaseQueues;

uses
  Forms,
  FmTestBaseQueues in 'FmTestBaseQueues.pas' {FormTestQueues};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestQueues, FormTestQueues);
  Application.Run;
end.
