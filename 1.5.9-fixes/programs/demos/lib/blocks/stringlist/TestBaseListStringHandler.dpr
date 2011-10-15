program TestBaseListStringHandler;

{%ToDo 'TestBaseListStringHandler.todo'}
{%ToDo 'TestBaseListStringHandler.todo'}

uses
  Forms,
  FmTestBaseListStringHandler in 'FmTestBaseListStringHandler.pas' {FormTestHandler};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestHandler, FormTestHandler);
  Application.Run;
end.
