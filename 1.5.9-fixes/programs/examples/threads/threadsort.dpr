program threadsort;

uses
  Forms,
  FmThreadSort in 'FmThreadSort.pas' {FormThreadSort};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormThreadSort, FormThreadSort);
  Application.Run;
end.
