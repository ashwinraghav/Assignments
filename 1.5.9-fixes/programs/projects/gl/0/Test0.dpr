program Test0;

uses
  Forms,
  FmTest0 in 'FmTest0.pas' {FormTest0};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest0, FormTest0);
  Application.Run;
end.
