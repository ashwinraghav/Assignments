program Test1;

uses
  Forms,
  FmTest1 in 'FmTest1.pas' {FormTest1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest1, FormTest1);
  Application.Run;
end.
