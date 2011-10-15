program gxtest;

uses
  Forms,
  FmGxTest in 'FmGxTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
