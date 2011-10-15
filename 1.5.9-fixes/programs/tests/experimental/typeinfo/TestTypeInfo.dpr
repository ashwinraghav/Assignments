program TestTypeInfo;



uses
  Forms,
  FmTestMain in 'FmTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
