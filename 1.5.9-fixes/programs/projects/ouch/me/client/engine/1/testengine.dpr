program testengine;

uses
  ShareMem,
  Forms,
  Sil,
  FmTestMain in 'src\FmTestMain.pas' {Form1},
  FmUserInfo in 'src\FmUserInfo.pas' {FormUserInfo};

{$R *.RES}

begin
  Sil.Os.Thread.List;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
