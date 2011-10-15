program httpsrv;

uses
  Forms,
  FmSrvMain in 'FmSrvMain.pas' {FormHttpServer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHttpServer, FormHttpServer);
  Application.Run;
end.
