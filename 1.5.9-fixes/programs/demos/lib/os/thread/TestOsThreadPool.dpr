program TestOsThreadPool;

uses
  Forms,
  FmTestOsThreadPool in 'FmTestOsThreadPool.pas' {FormTestThreadPool};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestThreadPool, FormTestThreadPool);
  Application.Run;
end.
