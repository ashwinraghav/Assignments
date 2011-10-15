program fd;

uses
  Forms,
  FmMain in 'FmMain.pas' {foMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
