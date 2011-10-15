program p2pft;

uses
  Forms,
  FmMain in 'FmMain.pas' {foMain},
  FmConectar in 'FmConectar.pas' {foConectar},
  UiPeer in 'UiPeer.pas',
  UmPeer in 'UmPeer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
