program p2pft;

uses
  Forms,
  FmMain in 'FmMain.pas' {foMain},
  FrPanel in 'FrPanel.pas' {faPanel: TFrame},
  UmLocalFSys in 'UmLocalFSys.pas',
  UiFileServer in 'UiFileServer.pas',
  UmFileServer in 'UmFileServer.pas',
  UmFileClient in 'UmFileClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
