program fm1;

uses
  Forms,
  FmMain in 'FmMain.pas' {FormMain},
  FrTreePanel in 'FrTreePanel.pas' {FrameTree: TFrame},
  FrFilePanel in 'FrFilePanel.pas' {FrameFiles: TFrame},
  UiDefs in 'UiDefs.pas',
  FrViewPanel in 'FrViewPanel.pas' {FrameView: TFrame},
  UiWorker in 'UiWorker.pas',
  UmWorker in 'UmWorker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
