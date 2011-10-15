program fbagent;

{%File 'fbagent.xml'}

uses
  Forms,
  FmSchedule in 'FmSchedule.pas' {foSchedule},
  FmRecurrence in 'FmRecurrence.pas' {foRecurrence},
  FmMain in 'FmMain.pas' {foMain},
  FmTask in 'FmTask.pas' {foTask},
  FmStep in 'FmStep.pas' {foStep},
  DmAgent in 'DmAgent.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
