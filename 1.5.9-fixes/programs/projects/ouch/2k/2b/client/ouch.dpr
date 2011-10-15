program ouch;

{%ToDo 'ouch.todo'}

uses
  SilSharemem,
  SilExport,
  Forms,
  FmOuchMain in 'FmOuchMain.pas' {OuchMain},
  UcOuch in '..\common\UcOuch.pas',
  UiOuch in 'UiOuch.pas',
  UmOuchEngine in 'UmOuchEngine.pas',
  UdOuch in '..\common\UdOuch.pas',
  UtOuch in '..\common\UtOuch.pas',
  UmOuchContacts in 'UmOuchContacts.pas',
  UmOuchContact in 'UmOuchContact.pas',
  UtLog in '..\common\UtLog.pas',
  FrOuchView in 'FrOuchView.pas' {FrameOuchView: TFrame},
  UmOuchMessage in 'UmOuchMessage.pas',
  FmOuchMessage in 'FmOuchMessage.pas' {FormOuchMessage},
  UmOuchAccount in 'UmOuchAccount.pas',
  FrOuchEvents in 'FrOuchEvents.pas' {FrameOuchEvents: TFrame},
  FrOuchHistory in 'FrOuchHistory.pas' {FrameOuchHistory: TFrame},
  FmOuchHistory in 'FmOuchHistory.pas' {FormOuchHistory},
  FkOuchBase in 'FkOuchBase.pas' {FormOuchBase},
  UmOuchWindows in 'UmOuchWindows.pas',
  FmOuchLogon in 'FmOuchLogon.pas' {FormOuchLogon},
  FrOuchButtons in 'FrOuchButtons.pas' {FrameOuchButtons: TFrame},
  DmTrayBar in 'DmTrayBar.pas' {DataTrayBar: TDataModule},
  UmOuchProfile in 'UmOuchProfile.pas',
  DmDataHistory in 'DmDataHistory.pas' {DataHistory: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOuchMain, OuchMain);
  Application.Run;
end.
