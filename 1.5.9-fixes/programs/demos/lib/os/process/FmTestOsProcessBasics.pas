unit FmTestOsProcessBasics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

type
  IEvOnStarted = interface
    ['{910204BF-7EE1-4984-BB48-A68EA6FAC9F4}']
    procedure OnProcessStarted(const Process: IProcess);
  end;

  IEvOnTerminated = interface
    ['{E3E08367-0C1C-4940-A92D-6E140DEF0314}']
    procedure OnProcessTerminated(const Process: IProcess);
  end;

  TFormTestOsProcessBasics = class(
    TForm,
    IEvOnStarted,
    IEvOnTerminated )
    Label1: TLabel;
    lbSelfProcess: TLabel;
    edProcessName: TEdit;
    ckWaitTermination: TCheckBox;
    btExecute: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
  private
    FProcesses: IInterfaceList;
  private
    procedure DoAddText(const Sender: IUnknown; Param: Pointer);
  protected // IEvOnStarted, IEvOnTerminated
    procedure OnProcessStarted(const Process: IProcess);
    procedure OnProcessTerminated(const Process: IProcess);
  end;

  TRunningProcess = class(
    TSilObject,
    IAdoptionTrait, // This is a mark to make the Thread's implementation to hold a reference on us.
    IRunnable )
  private
    FListener: IUnknown;
    FProcess: IProcess;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  public
    constructor Create(const Process: IProcess; const Listener: IUnknown); 
    destructor Destroy; override;
  end;

var
  FormTestOsProcessBasics: TFormTestOsProcessBasics;

implementation

{$R *.dfm}

{ TFormTestOsProcessBasics }

procedure TFormTestOsProcessBasics.FormCreate(Sender: TObject);
begin
  FProcesses := Sil.List.InterfaceList(True);
  lbSelfProcess.Caption := Sil.Os.Process.Current.Info.Version.FullName;
end;

procedure TFormTestOsProcessBasics.btExecuteClick(Sender: TObject);
var
  Process: IProcess;
begin
  Process := Sil.Os.Process.Execute('notepad.exe', pvNormal);
  if ckWaitTermination.Checked then
    Sil.Os.Thread.Spawn(TRunningProcess.Create(Process, Self));
end;

procedure TFormTestOsProcessBasics.OnProcessStarted(const Process: IProcess);
var
  Mesg: string;
begin
  Mesg := Sil.Str.Format('Process [%s] with ID = [%d] was started', [Process.Info.Name, Process.PID]);
  Sil.Os.Thread.SyncCall(DoAddText, PChar(Mesg));
end;

procedure TFormTestOsProcessBasics.OnProcessTerminated(const Process: IProcess);
var
  Mesg: string;
begin
  Mesg := Sil.Str.Format('Process [%s] with ID = [%d] has terminated', [Process.Info.Name, Process.PID]);
  Sil.Os.Thread.SyncCall(DoAddText, PChar(Mesg));
end;

procedure TFormTestOsProcessBasics.DoAddText(const Sender: IInterface; Param: Pointer);
begin
  Memo.Lines.Add(PChar(Param))
end;

{ TRunningProcess }

constructor TRunningProcess.Create(const Process: IProcess; const Listener: IUnknown);
var
  Event: IEvOnStarted;
begin
  inherited Create;
  FListener := Listener;
  FProcess := Process;
  if Sil.Ref.Get(FListener, IEvOnStarted, Event) then
    Event.OnProcessStarted(FProcess);
end;

destructor TRunningProcess.Destroy;
var
  Event: IEvOnTerminated;
begin
  if Sil.Ref.Get(FListener, IEvOnTerminated, Event) then
    Event.OnProcessTerminated(FProcess);
  FProcess := nil;
  FListener := nil;
  inherited;
end;

procedure TRunningProcess.Run(const Thread: IThread);
begin
  Sil.Os.Wait.Single(FProcess, INFINITE)
end;

end.
