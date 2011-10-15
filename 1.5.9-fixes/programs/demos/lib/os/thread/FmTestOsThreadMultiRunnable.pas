unit FmTestOsThreadMultiRunnable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

{$include Defines.inc}

const
  THREAD1 = 1024;
  THREAD2 = 1025;  

type
  TFormTestOsThreadMultiRunnable = class(TForm, IDispatchable)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread1: IThread;
    FThread2: IThread;
    FStopAll: IEvent;
    procedure DoUpdate1(const Sender: IInterface; Param: Pointer);
    procedure DoUpdate2(const Sender: IInterface; Param: Pointer);
  private
    procedure Thread1Run(var Msg: RThreadRunMessage); message THREAD1;
    procedure Thread2Run(var Msg: RThreadRunMessage); message THREAD2;
  public
    { Public declarations }
  end;

var
  FormTestOsThreadMultiRunnable: TFormTestOsThreadMultiRunnable;

implementation

{$R *.dfm}

procedure TFormTestOsThreadMultiRunnable.FormCreate(Sender: TObject);
begin
  FStopAll := Sil.OS.Ipc.Event;
  FThread1 := Sil.OS.Thread.Spawn(THREAD1, Self);
  FThread2 := Sil.OS.Thread.Spawn(THREAD2, Self);
end;

procedure TFormTestOsThreadMultiRunnable.FormDestroy(Sender: TObject);
begin
  FStopAll.Signal;

  FThread1.Termination.WaitFor(INFINITE, true);
  FThread2.Termination.WaitFor(INFINITE, true);

  FThread1 := nil;
  FThread2 := nil;
  FStopAll := nil;
end;

procedure TFormTestOsThreadMultiRunnable.Thread1Run(var Msg: RThreadRunMessage);
begin
  repeat
    Sil.OS.Thread.SyncCall(DoUpdate1);
  until FStopAll.WaitFor(1000) = wrSignaled;
end;

procedure TFormTestOsThreadMultiRunnable.Thread2Run(var Msg: RThreadRunMessage);
begin
  repeat
    Sil.OS.Thread.SyncCall(DoUpdate2);
  until FStopAll.WaitFor(1000) = wrSignaled;
end;

procedure TFormTestOsThreadMultiRunnable.DoUpdate1(const Sender: IUnknown; Param: Pointer);
begin
  label1.caption := Time.ToStr(Time.Now);
end;

procedure TFormTestOsThreadMultiRunnable.DoUpdate2(const Sender: IUnknown; Param: Pointer);
begin
  label2.caption := Time.ToStr(Time.Now);
end;

end.
