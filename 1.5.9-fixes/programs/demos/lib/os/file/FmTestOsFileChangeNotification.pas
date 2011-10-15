unit FmTestOsFileChangeNotification;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, SilVCL;

type
  TFormTestFileChangeNotification = class(TForm, IRunnable)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTerminated: IEvent;
    FThread: IThread;
  private
    procedure Run(const Thread: IThread);
  end;

var
  FormTestFileChangeNotification: TFormTestFileChangeNotification;

implementation

{$R *.DFM}

procedure TFormTestFileChangeNotification.FormCreate(Sender: TObject);
begin
  FTerminated := Sil.Os.Ipc.Event(); 
  FThread := OS.Thread.Spawn(Self);
end;

procedure TFormTestFileChangeNotification.FormDestroy(Sender: TObject);
begin
  FTerminated.Signal;

  Sil.Os.Wait.Single(FThread.Termination, INFINITE, True);
  
  FThread := nil;
  FTerminated := nil;
end;

procedure TFormTestFileChangeNotification.Run(const Thread: IThread);
var
  Change: IFilesystemChangeNotification;
  i: Integer;
begin
  Change := Sil.OS.FileSystem.Notification(ExtractFilePath(Application.ExeName), false, [cfLastWrite]);

  while Sil.OS.Wait.Any([Change, FTerminated], INFINITE, i) do
  begin
    if i = 1 then Break;

    VCL.SetProp.Value(Self, 'Caption', Sil.DateTime.ToStr(Sil.DateTime.Now));
    Beep;
    Change.Reset;
  end;
end;

end.
