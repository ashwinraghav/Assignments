unit FmTestOsThreadPool;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, SilTool, StdCtrls;

type
  TFormTestThreadPool = class(TForm, IRunnable)
    Signal: TButton;
    Spawn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SignalClick(Sender: TObject);
    procedure SpawnClick(Sender: TObject);
  private
    FEvent: IEvent;
    tp: IThreadManager;
    procedure Run(const Thread: IThread);
  public
    { Public declarations }
  end;

var
  FormTestThreadPool: TFormTestThreadPool;

implementation

{$R *.DFM}

procedure TFormTestThreadPool.FormCreate(Sender: TObject);
begin
  FEvent := Sil.OS.IPC.Event();
  tp := SilTool.Sv.ThreadPool.Create(4);
  tp.Spawn(Self);
end;

procedure TFormTestThreadPool.Run(const Thread: IThread);
begin
  FEvent.WaitFor()
end;

procedure TFormTestThreadPool.SignalClick(Sender: TObject);
begin
  FEvent.Pulse;
end;

procedure TFormTestThreadPool.SpawnClick(Sender: TObject);
begin
  tp.Spawn(Self);
end;

end.
