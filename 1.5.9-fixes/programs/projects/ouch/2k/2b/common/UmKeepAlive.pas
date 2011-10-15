unit UmKeepAlive;

interface

uses
  Sil,
  UiKeepAlive;

type
  TKeepAlive = class (
    // extends
    TSilInterfacedObject,
    // implements
    IKeepAlive,
    IRunnable)
  private
    FThread: IThread;
    FMustStop: IEvent;
    FInterval: LongWord;
  private
    procedure DoFireKeepAlive(out Success: Boolean);
  protected // IKeepAlive
    procedure Start;
    procedure Stop;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TKeepAlive }

constructor TKeepAlive.Create;
begin
  inherited Create;
end;

destructor TKeepAlive.Destroy;
begin
  inherited;
end;

procedure TKeepAlive.Start;
begin
  FInterval := 1000;
  FMustStop := Sil.OS.Ipc.Event;
  FThread := Sil.OS.Thread.Spawn('keepalive', Self);
end;

procedure TKeepAlive.Stop;
begin
  if Assigned(FThread) then
  begin
    FMustStop.Signal;
    FThread.Termination.WaitFor;

    FMustStop := nil;
    FThread := nil;
  end;
end;

procedure TKeepAlive.Run(const Thread: IThread);
var
  Success: Boolean;
begin
  repeat
    Success := true;
    DoFireKeepAlive(Success);
  until Success or (FMustStop.WaitFor(FInterval) <> wrTimeout);
end;

procedure TKeepAlive.DoFireKeepAlive(out Success: Boolean);
var
  Enum: IEnumerator;
  Sink: IKeepAliveEvents;
begin
  try
    if HasConnections then
      while Events.Enumerate(Enum, Sink, IKeepAliveEvents) do
        Sink.OnKeepAlive(Self, Success);
  except
    Success := false;
  end;
end;

end.
