unit UmStatus;

interface

uses
  Sil,

  UiData,
  UiStatus;

type
  TStatusDaemon = class (
    // extends
    TInterfacedObject,
    // implements
    IStatusDaemon,
    IRunnable)
  private
    FThread: IThread;
    FDataMgr: IDataMgr;
    FCheckInterval: LongWord;
  private
    procedure DoCheck;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IStatusDaemon
    procedure Startup;
    procedure Shutdown;
  public
    constructor Create(const DataMgr: IDataMgr);
    destructor Destroy; override;
  end;

implementation

uses
  UtData;

{ TStatusDaemon }

constructor TStatusDaemon.Create;
begin
  inherited Create;

  FDataMgr := DataMgr;
  FCheckInterval := 1000 * 60 * 10;
  //FCheckInterval := 1000;
  FThread := Sil.OS.Thread.Spawn('StatusDaemon', Self, true);
end;

destructor TStatusDaemon.Destroy;
begin
  Shutdown;
  FThread := nil;

  inherited;
end;

procedure TStatusDaemon.Startup;
begin
  FThread.Resume;
end;

procedure TStatusDaemon.Shutdown;
begin
  if FThread <> nil then
  begin
    FThread.Termination.Signal;
    Sil.OS.Wait.Single(FThread, INFINITE, true);
    FThread := nil;
  end;

  FDataMgr := nil;
end;

procedure TStatusDaemon.Run(const Thread: IThread);
begin
  while not Sil.OS.Wait.Single(Thread.Termination, FCheckInterval) do DoCheck;
end;

procedure TStatusDaemon.DoCheck;
begin
  //Db.DisconnectSession(FDataMgr, DateTime.Now);
end;

end.
