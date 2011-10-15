unit SilVmFlowThread;

interface

uses
  Sil; 

type
  TFlowThreadEvent = procedure ( Sender: TObject ) of object;

  IFlowThread = interface
    ['{2D331690-3B94-4307-BA86-21DF6FD0DE41}']
    procedure Activate( Sender: TObject; OnActivate: TFlowThreadEvent );
  end;

  FlowThread = class
    class function Create: IFlowThread;
  end;

implementation

type
  IFlowThreadMsg = interface
    ['{82EF9B94-72FB-4E77-BF14-DB5E58DD9161}']
    function GetObj: TObject;
    function GetOnActivate: TFlowThreadEvent;
    // properties
    property Obj: TObject read GetObj;
    property OnActivate: TFlowThreadEvent read GetOnActivate;
  end;

  IFlowRunner = interface
    ['{AF78D942-F7CF-4BB7-A329-60E8F540A466}']
    procedure Put(const Msg: IUnknown);
    procedure Shutdown;
  end;

type
  TFlowThread = class
  (
    TSilInterfacedObject,
    IFlowThread
  )
  private
    FThread: IThread;
    FRunner: IFlowRunner;
  protected // IFlowThread
    procedure Activate( Sender: TObject; OnActivate: TFlowThreadEvent );
  protected
    procedure CheckRunning;
    procedure Start;
    procedure Stop;
  public
    destructor Destroy; override;
  end;

  TFlowRunner = class
  (
    TSilInterfacedObject,
    IRunnable,
    IFlowRunner
  )
  private
    FQueue: IInterfaceQueue;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected
    procedure Put(const Msg: IUnknown);
    procedure Shutdown;
  public
    constructor Create; reintroduce;
  end;

  TFlowThreadMsg = class
  (
    TSilInterfacedObject,
    IFlowThreadMsg
  )
  private
    FObj: TObject;
    FOnActivate: TFlowThreadEvent;
  protected // IFlowThreadMsg
    function GetObj: TObject;
    function GetOnActivate: TFlowThreadEvent;
  public
    constructor Create(Sender: TObject; OnActivate: TFlowThreadEvent); reintroduce;
  end;

{ FlowThread }

class function FlowThread.Create: IFlowThread;
begin
  result := TFlowThread.Create;
end;

{ TFlowThread }

destructor TFlowThread.Destroy;
begin
  Stop;
  inherited;
end;

procedure TFlowThread.Activate( Sender: TObject; OnActivate: TFlowThreadEvent );
begin
  CheckRunning;
  FRunner.Put( TFlowThreadMsg.Create( Sender, OnActivate ) );
end;

procedure TFlowThread.CheckRunning;
begin
  if not Assigned( FThread ) then
    Start;
end;

procedure TFlowThread.Start;
begin
  Stop;
  FRunner := TFlowRunner.Create();
  FThread := Sil.Os.Thread.Spawn( FRunner as IRunnable );
end;

procedure TFlowThread.Stop;
begin
  if Assigned( FRunner ) then
  begin
    FRunner.Shutdown;
    FRunner := nil;
  end;

  if Assigned( FThread ) then
  begin
    FThread.Termination.WaitFor;
    FThread := nil;
  end;
end;

{ TFlowRunner }

constructor TFlowRunner.Create;
begin
  inherited Create;  
  FQueue := Sil.List.InterfaceQueue;
end;

procedure TFlowRunner.Put(const Msg: IInterface);
begin
  FQueue.Put( Msg );
end;

procedure TFlowRunner.Run(const Thread: IThread);
var
  msg: IFlowThreadMsg;
begin
  while FQueue.Get( IFlowThreadMsg, msg ) do
  begin
    if Assigned( msg.OnActivate ) then
      msg.OnActivate( msg.Obj );
  end;
end;

procedure TFlowRunner.Shutdown;
begin
  if Assigned( FQueue ) then
  begin
    FQueue.Cancel;
    FQueue := nil;
  end;
end;

{ TFlowThreadMsg }

constructor TFlowThreadMsg.Create(Sender: TObject; OnActivate: TFlowThreadEvent);
begin
  inherited Create;
  FObj := Sender;
  FOnActivate := OnActivate;
end;

function TFlowThreadMsg.GetObj: TObject;
begin
  result := FObj;
end;

function TFlowThreadMsg.GetOnActivate: TFlowThreadEvent;
begin
  result := FOnActivate;
end;

end.
