unit UmWorker;

interface

uses
  Sil, UiWorker;

type
  TFmWorkerThread = class(
    TSilObject,
    IFmWorkerThread,
    IRunnable )
  private
    FOwner: Pointer;
    FQueue: IInterfaceQueue;
    FThread: IThread;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IFmWorkerThread
    function Post(const Item: IFmWorkerItem; const Data: IUnknown = nil; Param: Pointer = nil): IFmWorkerResult;
    procedure Clear;
    procedure Terminate;
  public
    constructor Create(const Owner: IUnknown);
    destructor Destroy; override; 
  end;  

implementation

type
  IFmWorkerEvent = interface (IFmWorkerResult)
    ['{CEADE0F6-533A-484E-AC35-3AC0BE8B6362}']
    procedure Signal(Result: LongWord);
  end;

  IFmWorkerData = interface
    ['{B91D51A8-6D3F-4646-856B-532C172C809D}']
    function Item: IFmWorkerItem;
    function Data: IUnknown;
    function Param: Pointer;
    function Event: IFmWorkerEvent;
    procedure Execute(const Owner: IUnknown; out Result: LongWord);
  end;

type
  TFmWorkerItem = class(
    TSilObject,
    IFmWorkerData )
  private
    FItem: IFmWorkerItem;
    FCaller: IUnknown;
    FParam: Pointer;
    FEvent: IFmWorkerEvent;
  protected // IFmWorkerData
    function Item: IFmWorkerItem;
    function Data: IUnknown;
    function Param: Pointer;
    function Event: IFmWorkerEvent;
    procedure Execute(const Owner: IUnknown; out Result: LongWord);
  public
    constructor Create(const Item: IFmWorkerItem; const Event: IFmWorkerEvent; const Data: IUnknown; Param: Pointer);
    destructor Destroy; override; 
  end;

  TFmWorkerResult = class(
    TSilObject,
    IWaitable,
    IFmWorkerResult,
    IFmWorkerEvent )
  private
    FEvent: IEvent;
    FResult: LongWord;
  protected // IWaitable
    function WaitFor(const Timeout: Cardinal = INFINITE; AvoidMsgLock: Boolean = false): TSyncWaitResult;
  protected // IFmWorkerResult
    function GetResult: LongWord;
  protected     
    procedure Signal(Result: LongWord);
  public
    constructor Create;
    destructor Destroy; override; 
  end;

{ TFmWorkerThread }

constructor TFmWorkerThread.Create(const Owner: IInterface);
begin
  inherited Create;
  FOwner := Pointer(Owner);
  FQueue := Sil.Tk.InterfaceQueue();
  FThread := Sil.Os.Thread.Spawn('worker', Self);
end;

destructor TFmWorkerThread.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TFmWorkerThread.Clear;
begin
//  if Assigned(FQueue) then FQueue.Clear;
end;

procedure TFmWorkerThread.Terminate;
begin
  if Assigned(FQueue) then FQueue.Cancel;
  if Assigned(FThread) then FThread.Termination.WaitFor(INFINITE, True);
  FQueue := nil;
  FThread := nil;
end;

function TFmWorkerThread.Post(const Item: IFmWorkerItem; const Data: IInterface; Param: Pointer): IFmWorkerResult;
var
  Event: IFmWorkerEvent;
begin
  Event := TFmWorkerResult.Create();
  if Assigned(FQueue) then FQueue.Put(TFmWorkerItem.Create(Item, Event, Data, Param));
  Result := Event;
end;

procedure TFmWorkerThread.Run(const Thread: IThread);
var
  Data: IFmWorkerData;
  Result: LongWord;
begin
  while Assigned(FQueue) and FQueue.Get(IFmWorkerData, Data) do
  try
    Result := 0;
    try
      Data.Execute(IUnknown(FOwner), Result);
    finally
      Data.Event.Signal(Result);
      Data := nil;
    end;
  except
    Sil.Trace.Exception('TFmWorkerThread.Run');
  end;
end;

{ TFmWorkerItem }

constructor TFmWorkerItem.Create(const Item: IFmWorkerItem; const Event: IFmWorkerEvent; const Data: IInterface; Param: Pointer);
begin
  inherited Create;
  FItem := Item;
  FCaller := Data;
  FParam := Param;
  FEvent := Event;
end;

destructor TFmWorkerItem.Destroy;
begin
  FItem := nil;
  FCaller := nil;
  FParam := nil;
  FEvent := nil;
  inherited;
end;

function TFmWorkerItem.Event: IFmWorkerEvent;
begin
  Result := FEvent;
end;

function TFmWorkerItem.Item: IFmWorkerItem;
begin
  Result := FItem;
end;

function TFmWorkerItem.Param: Pointer;
begin
  Result := FParam;
end;

function TFmWorkerItem.Data: IUnknown;
begin
  Result := FCaller;
end;

procedure TFmWorkerItem.Execute(const Owner: IInterface; out Result: LongWord);
begin
  if Assigned(FItem) then
  try
    FItem.Execute(Owner, FCaller, FParam, Result);
  except
    Sil.Trace.Exception('TFmWorkerItem.Execute');
  end;
end;

{ TFmWorkerResult }

constructor TFmWorkerResult.Create;
begin
  inherited Create;
  FEvent := Sil.Os.IPC.Event();
end;

destructor TFmWorkerResult.Destroy;
begin
  FEvent := nil;
  inherited;
end;

function TFmWorkerResult.GetResult: LongWord;
begin
  Result := FResult;  
end;

procedure TFmWorkerResult.Signal(Result: LongWord);
begin
  FResult := Result;
  FEvent.Signal;
end;

function TFmWorkerResult.WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
begin
  Result := FEvent.WaitFor(Timeout, AvoidMsgLock);
end;

end.
