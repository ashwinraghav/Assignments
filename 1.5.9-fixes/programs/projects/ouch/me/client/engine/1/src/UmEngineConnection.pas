unit UmEngineConnection;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine,
  UiEngine,
  UkEngineChannel;

type
  TOuchEngineConnection = class(
    TOuchEngineChannel,
    IOuchConnection,
    IEngineConnection,
    IEngineSender )
  private
    FListener: IEngineListener;
    FEngine: Pointer;
    FPending: IOuchConnection;
  private
    procedure DoFireClosed;
    procedure DoFireEstablished;
    procedure DoFireFailed;
    function DoCreateConnection(const Parameters: IParameters): IClientSocketConnection;
  protected
    procedure OnDisconnected(const Event: TConnectionBreakEvent); override;
    procedure OnConnected(const Event: TConnectionEvent); override;
    procedure OnFailed(const Event: TConnectionFailedEvent); override;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IOuchConnection
    procedure Define(const User: IOuchLocalUser);
    procedure Logon(const User: IOuchLocalUser; const Listener: IUnknown);
    procedure Disconnect;
  protected // IEngineConnection
    function GetEngine: IOuchEngine;
    function GetListener: IEngineListener;
    procedure Activate;
  protected // IEngineSender
    function GetConnection: IEngineConnection;
    function GetChannel: IEngineChannel;
    procedure Startup;
    procedure Shutdown;
  protected
    property Engine: IOuchEngine read GetEngine;
  public
    constructor Create(const Engine: IOuchEngine; const Parameters: IParameters);
    destructor Destroy; override;
  end;

implementation

uses
  UkOuchChannel,
  UmEngineListener,
  UmEngineSession;

{ TOuchConnection }

constructor TOuchEngineConnection.Create(const Engine: IOuchEngine; const Parameters: IParameters);
begin
  inherited Create(DoCreateConnection(Parameters));
  FEngine := Pointer(Engine);
  FListener := TOuchEngineListener.Create(Self, Parameters);
end;

destructor TOuchEngineConnection.Destroy;
begin
  Disconnect;
  FEngine := nil;
  inherited;
end;

procedure TOuchEngineConnection.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  try
    DoFireClosed();
  finally
    inherited;
  end;
end;

procedure TOuchEngineConnection.OnConnected(const Event: TConnectionEvent);
begin
  try
    DoFireEstablished();
  finally
    FPending := nil; //! Limpiamos la ref porque en el evento se la tienen que haber guardado.
    inherited;
  end;
end;

procedure TOuchEngineConnection.OnFailed(const Event: TConnectionFailedEvent);
begin
  try
    DoFireFailed();
  finally
    inherited;
    FPending := nil; //! Limpiamos la ref porque en el evento se la tienen que haber guardado. Esto implica que se destruirá!!
  end;
end;

procedure TOuchEngineConnection.AddListener(const Listener: IUnknown);
begin
  inherited;
  Sil.Sink.Connect(FListener, Listener);
end;

procedure TOuchEngineConnection.RemoveListener(const Listener: IUnknown);
begin
  Sil.Sink.Disconnect(FListener, Listener);
  inherited;
end;

procedure TOuchEngineConnection.Define(const User: IOuchLocalUser);
begin
  Request.DefineUser(Sil.GUID.Create, User.ID, User.Password, User.Data);
end;

procedure TOuchEngineConnection.Logon(const User: IOuchLocalUser; const Listener: IUnknown);
var
  Session: IEngineSession;
begin
  Session := TOuchEngineSession.Create(Self, User);
  Sil.Sink.Connect(Session, Listener);
  Session.Request();
end;

procedure TOuchEngineConnection.Activate;
begin
  if Assigned(FListener) then
    FListener.Startup;

  FPending := Self; //!! guardamos una ref para que no nos destruyamos mientras la conexión se establece
  try
    Startup;
  except
    FPending := nil;
    raise;
  end;

end;

procedure TOuchEngineConnection.Disconnect;
begin
  try
    if Self.Connection <> nil then
      Self.Shutdown;
    if Assigned(FListener) then
    begin
      FListener.Shutdown;
      FListener := nil;
    end;
  finally
    FPending := nil;
  end;
end;

function TOuchEngineConnection.DoCreateConnection(const Parameters: IParameters): IClientSocketConnection;
begin
  Result := Sil.Sv.Socket.CreateClient();
  Result.Parameters.Host := Parameters['Server.Address'];
  Result.Parameters.Port := Parameters['Server.Port'];
  Result.Parameters.ReadTimeout := Parameters['Server.ReadTimeout'];
  Result.Parameters.WriteTimeout := Parameters['Server.WriteTimeout'];
  Result.RetryInterval := Parameters['Server.RetryInterval'];
  Result.RetryCount := Parameters['Server.RetryCount'];
end;

procedure TOuchEngineConnection.DoFireClosed;
var
  Enum: IEnumerator;
  Item: IOuchConnectionEvents;
  Event: ROuchConnectionEvent;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  while Events.Enumerate(Enum, Item, IOuchConnectionEvents) do
    Item.OnConnectionClosed(Event);
end;

procedure TOuchEngineConnection.DoFireEstablished;
var
  Enum: IEnumerator;
  Item: IOuchConnectionEvents;
  Event: ROuchConnectionEvent;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  while Events.Enumerate(Enum, Item, IOuchConnectionEvents) do
    Item.OnConnectionEstablished(Event);
end;

procedure TOuchEngineConnection.DoFireFailed;
var
  Enum: IEnumerator;
  Item: IOuchConnectionEvents;
  Event: ROuchConnectionEvent;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  while Events.Enumerate(Enum, Item, IOuchConnectionEvents) do
    Item.OnConnectionFailed(Event);
end;

function TOuchEngineConnection.GetChannel: IEngineChannel;
begin
  Result := Self;
end;

function TOuchEngineConnection.GetConnection: IEngineConnection;
begin
  Result := Self;
end;

procedure TOuchEngineConnection.Startup;
begin
  Self.Connection.Connect();
end;

procedure TOuchEngineConnection.Shutdown;
begin
  Self.Connection.Disconnect();
end;

function TOuchEngineConnection.GetEngine: IOuchEngine;
begin
  Result := IOuchEngine(FEngine);
end;

function TOuchEngineConnection.GetListener: IEngineListener;
begin
  Result := FListener;
end;

end.
