unit UmEngineListener;

interface

uses
  Sil,
  UiOuchEngine,
  UiEngine;

type
  TOuchEngineListener = class(
    TInterfacedObject,
    IEngineListener,
    ISocketServerEvents )
  private
    FConnection: IServerSocketConnection;
    FOwner: Pointer;
    function GetOwner: IEngineConnection;
    function DoCreateConnection(const Parameters: IParameters): IServerSocketConnection;
    function DoNewClient(const Socket: IClientSocketConnection): IEngineClient;
  protected // IEngineListener
    function GetConnection: IEngineConnection;
    function GetServer: IServerSocketConnection;
    procedure Startup;
    procedure Shutdown;
  protected // ISocketServerEvents
    procedure OnListen(const Event: TSocketConnectionEvent);
    procedure OnListenFailed(const Event: TSocketConnectionEvent);
  protected
    property Owner: IEngineConnection read GetOwner;
  public
    constructor Create(const Owner: IEngineConnection; const Parameters: IParameters);
    destructor Destroy; override;
  end;

implementation

uses
  UmEngineClient;

{ TOuchServer }

constructor TOuchEngineListener.Create(const Owner: IEngineConnection; const Parameters: IParameters);
begin
  inherited Create;
  FOwner := Pointer(Owner);
  FConnection := DoCreateConnection(Parameters);
  Sil.Sink.Connect(FConnection, Self);
end;

destructor TOuchEngineListener.Destroy;
begin
  Shutdown;
  FOwner := nil;
  inherited;
end;

function TOuchEngineListener.DoCreateConnection(const Parameters: IParameters): IServerSocketConnection;
begin
  Result := Sil.Sv.Socket.CreateServer();
  Result.Parameters.Port := Parameters['Local.Port'];
  Result.Parameters.ReadTimeout := Parameters['Local.ReadTimeout'];
  Result.Parameters.WriteTimeout := Parameters['Local.WriteTimeout'];
end;

function TOuchEngineListener.DoNewClient(const Socket: IClientSocketConnection): IEngineClient;
begin
  Result := TOuchEngineClient.Create(Self, Socket);
end;

procedure TOuchEngineListener.Startup;
begin
  if Assigned(FConnection) then
    FConnection.Listen;
end;

procedure TOuchEngineListener.Shutdown;
begin
  if Assigned(FConnection) then
  begin
    FConnection.DisconnectClients;
    FConnection.Cancel;
    FConnection.Thread.Termination.WaitFor(INFINITE, True);
  end;
end;

procedure TOuchEngineListener.OnListen(const Event: TSocketConnectionEvent);
var
  Socket: IClientSocketConnection;
begin
  try
    while Event.Sender.Accept(Socket) do
      DoNewClient(Socket);
  finally
    FConnection := nil;
  end;
  Event.Sender.Thread.Termination.Signal;
end;

procedure TOuchEngineListener.OnListenFailed(const Event: TSocketConnectionEvent);
begin

end;

function TOuchEngineListener.GetOwner: IEngineConnection;
begin
  Result := IEngineConnection(FOwner);
end;

function TOuchEngineListener.GetConnection: IEngineConnection;
begin
  Result := Self.Owner;
end;

function TOuchEngineListener.GetServer: IServerSocketConnection;
begin
  Result := FConnection;
end;

end.
