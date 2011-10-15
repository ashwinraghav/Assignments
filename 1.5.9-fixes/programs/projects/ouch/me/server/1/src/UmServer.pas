unit UmServer;

interface

uses
  Sil,
  SilClasses,
  SilProtocol,

  UiClient,
  UiEventLog,
  UiData,
  UiServer,
  UiStatus;

type
  TOuchServer = class (
    // extends
    TInterfacedObject,
    // implements
    IOuchServer,
    ISocketServerEvents,
    IRunnable)
  private
    FStatus: IStatusDaemon;
    FConnection: IServerSocketConnection;
    FUdpServer: IServerSocket;
    FDataMgr: IDataMgr;
    FLogger: IEventLog;
    FClients: IInterfaceList;
    FUdpThread: IThread;
  private
    procedure DoRelease;
    procedure DoLog(const Text: String; EventType: TEventType);
    procedure DoLogFmt(const Text: String; const Args: array of const; EventType: TEventType);
  protected // ISocketServerEvents
    procedure OnListen(const Event: TSocketConnectionEvent);
    procedure OnListenFailed(const Event: TSocketConnectionEvent);
  protected // IOuchServer
    procedure Shutdown;
    function GetClients: IInterfaceList;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  public
    constructor Create(const DataMgr: IDataMgr; const Connection: IServerSocketConnection; const UdpServer: IServerSocket);
    destructor Destroy; override;
  end;

implementation

uses
  UmClient,
  UmStatus, SilOtThread;

{ TOuchServer }

constructor TOuchServer.Create(const DataMgr: IDataMgr; const Connection: IServerSocketConnection; const UdpServer: IServerSocket);
begin
  inherited Create;

  FClients := Sil.Tk.InterfaceList(true);
  FStatus := TStatusDaemon.Create(DataMgr);
  FStatus.Startup;

  FDataMgr := DataMgr;
  FConnection := Connection;
  Sil.Sink.Connect(FConnection, Self);

  FConnection.Listen;

  FUdpServer := UdpServer;
  FUdpServer.Listen;

  FUdpThread := Sil.OS.Thread.Spawn('udpserver', Self);
end;

destructor TOuchServer.Destroy;
begin
  DoRelease;
  inherited;
end;

procedure TOuchServer.DoRelease;
begin
  if FConnection <> nil then
  begin
    Sil.Sink.Disconnect(FConnection, Self);
    FConnection := nil;
  end;

  if FStatus <> nil then
  begin
    FStatus.Shutdown;
    FStatus := nil;
  end;

  FDataMgr := nil;
  FLogger := nil;
  FClients := nil;
  FUdpServer := nil;
  FUdpThread := nil;
end;

procedure TOuchServer.DoLog(const Text: String; EventType: TEventType);
begin
  if FLogger <> nil then
    FLogger.LogMessage(Text, EventType);
end;

procedure TOuchServer.DoLogFmt(const Text: String; const Args: array of const; EventType: TEventType);
begin
  try
    DoLog(Str.Format(Text, Args), EventType);
  except
    DoLog(Text, EventType);
  end;
end;

procedure TOuchServer.OnListen(const Event: TSocketConnectionEvent);
var
  Socket: IClientSocketConnection;
begin
  while FConnection.Accept(Socket) do
  begin
    FClients.Add(TOuchClient.Create(Self, FDataMgr, Socket));
  end;
end;

procedure TOuchServer.OnListenFailed(const Event: TSocketConnectionEvent);
begin
  DoLogFmt('Error: no se puede abrir puerto %d', [FConnection.Parameters.Port], etError);
  DoRelease;
end;

procedure TOuchServer.Shutdown;
begin
  FConnection.DisconnectClients;
  FConnection.Cancel;
  FUdpServer.Cancel;

  Sil.OS.Wait.Single(FUdpThread, INFINITE);
  DoRelease;
end;

function TOuchServer.GetClients: IInterfaceList;
begin
  Result := FClients;
end;

procedure TOuchServer.Run(const Thread: IThread);
var
  Address: ISocketAddress;
  iSize: Integer;
  sBuffer: String;
  Packet: IProtocolPacket;
begin
  iSize := Sil.OS.Socket.Info.MaxUdpDatagramSize;
  SetLength(sBuffer, iSize);

  while true do
  begin
    iSize := FUdpServer.Stream.ReadFrom(sBuffer[1], Length(sBuffer), Address);
    if iSize <= 0 then Break;

    Packet := TProtocolPacket.Create(PChar(sBuffer), iSize);
  end;
end;

end.
