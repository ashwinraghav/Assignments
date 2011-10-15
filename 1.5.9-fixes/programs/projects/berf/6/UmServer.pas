unit UmServer;

interface

{$include Defines.inc}

uses
  Sil,
  UiBerf;

type
  TServer = class (
    // extends
    TSilObject,
    // implements
    IServer,
    IRunnable)
  private
    FLocalAddress: LongWord;
    FLocalPort: Word;
    FRemoteAddress: LongWord;
    FRemotePort: Word;
    FAuth: IAuthInfo;
    FAcl: IAclInfo;
    FSocket: ISocketServer;
    FThread: IThread;
    FClients: IInterfaceList;
    FList: IBerfList;
    FRemote: String;
    procedure DoStop(const Item: IClient);
    procedure DoCheck;
    procedure DoInit(const Client: ISocketClient);
  protected
    procedure Run(const Thread: IThread);
  protected // IServer
    function GetLocal: String;
    function GetRemote: String;
    function GetAuth: IAuthInfo;
    function GetAcl: IAclInfo;
    procedure SetLocal(const Value: String);
    procedure SetRemote(const Value: String);
    procedure SetAuth(const Value: IAuthInfo);
    procedure SetAcl(const Value: IAclInfo);
    procedure Start;
    procedure Stop;
  public
    constructor Create(const List: IBerfList);
    destructor Destroy; override;
  end;

implementation

uses
  UmClient, SilOjSocket;

{ TServer }

constructor TServer.Create(const List: IBerfList);
begin
  inherited Create;
  FList := List;
end;

destructor TServer.Destroy;
begin
  FList := nil;
  inherited;
end;

procedure TServer.Start;
begin
  Sil.Trace.Enter('TServer.Start');

  FClients := Sil.List.InterfaceList(true);
  FThread := Sil.OS.Thread.Spawn(Self);

  Sil.Trace.Leave;
end;

procedure TServer.DoStop(const Item: IClient);
begin
  Item.Stop;
  FClients.Remove(Item);
end;

procedure TServer.Stop;
var
  Enum: IEnumerator;
  Item: IClient;
begin
  Sil.Trace.Enter('TServer.Stop');

  FSocket.Cancel;
  FThread.Termination.WaitFor;

  if Assigned(FClients) then
    while FClients.Enumerate(Enum, Item) do
      DoStop(Item);

  Sil.Trace.Leave;
end;

procedure TServer.Run(const Thread: IThread);
var
  Client: ISocketClient;
begin
  Sil.Trace.Enter('TServer.Run');

  try
    FSocket := Sil.OS.Socket.CreateServer(stStream, spTCP, FLocalAddress, FLocalPort);
    FSocket.Listen;
  except
    on e: Exception do Sil.Trace.Error('1:' + e.Message);
  end;

  while FSocket.Accept(Client) do
    try
      if Assigned(FAcl) and FAcl.IsValid(Client.Info.Remote) then
      begin
        DoInit(Client);
        DoCheck;
      end else
      begin
        Sil.Trace.Log('cliente no autorizado %s', [Client.Info.Remote.Format]);
        Client.Disconnect;
        Client := nil;
      end;
    except
      on e: Exception do Sil.Trace.Error('2:' + e.Message);
    end;

  Sil.Trace.Leave;
end;

procedure TServer.DoInit(const Client: ISocketClient);
var
  Server: ISocketClient;
  Item: IClient;
begin
  Sil.Trace.Enter('TServer.DoInit');

  Server := Sil.OS.Socket.CreateClient(stStream, spTcp, FRemoteAddress, FRemotePort);
  Item := TClient.Create(Self, Server, Client);

  FClients.Add(Item);

  if Assigned(FAuth) then
    FAuth.Port := FRemotePort;

  Item.Start;

  Sil.Trace.Leave;
end;

procedure TServer.DoCheck;
var
  Enum: IEnumerator;
  Item: IClient;
begin
  while FClients.Enumerate(Enum, Item) do
    if Item.IsFinished then
      DoStop(Item);
end;

function TServer.GetAcl: IAclInfo;
begin
  Result := FAcl;
end;

function TServer.GetAuth: IAuthInfo;
begin
  Result := FAuth;
end;

function TServer.GetLocal: String;
begin
  Result := '';
end;

function TServer.GetRemote: String;
begin
  Result := FRemote;
end;

procedure TServer.SetAcl(const Value: IAclInfo);
begin
  FAcl := Value;
end;

procedure TServer.SetAuth(const Value: IAuthInfo);
begin
  FAuth := Value;
end;

procedure DoParseAddress(const Value: String; var Server: String; var Address: LongWord; var Port: Word);
var
  Right: String;
begin
  Str.Split(Value, ':', Server, Right);

  if Str.IsEmpty(Server) then
    Address := 0
  else
    Address := Sil.OS.Socket.IP.FromStr(Server);

  if Str.IsEmpty(Right) then
    Port := 0
  else
    Port := Str.ToInt(Right);
end;

procedure TServer.SetLocal(const Value: String);
var
  Dummy: String;
begin
  DoParseAddress(Value, Dummy, FLocalAddress, FLocalPort);
end;

procedure TServer.SetRemote(const Value: String);
begin
  DoParseAddress(Value, FRemote, FRemoteAddress, FRemotePort);

  if FRemotePort = 0 then
    FRemotePort := FLocalPort;
end;

end.
