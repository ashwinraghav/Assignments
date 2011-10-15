unit list;

interface

{$include Defines.inc}

uses
  Sil,
  SilXml,
  SilLog;

const
  TM_RUNCLIENT = $400;
  TM_RUNSERVER = $401;

type
  IServer = interface
    ['{C5B54E9A-01C1-11D5-98B5-00104B0FA1EF}']
    function GetBindAddress: String;
    procedure SetBindAddress(const Value: String);
    function GetBindPort: Integer;
    procedure SetBindPort(Value: Integer);
    function GetSocket: ISocketServer;
    procedure SetSocket(const Value: ISocketServer);
    function GetClients: IInterfaceList;
    function GetAllowNames: IStringList;
    procedure Start;
    procedure Stop;
    property Socket: ISocketServer read GetSocket write SetSocket;
    property BindAddress: String read GetBindAddress write SetBindAddress;
    property BindPort: Integer read GetBindPort write SetBindPort;
    property Clients: IInterfaceList read GetClients;
    property AllowNames: IStringList read GetAllowNames;
  end;

  IBerfList = interface
    ['{EF0EC2E4-2E3B-4FB5-BB0A-6ED417AE5971}']
    function ReadFile(const FileName: String): Boolean;
    function FindServer(Port: Integer): IServer;
    function Start: Boolean;
    procedure Stop;
  end;

  IClient = interface
    ['{16A30887-8B36-43D1-93F9-BD9C5A3AD093}']
    function GetIsFinished: Boolean;
    procedure Start;
    procedure Stop;
    property IsFinished: Boolean read GetIsFinished;
  end;

  TClient = class (
    TSilInterfacedObject,
    IClient)
  private
    FServerThread: IThread;
    FClientThread: IThread;
    FServer: ISocketClient;
    FClient: ISocketClient;
    FServerStarted: IEvent;
    FIsFinished: Boolean;
    procedure DoProcess(const Src, Dst: ISocketClient);
  protected
    function GetIsFinished: Boolean;
    procedure Start;
    procedure Stop;
    property IsFinished: Boolean read GetIsFinished;
    procedure RunClient(var Msg: RThreadRunMessage); message TM_RUNCLIENT;
    procedure RunServer(var Msg: RThreadRunMessage); message TM_RUNSERVER;
  public
    constructor Create(const Server, Client: ISocketClient);
    destructor Destroy; override;
  end;

  TServer = class (
    // extends
    TSilInterfacedObject,
    // implements
    IServer,
    IRunnable)
  private
    FBindAddress: String;
    FBindPort: Integer;
    FServer: ISocketServer;
    FThread: IThread;
    FClients: IInterfaceList;
    FList: IBerfList;
    FAllowNames: IStringList;
    procedure DoStop(const Item: IClient);
    procedure DoCheck;
    procedure DoInit(const Entry: IServer; const Client: ISocketClient);
    function DoResolveAllowNames: TIntegerArray;
  protected
    procedure Run(const Thread: IThread);
  protected // IServer
    function GetBindAddress: String;
    procedure SetBindAddress(const Value: String);
    function GetBindPort: Integer;
    procedure SetBindPort(Value: Integer);
    function GetSocket: ISocketServer;
    procedure SetSocket(const Value: ISocketServer);
    function GetClients: IInterfaceList;
    function GetAllowNames: IStringList;
    procedure Start;
    procedure Stop;
  public
    constructor Create(const List: IBerfList);
    destructor Destroy; override;
  end;

  TBerfList = class (
    // extends
    TSilInterfacedObject,
    // implements
    IBerfList)
  private
    FConfig: IXmlTree;
    FServers: IInterfaceList;
    FPairs: IInterfaceList;
  private
    function FindServer(Port: Integer): IServer;
  protected // IBerfList
    function ReadFile(const FileName: String): Boolean;
    function Start: Boolean;
    procedure Stop;
  end;

implementation

uses SilBtStr, SilSiXml, SilLtList;

const
  CLogKey: String = '$System\SOFTWARE\Siderca\SIL\'; 

procedure DoInitLog;
begin
  SilLog.Logger.Initialize(CLogKey, SilLog.LogService, SilLog.StackFmt);
end;

procedure DoFinLog;
begin
  SilLog.Logger.Finalize;
end;

{ TBerfList }

function TBerfList.Start: Boolean;
var
  e: IEnumerator;
  sIP, sPort: String;
  iPos: Integer;
  Server: IServer;
  Node: IXmlNode;
  Tag, AllowTag: IXmlTag;
  LocalValue, LocalNic: String;
  LocalPort: Word;
begin
  DoInitLog;
  Sil.Trace.Enter('TBerfList.Start');

  FServers := Sil.List.InterfaceList(true);
  FPairs := Sil.List.InterfaceList(true);

  while FConfig.Root.Childs.Enumerate(e, Node) do
    if Node.NodeKind = nkTag then
    begin
      Tag := Node.AsTag;

      if Str.CompareText(Tag.Name, 'listen', true) = 0 then
      begin
        LocalValue := Tag.Arguments.ReadString('localport');

        if Str.Pos(':', LocalValue) = 0 then
        begin
          LocalNic := '';
          LocalPort := Str.ToInt(LocalValue);
        end else
          Str.Scan(LocalValue, '%s:%w', [@LocalNic, @LocalPort]);

        sIP := Tag.Arguments.ReadString('remotehost');
        iPos := Str.Pos(':', sIP);

        if Str.IsEmpty(sIP) or (LocalPort = 0) or (iPos = 0) then Continue;

        sPort := Str.Copy(sIP, iPos + 1);
        sIP := Str.Copy(sIP, 1, iPos - 1);

        Server := TServer.Create(Self);
        Server.Socket := Sil.OS.Socket.CreateServer(stStream, spTCP, LocalNic, LocalPort);
        Server.BindAddress := sIP;
        Server.BindPort := Str.ToInt(sPort, 0);

        FServers.Add(Server);
        Sil.Sink.Connect(Server.Socket, Self);

        if Tag.FindTag('allow', AllowTag) then
          Server.AllowNames.AddStrings(AllowTag.Data);

        Server.Start;
      end;
    end;

  Result := true;
  Sil.Trace.Leave;
end;

procedure TBerfList.Stop;
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  while FServers.Enumerate(Enum, Item) do
    (Item as IServer).Stop;

  FServers.Clear;
  FServers := nil;

  DoFinLog;
end;

function TBerfList.FindServer(Port: Integer): IServer;
var
  e: IEnumerator;
  Server: IUnknown;
begin
  while FServers.Enumerate(e, Server) do
  begin
    Result := (Server as IServer);
    if Result.Socket.Info.Local.Port = Port then Exit;
  end;
  Result := nil;
end;

function TBerfList.ReadFile(const FileName: String): Boolean;
begin
  Sil.Trace.Enter('TBerfList.ReadFile', [FileName]);

  try
    FConfig := SilXml.Tool.ReadFile(FileName, nil, fmAccessRead, fmShareRead, true);
    Result := true;
  except
    on e: Exception do
    begin
      Sil.Trace.Error(e);
      Result := false;
    end;
  end;

  Sil.Trace.Leave;
end;

{ TServer }

constructor TServer.Create(const List: IBerfList);
begin
  inherited Create;

  FList := List;
  FAllowNames := Sil.List.StringList;
end;

destructor TServer.Destroy;
begin
  FList := nil;
  inherited;
end;

function TServer.GetBindAddress: String;
begin
  Result := FBindAddress;
end;

function TServer.GetBindPort: Integer;
begin
  Result := FBindPort;
end;

function TServer.GetSocket: ISocketServer;
begin
  Result := FServer;
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

  FServer.Cancel;
  FThread.Termination.WaitFor;

  if Assigned(FClients) then
    while FClients.Enumerate(Enum, Item) do
      DoStop(Item);

  Sil.Trace.Leave;
end;

procedure TServer.DoInit(const Entry: IServer; const Client: ISocketClient);
var
  Server: ISocketClient;
  Item: IClient;
begin
  Sil.Trace.Enter('TServer.DoInit');

  Server := Sil.OS.Socket.CreateClient(stStream, spTcp, Entry.BindAddress, Entry.BindPort);
  Item := TClient.Create(Server, Client);
  FClients.Add(Item);
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

function TServer.DoResolveAllowNames: TIntegerArray;
var
  e: IEnumerator;
  Item: String;
  Addr: ISocketAddress;
begin
  SetLength(Result, 0);

  while FAllowNames.Enumerate(e, Item) do
  begin
    try
      Addr := Sil.OS.Socket.Host.GetByAddress(Item);
    except
      Addr := nil;
    end;

    if Assigned(Addr) and (Addr.Address > 0) then
      Int.ArrayAdd(Result, Addr.Address);
  end;
end;

procedure TServer.Run(const Thread: IThread);
var
  Client: ISocketClient;
  Entry: IServer;
  AllowIP: TIntegerArray;
begin
  Sil.Trace.Enter('TServer.Run');

  AllowIP := DoResolveAllowNames;

  try
    FServer.Listen;
  except
    on e: Exception do Sil.Trace.Error('1:' + e.Message);
  end;

  Entry := FList.FindServer(FServer.Info.Local.Port);

  while FServer.Accept(Client) do
    try
      if (Length(AllowIP) = 0) or (Int.ArrayFind(AllowIP, Client.Info.Remote.Address) >= 0) then
      begin
        DoInit(Entry, Client);
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

procedure TServer.SetBindAddress(const Value: String);
begin
  FBindAddress := Value;
end;

procedure TServer.SetBindPort(Value: Integer);
begin
  FBindPort := Value;
end;

procedure TServer.SetSocket(const Value: ISocketServer);
begin
  FServer := Value;
end;

function TServer.GetClients: IInterfaceList;
begin
  Result := FClients;
end;

function TServer.GetAllowNames: IStringList;
begin
  Result := FAllowNames;
end;

{ TClient }

constructor TClient.Create(const Server, Client: ISocketClient);
begin
  inherited Create;

  FServer := Server;
  FClient := Client;
end;

destructor TClient.Destroy;
begin
  FServer := nil;
  FClient := nil;
  FServerThread := nil;
  FClientThread := nil;

  inherited;
end;

function TClient.GetIsFinished: Boolean;
begin
  Result := FIsFinished;
end;

procedure TClient.Start;
begin
  Sil.Trace.Enter('TClient.Start');

  FServerStarted := Sil.OS.Ipc.Event;
  FServerThread := Sil.OS.Thread.Spawn(TM_RUNSERVER, Self);
  FServerStarted.WaitFor(INFINITE);
  FClientThread := Sil.OS.Thread.Spawn(TM_RUNCLIENT, Self);

  Sil.Trace.Leave;
end;

procedure TClient.Stop;
begin
  Sil.Trace.Enter('TClient.Stop');

  if Assigned(FServerThread) then
  begin
    FServer.Disconnect;
    FClient.Disconnect;

    FServerThread.Termination.WaitFor;
    FClientThread.Termination.WaitFor;
  end;

  Sil.Trace.Leave;
end;

procedure TClient.RunClient(var Msg: RThreadRunMessage);
begin
  Sil.Trace.Enter('TClient.RunClient');

  try
    DoProcess(FClient, FServer);
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  FServer.Disconnect;
  FIsFinished := true;
  Sil.Trace.Leave;
end;

procedure TClient.RunServer(var Msg: RThreadRunMessage);
begin
  Sil.Trace.Enter('TClient.RunServer');

  try
    try
      FServer.Connect;
    finally
      FServerStarted.Signal;
    end;

    DoProcess(FServer, FClient);
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  FClient.Disconnect;
  FIsFinished := true;
  Sil.Trace.Leave;
end;

procedure TClient.DoProcess(const Src, Dst: ISocketClient);
var
  Buffer: String;
  Size: Integer;
begin
  Sil.Trace.Enter('TClient.DoProcess', [Src.Info.Remote.Format]);

  try
    SetLength(Buffer, Src.Parameters.ReceiveBufferSize);

    while Src.IsConnected do
    begin
      Size := Src.Stream.Read(Buffer[1], Length(Buffer));

      if Size > 0 then
      begin
        Sil.Trace.Log('leido: %d', [Size]);

        //if Sil.Debug.Check(10) then
          //Sil.Trace.Log(sLineBreak + Str.Copy(Buffer, 1, Size));

        Dst.Stream.Write(Buffer[1], Size);
      end else
        Src.Disconnect;
    end;

    Dst.Disconnect;
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  Sil.Trace.Leave;
end;

end.

