unit UmServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilHttp,
  SilLog,

  SilStNtlm,
  UiServer;

type
  TProxyInfo = class (TSilObject, IProxyInfo)
  private
    FServer: String;
    FPort: Word;
    FHost: String;
    FDomain: String;
    FPassword: String;
    FUser: String;
  protected
    function GetServer: String;
    function GetPort: Word;
    function GetHost: String;
    function GetDomain: String;
    function GetPassword: String;
    function GetUser: String;
    procedure SetServer(const Value: String);
    procedure SetPort(Value: Word);
    procedure SetHost(const Value: String);
    procedure SetDomain(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUser(const Value: String);
    Property Server: String read GetServer write SetServer;
    Property Port: Word read GetPort write SetPort;
    Property Host: String read GetHost write SetHost;
    Property Domain: String read GetDomain write SetDomain;
    Property Password: String read GetPassword write SetPassword;
    Property User: String read GetUser write SetUser;
  public
    constructor Create(const Server: String; Port: Word; const Host, Domain, User, Password: String);
  end;

  TServer = class (TSilObject, IServer, IRunnable)
  private
    FServer: ISocketServer;
    FThread: IThread;
    FRequests: IInterfaceList;
    FProxyInfo: IProxyInfo;
    FPort: Word;
  private
    procedure DoAddClient(const Client: ISocketClient);
  protected
    procedure Run(const Thread: IThread);
  protected
    function GetProxyInfo: IProxyInfo;
    function GetClients: IInterfaceList;
    procedure Start;
    procedure Stop;
    property ProxyInfo: IProxyInfo read GetProxyInfo;
    property Clients: IInterfaceList read GetClients;
  public
    constructor Create(const Port: Word; const ProxyInfo: IProxyInfo);
    destructor Destroy; override;
  end;

  TRequest = class (TSilObject, IRunnable)
  private
    FFiler: IHttpFiler;
    FThread: IThread;
    FSocket: ISocketClient;
    FServer: IServer;
  private
    procedure DoRequest(const Request: IHttpRequest);
  protected
    procedure Run(const Thread: IThread);
  public
    constructor Create(const Socket: ISocketClient; const Server: IServer);
    destructor Destroy; override;
  end;

implementation

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

{ TServer }

constructor TServer.Create(const Port: Word; const ProxyInfo: IProxyInfo);
begin
  DoInitLog;
  
  inherited Create;

  FPort := Port;
  FProxyInfo := ProxyInfo;
  FRequests := Sil.List.InterfaceList(true);
end;

destructor TServer.Destroy;
begin
  FProxyInfo := nil;
  FRequests := nil;

  inherited;

  DoFinLog;
end;

procedure TServer.Start;
begin
  FThread := Sil.OS.Thread.Spawn(Self);
end;

procedure TServer.Stop;
begin
  if Assigned(FThread) then
  begin
    FServer.Disconnect;
    FThread.Termination.WaitFor;
  end;

  FRequests.Clear;
end;

procedure TServer.Run(const Thread: IThread);
var
  Client: ISocketClient;
begin
  FServer := Sil.OS.Socket.CreateServer(stStream, spTCP, 0, FPort);
  FServer.Listen;

  while FServer.Accept(Client) do
    DoAddClient(Client);
end;

procedure TServer.DoAddClient(const Client: ISocketClient);
var
  Request: IUnknown;
begin
  Request := TRequest.Create(Client, Self);
  FRequests.Add(Request);
end;

function TServer.GetClients: IInterfaceList;
begin
  Result := FRequests;
end;

function TServer.GetProxyInfo: IProxyInfo;
begin
  Result := FProxyInfo;
end;

{ TRequest }

constructor TRequest.Create(const Socket: ISocketClient; const Server: IServer);
begin
  inherited Create;

  FServer := Server;
  FSocket := Socket;
  FFiler := SilHttp.Tk.Filer;
  FThread := Sil.OS.Thread.Spawn(Self);
end;

destructor TRequest.Destroy;
begin
  FSocket := nil;
  FFiler := nil;
  FServer := nil;
  
  FThread.Detach;
  FThread := nil;

  inherited;
end;

procedure TRequest.Run(const Thread: IThread);
var
  Request: IHttpRequest;
begin
  Sil.Trace.Enter('TRequest.Run');

  try   
    if FSocket.WaitFor([ssRead]) then
      FFiler.Read(FSocket.Stream, Request);

    DoRequest(Request);
  except
    on e: Exception do
      Sil.Trace.Error('run:1 ' + e.Message);
  end;

  try
    if FSocket.IsConnected then FSocket.Disconnect;
    FServer.Clients.Remove(Self);
  except
    on e: Exception do
      Sil.Trace.Error('run:2 ' + e.Message);
  end;

  Sil.Trace.Exit;
end;

procedure TRequest.DoRequest(const Request: IHttpRequest);
var
  Pos: Integer;
  Msg, Msg1, Msg2, Msg3: String;
  Proxy: ISocketClient;
  Response: IHttpResponse;


  function DoReceive(const Src, Dst: ISocketClient): Boolean;
  var
    Buffer: String;
    Size: LongWord;
  begin
    SetLength(Buffer, Src.Parameters.ReceiveBufferSize);
    Size := Src.Stream.Read(Buffer[1], Length(Buffer));

    Result := Size > 0;
    if Result then
      Dst.Stream.Write(Buffer[1], Size);
  end;

  procedure DoProxyConnected;
  begin
    while Proxy.IsConnected and FSocket.IsConnected do
    begin
      if DoReceive(Proxy, FSocket) then
        if not DoReceive(FSocket, Proxy) then
          Break;
    end;
  end;


  procedure DoProxy;
  var
    ProxyResponse: IHttpResponse;
  begin
    FFiler.Read(Proxy.Stream, ProxyResponse);
    FFiler.Write(FSocket.Stream, ProxyResponse);
  end;

begin
  Sil.Trace.Enter('DoRequest');

  Proxy := Sil.OS.Socket.CreateClient;
  Proxy.Connect(FServer.ProxyInfo.Server, FServer.ProxyInfo.Port);
  Proxy.Parameters.ReadTimeout := 30000;

  Msg1 := Ntlm.Message1(FServer.ProxyInfo.Host, FServer.ProxyInfo.Domain);

  Sil.Trace.Log('request:' + ccCRLF + FFiler.ToStr(Request));

  Request.Tags.List['Proxy-Authorization'].Value := 'NTLM ' + Msg1;

  FFiler.Write(Proxy.Stream, Request);

  Proxy.WaitFor([ssRead]);
  FFiler.Read(Proxy.Stream, Response);

  Msg := Response.Tags.List['Proxy-Authenticate'].Value;
  Pos := Str.Pos('NTLM ', Msg);

  if Pos > 0 then
  begin
    Msg2 := Ntlm.Message2(Str.Copy(Msg, Pos + 5));
    Msg3 := Ntlm.Message3(FServer.ProxyInfo.Domain, FServer.ProxyInfo.Host, FServer.ProxyInfo.User, FServer.ProxyInfo.Password, Msg2);

    Request.Tags.List['Proxy-Authorization'].Value := 'NTLM ' + Msg3;

    FFiler.Write(Proxy.Stream, Request);

    //if Request.Tags.List.IsPresent('Proxy-Connection') and
      //(Request.Header.List['Proxy-Connection'].Value = 'Keep-Alive') then
      //DoProxyConnected else
      DoProxy;
  end;

  Sil.Trace.Exit;
end;

{ TProxyInfo }

constructor TProxyInfo.Create(const Server: String; Port: Word; const Host, Domain, User, Password: String);
begin
  inherited Create;

  FServer := Server;
  FPort := Port;
  FHost := Host;
  FDomain := Domain;
  FUser := User;
  FPassword := Password;
end;

function TProxyInfo.GetDomain: String;
begin
  Result := FDomain;
end;

function TProxyInfo.GetHost: String;
begin
  Result := FHost;
end;

function TProxyInfo.GetPassword: String;
begin
  Result := FPassword;
end;

function TProxyInfo.GetPort: Word;
begin
  Result := FPort;
end;

function TProxyInfo.GetServer: String;
begin
  Result := FServer;
end;

function TProxyInfo.GetUser: String;
begin
  Result := FUser;
end;

procedure TProxyInfo.SetDomain(const Value: String);
begin
  FDomain := Value;
end;

procedure TProxyInfo.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TProxyInfo.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TProxyInfo.SetPort(Value: Word);
begin
  FPort := Value;
end;

procedure TProxyInfo.SetServer(const Value: String);
begin
  FServer := Value;
end;

procedure TProxyInfo.SetUser(const Value: String);
begin
  FUser := Value;
end;

end.
