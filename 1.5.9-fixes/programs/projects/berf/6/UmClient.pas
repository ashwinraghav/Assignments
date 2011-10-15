unit UmClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilHttp,
  SilNtlm,
  UiBerf;

type
  TClient = class (
    TSilObject,
    IClient)
  private
    FServerThread: IThread;
    FClientThread: IThread;
    FServer: ISocketClient;
    FClient: ISocketClient;
    FServerStarted: IEvent;
    FIsFinished: Boolean;
    FFiler: IHttpFiler;
    FAuth: IAuthInfo;
    FOwner: IServer;
    FLocalHost: String;
    procedure DoProcess(const Src, Dst: ISocketClient; FromClient: Boolean);
    procedure DoRequest(const Src, Dst: ISocketClient);
  protected
    function GetIsFinished: Boolean;
    procedure Start;
    procedure Stop;
    property IsFinished: Boolean read GetIsFinished;
    procedure RunClient(var Msg: RThreadRunMessage); message TM_RUNCLIENT;
    procedure RunServer(var Msg: RThreadRunMessage); message TM_RUNSERVER;
  public
    constructor Create(const Owner: IServer; const Server, Client: ISocketClient);
    destructor Destroy; override;
  end;

implementation

uses SilLtStream;

{ TClient }

constructor TClient.Create(const Owner: IServer; const Server, Client: ISocketClient);
begin
  inherited Create;

  FServer := Server;
  FClient := Client;
  FLocalHost := Sil.OS.Computer.Local.Name;
  FOwner := Owner;

  if Assigned(Owner.Auth) and Sil.Text.IsEqual(Owner.Auth.Method, 'ntlm') then
  begin
    FFiler := SilHttp.Tk.Filer;
    FAuth := Owner.Auth;
  end;
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

  if FAuth = nil then
  begin
    FServerStarted := Sil.OS.Ipc.Event;
    FServerThread := Sil.OS.Thread.Spawn(TM_RUNSERVER, Self);
    FServerStarted.WaitFor(INFINITE);
  end;

  FClientThread := Sil.OS.Thread.Spawn(TM_RUNCLIENT, Self);

  Sil.Trace.Leave;
end;

procedure TClient.Stop;
begin
  Sil.Trace.Enter('TClient.Stop');

  if Assigned(FServer) then FServer.Disconnect;
  if Assigned(FClient) then FClient.Disconnect;

  if Assigned(FServerThread) then FServerThread.Termination.WaitFor;
  if Assigned(FClientThread) then FClientThread.Termination.WaitFor;

  Sil.Trace.Leave;
end;

procedure TClient.RunClient(var Msg: RThreadRunMessage);
begin
  Sil.Trace.Enter('TClient.RunClient');

  try
    DoProcess(FClient, FServer, true);
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

    DoProcess(FServer, FClient, false);
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  FClient.Disconnect;
  FIsFinished := true;
  Sil.Trace.Leave;
end;

procedure TClient.DoProcess(const Src, Dst: ISocketClient; FromClient: Boolean);
var
  Buffer: String;
  Size: Integer;
begin
  Sil.Trace.Enter('TClient.DoProcess', [Src.Info.Remote.Format]);

  try
    SetLength(Buffer, Src.Parameters.ReceiveBufferSize);

    if FromClient and Assigned(FAuth) then
    begin
      DoRequest(Src, Dst);
      if Src.IsConnected then Src.Disconnect;
    end else
      while Src.IsConnected do
      begin
        Size := Src.Stream.Read(Buffer[1], Length(Buffer));

        if Size > 0 then
          Dst.Stream.Write(Buffer[1], Size)
        else
          Src.Disconnect;
      end;

    Dst.Disconnect;
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  Sil.Trace.Leave;
end;

procedure TClient.DoRequest(const Src, Dst: ISocketClient);
var
  Request: IHttpRequest;
  Response: IHttpResponse;
  Pos: Integer;
  Msg, Msg1, Msg2, Msg3: String;

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
    while Dst.IsConnected and Src.IsConnected do
    begin
      if DoReceive(Dst, Src) then
        if not DoReceive(Src, Dst) then
          Break;
    end;
  end;

  procedure DoProxy;
  var
    ProxyResponse: IHttpResponse;
  begin
    if Dst.WaitFor([ssRead], 3000) then
    begin
      FFiler.Read(Dst.Stream, ProxyResponse);
      FFiler.Write(Src.Stream, ProxyResponse);
    end;

    (*)while true do
    begin
      FFiler.Read(Dst.Stream, ProxyResponse);
      FFiler.Write(Src.Stream, ProxyResponse);

      if not Dst.WaitFor([ssRead], 500) then Break;
    end;(*)
  end;

begin
  Sil.Trace.Enter(Self, 'DoRequest');

  Dst.Connect(FOwner.Remote, FOwner.Auth.Port);
  Dst.Parameters.ReadTimeout := 30000;

  while true do
  begin
    if not Src.WaitFor([ssRead], 3000) then Exit;
    FFiler.Read(Src.Stream, Request);

    Msg1 := Ntlm.Message1(FOwner.Remote, FOwner.Auth.Domain);

    Sil.Trace.Log('request:' + ccCRLF + FFiler.ToStr(Request));

    Request.Tags.List['Proxy-Authorization'].Value := 'NTLM ' + Msg1;

    FFiler.Write(Dst.Stream, Request);

    if not Dst.WaitFor([ssRead], 3000) then Exit;
    FFiler.Read(Dst.Stream, Response);

    Msg := Response.Tags.List['Proxy-Authenticate'].Value;
    Pos := Str.Pos('NTLM ', Msg);

    if Pos > 0 then
    begin
      Msg2 := Ntlm.Message2(Str.Copy(Msg, Pos + 5));
      Msg3 := Ntlm.Message3(FOwner.Auth.Domain, FLocalHost, FOwner.Auth.User, FOwner.Auth.Password, Msg2);

      Request.Tags.List['Proxy-Authorization'].Value := 'NTLM ' + Msg3;

      FFiler.Write(Dst.Stream, Request);
      DoProxy;

      //if not Request.Tags.List.IsPresent('Proxy-Connection') or not Sil.Text.IsEqual(Request.Header.List['Proxy-Connection'].Value, 'Keep-Alive') then
        Break;
    end;
  end;

  Sil.Trace.Exit;
end;

end.
