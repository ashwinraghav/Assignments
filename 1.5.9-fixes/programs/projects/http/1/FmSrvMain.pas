unit FmSrvMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, SilTool, SilUrl, SilLog, SilHttp;

const
  CMsgFirst = EV_FIRST;

const
  CListener = CMsgFirst + 1;
  CClient   = CMsgFirst + 2;

const
  CHttpBase = CMsgFirst + 10;
  CHttpGet  = CHttpBase + Ord(httpGet);

const
  CMsgLast  = CHttpBase + Ord(High(THttpMethod));

// D:\DATA\Doc\INFO\NTDLL\ntdll.htm

type
  IHttpSession = interface;
  
  IHttpServer = interface
    ['{D62BDAB9-946D-43DE-B331-4958817557FF}']
    procedure Add(const Session: IHttpSession);
    procedure Remove(const Session: IHttpSession);
    function Process(const Session: IHttpSession; const Request: IHttpRequest): IHttpResponse;
  end;
  
  IHttpSession = interface (IObject)
    ['{458E9115-ED4F-4505-BB89-457AECEF6A22}']
    function GetSocket: ISocketClient;
    property Socket: ISocketClient read GetSocket;
  end;

  RHttpMsg = record
    ID: Integer;
    Sender: IHttpSession;
    Query: IHttpRequest;
    Reply: IHttpResponse;
  end;

  TFormHttpServer = class(
    TForm,
    IHttpServer,
    IDispatchable )
    lbClients: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: ISocketServer;
    FClients: IInterfaceList;
    FListener: IThread;
    FRoot: string;
  private
    procedure Listener(var Msg: RThreadRunMessage); message CListener;
    procedure HttpUnknown(var Msg: RHttpMsg); 
    procedure HttpGet(var Msg: RHttpMsg); message CHttpGet;
  private
    procedure DoNewClient(const Socket: ISocketClient);
    procedure DoVclAddClient(const Sender: IUnknown; Param: Pointer);
    procedure DoVclRemoveClient(const Sender: IUnknown; Param: Pointer);
    function DoOpenFile(const Url: IUrl): IFile;
    function DoGetType(const Data: IFile): string;
  protected // IHttpServer
    procedure Add(const Session: IHttpSession);
    procedure Remove(const Session: IHttpSession);
    function Process(const Session: IHttpSession; const Operation: IHttpRequest): IHttpResponse;
  public
    procedure DefaultHandler(var Msg); override;
  end;

  THttpSession = class(
    TSilObject,
    IHttpSession )
  private
    FHttp: IHttpFiler;
    FServer: IHttpServer;
    FSocket: ISocketClient;
    FThread: IThread;
  private
    procedure ThreadSession(var Msg: RThreadRunMessage); message CClient;
    procedure DoSend(const Data: IHttpResponse);
  protected // IHttpSession
    function GetSocket: ISocketClient;
  public
    constructor Create(const Server: IHttpServer; const Socket: ISocketClient);
    destructor Destroy; override;
  end;

var
  FormHttpServer: TFormHttpServer;

implementation

{$R *.dfm}

{ TFormHttpServer }

procedure TFormHttpServer.FormCreate(Sender: TObject);
begin
  SilLog.Logger.Initialize('$SYSTEM\SOFTWARE\SIDERCA\SIL', LogService, StackFmt, ClassType);

  FRoot := 'D:\';

  FClients := Sil.List.InterfaceList(True);
  FServer := Sil.Os.Socket.CreateServer(stStream, spTCP, 0, 80);
  FServer.Parameters.ReadTimeout := 1000;
  FServer.Parameters.WriteTimeout := 1000;
  FServer.Listen;
  
  FListener := Sil.Os.Thread.Spawn(CListener, 'listener', Self);
end;

procedure TFormHttpServer.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    FServer.Disconnect;
    if Assigned(FListener) then
      FListener.Termination.WaitFor();
  end;

  FListener := nil;
  FServer := nil;
  FClients := nil;
  
  SilLog.Logger.Finalize;
end;

procedure TFormHttpServer.Add(const Session: IHttpSession);
begin
  FClients.Add(Session);
  Sil.Os.Thread.SyncCall(DoVclAddClient, Session);
end;

procedure TFormHttpServer.Remove(const Session: IHttpSession);
begin
  Sil.Os.Thread.SyncCall(DoVclRemoveClient, Session);
  FClients.Remove(Session);
end;

function TFormHttpServer.Process(const Session: IHttpSession; const Operation: IHttpRequest): IHttpResponse;
var
  Msg: RHttpMsg;
begin
  Sil.Trace.Enter('TFormHttpServer.Process');
  try
    Msg.ID := CHttpBase + Ord(Operation.Request.Method);
    Msg.Sender := Session;
    Msg.Query := Operation;
    Msg.Reply :=  SilHttp.Tk.Response();
    Dispatch(Msg);
    Result := Msg.Reply;
  except
    Sil.Trace.Exception('');
  end;
  Sil.Trace.Leave;
end;

procedure TFormHttpServer.HttpGet(var Msg: RHttpMsg);
var
  Url: IUrl;
  Resource: IFile;
  Stream: IMemoryStream;
begin
  Sil.Trace.Enter('TFormHttpServer.HttpGet');
  try
    Url := Msg.Query.Request.Resource.Url;
    
    Sil.Trace.Log('GET %s from %s', [Url.Text, Msg.Sender.Socket.Info.Remote.Format()]);

    Resource := DoOpenFile(Url);

    Msg.Reply.Status.Value := hstOK;
    Msg.Reply.Tags.Server.Value := 'httpsrv/1.00 sil/1.5.0 (Sil Http Server V1.00)';
    
    if not Msg.Query.Header.List.IsPresent(httConnection) or (Str.CompareText(Msg.Query.Header.Connection.Value, 'Keep-Alive', True) <> 0) then
      Msg.Reply.Header.Connection.Value := 'close' else
      Msg.Reply.Entity.Tags.ContentLength.Value := Resource.Info.Size;
      
    Msg.Reply.Entity.Tags.ContentType.Value := DoGetType(Resource);
    Stream := Msg.Reply.Entity.Stream;
    Stream.Size := Resource.Info.Size;
    Resource.Stream.Read(Stream.Memory^, Stream.Size);

  except
    raise;
  end;
  Sil.Trace.Leave;
end;

procedure TFormHttpServer.HttpUnknown(var Msg: RHttpMsg);
begin
  Sil.Trace.Enter('TFormHttpServer.HttpGet');
  try
    Msg.Reply.Status.Value := hseNotImplemented;
    Msg.Reply.Tags.Server.Value := 'httpsrv/1.00 sil/1.5.0 (Sil Http Server V1.00)';
    Msg.Reply.Header.Connection.Value := 'close';
  except
    raise;
  end;
  Sil.Trace.Leave;
end;

procedure TFormHttpServer.Listener(var Msg: RThreadRunMessage);
var
  Session: ISocketClient;
begin
  Sil.Trace.Enter('TFormHttpServer.Listener');
  try

    while Assigned(FServer) and FServer.Accept(Session) do
      DoNewClient(Session);

  except
    Sil.Trace.Exception('');
  end;
  Sil.Trace.Leave;
end;

procedure TFormHttpServer.DoNewClient(const Socket: ISocketClient);
begin
  THttpSession.Create(Self, Socket);
end;

procedure TFormHttpServer.DoVclAddClient(const Sender: IInterface; Param: Pointer);
var
  Session: IHttpSession;
begin
  Session := Sender as IHttpSession; 
  lbClients.Items.AddObject(Session.Socket.Info.Remote.Format(), Session.Instance);
end;

procedure TFormHttpServer.DoVclRemoveClient(const Sender: IInterface; Param: Pointer);
var
  Session: IHttpSession;
  Index: Integer;
begin
  Session := Sender as IHttpSession;
  Index := lbClients.Items.IndexOfObject(Session.Instance);
  if Index >= 0 then
    lbClients.Items.Delete(Index);
end;

function TFormHttpServer.DoOpenFile(const Url: IUrl): IFile;
var
  Path: string;  
begin
  Path := Url.Path.Text;

  if Str.FirstChar(Path) in ['/', '\'] then
    Path := Str.Copy(Path, 2);
    
  Path := Sil.Os.Filesystem.AddSlash(FRoot) + Sil.Os.Filesystem.TranslatePath(Path);

  Result := Sil.Os.Filesystem.OpenFile(Path, fmAccessRead, fmShareRead, True);
end;

function TFormHttpServer.DoGetType(const Data: IFile): string;
var
  Kind: string;
begin
  Kind := Sil.Os.Filesystem.GetFileExt(Data.Info.FullName);
  Result := Sil.Os.Registry.Read('$CLASSES\' + Kind, 'Content Type');
end;

procedure TFormHttpServer.DefaultHandler(var Msg);
begin
  if (CMsgFirst <= Word(Msg)) and (Word(Msg) <= CMsgLast) then
    HttpUnknown(RHttpMsg(Msg)) else
    inherited;
end;

{ THttpSession }

constructor THttpSession.Create(const Server: IHttpServer; const Socket: ISocketClient);
begin
  inherited Create;
  FHttp := SilHttp.Tk.Filer;
  FServer := Server;
  FSocket := Socket;
  FThread := Sil.Os.Thread.Spawn(CClient, 'session', Self);
end;

destructor THttpSession.Destroy;
begin
  FThread := nil;
  FSocket := nil;
  FServer := nil;
  FHttp := nil;
  inherited;
end;

procedure THttpSession.ThreadSession(var Msg: RThreadRunMessage);
var
  Request: IHttpRequest;
  Response: IHttpResponse;
begin
  Sil.Trace.Enter('THttpSession.ThreadSession');
  try

    FServer.Add(Self);
    try
                                      
      while FSocket.IsConnected and FSocket.WaitFor([ssRead]) do
      begin
        Sil.Trace.Log('new request');
        
        FHttp.Read(FSocket.Stream, Request);
        Sil.Trace.Log('request parsed');
        
        Response := FServer.Process(Self, Request);
        Sil.Trace.Log('request processed');

        DoSend(Response);        

        Sil.Trace.Log('response sent');

        if not Request.Header.List.IsPresent(httConnection) or (Request.Header.Connection.Value <> 'Keep-Alive') then
        begin
          FSocket.Disconnect;
          Sil.Trace.Log('closing connection');
        end;
      end;
    finally
      FServer.Remove(Self);
    end;

  except
    Sil.Trace.Exception('');
  end;
  Sil.Trace.Leave();
end;

function THttpSession.GetSocket: ISocketClient;
begin
  Result := FSocket;
end;

procedure THttpSession.DoSend(const Data: IHttpResponse);
var
  Buffer: IMemoryStream;
  P: PChar;
  Size, Remaining, Written: LongWord;
begin
  Sil.Trace.Enter('THttpSession.DoSend');
  try

    if Assigned(Data) then
    begin
      Buffer := Sil.MemoryStream.Create();

      FHttp.Write(Buffer, Data);

      Remaining := Buffer.Size;
      P := Buffer.Memory;

      while Remaining > 0 do
      begin
        if Remaining > 1024 then
          Size := 1024 else
          Size := Remaining; 
        Written := FSocket.Stream.Write(P^, Size);
        if Written = 0 then
          Break;
        Dec(Remaining, Written);
        Inc(P, Written);
      end;
    end;
  
  except
    Sil.Trace.Exception('error sending response');
  end;
  Sil.Trace.Leave();
end;

end.
