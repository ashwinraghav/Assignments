unit UmLogan;

interface

{$include Defines.inc}

uses
  Sil,
  SilLog,

  UiLogan;

const
  CRootKey  = '$System\SOFTWARE\SIL';

  CSYSID    = 'Logan - Servidor de log';

  TH_BASE   = $400;
  TH_SOCKET = TH_BASE + 1;
  TH_LOGGER = TH_BASE + 2;
  TH_CCHECK = TH_BASE + 3;

type
  TLogan = class (TSilObject, ILogan)
  private
    FServer: ISocketClient;
    FSocketThread: IThread;
    FLoggerThread: IThread;
    FCCheckThread: IThread;
    FCCheckClose: IEvent;
    FQueue: IInterfaceQueue;
    FClients: IInterfaceList;
    FTimeout: LongWord;
    function DoGetClient(const ModuleName: String): ILoganClient;
    procedure DoWrite(const Packet: IPacket);
    procedure DoClientCheck;
  private
    procedure DoFinalizeLog;
    procedure DoInitializeLog;
    procedure DoReadConfig;
    procedure DoShutdown;
  protected
    procedure SocketRun(var Msg: RThreadRunMessage); message TH_SOCKET;
    procedure LoggerRun(var Msg: RThreadRunMessage); message TH_LOGGER;
    procedure CCheckRun(var Msg: RThreadRunMessage); message TH_CCHECK;
  protected
    procedure Start;
    procedure Stop;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLoganClient = class (TSilObject, ILoganClient)
  private
    FModuleName: String;
    FLastMsg: TDateTime;
    FTimeout: TDateTime;
    FLogger: ILogService;
  protected
    function GetModuleName: String;
    function IsOnline: Boolean;
    procedure Write(const Packet: IPacket);
    property ModuleName: String read GetModuleName;
  public
    constructor Create(const ModuleName: String; Timeout: LongWord);
    destructor Destroy; override;
  end;

implementation

uses SilLtList, SilOtFile;

{ TLogan }

constructor TLogan.Create;
begin
  inherited Create;

  DoInitializeLog;
  DoReadConfig;
end;

destructor TLogan.Destroy;
begin
  DoShutdown;
  DoFinalizeLog;

  inherited;
end;

procedure TLogan.DoInitializeLog;
begin
  SilLog.Logger.Initialize(CRootKey, SilLog.LogService, SilLog.StackFmt);

  Sil.Trace.Enter(CSYSID);
  Sil.Trace.Log('version: %s', [Date.ToStr((Sil.OS.Module.Current as IModule2).Info.Time, 'yymmdd-hhnn')]);
  Sil.Trace.Leave;
end;

procedure TLogan.DoFinalizeLog;
begin
  SilLog.Logger.Finalize;
end;

procedure TLogan.DoReadConfig;
var
  Tree: IXmlTree;
  FileName, Address: String;
  Port: Word;
begin
  Sil.Trace.Enter('DoReadConfig');

  try
    FileName := Sil.OS.Module.Current.Path + 'logan.xml';
    Tree := Sil.Xml.ReadFile(FileName);

    with Tree.Root.AsTag.GetTag('listen', true) do
    begin
      Address := Arguments.ReadString('address', '', true);
      Port := Arguments.ReadInteger('port', 24557, true);
    end;

    with Tree.Root.AsTag.GetTag('params', true) do
    begin
      FTimeout := Arguments.ReadInteger('timeout', 60, true);
    end;

    FServer := Sil.OS.Socket.CreateClient;
    FServer.Bind(stDatagram, spUDP, Address, Port);
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  Sil.Trace.Leave;
end;

procedure TLogan.Start;
begin
  Sil.Trace.Enter('TLogan.Start');

  try
    FClients := Sil.List.InterfaceList(true);
    FQueue := Sil.List.InterfaceQueue;
    FCCheckClose := Sil.OS.Ipc.Event;

    FSocketThread := Sil.OS.Thread.Spawn(TH_SOCKET, 'socket', Self);
    FLoggerThread := Sil.OS.Thread.Spawn(TH_LOGGER, 'logger', Self);
    FCCheckThread := Sil.OS.Thread.Spawn(TH_CCHECK, 'ccheck', Self);
  except
    on e: Exception do Sil.Trace.Error(e);
  end;

  Sil.Trace.Leave;
end;

procedure TLogan.DoShutdown;
begin
  Sil.Trace.Enter('TLogan.DoShutdown');

  if Assigned(FCCheckClose) then FCCheckClose.Signal;
  if Assigned(FServer) then FServer.Disconnect;
  if Assigned(FQueue) then FQueue.Cancel;
  if Assigned(FSocketThread) then FSocketThread.Termination.WaitFor(INFINITE, true);
  if Assigned(FLoggerThread) then FLoggerThread.Termination.WaitFor(INFINITE, true);
  if Assigned(FCCheckThread) then FCCheckThread.Termination.WaitFor(INFINITE, true);
  if Assigned(FClients) then FClients.Clear;

  FSocketThread := nil;
  FServer := nil;
  FQueue := nil;
  FClients := nil;

  Sil.Trace.Leave;
end;

procedure TLogan.Stop;
begin
  Sil.Trace.Enter('TLogan.Stop');
  DoShutdown;
  Sil.Trace.Leave;
end;

procedure TLogan.SocketRun(var Msg: RThreadRunMessage);
var
  Address: ISocketAddress;
  Size: LongWord;
  Buffer: String;
  Packet: IPacket;
begin
  SetLength(Buffer, FServer.Parameters.SendBufferSize);

  repeat
    Size := FServer.Stream.ReadFrom(Buffer[1], Length(Buffer), Address);

    if Size > 0 then
    begin
      Packet := Sil.Stream.Typed.Packet;
      Packet.Buffer.Write(Buffer[1], Size);
      Packet.Buffer.Position := 0;
      FQueue.Put(Packet);
    end;
  until Size = 0;
end;

procedure TLogan.LoggerRun(var Msg: RThreadRunMessage);
var
  Ver: Word;
  Packet: IPacket;
begin
  while FQueue.Get(IPacket, Packet) do
    try
      Ver := Packet.Reader.ReadWord;
      if Ver = 1 then DoWrite(Packet);
    except
      // log
    end;
end;

function TLogan.DoGetClient(const ModuleName: String): ILoganClient;
var
  Enum: IEnumerator;
begin
  while FClients.Enumerate(Enum, Result) do
    if Result.ModuleName = ModuleName then
      Exit;

  Result := TLoganClient.Create(ModuleName, FTimeout);
  FClients.Add(Result);
end;

procedure TLogan.DoWrite(const Packet: IPacket);
var
  Client: ILoganClient;
begin
  Client := DoGetClient(Packet.Reader.ReadString);
  Client.Write(Packet);
end;

procedure TLogan.CCheckRun(var Msg: RThreadRunMessage);
begin
  while FCCheckClose.WaitFor(60000) = wrTimeout do
    DoClientCheck;
end;

procedure TLogan.DoClientCheck;
var
  Enum: IEnumerator;
  Client: ILoganClient;
begin
  try
    while FClients.Enumerate(Enum, Client) do
      if not Client.IsOnline then
        FClients.Delete(Enum.Iteration);
  except
    on e: Exception do
      Sil.Trace.Error('TLogan.DoClientCheck:' + e.Message);
  end;
end;

{ TLoganClient }

constructor TLoganClient.Create(const ModuleName: String; Timeout: LongWord);
var
  Params: IParameterList;
begin
  Sil.Trace.Enter('TLoganClient.Create ', [ModuleName]);
  inherited Create;

  FModuleName := ModuleName;
  FTimeout := DateTime.FromSecs(Timeout);
  FLastMsg := DateTime.Now;

  Params := SilLog.LogService.ReadConfig(CRootKey + '\logger\parameters');
  Params[CLogFileName] := Sil.OS.FSys.GetFilePath(Params[CLogFileName]) + Sil.OS.FSys.ChangeFileExt(ModuleName, '.log');

  FLogger := SilLog.LogService.Create as ILogService;
  FLogger.Params := Params;

  Sil.Trace.Leave;
end;

destructor TLoganClient.Destroy;
begin
  Sil.Trace.Enter('TLoganClient.Destroy');
  inherited;
  Sil.Trace.Leave;
end;

function TLoganClient.GetModuleName: String;
begin
  Result := FModuleName;
end;

function TLoganClient.IsOnline: Boolean;
begin
  Result := DateTime.Now - FLastMsg < FTimeout;
end;

procedure TLoganClient.Write(const Packet: IPacket);
var
  Text, Sender, Category: String;
  Kind: TTraceKind;
  Stamp: TDateTime;
  Data: ITraceData;
begin
  FLastMsg := DateTime.Now;

  Packet.Reader.Read(Kind, SizeOf(TTraceKind));
  Packet.Reader.ReadLongWord;
  Packet.Reader.ReadInteger;
  Stamp := Packet.Reader.ReadDate;
  Text := Packet.Reader.ReadString;
  Sender := Packet.Reader.ReadString;
  Category := Packet.Reader.ReadString;

  FLogger.Log(Stamp, Kind, Data, Text, Sender, Category);
end;

end.
