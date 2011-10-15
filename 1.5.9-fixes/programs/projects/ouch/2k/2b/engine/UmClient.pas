unit UmClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer,
  SilCoder,
  SilData,

  UtLog,
  UiProtocol,
  UiClient,
  UiKeepAlive,
  UmKeepAlive,
  UiConnection,
  UmConnection;

type
  TClient = class (
    TSilObject,
    IClient,
    IClientSideEvents,
    IConnectionEvents,
    IConnectionClientEvents)
  private
    FConnection: IConnection;
    FPendings: IInterfaceList;
    FProtocol: IClientSide;
    FHook: Pointer;
    FIsOnline: Boolean;
    FData: IData;
    FRootPath: String;
    FConfig: IXmlTree;
    FNick: String;
    FPassword: String;
    FId: TGuid;
    FServerName: String;
    FServerPort: Word;
    FConfigFile: String;
    FFileProtocol: IClientSideFileProtocol;
    FLastAccount: TGuid;
  private
    function DoReadList(const NodeName: String): IParameterList;
    function DoGetBasePath(const Id: TGuid): string;
    procedure DoWriteList(const NodeName: String; const List: IParameterList);
    procedure DoClean;
    procedure DoCheckUser(const User: TUser);
    procedure DoUpdateUser(const ID: TGUID; out Info: IParameterList);
    procedure DoSetup(const Hook: IClientHook);
    function DoGetHook: IClientHook;
  protected // IClient
    function GetFileProtocol: IClientSideFileProtocol;
    function GetIsOnline: Boolean;
    function GetNick: String;
    function GetId: TGuid;
    function GetRootPath: string;
    function GetBasePath: string;
    procedure Start;
    procedure Stop;
    procedure Finalize;
    procedure Initialize;
    procedure Configure(const RootPath: string = ''; const ConfigName: string = '');
    procedure CreateLocalAccount(const Nick, Password: String);
    procedure GetAccount(const Nick, Password: String);
    procedure Logon;
    procedure SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; Bc: Boolean);
    procedure SendInfo(const Data: IParameters);
    procedure QueryUsers(Status: TUserStatus; out Users: TUserArray);
    function QueryUser(const Id: TGuid; out Info: IParameterList; Update: Boolean = false): Boolean;
    procedure QueryOfflines;
    function OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
    procedure RequestFileTransfer(const User: TGuid; const List: IFileInfoList);
    property IsOnline: Boolean read GetIsOnline;
    property Nick: String read GetNick;
    property Id: TGuid read GetId;
    property FileProtocol: IClientSideFileProtocol read GetFileProtocol;
  protected // IClientSideEvents
    procedure OnReceiveMessage(var Event: RClientSideMessageEvent);
    procedure OnReceiveResponse(var Event: RClientSideResponseEvent);
    procedure OnReceiveInfo(var Event: RClientSideInfoEvent);
    procedure OnConnectionRequest(var Event: RClientSideConnectionEvent);
    procedure OnConnectionAccept(var Event: RClientSideConnectionEvent);
  protected // IConnectionClientEvents
    procedure OnConnected(const Event: RConnectionEvent);
    procedure OnDisconnected(const Event: RConnectionEvent);
    procedure OnConnectionFailed(const Event: RConnectionEvent);
  public
    constructor CreateNew(const Owner: IUnknown; Param: Pointer); override; 
    constructor Create(const Hook: IClientHook);
    destructor Destroy; override;
    procedure BeforeConstruction; override;
    procedure AfterDestruction; override;
    property Hook: IClientHook read DoGetHook;
  end;

implementation

uses
  UmProtocolClient,
  UmData;

{ TClient }

constructor TClient.CreateNew(const Owner: IUnknown; Param: Pointer); 
var
  Hook: IClientHook;
begin
  inherited;
  if not Sil.Ref.Get(Owner, IClientHook, Hook) then
    Sil.Ref.Get(Param, IClientHook, Hook);
  DoSetup(Hook);
end;

constructor TClient.Create(const Hook: IClientHook);
begin
  CreateNew(Hook, nil);
end;

destructor TClient.Destroy;
begin
  FHook := nil;
  FData := nil;

  inherited;
end;

procedure TClient.BeforeConstruction;
begin
  GlobalLog.Initialize;
  inherited;
end;

procedure TClient.AfterDestruction;
begin
  inherited;
  GlobalLog.Finalize;
end;

procedure TClient.Start;
begin
  Sil.Trace.Enter('TClient.Start');

  FPendings := Sil.List.InterfaceList(true);
  FConnection := TConnectionClient.Create(nil, FServerName, FServerPort);
  Sil.Sink.Connect(FConnection, Self);
  FConnection.Start;

  Sil.Trace.Leave;
end;

procedure TClient.Stop;
begin
  Sil.Trace.Enter('TClient.Stop');

  if Assigned(FConnection) then
  begin
    FConnection.Stop;
    Sil.Sink.Disconnect(FConnection, Self);
    FConnection := nil;
  end;

  DoClean;

  Sil.Trace.Leave;
end;

procedure TClient.Initialize;
var
  List: IParameterList;
begin
  Sil.Trace.Enter('TClient.Initialize');

  FData.Initialize(DoGetBasePath(FId));

  if not FData.AccountExists(FId, FPassword) then
    FData.CreateLocalAccount(FId, FNick, FPassword);

  if Guid.Compare(FId, FLastAccount) <> 0 then
  begin
    List := Sil.List.Parameters;
    List['LastAccount'] := Guid.ToStr(FId);
    List['Server'] := Str.Format('%s:%d', [FServerName, FServerPort]);
    DoWriteList('initialization', List);
  end;

  Sil.Trace.Leave;
end;

procedure TClient.Finalize;
begin
  Sil.Trace.Enter('TClient.Finalize');

  if Assigned(FData) then
    FData.Finalize;

  FId := Guid.Null;
  FNick := '';
  FPassword := '';

  Sil.Trace.Leave;
end;

procedure TClient.DoSetup(const Hook: IClientHook);
begin
  FHook := Pointer(Hook);
  if not Assigned(FData) then
    FData := TData.Create;
end;

function TClient.DoGetHook: IClientHook;
begin
  Result := IClientHook(FHook);
end;

procedure TClient.DoClean;
begin
  FProtocol := nil;
  FFileProtocol := nil;
  FPendings := nil;
end;

procedure TClient.OnReceiveMessage(var Event: RClientSideMessageEvent);
begin
  if not Assigned(FHook) then Exit;

  FData.AppendHistory(Event.MsgId, Event.Time, DateTime.Now, Event.UserId, FId, 'im', 'ok', Event.Text);
  Hook.OnMessage(Event.UserId, Event.Text, Event.Time, Event.Recipients);
end;

procedure TClient.OnReceiveResponse(var Event: RClientSideResponseEvent);
begin
  if not Assigned(FHook) then Exit;

  //FData.AppendHistory(Event.Time, DateTime.Now, Event.UserId, FId, 'im', 'ok', Event.Text);
end;

procedure TClient.OnReceiveInfo(var Event: RClientSideInfoEvent);
begin
  if not Assigned(FHook) then Exit;
  Hook.OnInfo(Event.UserId, Event.Data);
end;

procedure TClient.OnConnectionAccept(var Event: RClientSideConnectionEvent);
begin
  if not Assigned(FHook) then Exit;
  Hook.OnConnectionAccept(Event.Params);
end;

procedure TClient.OnConnectionRequest(var Event: RClientSideConnectionEvent);
begin
  if not Assigned(FHook) then Exit;
  Hook.OnConnectionRequest(Event.Params);
end;

procedure TClient.OnConnected(const Event: RConnectionEvent);
begin
  FIsOnline := true;

  FFileProtocol := SilLayer.Protocol.FileClient;
  Event.Sender.AddProtocol(FFileProtocol, IServerSideFileProtocol, true, Self);

  FProtocol := TClientSide.Create;
  Event.Sender.AddProtocol(FProtocol, IServerSide, true, Self);

  if Assigned(FHook) then Hook.OnConnected;
end;

procedure TClient.OnDisconnected(const Event: RConnectionEvent);
begin
  DoClean;
  FIsOnline := false;
  if Assigned(FHook) then Hook.OnDisconnected;
end;

procedure TClient.OnConnectionFailed(const Event: RConnectionEvent);
begin
  DoClean;
  if not Assigned(FHook) then Exit;
  Hook.OnConnectionFailed;
end;

function TClient.GetIsOnline: Boolean;
begin
  Result := FIsOnline;
end;

procedure TClient.DoWriteList(const NodeName: String; const List: IParameterList);
var
  Node: IXmlTag;
  Item: RParameter;
  Enum: IEnumerator;
begin
  Node := FConfig.Root.AsTag.GetTag(NodeName + '/valuelist', true);
  Node.Childs.Clear;

  while List.Enumerate(Enum, Item) do
    with Node.Childs.Add(nkTag).AsTag do
    begin
      TagKind := tkBlock;
      Name := 'value';
      Arguments.WriteString('name', Item.Name);
      Data.Text := Item.Value;
    end;

  Sil.Xml.WriteFile(FConfig, FConfigFile);
end;

function TClient.DoReadList(const NodeName: String): IParameterList;
var
  Node: IXmlTag;
  Item: IXmlNode;
  Enum: IEnumerator;
begin
  Result := Sil.List.Parameters;
  Node := FConfig.Root.AsTag.GetTag(NodeName + '/valuelist', true);

  while Node.Childs.Enumerate(Enum, Item) do
    if Item.NodeKind = nkTag then
      with Item.AsTag do
        Result[Arguments.ReadString('name')] := Data.Text;
end;

procedure TClient.Configure(const RootPath: string; const ConfigName: string);
var
  Server: String;
  List, Account: IParameterList;
  Name: string;
begin
  Sil.Trace.Enter('TClient.Configure');
  try

    if Sil.Str.IsAssigned(RootPath) and Sil.Os.FileSystem.DirectoryExists(RootPath) then
      FRootPath := Sil.Os.FileSystem.AddSlash(RootPath) else
      FRootPath := Sil.OS.Process.Current.Info.Path;

    Sil.Os.Filesystem.ForceDirectories(FRootPath);

    if Sil.Str.IsAssigned(ConfigName) then
      Name := ConfigName else
      Name := Sil.Os.Process.Current.Info.Name;

    FConfigFile := Sil.OS.FileSystem.ChangeFileExt(FRootPath + Name, '.xml');
    FConfig := Sil.Xml.ReadFile(FConfigFile, nil, false);

    List := DoReadList('initialization');

    Server := Sil.Os.Environment.Expand(List.Get('Server', '%OUCHSERVER%'), '', 'OUCHSERVER');;

    try
      FLastAccount := Sil.Guid.FromStr(List['LastAccount']);
    except
      FLastAccount := Sil.Guid.Null;
      Sil.Trace.Exception('server');
    end;

    try
      Str.Scan(Server, '%s:%w', [@FServerName, @FServerPort]);
    except
      FServerName := Server;
    end;

    if FServerPort = 0 then
      FServerPort := 23360;

    try
      if FData.FindAccount(FLastAccount, Account) then
      begin
        FId := Guid.FromStr(List['id']);
        FNick := List['nick'];
        FPassword := List['password'];

        Sil.Os.Filesystem.ForceDirectories(DoGetBasePath(FId));

        List := Account;
      end;
    except
      Sil.Trace.Exception('account');
    end;

    List['LastAccount'] := Guid.ToStr(FLastAccount);
    List['Server'] := Str.Format('%s:%d', [FServerName, FServerPort]);
    DoWriteList('initialization', List);
    
  except
    Sil.Trace.Exception('general');
  end;
  Sil.Trace.Leave;
end;

function TClient.GetId: TGuid;
begin
  Result := FId;
end;

function TClient.GetBasePath: string;
begin
  if GUID.NotEmpty(FId) then
    Result := DoGetBasePath(FId) else
    Result := FRootPath;
end;

function TClient.GetRootPath: string;
begin
  Result := Sil.Os.Filesystem.AddSlash(FRootPath);
end;

function TClient.GetNick: String;
begin
  Result := FNick;
end;

function TClient.DoGetBasePath(const Id: TGuid): string;
begin
  Result := Sil.Os.Filesystem.AddSlash(FRootPath + 'user' + CPathSeparator + Guid.ToStr(Id));
end;

procedure TClient.Logon;
begin
  FProtocol.Logon(FId, FPassword);
end;

procedure TClient.CreateLocalAccount(const Nick, Password: String);
var
  Id: TGuid;
begin
  Sil.Trace.Enter('TClient.CreateLocalAccount', [Nick]);

  FProtocol.CreateAccount(Nick, Password, Id);

  FId := Id;
  FNick := Nick;
  FPassword := Password;

  // DoSetBasePath;

  Sil.Trace.Leave;
end;

function TClient.GetFileProtocol: IClientSideFileProtocol;
begin
  Result := FFileProtocol;
end;

procedure TClient.QueryOfflines;
begin
  FProtocol.QueryOfflines;
end;

procedure TClient.GetAccount(const Nick, Password: String);
begin
  FProtocol.GetAccount(Nick, Password, FId);
  FNick := Nick;
  FPassword := Password;
  
  // DoSetBasePath;
end;

procedure TClient.QueryUsers(Status: TUserStatus; out Users: TUserArray);
var
  i: Integer;
begin
  FProtocol.QueryUsers(Status, Users);

  for i := 0 to Length(Users) - 1 do
    DoCheckUser(Users[i]);
end;

procedure TClient.DoCheckUser(const User: TUser);
var
  Info: IParameterList;
  MustUpdate: Boolean;
begin
  if FData.QueryUser(User.Id, Info) then
    MustUpdate := User.Updated > Info['updated'] else
    MustUpdate := true;

  if MustUpdate then
    DoUpdateUser(User.Id, Info);
end;

procedure TClient.DoUpdateUser(const Id: TGUID; out Info: IParameterList);
begin
  FProtocol.QueryInfo(Id, Info);
  FData.UpdateUser(Id, Info);
end;

procedure TClient.SendInfo(const Data: IParameters);
begin
  FProtocol.SendInfo(Data);
end;

procedure TClient.SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; Bc: Boolean);
var
  Stamp: TDateTime;
  i: Integer;
begin
  Stamp := DateTime.Now;
  FProtocol.SendMessage(MsgId, Recipients, Text, Stamp, Bc);

  for i := 0 to Length(Recipients) - 1 do
    FData.AppendHistory(MsgId, Stamp, 0, FId, Recipients[i], 'im', 'sn', Text);
end;

function TClient.QueryUser(const Id: TGuid; out Info: IParameterList; Update: Boolean): Boolean;
begin
  Result := not Update and FData.QueryUser(Id, Info);

  if not Result then
  begin
    DoUpdateUser(ID, Info);
    Result := True;
  end;
end;

function TClient.OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
begin
  Result := FData.OpenHistory(User, Table);
end;

procedure TClient.RequestFileTransfer(const User: TGuid; const List: IFileInfoList);
var
  Params: IParameterList;
  Enum: IEnumerator;
  Item: IFileInfo;
  FileList: String;
//  Pending: IOuchPending;
begin
  //if not FIsOnline then raise

  FileList := '';

  while List.Enumerate(Enum, Item) do
    FileList := FileList + Str.Format('%s (%s); ', [Item.Name, Large.BytesToStr(Item.Size)]);

  Params := Sil.List.Parameters;
  Params['user.id'] := Guid.ToStr(User);
  Params['protocol.id'] := Guid.ToStr(IServerSideFileProtocol);
  Params['protocol.data'] := FileList;

  FProtocol.RequestConnection(Params);

  //Pending := TOuchPendingTransfer.Create(User, List);
  //FPendings.Add(Pending);
end;

end.
