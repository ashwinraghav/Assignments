unit UmOuchEngine;

interface

uses
  Sil, SilTool,

  UiProtocol, UiClient, UiOuch;
  
const
  CThreadUpdate = EV_FIRST + 1;

type
  RStateMsg = record
    ID: Integer;
    Dummy: Integer;
    State: TOuchEngineState;
    Result: Integer;
  end;

  TOuchEngine = class(
    TSilInterfacedObject,
    IOuchEngine,
    IOuchFactory,
    IClientHook )
  private
    FApplication: IOuchApplication;
    FView: IOuchView;
    FState: TOuchEngineState;
    FClient: IClient;
    FUsers: IOuchAccountList;
    FProfile: IOuchProfile;
    FInitialized: Boolean;
    FConnected: Boolean;
    FConnecting: Boolean;
  private
    procedure DoTerminate;
    procedure DoInitialize;
    procedure DoFinalize;
    procedure DoActivate;
    procedure DoDeactivate;
    procedure DoChangeState(NewState: TOuchEngineState);
    procedure DoUpdateNotify(Stage: TOuchUpdateStage; const Message: string);
    procedure DoFireStateChange(const Sender: IUnknown; Data: Pointer);
    procedure DoFireMessage(const Sender: IUnknown; Data: Pointer);
    procedure DoFireUpdateNotify(const Sender: IUnknown; Data: Pointer);
    procedure DoFireConnected(const Sender: IUnknown; Data: Pointer);
    procedure DoFireDisconnected(const Sender: IUnknown; Data: Pointer);
    procedure DoQueryAllUsers(const List: IOuchAccountList);
    procedure DoQueryUsers(State: TUserStatus; const List: IOuchAccountList);
    function DoQueryUser(const ID: TGUID): IParameterList;
    function DoGetUser(const ID: TGUID): IOuchAccount;
    function DoAddUser(const ID: TGUID): IOuchAccount;
    procedure DoBeginUpdate;
    procedure DoEndUpdate;
    procedure DoSetUsersOffline;
    function DoEditAccount(const Item: IOuchAccount): IOuchContact;
    procedure DoUpdateThread(var Msg: RThreadRunMessage); message CThreadUpdate;
    function DoAccountGet(const Nick, Password: string; out Error: string): Boolean;
    function DoAccountCreate(const Nick, Password: string; out Error: string): Boolean;
  protected // IOuchEngine
    function GetFactory: IOuchFactory;
    function GetClient: IClient;
    function GetState: TOuchEngineState;
    function GetIsOnline: Boolean;
    function GetAccount: IOuchLocalAccount;
    function GetProfile: IOuchProfile;
    procedure Reset;
    procedure Setup(const Nick, Password: string; MustExist: Boolean = True);
    procedure Logon;
    procedure Send(const Msg: IOuchMessage);
    procedure Update;
  protected // IOuchFactory
    function AccountList: IOuchAccountList; overload;
    function AccountList(const Contacts: IOuchAccounts): IOuchAccountList; overload;
    function AccountList(const Users: TGuidArray): IOuchAccountList; overload;
    function Message(const From: IOuchAccount; const Text: String; const Time: TDateTime; const Recipients: IOuchAccounts): IOuchMessage; overload;
    function Message(const From: IOuchAccount; const Recipients: IOuchAccounts): IOuchMessage; overload;
    function Message: IOuchMessage; overload;
    function Profile(const FileName: string): IOuchProfile;
  protected // IClientHook
    procedure OnMessage(const From: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
    procedure OnInfo(const Id: TGuid; const Data: IParameters);
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnConnectionFailed;
    procedure OnConnectionRequest(const Params: IParameters);
    procedure OnConnectionAccept(const Params: IParameters);
  public
    constructor Create(const Application: IOuchApplication);
    destructor Destroy; override;
  public 
    property IsOnline: Boolean read GetIsOnline;
  end; 

implementation

uses
  UcOuch, UdOuch,
  UmOuchContacts, UmOuchContact, UmOuchMessage, UmOuchAccount, UtUpdate,
  SilLkInterfaced, UmOuchProfile;

{ TOuchEngine }

constructor TOuchEngine.Create(const Application: IOuchApplication);
begin
  inherited Create;
  FApplication := Application;
  FView := FApplication.View;
  FUsers := TOuchAccounts.Create(Self);
  Sil.Sink.Connect(FUsers, FApplication);
  Sil.Sink.Connect(FUsers, FView);
  DoChangeState(esDisconnected);
  SilTool.Sv.SharedObject.CreateObject(SOuchClient, IClient, FClient, Self);
  FClient.Configure(Sil.Os.Process.Current.CurrentPath, ParamStr(1));
  if Guid.NotEmpty(FClient.Id) then
    DoInitialize;
  DoActivate;
end;

destructor TOuchEngine.Destroy;
begin
  if Assigned(FUsers) then
  begin
    FUsers.Clear;
    Sil.Sink.Disconnect(FUsers, FView);
    Sil.Sink.Disconnect(FUsers, FApplication);
    FUsers := nil;
  end;

  if Assigned(FClient) then
  begin
    DoTerminate;
    SilTool.Sv.SharedObject.ReleaseObject(FClient);
  end;
  
  FView := nil;
  FApplication := nil;
  inherited;
end;

function TOuchEngine.GetFactory: IOuchFactory;
begin
  Result := Self;
end;

function TOuchEngine.GetClient: IClient;
begin
  Result := FClient;
end;

function TOuchEngine.GetState: TOuchEngineState;
begin
  Result := FState;
end;

function TOuchEngine.GetIsOnline: Boolean;
begin
  Result := Assigned(FClient) and FClient.IsOnline and Guid.NotEmpty(FClient.Id);
end;

function TOuchEngine.GetAccount: IOuchLocalAccount;
var
  Info: IParameterList;
begin
  Info := Sil.List.Parameters(True);
//  Info[CUserId] := FClient.Id;
  Info[CUserNick] := FClient.Nick;
  if Self.IsOnline then
    Info[CUserState] := usOnline else
    Info[CUserState] := usOffline;
  Result := TOuchLocalAccount.Create(FClient.Id, Info);
end;

function TOuchEngine.GetProfile: IOuchProfile;
begin
  Result := FProfile;
end;

procedure TOuchEngine.Reset;
begin
  try
  
    if FInitialized then
      DoFinalize;

    if FConnected or FConnecting then
      DoDeactivate;

    DoActivate;

  except
    Sil.Trace.Exception('reset');
    raise;
  end;
end;

procedure TOuchEngine.Setup(const Nick, Password: string; MustExist: Boolean);
var
  Message: string;
  Succeeded: Boolean;
begin
  try
    if FConnected and not FInitialized then
      begin

        Succeeded := DoAccountGet(Nick, Password, Message);

        if not Succeeded then
          Succeeded := not MustExist and DoAccountCreate(Nick, Password, Message);

        if not Succeeded then
        begin
          DoDeactivate;
          raise Sil.Error.Create(Message);
        end;

        DoInitialize;

      end
    else if not FConnected then
      raise Sil.Error.Create('el cliente está desconectado')
    else if FInitialized then
      raise Sil.Error.Create('ya hay un usuario logeado');

  except
    Sil.Trace.Exception('setup');
    raise;
  end;
end;

procedure TOuchEngine.Logon;
begin
  if IsOnline then
  try
    FClient.Logon;
    DoChangeState(esLoggedOn);
    DoQueryAllUsers(FUsers);
    FClient.QueryOfflines;
  except
    Sil.Trace.Exception('logon');
    raise;
  end;
end;

procedure TOuchEngine.Send(const Msg: IOuchMessage);
begin
  FClient.SendMessage(Msg.Recipients.ToGuidList, Msg.Text, Msg.BlindCopy);
end;

procedure TOuchEngine.Update;
begin
  Sil.Os.Thread.Spawn(CThreadUpdate, 'updater', Self);
end;

function TOuchEngine.AccountList: IOuchAccountList;
begin
  Result := TOuchAccounts.Create(Self);
end;

function TOuchEngine.AccountList(const Contacts: IOuchAccounts): IOuchAccountList;
var
  Enum: IEnumerator;
  Item: IOuchAccount;
begin
  Result := AccountList();
  while Contacts.Enumerate(Enum, Item) do
    Result.Add(Item);
end;

function TOuchEngine.AccountList(const Users: TGuidArray): IOuchAccountList;
var
  I: Integer;
begin
  Result := AccountList();
  for I := Low(Users) to High(Users) do
    Result.Add(DoGetUser(Users[I]));
end;

function TOuchEngine.Message(const From: IOuchAccount; const Recipients: IOuchAccounts): IOuchMessage;
begin
  Result := Message();
  Result.From := From;
  Result.Recipients := AccountList(Recipients);
end;

function TOuchEngine.Message(const From: IOuchAccount; const Text: String; const Time: TDateTime; const Recipients: IOuchAccounts): IOuchMessage;
begin
  Result := Message(From, Recipients);
  Result.Text := Text;
end;

function TOuchEngine.Message: IOuchMessage;
begin
  Result := TOuchMessage.Create(Self);
end;

function TOuchEngine.Profile(const FileName: string): IOuchProfile;
begin
  Result := TOuchProfile.Create(Sil.Xml.ReadFile(FileName, nil, False));
end;

procedure TOuchEngine.OnConnected;
begin
  FConnecting := False;
  FConnected := True;
  DoChangeState(esConnected);
  Sil.Os.Thread.AsyncCall(DoFireConnected);
end;

procedure TOuchEngine.OnDisconnected;
begin
  FConnected := False;
  DoChangeState(esDisconnected);
  DoSetUsersOffline;
  Sil.Os.Thread.AsyncCall(DoFireDisconnected);
end;

procedure TOuchEngine.OnConnectionFailed;
begin
  // fallo la conexion!!!
end;

procedure TOuchEngine.OnInfo(const Id: TGuid; const Data: IParameters);
var
  User: IOuchContact;
  Info: IParameterList;
begin
  Info := DoQueryUser(ID);
  Info.Merge(Data);
  User := DoEditAccount(DoGetUser(Id));
  User.Info := Info; 
end;

procedure TOuchEngine.OnMessage(const From: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
var
  Msg: IOuchMessage;
begin
  Msg := Message(DoGetUser(From), Text, Time, AccountList(Recipients));
  Sil.Os.Thread.AsyncCall(DoFireMessage, Msg);
end;

procedure TOuchEngine.DoTerminate;
begin
  try
    DoFinalize;
    DoDeactivate;
  except
    Sil.Trace.Exception('terminate');
  end;
end;

procedure TOuchEngine.DoInitialize;
begin
  if not FInitialized then
  try
    FClient.Initialize;
    FProfile := Profile(FClient.BasePath + 'profile.xml');
    FInitialized := True;
  except
    Sil.Trace.Exception('initialize');
    raise;
  end;
end;

procedure TOuchEngine.DoFinalize;
begin
  if FInitialized then
  try
    FInitialized := False;
    FProfile.Save(FClient.BasePath + 'profile.xml');
    FClient.Finalize;
    FProfile := nil;
  except
    Sil.Trace.Exception('finalize');
    raise;
  end;
end;

procedure TOuchEngine.DoActivate;
begin
  if not FConnecting then
  try
    FConnecting := True;
    FClient.Start;
  except
    Sil.Trace.Exception('initialize');
    raise;
  end;
end;

procedure TOuchEngine.DoDeactivate;
begin
  try
    FConnected := False;
    FClient.Stop;
  except
    Sil.Trace.Exception('finalize');
    raise;
  end;
end;

function TOuchEngine.DoAccountCreate(const Nick, Password: string; out Error: string): Boolean;
begin
  Result := False;
  try
    FClient.CreateLocalAccount(Nick, Password);
    Result := True;
  except on Ex: Exception do
    Error := Ex.Message;
  end;
end;

function TOuchEngine.DoAccountGet(const Nick, Password: string; out Error: string): Boolean;
begin
  Result := False;
  try
    FClient.GetAccount(Nick, Password);
    Result := True;
  except on Ex: Exception do
    Error := Ex.Message;
  end;
end;

procedure TOuchEngine.DoChangeState(NewState: TOuchEngineState);
begin
  if NewState <> FState then
  begin
    FState := NewState;
    Sil.OS.Thread.AsyncCall(DoFireStateChange);
  end;
end;

procedure TOuchEngine.DoUpdateNotify(Stage: TOuchUpdateStage; const Message: string);
var
  Status: POuchUpdateStatus;
begin
  New(Status);
  Status.Stage := Stage;
  Status.Message := Message;
  Sil.Os.Thread.AsyncCall(DoFireUpdateNotify, Status);
end;

procedure TOuchEngine.DoFireStateChange(const Sender: IInterface; Data: Pointer);
var
  Sink: IOuchEngineStatusEvent;
  Msg: RStateMsg;
begin
  if Assigned(FApplication) then
  try
    if Sil.Ref.Get(FApplication, IOuchEngineStatusEvent, Sink) then
      Sink.OnStatusChanged(Self);
    Msg.ID := EV_OUCHBASE + Ord(FState);
    Msg.State := FState;
    Msg.Result := 0;
    FApplication.Dispatch(Msg);
  except
    Sil.Trace.Exception('FireStateChange');
  end;
end;

procedure TOuchEngine.DoFireMessage(const Sender: IInterface; Data: Pointer);
var
  Sink: IOuchEngineMessageEvent;
  Msg: IOuchMessage;
begin
  if Assigned(FApplication) then
  try
    Msg := Sender as IOuchMessage;
    try
      if Sil.Ref.Get(FApplication, IOuchEngineMessageEvent, Sink) then
        Sink.OnMessage(Msg);
    finally
      Msg := nil;
    end;
  except
    Sil.Trace.Exception('FireMessage');
  end;
end;

procedure TOuchEngine.DoFireUpdateNotify(const Sender: IInterface; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchEngineUpdateEvent;
  Event: ROuchEvUpdateNotification;
begin
  if HasConnections and Assigned(Data) then
  try
    Event.Sender := Self;
    Event.Status := POuchUpdateStatus(Data)^;
    with Events do
      while Enumerate(Enum, Sink, IOuchEngineUpdateEvent) do
        try
          Sink.OnUpdateNotification(Event);
        except
          Sil.Trace.Exception('update notify');
        end;
  finally
    Dispose(POuchUpdateStatus(Data));
  end;
end;

procedure TOuchEngine.DoFireConnected(const Sender: IUnknown; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchEngineConnectedEvent;
begin
  if HasConnections then
    with Events do
      while Enumerate(Enum, Sink, IOuchEngineConnectedEvent) do
      try
        Sink.OnEngineConnected(Self);
      except
        Sil.Trace.Exception('onconnected');
      end;
end;

procedure TOuchEngine.DoFireDisconnected(const Sender: IUnknown; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchEngineDisconnectedEvent;
begin
  if HasConnections then
    with Events do
      while Enumerate(Enum, Sink, IOuchEngineDisconnectedEvent) do
      try
        Sink.OnEngineDisconnected(Self);
      except
        Sil.Trace.Exception('onconnected');
      end;
end;

procedure TOuchEngine.DoQueryAllUsers(const List: IOuchAccountList);
begin
  DoBeginUpdate;
  try
    DoQueryUsers(usOffline, List);
    DoQueryUsers(usOnline, List);
  finally
    DoEndUpdate;
  end;
end;

procedure TOuchEngine.DoQueryUsers(State: TUserStatus; const List: IOuchAccountList);
var
  Users: TUserArray;
  I: Integer;
  ID: TGUID;
  Info: IParameters;
  Item: IOuchContact;
begin
  FClient.QueryUsers(State, Users);
  for I := Low(Users) to High(Users) do
  begin
    ID := Users[I].Id;
    Info := DoQueryUser(ID);
    Item := DoEditAccount(DoGetUser(ID));
    Item.Info := Info;  
    if Item.State <> State then
      Item.State := State;
  end;
end;

function TOuchEngine.DoQueryUser(const ID: TGUID): IParameterList;
var
  Info: IParameters;
begin
  Result := Sil.List.Parameters();
  if not FClient.QueryUser(ID, Info) then
  begin
    Result[CUserNick] := SUserUnknown;
    Result[CUserState] := usOffline;
  end else
    Result.Merge(Info);
end;

function TOuchEngine.DoGetUser(const ID: TGUID): IOuchAccount;
begin
  if not FUsers.Find(ID, Result) then
    Result := DoAddUser(ID);
end;

function TOuchEngine.DoAddUser(const ID: TGUID): IOuchAccount;
begin
  Result := TOuchContact.Create(FUsers, ID);
end;

procedure TOuchEngine.DoBeginUpdate;
begin
  if Assigned(FView) then FView.BeginUpdate;
end;

procedure TOuchEngine.DoEndUpdate;
begin
  if Assigned(FView) then FView.EndUpdate;
end;

procedure TOuchEngine.DoSetUsersOffline;
var
  Enum: IEnumerator;
  User: IOuchAccount;
begin
  DoBeginUpdate;
  try
    while FUsers.Enumerate(Enum, User) do
      User.State := usOffline;
  finally
    DoEndUpdate;
  end;
end;

function TOuchEngine.DoEditAccount(const Item: IOuchAccount): IOuchContact;
begin
  if not Sil.Ref.GetInterface(Item, IOuchContact, Result) then
    raise Sil.Error.Create(SCannotEditAccount);
end;

procedure TOuchEngine.DoUpdateThread(var Msg: RThreadRunMessage);
var
  Path, FileName: String;
  OK: Boolean;
begin
  try
    DoUpdateNotify(usStart, SUpdateStarted);
    
    Path := Sil.OS.Process.Current.Info.Path + 'update';
    FileName := 'update.xml';
    try

      OK := Updater.Download(FClient.FileProtocol, Path, FileName);
     
      if OK then
      try
        DoUpdateNotify(usDownload, SUpdateDownloaded);
        try

          Updater.ChangeFiles(Path, FileName);
          DoUpdateNotify(usChange, SUpdateChanged);
          
        except on Ex: Exception do
          DoUpdateNotify(usChange, SUpdateError + Ex.Message);
        end;
        
      except on Ex: Exception do
        DoUpdateNotify(usDownload, SUpdateError + Ex.Message);
      end else
        DoUpdateNotify(usEnd, SUpdateCompleted);

    except on Ex: Exception do
      DoUpdateNotify(usError, SUpdateError + Ex.Message);
    end;

  except
    Sil.Trace.Exception('update thread');
  end;

end;

procedure TOuchEngine.OnConnectionAccept(const Params: IParameters);
begin

end;

procedure TOuchEngine.OnConnectionRequest(const Params: IParameters);
begin

end;

end.
