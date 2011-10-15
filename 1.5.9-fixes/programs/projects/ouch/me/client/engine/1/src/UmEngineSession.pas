unit UmEngineSession;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine,
  UiEngine;

type
  RSessionData = record
    User: IOuchLocalUser;
    SessionID: TGUID;
  end;

  TOuchEngineSession = class(
    TInterfacedObject,
    IConnectable,
    IEngineSession,
    IOuchSession,
    IOuchSessionData,
    IOuchReply,
    IOuchTransport,
    IOuchUserDataEvents )
  private
    FOwner: Pointer;
    FPending: IOuchSession;
    FEvents: IEventList;
    FData: RSessionData;
    FEstablished: Boolean;
    FPendings: IInterfaceList;
    function GetOwner: IEngineConnection;
    function DoAddPending(const Item: IUnknown; const ID: TGUID): Integer;
    function DoRemovePending(const Pending: IEnginePending): Integer;
    function DoFindPending(const ID: TGUID; out Pending: IEnginePending): Boolean;
  protected
    procedure FireEstablished(const Reply: ROuchReply);
    procedure FireFailed(const Reply: ROuchReply);
    procedure FireClosed;
    procedure FireGroups(const Reply: ROuchReply; const Groups: IOuchGroups);
    procedure FireJoin(const Reply: ROuchReply; const Group: IOuchGroup);
    procedure FireError(const Reply: ROuchReply);
  protected // IOuchUserDataEvents 
    procedure OnUserDataChanged(const Event: ROuchUserDataEvent);
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown);
    procedure RemoveListener(const Listener: IUnknown);
  protected // IEngineSession
    function GetSender: IEngineSender;
    function GetOuch: IOuchSession;
    procedure Request;
  protected // IOuchSession
    function GetData: IOuchSessionData;
    function GetTransport: IOuchTransport;
    procedure Close;
    procedure Query(const Listener: IUnknown);
    procedure Join(const Group: IOuchGroupData; const Password: string; const Listener: IUnknown);
  protected // IOuchSessionData
    function GetUser: IOuchLocalUser;
    function GetHandle: TGUID;
  protected // IOuchReply
    procedure DefineUserReply(const Data: ROuchDefineUserReply);
    procedure ConnectReply(const Data: ROuchConnectReply);
    procedure QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
    procedure NotifyStatusReply(const Data: ROuchNotifyStatusReply);
    procedure QueryGroupsReply(const Data: ROuchQueryGroupsReply);
    procedure GroupLogonReply(const Data: ROuchGroupLogonReply);
    procedure SendMessageReply(const Data: ROuchSendMessageReply);
  protected // IOuchReply
    procedure Notify(const Status: TOuchUserStatus);
    procedure SendMessage(const Msg: IOuchMessage);
  protected
    procedure UserInfoChanged;
    procedure SessionChanged(const Reply: ROuchReply; const UserID, SessionID: TGUID);
  protected
    property Owner: IEngineConnection read GetOwner;
    property Sender: IEngineSender read GetSender;
  public
    constructor Create(const Owner: IEngineConnection; const User: IOuchLocalUser);
    destructor Destroy; override;
  end;

  TEnginePending = class(
    TInterfacedObject,
    IUnknown,
    IEnginePending )
  private
    FItem: IUnknown;
    FID: TGUID;
  protected // IEnginePending
    function GetID: TGUID;
    function GetItem: IUnknown;
    function Get(const IID: TGUID; out Obj): Boolean;
  public
    constructor Create(const Item: IUnknown; const ID: TGUID);
    destructor Destroy; override;
  end;

implementation

uses
  UmEngineGroup, SilLiEnumerator;

{ TOuchEngineSession }

constructor TOuchEngineSession.Create(const Owner: IEngineConnection; const User: IOuchLocalUser);
begin
  inherited Create;
  FPendings := Sil.List.InterfaceList(True);
  FOwner := Pointer(Owner);
  FData.SessionID := Sil.GUID.Null;
  FData.User := User;
  Sil.Sink.Connect(FData.User, Self);
  Sil.Sink.Connect(Sender.Channel.Reply, Self);
end;

destructor TOuchEngineSession.Destroy;
begin
  if FEstablished then Close;
  FData.User := nil;
  FData.SessionID := Sil.GUID.Null;
  FOwner := nil;
  FPendings := nil;
  inherited;
end;

function TOuchEngineSession.GetSender: IEngineSender;
begin
  Result := Owner as IEngineSender;
end;

function TOuchEngineSession.GetOuch: IOuchSession;
begin
  Result := Self;
end;

procedure TOuchEngineSession.Request;
begin
  FPending := Self;  //!! guardamos una ref para que no nos destruyamos mientras la conexión se establece
  try
    
    with Sender.Channel.Request do
      Connect(FData.User.ID, FData.User.Password) // connect con el usuario / password que me pasaron
  except
    FPending := nil;
    raise;
  end;
end;

function TOuchEngineSession.GetData: IOuchSessionData;
begin
  Result := Self;
end;

function TOuchEngineSession.GetOwner: IEngineConnection;
begin
  Result := IEngineConnection(FOwner);
end;

function TOuchEngineSession.GetTransport: IOuchTransport;
begin
  Result := Self;
end;

procedure TOuchEngineSession.Close;
begin
  if FEstablished then
  begin
    FPending := Self;
    try
      FireClosed();
      FData.SessionID := Sil.GUID.Null;
      FEstablished := False;
      Sil.Sink.Disconnect(Sender.Channel.Reply, Self);
    finally
      FPending := nil;
    end;
  end;
end;

procedure TOuchEngineSession.Query(const Listener: IUnknown);
begin
  Sender.Channel.Request.QueryGroups(Sil.GUID.Create(), FData.SessionID);
end;

procedure TOuchEngineSession.Join(const Group: IOuchGroupData; const Password: string; const Listener: IUnknown);
var
  Request: TGUID;
begin
  Request := Sil.GUID.Create();
  DoAddPending(TEngineGroup.Create(Self, Group, Password), Request);
  Sender.Channel.Request.GroupLogon(Request, FData.SessionID, Group.ID, Password);
end;

function TOuchEngineSession.GetHandle: TGUID;
begin
  Result := FData.SessionID;
end;

function TOuchEngineSession.GetUser: IOuchLocalUser;
begin
  Result := FData.User;
end;

procedure TOuchEngineSession.DefineUserReply(const Data: ROuchDefineUserReply);
begin
//@@  SessionChanged(Data.Reply, Data.CreateUser.IdUser, Data.CreateUser.IdSession);
end;

procedure TOuchEngineSession.ConnectReply(const Data: ROuchConnectReply);
begin
  SessionChanged(Data.Reply, Data.Connect.IdUser, Data.Connect.IdSession);
end;

procedure TOuchEngineSession.SessionChanged(const Reply: ROuchReply; const UserID, SessionID: TGUID);
begin
  try
    if Reply.Result = 0 then
    begin
      FData.SessionID := SessionID;
      FData.User.ID := UserID;
      FireEstablished(Reply);
    end else
      FireFailed(Reply);
    FEstablished := True;
  finally
    FPending := nil;
  end;
end;

procedure TOuchEngineSession.UserInfoChanged;
begin
//@@  Sender.Channel.Request.UserInfo(Sil.GUID.Create(), FData.SessionID, FData.User.Nick, FData.User.Mail, FData.User.Password);
end;

procedure TOuchEngineSession.QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
begin
end;

(*)
procedure TOuchEngineSession.UserInfoReply(const Data: ROuchUserInfoReply);
begin
  if Data.Reply.Result = 0 then
  begin
    FData.IsNew := False;
  end;
end;
(*)

procedure TOuchEngineSession.QueryGroupsReply(const Data: ROuchQueryGroupsReply);
begin
  if Data.Reply.Result = 0 then
    FireGroups(Data.Reply, Owner.Engine.Toolkit.NewGroupList(Data.QueryGroups.Groups)) else
    FireError(Data.Reply);
end;

procedure TOuchEngineSession.GroupLogonReply(const Data: ROuchGroupLogonReply);
var
  Pending: IEnginePending;
  Group: IEngineGroup;
begin
  if (Data.Reply.Result = 0) then
    begin
      if DoFindPending(Data.Reply.ID, Pending) and Pending.Get(IEngineGroup, Group) then
        FireJoin(Data.Reply, Group.SetUsers(Owner.Engine.Toolkit.NewUserList(Data.GroupLogon.Users)).Ouch);
    end
  else
    FireError(Data.Reply);
end;

procedure TOuchEngineSession.NotifyStatusReply(const Data: ROuchNotifyStatusReply);
begin
end;

procedure TOuchEngineSession.SendMessageReply(const Data: ROuchSendMessageReply);
begin
end;

procedure TOuchEngineSession.Notify(const Status: TOuchUserStatus);
begin
  Sender.Channel.Request.NotifyStatus(Sil.GUID.Create, FData.SessionID, Status);
end;

procedure TOuchEngineSession.SendMessage(const Msg: IOuchMessage);
var
  Enum: IEnumerator;
  Item: IUnknown;
  User: IOuchUserData;
begin
  with Msg.Data.Recipients do
    while Enumerate(Enum, Item) do
    begin
      User := Item as IOuchUserData;
      Sender.Channel.Request.SendMessage(Msg.Data.ID, FData.SessionID, User.ID, Msg.Data.Text,
        Msg.Data.Priority, Msg.Data.Kind, Msg.Data.Timestamp, Msg.Data.Senders.ToArray());
    end;
end;

procedure TOuchEngineSession.AddListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Add(FEvents, Listener);
end;

procedure TOuchEngineSession.RemoveListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Remove(FEvents, Listener);
end;

procedure TOuchEngineSession.FireEstablished(const Reply: ROuchReply);
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionReply;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  Event.Reply := Reply;
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionEstablished(Event);
end;

procedure TOuchEngineSession.FireFailed(const Reply: ROuchReply);
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionReply;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  Event.Reply := Reply;
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionFailed(Event);
end;

procedure TOuchEngineSession.FireClosed;
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionEvent;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionClosed(Event);
end;

procedure TOuchEngineSession.FireGroups(const Reply: ROuchReply; const Groups: IOuchGroups);
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionGroups;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  Event.Reply := Reply;
  Event.Groups := Groups;
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionGroups(Event);
end;

procedure TOuchEngineSession.FireJoin(const Reply: ROuchReply; const Group: IOuchGroup);
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionJoin;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  Event.Reply := Reply;
  Event.Group := Group; 
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionJoin(Event);
end;

procedure TOuchEngineSession.FireError(const Reply: ROuchReply);
var
  Enum: IEnumerator;
  Item: IOuchSessionEvents;
  Event: ROuchSessionReply;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  Event.Reply := Reply;
  while FEvents.Enumerate(Enum, Item, IOuchSessionEvents) do
    Item.OnSessionError(Event);
end;

function TOuchEngineSession.DoAddPending(const Item: IInterface; const ID: TGUID): Integer;
begin
  Result := FPendings.Add(TEnginePending.Create(Item, ID));
end;

function TOuchEngineSession.DoRemovePending(const Pending: IEnginePending): Integer;
begin
  Result := FPendings.Remove(Pending);
end;

function TOuchEngineSession.DoFindPending(const ID: TGUID; out Pending: IEnginePending): Boolean;
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  with FPendings do
    while Enumerate(Enum, Item) do
    begin
      Pending := Item as IEnginePending;
      Result := Sil.GUID.IsEqual(ID, Pending.ID);
      if Result then
      begin
        DoRemovePending(Pending);
        Exit;
      end;
    end;
  Pending := nil;
  Result := False;
end;

procedure TOuchEngineSession.OnUserDataChanged(const Event: ROuchUserDataEvent);
begin
  UserInfoChanged;
end;

{ TEnginePending }

constructor TEnginePending.Create(const Item: IInterface; const ID: TGUID);
begin
  inherited Create;
  FItem := Item;
  FID := ID;
end;

destructor TEnginePending.Destroy;
begin
  FItem := nil;
  inherited;
end;

function TEnginePending.Get(const IID: TGUID; out Obj): Boolean;
begin
  Result := Assigned(FItem) and (FItem.QueryInterface(IID, Obj) = 0);
end;

function TEnginePending.GetID: TGUID;
begin
  Result := FID;
end;

function TEnginePending.GetItem: IUnknown;
begin
  Result := FItem;
end;

end.

