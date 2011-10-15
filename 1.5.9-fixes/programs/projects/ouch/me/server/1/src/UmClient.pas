unit UmClient;

interface

uses
  Sil,
  SilProtocol,

  UiEventLog,
  UiData,
  UiClient,
  UiServer,
  UcClient,
  UiOuchProtocol,
  UkOuchChannel;

type
  TOuchClient = class (
    // extends
    TOuchChannel,
    // implements
    IOuchClient,
    IOuchRequest,
    IOuchReply)
  private
    FServer: IOuchServer;
    FDataMgr: IDataMgr;
    FLogger: IEventLog;
    FUserId: TGuid;
  private
    procedure DoLog(const Text: String; EventType: TEventType);
    procedure DoLogFmt(const Text: String; const Args: array of const; EventType: TEventType);
    function DoAddSession(var Session: TGuid; const Address: String): ROuchReply;
    procedure DoCheckOffline(const User, Session: TGuid);
  protected // TOuchConnection
    procedure DoStartup; override;
    procedure DoRelease; override;
  protected // IConnectedEvents
    procedure OnDisconnected(const Event: TConnectionBreakEvent); override;
  protected // IOuchClient
  protected // IOuchRequest
    procedure DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters);
    procedure Connect(const User: TGuid; const Password: String);
    procedure QueryOfflines(const RequestId, Session: TGuid);
    procedure NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus);
    procedure QueryGroups(const RequestId, Session: TGuid);
    procedure GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String);
    procedure SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray);
  protected // IOuchReply
    procedure DefineUserReply(const Data: ROuchDefineUserReply);
    procedure ConnectReply(const Data: ROuchConnectReply);
    procedure QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
    procedure NotifyStatusReply(const Data: ROuchNotifyStatusReply);
    procedure QueryGroupsReply(const Data: ROuchQueryGroupsReply);
    procedure GroupLogonReply(const Data: ROuchGroupLogonReply);
    procedure SendMessageReply(const Data: ROuchSendMessageReply);
  public
    constructor Create(const Server: IOuchServer; const DataMgr: IDataMgr; const Connection: IClientSocketConnection); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  UmOuchProtocol,
  UtData;

{ TOuchClient }

constructor TOuchClient.Create(const Server: IOuchServer; const DataMgr: IDataMgr; const Connection: IClientSocketConnection);
begin
  FDataMgr := DataMgr;
  FServer := Server;
  inherited Create(Connection);
end;

destructor TOuchClient.Destroy;
begin
  FDataMgr := nil;
  FServer := nil;

  inherited;
end;

procedure TOuchClient.DoStartup;
begin
  inherited;
end;

procedure TOuchClient.DoRelease;
begin
  inherited;

  FDataMgr := nil;
  FLogger := nil;
end;

procedure TOuchClient.DoLog(const Text: String; EventType: TEventType);
begin
  if FLogger <> nil then
    FLogger.LogMessage(Text, EventType);
end;

procedure TOuchClient.DoLogFmt(const Text: String; const Args: array of const; EventType: TEventType);
begin
  try
    DoLog(Str.Format(Text, Args), EventType);
  except
    DoLog(Text, EventType);
  end;
end;


{ IOuchRequest }

function DoMakeReply(ResultCode: Integer; const Comment: String; const Id: TGuid): ROuchReply;
begin
  Result.Result := ResultCode;
  Result.Comment := Comment;
  Result.Id := Id;
end;

function TOuchClient.DoAddSession(var Session: TGuid; const Address: String): ROuchReply;
var
  dtTimeout: TDateTime;
begin
  dtTimeout := 0;

  if Db.AppendSession(FDataMgr, FUserId, Session, Address, dtTimeout) then
    Result := DoMakeReply(0, CUserAdded, Guid.Null) else
    Result := DoMakeReply(1, CErrorAddingUser, Guid.Null);
end;

procedure TOuchClient.DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters);
var
  Info: ROuchDefineUserReply;
begin
  if Guid.IsEmpty(User) then
  begin
    FUserId := Sil.Guid.Create;

    if Db.AppendUser(FDataMgr, FUserId, Password) then
    begin
      Db.WriteData(FDataMgr, FUserId, Data);
      Info.Reply := DoMakeReply(0, CUserAdded, Guid.Null);
    end else
    begin
      Info.Reply := DoMakeReply(1, CErrorAddingUser, Guid.Null);
      Info.DefineUser.IdUser := Guid.Null;
    end;
  end else
  begin
    FUserId := User;

    if Db.WriteData(FDataMgr, FUserId, Data) then
      Info.Reply := DoMakeReply(0, CUserInfoUpdated, Guid.Null) else
      Info.Reply := DoMakeReply(1, CErrorUpdatingUserInfo, Guid.Null);
  end;

  if Info.Reply.Result = 0 then
    Info.DefineUser.IdUser := FUserId;
    
  Reply.DefineUserReply(Info);
end;

procedure TOuchClient.Connect(const User: TGuid; const Password: String);
var
  Resp: ROuchConnectReply;
  dtTimeout: TDateTime;
  sAddress: String;
begin
  dtTimeout := DateTime.Now + FDataMgr.ValidateTime;

  if Db.FindUser(FDataMgr, User, Password) then
    FUserId := User else
    Resp.Reply := DoMakeReply(1, CInvalidUser, Guid.Null);

  if Resp.Reply.Result = 0 then
  begin
    sAddress := Connection.Parameters.RemoteAddress;
    Resp.Reply := DoAddSession(Resp.Connect.IdSession, sAddress);

    if Resp.Reply.Result <> 0 then
      if Db.ChangeSession(FDataMgr, Resp.Connect.IdSession, usUnknown, sAddress, dtTimeout) then
        Resp.Reply := DoMakeReply(0, CSessionResumed, Guid.Null) else
        Resp.Reply := DoMakeReply(1, CErrorOpeningSession, Guid.Null);
  end;

  Resp.Connect.IdUser := FUserId;
  Reply.ConnectReply(Resp);
end;

procedure TOuchClient.QueryOfflines(const RequestId, Session: TGuid);
var
  Resp: ROuchQueryOfflinesReply;
  Users: TOuchUserArray;
begin
  Users := nil;

  if Db.FindSession(FDataMgr, Session, FUserId) then
  begin
    DoCheckOffline(FUserID, Session);
    Resp.Reply := DoMakeReply(1, CQueryOfflineFinished, RequestId);
  end else
    Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);

  Reply.QueryOfflinesReply(Resp);
end;

procedure TOuchClient.DoCheckOffline(const User, Session: TGuid);
var
  i: Integer;
  Info: TOfflineInfoArray;
  ReplyTo: TGuidArray;
  Resp: ROuchSendMessageReply;
  sStatus: String;

  procedure DoExtract(const Substr, Buffer: String; var Value: String);
  var
    i: Integer;
  begin
    i := Str.Pos(SubStr + '=', Buffer);

    if i > 0 then
    begin
      Value := Str.Copy(Buffer, i + Length(Substr) + 2);  // nick="nick" mail="mail" | status=usOffline
      i := Str.Pos('"', Value);

      if i > 0 then
        Value := Str.Copy(Value, 1, i - 1);
    end else
      Value := '';
  end;

begin
  ReplyTo := nil;

  try
    if Db.CollectOffline(FDataMgr, User, Info) then
      for i := 0 to Length(Info) - 1 do
        case Info[i].Kind of
          okMessage:
            Request.SendMessage(Session, User, Info[i].Id, Info[i].Text, 0, mkInstantMessage, Info[i].Time, ReplyTo);

          okMessageReply:
          begin
            Resp.Reply := DoMakeReply(0, COfflineMessageDelivered, Info[i].Id);
            Resp.SendMessage.Status := msReceived;
            Reply.SendMessageReply(Resp);
          end;

          okUserStatus:
          begin
            DoExtract('status', Info[i].Text, sStatus);
            Request.NotifyStatus(Session, Info[i].Id, TOuchUserStatus(Sil.Enum.Value(TypeInfo(TOuchUserStatus), sStatus, 'us')));
          end;
        end;
  except
    on e: Exception do
      DoLogFmt('TOuchClient.DoCheckOffline: %s', [e.Message], etError);
  end;
end;

procedure TOuchClient.GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String);
var
  Resp: ROuchGroupLogonReply;
  Users: TOuchUserArray;
begin
  Users := nil;

  if Db.FindSession(FDataMgr, Session, FUserId) then
  begin
    if Db.AppendGroupUser(FDataMgr, Group, FUserId, Password) then
    begin
      Resp.Reply := DoMakeReply(0, CLoggedIntoGroup, RequestId);
      Db.CollectGroupUser(FDataMgr, Group, FUserId, Users);
    end else
      Resp.Reply := DoMakeReply(1, CInvalidGroupOrPassword, RequestId);
  end else
    Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);

  Resp.GroupLogon.Users := Users;
  Reply.GroupLogonReply(Resp);
end;

procedure TOuchClient.NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus);
var
  Resp: ROuchNotifyStatusReply;
  dtTimeout: TDateTime;
  sData, sStatus: String;
begin
  dtTimeout := DateTime.Now + FDataMgr.ValidateTime;

  if Db.FindSession(FDataMgr, Session, FUserId) then
  begin
    if Db.ChangeSession(FDataMgr, Session, UserStatus, Connection.Parameters.RemoteAddress, dtTimeout) then
      Resp.Reply := DoMakeReply(0, CUserStatusUpdated, RequestId) else
      Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);
  end else
    Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);

  Resp.NotifyStatus.ExpirationTime := dtTimeout;
  Reply.NotifyStatusReply(Resp);

  if Resp.Reply.Result = 0 then
  begin
    sStatus := Sil.Enum.Name(TypeInfo(TOuchUserStatus), Ord(UserStatus), 'us');
    sData := Str.Format('status=%s', [sStatus]);
    Db.GroupAppendOffline(FDataMgr, RequestId, FUserId, DateTime.Now, okUserStatus, sData);
  end;
end;

procedure TOuchClient.QueryGroups(const RequestId, Session: TGuid);
var
  Resp: ROuchQueryGroupsReply;
  Groups: TOuchGroupArray;
begin
  Groups := nil;

  if Db.FindSession(FDataMgr, Session, FUserId) then
  begin
    if Db.CollectGroup(FDataMgr, Groups) then
      Resp.Reply := DoMakeReply(0, CGroupListRetrieved, RequestId) else
      Resp.Reply := DoMakeReply(1, CErrorAccessingGroupList, RequestId);
  end else
    Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);

  Resp.QueryGroups.Groups := Groups;
  Reply.QueryGroupsReply(Resp);
end;

procedure TOuchClient.SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray);
var
  Resp: ROuchSendMessageReply;
  Groups: TOuchGroupArray;
begin
  Groups := nil;

  if Db.FindSession(FDataMgr, Session, FUserId) then
  begin
    if Db.AppendOffline(FDataMgr, RequestId, User, FUserId, Time, okMessage, Text) then
      Resp.Reply := DoMakeReply(0, COfflineMessageStored, RequestId) else
      Resp.Reply := DoMakeReply(1, CErrorStoringOfflineMessage, RequestId);
  end else
    Resp.Reply := DoMakeReply(1, CInvalidSession, RequestId);

  Resp.SendMessage.Status := msOffline;
  Reply.SendMessageReply(Resp);
end;

{ IOuchReply }

procedure TOuchClient.DefineUserReply(const Data: ROuchDefineUserReply);
begin
  { none }
end;

procedure TOuchClient.QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
begin

end;

procedure TOuchClient.ConnectReply(const Data: ROuchConnectReply);
begin
  { none }
end;

procedure TOuchClient.NotifyStatusReply(const Data: ROuchNotifyStatusReply);
var
  Info: ROfflineInfo;
begin
  if Data.Reply.Result = 0 then
    Db.RemoveOfflineId(FDataMgr, Data.Reply.Id, FUserId, Info);
end;

procedure TOuchClient.GroupLogonReply(const Data: ROuchGroupLogonReply);
begin
  { none }
end;

procedure TOuchClient.QueryGroupsReply(const Data: ROuchQueryGroupsReply);
begin
  { none }
end;

procedure TOuchClient.SendMessageReply(const Data: ROuchSendMessageReply);
var
  Info: ROfflineInfo;
begin
  if Data.Reply.Result = 0 then
  begin
    Db.RemoveOfflineId(FDataMgr, Data.Reply.Id, FUserId, Info);
    Db.AppendOffline(FDataMgr, Data.Reply.Id, Info.FromUser, Info.ToUser, DateTime.Now, okMessageReply, '');
  end;
end;

{procedure TOuchClient.UserInfoReply(const Data: ROuchUserInfoReply);
var
  Info: ROfflineInfo;
begin
  if Data.Reply.Result = 0 then
    Db.RemoveOfflineId(FDataMgr, Data.Reply.Id, FUserId, Info);
end;}

procedure TOuchClient.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  inherited;
  FServer.Clients.Remove(IOuchClient(Self));
end;

end.
