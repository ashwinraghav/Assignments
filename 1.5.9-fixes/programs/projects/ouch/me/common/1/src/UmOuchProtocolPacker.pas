unit UmOuchProtocolPacker;

interface

uses
  Sil,
  SilClasses,
  SilCoder,

  UiOuchProtocol,
  UcOuchProtocol;

type
  TOuchProtocolPacker = class (
    // extends
    TInterfacedObject,
    // implements
    IOuchRequestPacker)
  private
    FProtocol: Pointer;
  private
    procedure DoWriteGroups(const Packet: IProtocolPacket; const Groups: TOuchGroupArray);
    procedure DoWriteGuids(const Packet: IProtocolPacket; const Guids: TGuidArray);
    procedure DoWriteReply(const Packet: IProtocolPacket; const Reply: ROuchReply);
    procedure DoWriteUsers(const Packet: IProtocolPacket; const Users: TOuchUserArray);
    procedure DoWriteParams(const Packet: IProtocolPacket; const Data: IParameters);
    function DoCreatePacket(DataID: LongWord = 0): IProtocolPacket;
  protected // IOuchRequest
    function DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters): IProtocolPacket;
    function Connect(const User: TGuid; const Password: String): IProtocolPacket;
    function QueryOfflines(const RequestId, Session: TGuid): IProtocolPacket;
    function NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus): IProtocolPacket;
    function QueryGroups(const RequestId, Session: TGuid): IProtocolPacket;
    function GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String): IProtocolPacket;
    function SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray): IProtocolPacket;
    function DefineUserReply(const Data: ROuchDefineUserReply): IProtocolPacket;
    function ConnectReply(const Data: ROuchConnectReply): IProtocolPacket;
    function QueryOfflinesReply(const Data: ROuchQueryOfflinesReply): IProtocolPacket;
    function NotifyStatusReply(const Data: ROuchNotifyStatusReply): IProtocolPacket;
    function QueryGroupsReply(const Data: ROuchQueryGroupsReply): IProtocolPacket;
    function GroupLogonReply(const Data: ROuchGroupLogonReply): IProtocolPacket;
    function SendMessageReply(const Data: ROuchSendMessageReply): IProtocolPacket;
  public
    constructor Create(const Protocol: IProtocolBase = nil);
    destructor Destroy; override;
  end;

implementation

{ TOuchProtocolPacker }

constructor TOuchProtocolPacker.Create(const Protocol: IProtocolBase);
begin
  inherited Create;
  FProtocol := Pointer(Protocol);
end;

destructor TOuchProtocolPacker.Destroy;
begin
  FProtocol := nil;
  inherited;
end;

function TOuchProtocolPacker.DoCreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := TProtocolPacket.Create;

  if FProtocol <> nil then
    Result.ProtoID := IProtocolBase(FProtocol).ProtocolID else
    Result.ProtoID := 0;

  Result.ProtoVer := PROT_BUILD;
  Result.HeaderVer := HEADER_VER;
  Result.SessionID := 0;
  Result.DataID := DataID;
end;

procedure TOuchProtocolPacker.DoWriteReply(const Packet: IProtocolPacket; const Reply: ROuchReply);
begin
  Packet.Data.WriteInteger(Reply.Result);
  Packet.Data.WriteString(Reply.Comment);
  Packet.Data.WriteGuid(Reply.Id);
end;

procedure TOuchProtocolPacker.DoWriteUsers(const Packet: IProtocolPacket; const Users: TOuchUserArray);
var
  i: Integer;
begin
  Packet.Data.WriteLongWord(Length(Users));

  for i := 0 to Length(Users) - 1 do
  begin
    Packet.Data.WriteGuid(Users[i].User);
    DoWriteParams(Packet, Users[i].Data);
  end;
end;

procedure TOuchProtocolPacker.DoWriteGuids(const Packet: IProtocolPacket; const Guids: TGuidArray);
var
  i: Integer;
begin
  Packet.Data.WriteLongWord(Length(Guids));

  for i := 0 to Length(Guids) - 1 do
    Packet.Data.WriteGuid(Guids[i]);
end;

procedure TOuchProtocolPacker.DoWriteGroups(const Packet: IProtocolPacket; const Groups: TOuchGroupArray);
var
  i: Integer;
begin
  Packet.Data.WriteLongWord(Length(Groups));

  for i := 0 to Length(Groups) - 1 do
  begin
    Packet.Data.WriteGuid(Groups[i].Parent);
    Packet.Data.WriteGuid(Groups[i].Id);
    Packet.Data.WriteString(Groups[i].Name);
  end;
end;

function TOuchProtocolPacker.Connect(const User: TGuid; const Password: string): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_CONNECT);
  Result.Data.WriteGuid(User);
  Result.Data.WriteString(Password);
end;

procedure TOuchProtocolPacker.DoWriteParams(const Packet: IProtocolPacket; const Data: IParameters);
var
  e: IEnumerator;
  Item: RParameter;
begin
  Packet.Data.WriteLongWord(Data.Count);

  while Data.Enumerate(e, Item) do
  begin
    Packet.Data.WriteString(Item.Name);
    Packet.Data.WriteVariant(Item.Value);
  end;
end;

function TOuchProtocolPacker.DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_DEFINEUSER);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(User);
  Result.Data.WriteString(Password);
  DoWriteParams(Result, Data);
end;

function TOuchProtocolPacker.GroupLogon(const RequestId, Session, Group: TGuid; const Password: String): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_GROUPLOGON);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(Session);
  Result.Data.WriteGuid(Group);
  Result.Data.WriteString(Password);
end;

function TOuchProtocolPacker.NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_NOTIFYSTATUS);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(Session);
  Result.Data.WriteByte(Ord(UserStatus));
end;

function TOuchProtocolPacker.QueryGroups(const RequestId, Session: TGuid): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_QUERYGROUPS);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(Session);
end;

function TOuchProtocolPacker.QueryOfflines(const RequestId, Session: TGuid): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_QUERYOFFLINES);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(Session);
end;

function TOuchProtocolPacker.SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_SENDMESSAGE);
  Result.Data.WriteGuid(RequestId);
  Result.Data.WriteGuid(Session);
  Result.Data.WriteGuid(User);
  Result.Data.WriteString(Text);
  Result.Data.WriteWord(Priority);
  Result.Data.WriteByte(Ord(Kind));
  Result.Data.WriteFloat(Sil.OS.DateTime.LocalToGlobal(Time));
  DoWriteGuids(Result, ReplyTo);
end;

function TOuchProtocolPacker.ConnectReply(const Data: ROuchConnectReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_CONNECT_REPLY);
  DoWriteReply(Result, Data.Reply);
  Result.Data.WriteGuid(Data.Connect.IdUser);
  Result.Data.WriteGuid(Data.Connect.IdSession);
  Result.Data.WriteFloat(Sil.OS.DateTime.LocalToGlobal(Data.Connect.ExpirationTime));
end;

function TOuchProtocolPacker.DefineUserReply(const Data: ROuchDefineUserReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_DEFINEUSER_REPLY);
  DoWriteReply(Result, Data.Reply);
  Result.Data.WriteGuid(Data.DefineUser.IdUser);
end;

function TOuchProtocolPacker.GroupLogonReply(const Data: ROuchGroupLogonReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_GROUPLOGON_REPLY);
  DoWriteReply(Result, Data.Reply);
  DoWriteUsers(Result, Data.GroupLogon.Users);
end;

function TOuchProtocolPacker.NotifyStatusReply(const Data: ROuchNotifyStatusReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_NOTIFYSTATUS_REPLY);
  DoWriteReply(Result, Data.Reply);
  Result.Data.WriteFloat(Sil.OS.DateTime.LocalToGlobal(Data.NotifyStatus.ExpirationTime));
end;

function TOuchProtocolPacker.QueryGroupsReply(const Data: ROuchQueryGroupsReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_QUERYGROUPS_REPLY);
  DoWriteReply(Result, Data.Reply);
  DoWriteGroups(Result, Data.QueryGroups.Groups);
end;

function TOuchProtocolPacker.QueryOfflinesReply(const Data: ROuchQueryOfflinesReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_QUERYOFFLINES_REPLY);
  DoWriteReply(Result, Data.Reply);
end;

function TOuchProtocolPacker.SendMessageReply(const Data: ROuchSendMessageReply): IProtocolPacket;
begin
  Result := DoCreatePacket(PS_SENDMESSAGE_REPLY);
  DoWriteReply(Result, Data.Reply);
  Result.Data.WriteByte(Ord(Data.SendMessage.Status));
end;

end.
