unit UmOuchProtocol;

interface

uses
  Sil,
  SilClasses,
  SilCoder,

  UcOuchProtocol,
  UiOuchProtocol;

type
  TOuchProtocol = class (
    // extends
    TProtocolBase,
    // implements
    IOuchProtocol,
    IOuchRequest,
    IOuchReply)
  private
    FRemoteBuild: LongWord;
    FCipher: ICipher;
    FPacker: IOuchRequestPacker;
  private
    procedure DoReadReply(const Packet: IProtocolPacket; out Reply: ROuchReply);
    procedure DoReadUsers(const Packet: IProtocolPacket; out Users: TOuchUserArray);
    procedure DoReadGroups(const Packet: IProtocolPacket; out Groups: TOuchGroupArray);
    procedure DoReadGuids(const Packet: IProtocolPacket; out Guids: TGuidArray);
    function DoReadParams(const Packet: IProtocolPacket): IParameterList;
  private
    procedure FireDefineUser(var Msg: TProtocolBaseMessage); message PS_DEFINEUSER;
    procedure FireDefineUserReply(var Msg: TProtocolBaseMessage); message PS_DEFINEUSER_REPLY;
    procedure FireConnect(var Msg: TProtocolBaseMessage); message PS_CONNECT;
    procedure FireConnectReply(var Msg: TProtocolBaseMessage); message PS_CONNECT_REPLY;
    procedure FireQueryOfflines(var Msg: TProtocolBaseMessage); message PS_QUERYOFFLINES;
    procedure FireQueryOfflinesReply(var Msg: TProtocolBaseMessage); message PS_QUERYOFFLINES_REPLY;
    procedure FireNotifyStatus(var Msg: TProtocolBaseMessage); message PS_NOTIFYSTATUS;
    procedure FireNotifyStatusReply(var Msg: TProtocolBaseMessage); message PS_NOTIFYSTATUS_REPLY;
    procedure FireQueryGroups(var Msg: TProtocolBaseMessage); message PS_QUERYGROUPS;
    procedure FireQueryGroupsReply(var Msg: TProtocolBaseMessage); message PS_QUERYGROUPS_REPLY;
    procedure FireGroupLogon(var Msg: TProtocolBaseMessage); message PS_GROUPLOGON;
    procedure FireGroupLogonReply(var Msg: TProtocolBaseMessage); message PS_GROUPLOGON_REPLY;
    procedure FireSendMessage(var Msg: TProtocolBaseMessage); message PS_SENDMESSAGE;
    procedure FireSendMessageReply(var Msg: TProtocolBaseMessage); message PS_SENDMESSAGE_REPLY;
  protected // IOuchProtocol
    function GetCipher: ICipher;
    procedure SetCipher(const Value: ICipher);
  protected // IOuchRequest
    procedure DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters = nil);
    procedure Connect(const User: TGuid; const Password: string);
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
  protected // IProtocolBase
    function GetName: String; override;
    procedure SetConnection(const Value: IAbstractConnection); override;
    procedure PacketEncode(const Packet: IProtocolPacket); override;
    procedure PacketDecode(const Packet: IProtocolPacket); override;
  public
    constructor Create(ID: LongWord; const Connection: IAbstractConnection);
    destructor Destroy; override;
  end;

implementation

uses
  UmOuchProtocolPacker, SilSiPacketBuilder;

{ TOuchProtocol }

constructor TOuchProtocol.Create(ID: LongWord; const Connection: IAbstractConnection);
begin
  inherited Create(ID);
  SetConnection(Connection);
  FPacker := TOuchProtocolPacker.Create(Self);
end;

destructor TOuchProtocol.Destroy;
begin
  inherited;
end;

procedure TOuchProtocol.SetConnection(const Value: IAbstractConnection);
begin
  if FConnection <> nil then Sil.Sink.Disconnect(FConnection, Self);
  if Value <> nil then Sil.Sink.Connect(Value, Self);
  inherited SetConnection(Value);
end;

function TOuchProtocol.GetName: String;
begin
  Result := 'OuchProtocol';
end;

procedure TOuchProtocol.PacketDecode(const Packet: IProtocolPacket);
var
  Dest: String;
begin
  if FCipher <> nil then
    with Packet do
    begin
      FCipher.Encode(Data.Buffer^, Dest, Data.Size);
      Data.ResetBuffer(PChar(Dest), Length(Dest));
    end;
end;

procedure TOuchProtocol.PacketEncode(const Packet: IProtocolPacket);
var
  Dest: String;
begin
  if FCipher <> nil then
    with Packet do
    begin
      FCipher.Decode(Data.Buffer^, Dest, Data.Size);
      Data.ResetBuffer(PChar(Dest), Length(Dest));
    end;
end;

function TOuchProtocol.GetCipher: ICipher;
begin
  Result := FCipher;
end;

procedure TOuchProtocol.SetCipher(const Value: ICipher);
begin
  FCipher := Value;
end;

procedure TOuchProtocol.Connect(const User: TGuid; const Password: string);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.Connect(User, Password);
  Send(Packet, 'TOuchProtocol.Connect');
end;

procedure TOuchProtocol.DoReadReply(const Packet: IProtocolPacket; out Reply: ROuchReply);
begin
  Reply.Result := Packet.Data.ReadInteger;
  Reply.Comment := Packet.Data.ReadString;
  Reply.Id := Packet.Data.ReadGuid;
end;

function TOuchProtocol.DoReadParams(const Packet: IProtocolPacket): IParameterList;
var
  i, iCount: Integer;
  sName: String;
begin
  Result := Sil.List.Parameters;
  iCount := Packet.Data.ReadLongWord;

  for i := 1 to iCount do
  begin
    sName := Packet.Data.ReadString;
    Result[sName] := Packet.Data.ReadVariant;
  end;
end;

procedure TOuchProtocol.DoReadUsers(const Packet: IProtocolPacket; out Users: TOuchUserArray);
var
  i: Integer;
begin
  SetLength(Users, Packet.Data.ReadLongWord);

  for i := 0 to Length(Users) - 1 do
  begin
    Users[i].User := Packet.Data.ReadGuid;
    Users[i].Data := DoReadParams(Packet);
  end;
end;

procedure TOuchProtocol.DoReadGroups(const Packet: IProtocolPacket; out Groups: TOuchGroupArray);
var
  i: Integer;
begin
  SetLength(Groups, Packet.Data.ReadLongWord);

  for i := 0 to Length(Groups) - 1 do
  begin
    Groups[i].Parent := Packet.Data.ReadGuid;
    Groups[i].Id := Packet.Data.ReadGuid;
    Groups[i].Name := Packet.Data.ReadString;
  end;
end;

procedure TOuchProtocol.DoReadGuids(const Packet: IProtocolPacket; out Guids: TGuidArray);
var
  i: Integer;
begin
  SetLength(Guids, Packet.Data.ReadLongWord);

  for i := 0 to Length(Guids) - 1 do
    Guids[i] := Packet.Data.ReadGuid;
end;

procedure TOuchProtocol.FireConnect(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  User: TGuid;
  Password: string;
begin
  FRemoteBuild := Msg.Packet.ProtoVer;

  if FEvents <> nil then
  begin
    User := Msg.Packet.Data.ReadGuid;
    Password := Msg.Packet.Data.ReadString;

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.Connect(User, Password);
  end;
end;

procedure TOuchProtocol.ConnectReply(const Data: ROuchConnectReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.ConnectReply(Data);
  Send(Packet, 'TOuchProtocol.ConnectReply');
end;

procedure TOuchProtocol.FireConnectReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchConnectReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    Data.Connect.IdUser := Msg.Packet.Data.ReadGuid;
    Data.Connect.IdSession := Msg.Packet.Data.ReadGuid;
    Data.Connect.ExpirationTime := Sil.OS.DateTime.GlobalToLocal(Msg.Packet.Data.ReadFloat);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.ConnectReply(Data);
  end;
end;

procedure TOuchProtocol.GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.GroupLogon(RequestId, Session, Group, Password);
  Send(Packet, 'TOuchProtocol.GroupLogon');
end;

procedure TOuchProtocol.FireGroupLogon(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  Session, Id, Group: TGuid;
  Password: String;
begin
  if FEvents <> nil then
  begin
    Id := Msg.Packet.Data.ReadGuid;
    Session := Msg.Packet.Data.ReadGuid;
    Group := Msg.Packet.Data.ReadGuid;
    Password := Msg.Packet.Data.ReadString;

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.GroupLogon(Id, Session, Group, Password);
  end;
end;

procedure TOuchProtocol.GroupLogonReply(const Data: ROuchGroupLogonReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.GroupLogonReply(Data);
  Send(Packet, 'TOuchProtocol.GroupLogonReply');
end;

procedure TOuchProtocol.FireGroupLogonReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchGroupLogonReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    DoReadUsers(Msg.Packet, Data.GroupLogon.Users);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.GroupLogonReply(Data);
  end;
end;

procedure TOuchProtocol.NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.NotifyStatus(RequestId, Session, UserStatus);
  Send(Packet, 'TOuchProtocol.NotifyStatus');
end;

procedure TOuchProtocol.FireNotifyStatus(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  Session, Id: TGuid;
  UserStatus: TOuchUserStatus;
begin
  if FEvents <> nil then
  begin
    Id := Msg.Packet.Data.ReadGuid;
    Session := Msg.Packet.Data.ReadGuid;
    UserStatus := TOuchUserStatus(Msg.Packet.Data.ReadByte);

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.NotifyStatus(Id, Session, UserStatus);
  end;
end;

procedure TOuchProtocol.NotifyStatusReply(const Data: ROuchNotifyStatusReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.NotifyStatusReply(Data);
  Send(Packet, 'TOuchProtocol.NotifyStatusReply');
end;

procedure TOuchProtocol.FireNotifyStatusReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchNotifyStatusReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    Data.NotifyStatus.ExpirationTime := Sil.OS.DateTime.GlobalToLocal(Msg.Packet.Data.ReadFloat);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.NotifyStatusReply(Data);
  end;
end;

procedure TOuchProtocol.QueryGroups(const RequestId, Session: TGuid);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.QueryGroups(RequestId, Session);
  Send(Packet, 'TOuchProtocol.QueryGroups');
end;

procedure TOuchProtocol.FireQueryGroups(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  Session, Id: TGuid;
begin
  if FEvents <> nil then
  begin
    Id := Msg.Packet.Data.ReadGuid;
    Session := Msg.Packet.Data.ReadGuid;

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.QueryGroups(Id, Session);
  end;
end;

procedure TOuchProtocol.QueryGroupsReply(const Data: ROuchQueryGroupsReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.QueryGroupsReply(Data);
  Send(Packet, 'TOuchProtocol.QueryGroupsReply');
end;

procedure TOuchProtocol.FireQueryGroupsReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchQueryGroupsReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    DoReadGroups(Msg.Packet, Data.QueryGroups.Groups);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.QueryGroupsReply(Data);
  end;
end;

procedure TOuchProtocol.SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.SendMessage(RequestId, Session, User, Text, Priority, Kind, Time, ReplyTo);
  Send(Packet, 'TOuchProtocol.SendMessage');
end;

procedure TOuchProtocol.FireSendMessage(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  Session, User, Id: TGuid;
  Text: String;
  Priority: Word;
  Kind: TOuchMessageKind;
  Time: TDateTime;
  ReplyTo: TGuidArray;
begin
  if FEvents <> nil then
  begin
    Id := Msg.Packet.Data.ReadGuid;
    Session := Msg.Packet.Data.ReadGuid;
    User := Msg.Packet.Data.ReadGuid;
    Text := Msg.Packet.Data.ReadString;
    Priority := Msg.Packet.Data.ReadWord;
    Kind := TOuchMessageKind(Msg.Packet.Data.ReadByte);
    Time := Sil.OS.DateTime.GlobalToLocal(Msg.Packet.Data.ReadFloat);
    DoReadGuids(Msg.Packet, ReplyTo);

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.SendMessage(Id, Session, User, Text, Priority, Kind, Time, ReplyTo);
  end;
end;

procedure TOuchProtocol.SendMessageReply(const Data: ROuchSendMessageReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.SendMessageReply(Data);
  Send(Packet, 'TOuchProtocol.SendMessageReply');
end;

procedure TOuchProtocol.FireSendMessageReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchSendMessageReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    Data.SendMessage.Status := TOuchMessageStatus(Msg.Packet.Data.ReadByte);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.SendMessageReply(Data);
  end;
end;

procedure TOuchProtocol.DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters = nil);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.DefineUser(RequestId, User, Password, Data);
  Send(Packet, 'TOuchProtocol.DefineUser');
end;

procedure TOuchProtocol.FireDefineUser(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  RequestId, User: TGuid;
  sPassword: String;
  Data: IParameters;
begin
  if FEvents <> nil then
  begin
    RequestId := Msg.Packet.Data.ReadGuid;
    User := Msg.Packet.Data.ReadGuid;
    sPassword := Msg.Packet.Data.ReadString;
    Data := DoReadParams(Msg.Packet);

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.DefineUser(RequestId, User, sPassword, Data);
  end;
end;

procedure TOuchProtocol.DefineUserReply(const Data: ROuchDefineUserReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.DefineUserReply(Data);
  Send(Packet, 'TOuchProtocol.DefineUserReply');
end;

procedure TOuchProtocol.FireDefineUserReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchDefineUserReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);
    Data.DefineUser.IdUser := Msg.Packet.Data.ReadGuid;

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.DefineUserReply(Data);
  end;
end;

procedure TOuchProtocol.QueryOfflines(const RequestId, Session: TGuid);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.QueryOfflines(RequestId, Session);
  Send(Packet, 'TOuchProtocol.QueryOfflines');
end;

procedure TOuchProtocol.FireQueryOfflines(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchRequest;
  RequestID, Session: TGuid;
begin
  if FEvents <> nil then
  begin
    RequestID := Msg.Packet.Data.ReadGuid;
    Session := Msg.Packet.Data.ReadGuid;

    while FEvents.Enumerate(e, Sink, IOuchRequest) do
      Sink.QueryOfflines(RequestID, Session);
  end;
end;

procedure TOuchProtocol.QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
var
  Packet: IProtocolPacket;
begin
  Packet := FPacker.QueryOfflinesReply(Data);
  Send(Packet, 'TOuchProtocol.QueryOfflinesReply');
end;

procedure TOuchProtocol.FireQueryOfflinesReply(var Msg: TProtocolBaseMessage);
var
	e: IEnumerator;
	Sink: IOuchReply;
  Data: ROuchQueryOfflinesReply;
begin
  if FEvents <> nil then
  begin
    DoReadReply(Msg.Packet, Data.Reply);

    while FEvents.Enumerate(e, Sink, IOuchReply) do
      Sink.QueryOfflinesReply(Data);
  end;
end;

end.
