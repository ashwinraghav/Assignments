unit UmProtocolClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer,

  UtProtocol,
  UiProtocol;

type
  TClientSide = class (
    // extends
    TSilCustomImateProtocol,
    // implements
    IClientSide)
  private
    FHook: IClientSideEvents;
  protected
    procedure Initialize(const Hook: IUnknown); override;
    procedure Finalize; override;
  private
    procedure DoFireSendMessage(var Msg: RImateProtocolMessage); message PS_SENDMESSAGE;
    procedure DoFireSendInfo(var Msg: RImateProtocolMessage); message PS_SENDINFO;
    procedure DoFireConnectionRequest(var Msg: RImateProtocolMessage); message PS_REQUESTCONNECTION;
    procedure DoFireConnectionAccept(var Msg: RImateProtocolMessage); message PS_ACCEPTCONNECTION;
  protected // IClientSide
    procedure CreateAccount(const Nick, Password: String; out Id: TGuid);
    procedure GetAccount(const Nick, Password: String; out Id: TGuid);
    procedure Logon(const Id: TGuid; const Password: String);
    procedure SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure SendResponse(const MsgId: TGUID; const ToId: TGuid; const Time: TDateTime);
    procedure SendInfo(const Data: IParameters);
    procedure QueryInfo(const Id: TGuid; out Data: IParameterList);
    procedure QueryUsers(Status: TUserStatus; out Users: TUserArray);
    procedure QueryOfflines;
    procedure RequestConnection(const Params: IParameters);
    procedure AcceptConnection(const Params: IParameters);
  end;

implementation

uses SilLiFiler;

{ TClientSide }

procedure TClientSide.Initialize(const Hook: IUnknown);
begin
  Sil.Trace.Log('TClientSide.Initialize');
  Ref.GetInterface(Hook, IClientSideEvents, FHook);
end;

procedure TClientSide.Finalize;
begin
  Sil.Trace.Log('TClientSide.Finalize');
  FHook := nil;
end;

procedure TClientSide.CreateAccount(const Nick, Password: String; out Id: TGuid);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.CreateAccount', [Nick]);

  Packet := Protocol.CreatePacket(PS_CREATEACCOUNT);

  with Packet.Writer do
  begin
    WriteString(Nick);
    WriteString(Password);
  end;

  Protocol.SendPacket(Packet, Reply);
  Id := Reply.Reader.ReadGuid;

  Sil.Trace.Leave;
end;

procedure TClientSide.GetAccount(const Nick, Password: String; out Id: TGuid);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.GetAccount', [Nick]);

  Packet := Protocol.CreatePacket(PS_GETACCOUNT);

  with Packet.Writer do
  begin
    WriteString(Nick);
    WriteString(Password);
  end;

  Protocol.SendPacket(Packet, Reply);
  Id := Reply.Reader.ReadGuid;

  Sil.Trace.Leave;
end;

procedure TClientSide.Logon(const Id: TGuid; const Password: String);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.Logon', [Guid.ToStr(Id)]);

  Packet := Protocol.CreatePacket(PS_LOGON);

  with Packet.Writer do
  begin
    WriteGuid(Id);
    WriteString(Password);
  end;

  Protocol.SendPacket(Packet, Reply);

  Sil.Trace.Leave;
end;

procedure TClientSide.QueryInfo(const Id: TGuid; out Data: IParameterList);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.QueryInfo', [Guid.ToStr(Id)]);

  Packet := Protocol.CreatePacket(PS_QUERYINFO);
  Packet.Writer.WriteGuid(Id);

  Protocol.SendPacket(Packet, Reply);
  Data := Prot.ReadParams(Reply);

  Sil.Trace.Leave;
end;

procedure TClientSide.QueryUsers(Status: TUserStatus; out Users: TUserArray);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.QueryUsers', [Ord(Status)]);

  Packet := Protocol.CreatePacket(PS_QUERYUSERS);
  Packet.Writer.Write(Status, SizeOf(Status));

  Protocol.SendPacket(Packet, Reply);
  Users := Prot.ReadUserArray(Reply);

  Sil.Trace.Leave;
end;

procedure TClientSide.QueryOfflines;
var
  Packet: IPacket;
begin
  Sil.Trace.Enter('TClientSide.QueryOfflines');

  Packet := Protocol.CreatePacket(PS_QUERYOFFLINES);
  Protocol.SendPacket(Packet);

  Sil.Trace.Leave;
end;

procedure TClientSide.SendInfo(const Data: IParameters);
var
  Packet, Reply: IPacket;
begin
  Sil.Trace.Enter('TClientSide.SendInfo');

  Packet := Protocol.CreatePacket(PS_SENDINFO);
  Prot.WriteParams(Packet, Data);

  Protocol.SendPacket(Packet, Reply);

  Sil.Trace.Leave;
end;

procedure TClientSide.SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
var
  Packet: IPacket;
begin
  Sil.Trace.Enter('TClientSide.SendMessage');

  Packet := Protocol.CreatePacket(PS_SENDMESSAGE);
  Prot.WriteGuidArray(Packet, Recipients);

  with Packet.Writer do
  begin
    WriteGuid(MsgId);
    WriteString(Text);
    WriteDate(Sil.OS.TimeZone.LocalToGlobal(Time));
    WriteBoolean(Bc);
  end;

  Protocol.SendPacket(Packet);

  Sil.Trace.Leave;
end;

procedure TClientSide.SendResponse(const MsgId: TGUID; const ToId: TGuid; const Time: TDateTime);
begin

end;

procedure TClientSide.DoFireSendMessage(var Msg: RImateProtocolMessage);
var
  Event: RClientSideMessageEvent;
begin
  Sil.Trace.Enter('TClientSide.DoFireSendMessage');

  if not Assigned(FHook) then Exit;

  with Msg.Packet.Reader do
  begin
    Event.Sender := Self;
    Event.MsgId := ReadGuid;
    Event.UserId := ReadGuid;
    Event.Text := ReadString;
    Event.Time := Sil.OS.TimeZone.GlobalToLocal(ReadDate);
    Event.Recipients := Prot.ReadGuidArray(Msg.Packet);
  end;

  FHook.OnReceiveMessage(Event);

  Sil.Trace.Leave;
end;

procedure TClientSide.DoFireSendInfo(var Msg: RImateProtocolMessage);
var
  Event: RClientSideInfoEvent;
begin
  Sil.Trace.Enter('TClientSide.DoFireSendInfo');

  if not Assigned(FHook) then Exit;

  Event.UserId := Msg.Packet.Reader.ReadGuid;
  Event.Data := Prot.ReadParams(Msg.Packet);

  FHook.OnReceiveInfo(Event);

  Sil.Trace.Leave;
end;

procedure TClientSide.AcceptConnection(const Params: IParameters);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_ACCEPTCONNECTION);
  Prot.WriteParams(Packet, Params);
  Protocol.SendPacket(Packet);
end;

procedure TClientSide.DoFireConnectionAccept(var Msg: RImateProtocolMessage);
var
  Event: RClientSideConnectionEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Params := Prot.ReadParams(Msg.Packet);
  FHook.OnConnectionAccept(Event);
end;

procedure TClientSide.RequestConnection(const Params: IParameters);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_REQUESTCONNECTION);
  Prot.WriteParams(Packet, Params);
  Protocol.SendPacket(Packet);
end;

procedure TClientSide.DoFireConnectionRequest(var Msg: RImateProtocolMessage);
var
  Event: RClientSideConnectionEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Params := Prot.ReadParams(Msg.Packet);
  FHook.OnConnectionRequest(Event);
end;

end.
