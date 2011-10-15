unit UmProtocolServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer,

  UtProtocol,
  UiProtocol;

type
  TServerSide = class (
    // extends
    TSilCustomImateProtocol,
    // implements
    IServerSide)
  private
    FHook: IServerSideEvents;
  protected
    procedure Initialize(const Hook: IUnknown); override;
    procedure Finalize; override;
  private
    procedure DoFireCreateAccount(var Msg: RImateProtocolMessage); message PS_CREATEACCOUNT;
    procedure DoFireGetAccount(var Msg: RImateProtocolMessage); message PS_GETACCOUNT;
    procedure DoFireLogon(var Msg: RImateProtocolMessage); message PS_LOGON;
    procedure DoFireSendMessage(var Msg: RImateProtocolMessage); message PS_SENDMESSAGE;
    procedure DoFireSendResponse(var Msg: RImateProtocolMessage); message PS_RESPONSE;
    procedure DoFireSendInfo(var Msg: RImateProtocolMessage); message PS_SENDINFO;
    procedure DoFireQueryInfo(var Msg: RImateProtocolMessage); message PS_QUERYINFO;
    procedure DoFireQueryUsers(var Msg: RImateProtocolMessage); message PS_QUERYUSERS;
    procedure DoFireQueryOfflines(var Msg: RImateProtocolMessage); message PS_QUERYOFFLINES;
    procedure DoFireConnectionRequest(var Msg: RImateProtocolMessage); message PS_REQUESTCONNECTION;
    procedure DoFireConnectionAccept(var Msg: RImateProtocolMessage); message PS_ACCEPTCONNECTION;
  protected // IServerSide
    procedure SendMessage(const MsgId: TGUID; const UserId: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
    procedure SendInfo(const Id: TGuid; const Data: IParameters);
    procedure RequestConnection(const Params: IParameters);
    procedure AcceptConnection(const Params: IParameters);
    procedure SendResponse(const MsgId: TGUID; const FromId: TGuid; const Time: TDateTime);
  end;

implementation

uses SilLiFiler;

{ TServerSide }

procedure TServerSide.Initialize(const Hook: IUnknown);
begin
  Ref.GetInterface(Hook, IServerSideEvents, FHook);
end;

procedure TServerSide.Finalize;
begin
  FHook := nil;
end;

procedure TServerSide.SendInfo(const Id: TGuid; const Data: IParameters);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_SENDINFO);
  Packet.Writer.WriteGuid(Id);
  Prot.WriteParams(Packet, Data);

  Protocol.SendPacket(Packet);
end;

procedure TServerSide.SendMessage(const MsgId: TGUID; const UserId: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_SENDMESSAGE);

  with Packet.Writer do
  begin
    WriteGuid(MsgId);
    WriteGuid(UserId);
    WriteString(Text);
    WriteDate(Time);
  end;

  Prot.WriteGuidArray(Packet, Recipients);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireCreateAccount(var Msg: RImateProtocolMessage);
var
  Event: RServerSideCreateAccountEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  with Msg.Packet.Reader do
  begin
    Event.Sender := Self;
    Event.Nick := ReadString;
    Event.Password := ReadString;
  end;

  FHook.OnCreateAccount(Event);

  Packet := Protocol.CreatePacket(PS_CREATEACCOUNT);
  Packet.Writer.WriteGuid(Event.UserId);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireGetAccount(var Msg: RImateProtocolMessage);
var
  Event: RServerSideGetAccountEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  with Msg.Packet.Reader do
  begin
    Event.Sender := Self;
    Event.Nick := ReadString;
    Event.Password := ReadString;
  end;

  FHook.OnGetAccount(Event);

  Packet := Protocol.CreatePacket(PS_GETACCOUNT);
  Packet.Writer.WriteGuid(Event.UserId);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireLogon(var Msg: RImateProtocolMessage);
var
  Event: RServerSideLogonEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  with Msg.Packet.Reader do
  begin
    Event.Sender := Self;
    Event.UserId := ReadGuid;
    Event.Password := ReadString;
  end;

  FHook.OnLogon(Event);

  Packet := Protocol.CreatePacket(PS_LOGON);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireQueryInfo(var Msg: RImateProtocolMessage);
var
  Event: RServerSideQueryInfoEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.UserId := Msg.Packet.Reader.ReadGuid;
  FHook.OnQueryInfo(Event);

  Packet := Protocol.CreatePacket(PS_QUERYINFO);
  Prot.WriteParams(Packet, Event.Data);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireQueryUsers(var Msg: RImateProtocolMessage);
var
  Event: RServerSideQueryUsersEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Msg.Packet.Reader.Read(Event.Status, SizeOf(Event.Status));
  FHook.OnQueryUsers(Event);

  Packet := Protocol.CreatePacket(PS_QUERYUSERS);
  Prot.WriteUserArray(Packet, Event.Users);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireSendInfo(var Msg: RImateProtocolMessage);
var
  Event: RServerSideInfoEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Data := Prot.ReadParams(Msg.Packet);

  FHook.OnSendInfo(Event);
end;

procedure TServerSide.DoFireSendMessage(var Msg: RImateProtocolMessage);
var
  Event: RServerSideMessageEvent;
begin
  if not Assigned(FHook) then Exit;

  with Msg.Packet.Reader do
  begin
    Event.Sender := Self;
    Event.MsgId := ReadGuid;
    Event.Recipients := Prot.ReadGuidArray(Msg.Packet);
    Event.Text := ReadString;
    Event.Time := Sil.OS.TimeZone.GlobalToLocal(ReadDate);
    Event.Bc := ReadBoolean;
  end;

  FHook.OnSendMessage(Event);
end;

procedure TServerSide.DoFireQueryOfflines(var Msg: RImateProtocolMessage);
var
  Event: RServerSideQueryOfflinesEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  FHook.OnQueryOfflines(Event);
end;

procedure TServerSide.AcceptConnection(const Params: IParameters);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_ACCEPTCONNECTION);
  Prot.WriteParams(Packet, Params);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireConnectionAccept(var Msg: RImateProtocolMessage);
var
  Event: RServerSideConnectionEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Params := Prot.ReadParams(Msg.Packet);
  FHook.OnConnectionAccept(Event);
end;

procedure TServerSide.RequestConnection(const Params: IParameters);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_REQUESTCONNECTION);
  Prot.WriteParams(Packet, Params);
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireConnectionRequest(var Msg: RImateProtocolMessage);
var
  Event: RServerSideConnectionEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Params := Prot.ReadParams(Msg.Packet);
  FHook.OnConnectionRequest(Event);
end;

procedure TServerSide.SendResponse(const MsgId: TGUID; const FromId: TGuid; const Time: TDateTime);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_RESPONSE);

  with Packet.Writer do
  begin
    WriteGuid(MsgId);
    WriteGuid(FromId);
    WriteDate(Time);
  end;                                                       
  
  Protocol.SendPacket(Packet);
end;

procedure TServerSide.DoFireSendResponse(var Msg: RImateProtocolMessage);
var
  Event: RServerSideResponseEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;

  with Msg.Packet.Reader do
  begin
    Event.MsgId := ReadGuid;
    Event.Recipient := ReadGuid;
    Event.Time := ReadDate;
  end;

  FHook.OnSendResponse(Event);
end;

end.
