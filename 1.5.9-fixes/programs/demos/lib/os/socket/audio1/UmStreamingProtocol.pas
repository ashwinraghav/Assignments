unit UmStreamingProtocol;

interface

{$include Defines.inc}

uses
  Sil,
  UiStreamingProtocol;

const
  SP_BASE                 = $400;
  SP_SUBSCRIBE            = SP_BASE                 + 1;
  SP_UNSUBSCRIBE          = SP_SUBSCRIBE            + 1;
  SP_SENDBUFFER           = SP_UNSUBSCRIBE          + 1;
  SP_SENDPARAMS           = SP_SENDBUFFER           + 1;

const
  SM_UNKNOWN              = 0;
  SM_SUBSCRIBER           = 1;
  SM_SUBSCRIPTION         = 2;

type
  RStreamingMessage = record
    Id: LongWord;
    Host: ISocketAddress;
    Subscriber: ISubscriber;
    Packet: IPacket;
  end;

  TStreamingProtocol = class (TSilInterfacedObject, IStreamingProtocol)
  private
    FSocket: ISocketPeer;
    FSubscriptions: IInterfaceList;
    FSubscribers: IInterfaceList;
    FHook: Pointer;
    function GetHook: IStreamingProtocolEvents;
    function DoIndexOf(const List: IInterfaceList; const Host: ISocketAddress): Integer;
    function DoGetSubscriber(const List: IInterfaceList; const Host: ISocketAddress): ISubscriber;
    function DoAdd(const List: IInterfaceList; const Item: ISubscriber): Integer;
    function DoRemove(const List: IInterfaceList; const Item: ISubscriber): Integer;
  private
    function DoSend(const Packet: IPacket; const Host: ISocketAddress): Boolean;
    procedure FireDataReceived(var Msg: RStreamingMessage); message SP_SENDBUFFER;
    procedure FireSubscriberReceived(var Msg: RStreamingMessage); message SP_SUBSCRIBE;
    procedure FireParamsReceived(var Msg: RStreamingMessage); message SP_SENDPARAMS;
    //procedure FireSubscriptionRevoked(var Msg: RStreamingMessage);
  protected // IStreamingProtocol
    function SendBuffer(const Buffer; Size: LongWord): LongWord;
    function SendParams(const Params: IParameters = nil): Boolean; overload;
    function SendParams(const Host: ISocketAddress; const Params: IParameters): Boolean; overload;
    function Receive(const Host: ISocketAddress; const Buffer; Size: LongWord): LongWord;
    function Subscribe(const Host: ISocketAddress): Boolean;
    function UnSubscribe(const Host: ISocketAddress): Boolean;
    function RemoveSubscriber(const Host: ISocketAddress): Boolean;
    function AddSubscriber(RemoteId: Integer; const Host: ISocketAddress): Boolean;
  public
    constructor Create(const Socket: ISocketPeer; const Hook: IStreamingProtocolEvents);
    destructor Destroy; override;
  end;

  TSubscriber = class (TSilObject, ISubscriber)
  private
    FHost: ISocketAddress;
    FData: IUnknown;
    FLocalId: Integer;
    FRemoteId: Integer; 
  protected
    function GetHost: ISocketAddress;
    function GetData: IUnknown;
    procedure SetData(const Value: IUnknown);
    function GetLocalId: Integer;
    procedure SetLocalId(Value: Integer);
    function GetRemoteId: Integer;
    procedure SetRemoteId(Value: Integer);
    property Host: ISocketAddress read GetHost;
    property Data: IUnknown read GetData write SetData;
    property LocalId: Integer read GetLocalId write SetLocalId;
    property RemoteId: Integer read GetRemoteId write SetRemoteId;
  public
    constructor Create(const Host: ISocketAddress; const Data: IUnknown);
    destructor Destroy; override;
  end;

implementation

uses SilLtList, SilLiPacket, SilLiStream, SilLiFiler, SilLiParameters,
  SilLiInterfaceList, SilLiEnumerator;

{ TStreamingProtocol }

constructor TStreamingProtocol.Create(const Socket: ISocketPeer; const Hook: IStreamingProtocolEvents);
begin
  inherited Create;

  MakeRef(Hook, @FHook);

  FSocket := Socket;
  FSubscriptions := Sil.List.InterfaceList(true);
  FSubscribers := Sil.List.InterfaceList(true);
end;

destructor TStreamingProtocol.Destroy;
begin
  FSocket := nil;
  FSubscriptions := nil;
  FSubscribers := nil;

  DropRef(@FHook);

  inherited;
end;

function TStreamingProtocol.GetHook: IStreamingProtocolEvents;
begin
  Result := IStreamingProtocolEvents(FHook);
end;

function TStreamingProtocol.DoSend(const Packet: IPacket; const Host: ISocketAddress): Boolean;
begin
  Result := FSocket.Stream.WriteTo(Packet.Buffer.Memory^, Packet.Buffer.Size, Host) > 0;
end;

function TStreamingProtocol.DoIndexOf(const List: IInterfaceList; const Host: ISocketAddress): Integer;
var
  Enum: IEnumerator;
  Item: ISubscriber;
begin
  while List.Enumerate(Enum, Item) do
    if Item.Host.Address = Host.Address then
    begin
      Result := Enum.Iteration;
      Exit;
    end;

  Result := -1;
end;

function TStreamingProtocol.DoAdd(const List: IInterfaceList; const Item: ISubscriber): Integer;
begin
  Result := List.IndexOf(nil);

  if Result = -1 then
    Result := List.Add(Item) else
    List[Result] := Item;
end;

function TStreamingProtocol.DoRemove(const List: IInterfaceList; const Item: ISubscriber): Integer;
begin
  Result := List.IndexOf(Item);

  if Result >= 0 then
  begin
    List[Result] := nil;

    while (List.Count > 0) and not Assigned(List.Last) do
      List.Delete(List.Count - 1);
  end;
end;

function TStreamingProtocol.DoGetSubscriber(const List: IInterfaceList; const Host: ISocketAddress): ISubscriber;
var
  Index: Integer;
begin
  Index := DoIndexOf(List, Host);

  if Index > -1 then
    Result := ISubscriber(List[Index]) else
    Result := nil;
end;

function TStreamingProtocol.AddSubscriber(RemoteId: Integer; const Host: ISocketAddress): Boolean;
var
  Suscriber: ISubscriber;
begin
  Result := DoIndexOf(FSubscribers, Host) = -1;
  if not Result then Exit;

  Suscriber := TSubscriber.Create(Host, nil);
  Suscriber.LocalId := DoAdd(FSubscribers, Suscriber);
  Suscriber.RemoteId := RemoteId;
end;

function TStreamingProtocol.RemoveSubscriber(const Host: ISocketAddress): Boolean;
var
  Index: Integer;
begin
  Index := DoIndexOf(FSubscribers, Host);
  Result := Index >= 0;

  if Result then
    DoRemove(FSubscribers, ISubscriber(FSubscribers[Index]));
end;

function TStreamingProtocol.Receive(const Host: ISocketAddress; const Buffer; Size: LongWord): LongWord;
var
  Packet: IPacket;
  Msg: RStreamingMessage;
  Index: Integer;
  Mode: Byte;
begin
  Result := 0;
  if Size = 0 then Exit;

  try
    Packet := Sil.Stream.TypeFactory.Packet;
    Packet.Writer.Write(Buffer, Size);
    Packet.Buffer.Position := 0;

    Msg.Id := Packet.Reader.ReadWord;
    Msg.Host := Host;

    Mode := Packet.Reader.ReadByte;

    case Mode of
      SM_SUBSCRIBER:
      begin
        Index := Packet.Reader.ReadInteger;

        if FSubscribers.ValidIndex(Index) then
          Msg.Subscriber := ISubscriber(FSubscribers[Index]) else
          Msg.Subscriber := nil;
      end;

      SM_SUBSCRIPTION:
      begin
        Index := Packet.Reader.ReadInteger;

        if FSubscriptions.ValidIndex(Index) then
          Msg.Subscriber := ISubscriber(FSubscriptions[Index]) else
          Msg.Subscriber := nil;
      end;
    end;

    Msg.Packet := Packet;
    Result := Packet.Buffer.Size;
    Dispatch(Msg);
  except end;
end;

function TStreamingProtocol.Subscribe(const Host: ISocketAddress): Boolean;
var
  Packet: IPacket;
  Subscriber: ISubscriber;
begin
  if DoIndexOf(FSubscriptions, Host) <> -1 then
  begin
    Result := false;
    Exit;
  end;

  Subscriber := TSubscriber.Create(Host, nil);
  Subscriber.LocalId := FSubscriptions.Add(Subscriber);

  Packet := Sil.Stream.TypeFactory.Packet;
  Packet.Writer.WriteWord(SP_SUBSCRIBE);
  Packet.Writer.WriteByte(SM_SUBSCRIBER);
  Packet.Writer.WriteInteger(-1);
  Packet.Writer.WriteInteger(Subscriber.LocalId);
  Result := DoSend(Packet, Host);
end;

procedure TStreamingProtocol.FireSubscriberReceived(var Msg: RStreamingMessage);
var
  Hook: IStreamingProtocolEvents;
  Event: RStreamingSuscriberEvent;
begin
  Hook := GetHook;

  if Assigned(Hook) then
  begin
    Event.Sender := Self;
    Event.Id := Msg.Packet.Reader.ReadInteger;
    Event.Host := Msg.Host;
    Hook.OnSubscriberReceived(Event);
  end;
end;

function TStreamingProtocol.UnSubscribe(const Host: ISocketAddress): Boolean;
var
  Packet: IPacket;
  Subscriber: ISubscriber;
begin
  Subscriber := DoGetSubscriber(FSubscriptions, Host);

  if Assigned(Subscriber) then
  begin
    Packet := Sil.Stream.TypeFactory.Packet;
    Packet.Writer.WriteWord(SP_UNSUBSCRIBE);
    Packet.Writer.WriteByte(SM_SUBSCRIBER);
    Packet.Writer.WriteInteger(Subscriber.RemoteId);
    Result := DoSend(Packet, Host);

    if Result then
      FSubscriptions.Remove(Subscriber);
  end else
    Result := false;
end;

{procedure TStreamingProtocol.FireSubscriptionRevoked(var Msg: RStreamingMessage);
begin

end;}

function TStreamingProtocol.SendParams(const Params: IParameters = nil): Boolean;
var
  Enum: IEnumerator;
  Item: ISubscriber;
begin
  Result := false;

  while FSubscribers.Enumerate(Enum, Item) do
    Result := SendParams(Item.Host, Params);
end;

function TStreamingProtocol.SendParams(const Host: ISocketAddress; const Params: IParameters): Boolean;
var
  Packet: IPacket;
  Enum: IEnumerator;
  Item: RParameter;
  Subscriber: ISubscriber;
begin
  Result := false;
  Subscriber := DoGetSubscriber(FSubscribers, Host);
  if not Assigned(Subscriber) then Exit;

  Packet := Sil.Stream.TypeFactory.Packet;
  Packet.Writer.WriteWord(SP_SENDPARAMS);
  Packet.Writer.WriteByte(SM_SUBSCRIPTION);
  Packet.Writer.WriteInteger(Subscriber.RemoteId);
  Packet.Writer.WriteInteger(Subscriber.LocalId);

  if Assigned(Params) then
  begin
    Packet.Writer.WriteLongWord(Params.Count);

    while Params.Enumerate(Enum, Item) do
    begin
      Packet.Writer.WriteString(Item.Name);
      Packet.Writer.WriteVariant(Item.Value);
    end;
  end else
    Packet.Writer.WriteLongWord(0);

  Result := DoSend(Packet, Host);
end;

procedure TStreamingProtocol.FireParamsReceived(var Msg: RStreamingMessage);
var
  Hook: IStreamingProtocolEvents;
  Event: RStreamingParamsEvent;
  Count: LongWord;
  Item: RParameter;
  Subscription: ISubscriber;
begin
  Hook := GetHook;

  if Assigned(Hook) then
  begin
    Subscription := DoGetSubscriber(FSubscriptions, Msg.Host);

    if Assigned(Subscription) then
    begin
      Subscription.RemoteId := Msg.Packet.Reader.ReadInteger;

      Event.Sender := Self;
      Event.Subscription := Subscription;
      Event.Params := ListTool.Parameters;

      Count := Msg.Packet.Reader.ReadLongWord;

      while Count > 0 do
      begin
        Dec(Count);
        Item.Name := Msg.Packet.Reader.ReadString;
        Item.Value := Msg.Packet.Reader.ReadVariant;
        Event.Params[Item.Name] := Item.Value;
      end;

      Hook.OnParamsReceived(Event);
    end;
  end;
end;

function TStreamingProtocol.SendBuffer(const Buffer; Size: LongWord): LongWord;
var
  Packet: IPacket;
  Enum: IEnumerator;
  Item: ISubscriber;
  IdPos: LongWord;
begin
  Result := 0;

  Packet := Sil.Stream.TypeFactory.Packet;
  Packet.Writer.WriteWord(SP_SENDBUFFER);
  Packet.Writer.WriteByte(SM_SUBSCRIPTION);
  IdPos := Packet.Buffer.Position;
  Packet.Writer.WriteInteger(-1);
  Packet.Writer.WriteLongWord(Size);
  Packet.Writer.Write(Buffer, Size);

  while FSubscribers.Enumerate(Enum, Item) do
  begin
    Packet.Buffer.Position := IdPos;
    Packet.Writer.WriteInteger(Item.RemoteId);
    if DoSend(Packet, Item.Host) then Inc(Result);
  end;
end;

procedure TStreamingProtocol.FireDataReceived(var Msg: RStreamingMessage);
var
  Hook: IStreamingProtocolEvents;
  Event: RStreamingDataEvent;
begin
  Hook := GetHook;

  if Assigned(Hook) then
  begin
    Event.Sender := Self;
    Event.Subscription := Msg.Subscriber;
    Event.Size := Msg.Packet.Reader.ReadLongWord;
    Event.Buffer := Msg.Packet.Buffer.Current;
    Hook.OnDataReceived(Event);
  end;
end;

{ TSubscriber }

constructor TSubscriber.Create(const Host: ISocketAddress; const Data: IInterface);
begin
  inherited Create;

  FLocalId := -1;
  FRemoteId := -1;
  FHost := Host;
  FData := Data;
end;

destructor TSubscriber.Destroy;
begin
  FHost := nil;
  FData := nil;
  inherited;
end;

function TSubscriber.GetData: IUnknown;
begin
  Result := FData;
end;

function TSubscriber.GetHost: ISocketAddress;
begin
  Result := FHost;
end;

function TSubscriber.GetLocalId: Integer;
begin
  Result := FLocalId;
end;

function TSubscriber.GetRemoteId: Integer;
begin
  Result := FRemoteId;
end;

procedure TSubscriber.SetData(const Value: IInterface);
begin
  FData := Value;
end;

procedure TSubscriber.SetLocalId(Value: Integer);
begin
  FLocalId := Value;
end;

procedure TSubscriber.SetRemoteId(Value: Integer);
begin
  FRemoteId := Value;
end;

end.
