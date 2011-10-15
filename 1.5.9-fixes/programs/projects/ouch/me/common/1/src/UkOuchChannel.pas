unit UkOuchChannel;

interface

uses
  Sil,
  SilProtocol,
  SilCoder,

  UiOuchProtocol;

{ TODO -olconde : Implementar un mecanismo que permita manejar operaciones asincronicas con timeout!! }

type
  TOuchChannel = class (
    // extends
    TInterfacedObject,
    // implements
    IConnectingEvents,
    IConnectedEvents,
    IBlindProtocolEvents,
    IEblisProtocolEvents)
  private
    FConnection: IClientSocketConnection;
    FRequest: IOuchRequest;
    FReply: IOuchReply;
    FBlind: IBlindProtocol;
    FEblis: IEBlisProtocol;
    FEblisKey: String;
  protected
    procedure DoStartup; virtual;
    procedure DoRelease; virtual;
  protected
    property Connection: IClientSocketConnection read FConnection;
    property Request: IOuchRequest read FRequest;
    property Reply: IOuchReply read FReply;
    property Blind: IBlindProtocol read FBlind;
    property Eblis: IEBlisProtocol read FEblis;
    property EblisKey: String read FEblisKey;
  protected // IConnectingEvents
    procedure OnConnected(const Event: TConnectionEvent); virtual;
    procedure OnFailed(const Event: TConnectionFailedEvent); virtual;
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent); virtual;
    procedure OnDataSent(const Event: TConnectionDataEvent); virtual;
    procedure OnDisconnected(const Event: TConnectionBreakEvent); virtual;
  protected // IBlindProtocolEvents
    procedure OnText(var Event: RBlindTextEvent); virtual;
    procedure OnRequest(var Event: RBlindProtocolIDEvent); virtual;
  protected // IEblisProtocolEvents
    procedure OnNegotiate(var Event: REblisNegotiateEvent); virtual;
  public
    constructor Create(const Connection: IClientSocketConnection);
    destructor Destroy; override;
  end;

implementation

uses
  UmOuchProtocol;

{ TOuchChannel }

constructor TOuchChannel.Create(const Connection: IClientSocketConnection);
begin
  inherited Create;

  FConnection := Connection;
  Sil.Sink.Connect(FConnection, Self);

  if FConnection.IsConnected then
    FConnection.SpawnThread;
end;

destructor TOuchChannel.Destroy;
begin
  DoRelease;
  inherited;
end;

procedure TOuchChannel.DoStartup;
begin
  FEblisKey := '';
  FBlind := SilProtocol.Tk.Blind(FConnection);
  Sil.Sink.Connect(FBlind, Self);
end;

procedure TOuchChannel.DoRelease;
begin
  if FBlind <> nil then
  begin
    Sil.Sink.Disconnect(FBlind, Self);
    FBlind := nil;
  end;

  if FEblis <> nil then
  begin
    Sil.Sink.Disconnect(FEblis, Self);
    FEblis := nil;
  end;

  if FReply <> nil then
  begin
    Sil.Sink.Disconnect(FReply, Self);
    FRequest := nil;
    FReply := nil;
  end;

  if FConnection <> nil then
  begin
    Sil.Sink.Disconnect(FConnection, Self);
    FConnection := nil;
  end;
end;

{ IConnectingEvents }

procedure TOuchChannel.OnConnected(const Event: TConnectionEvent);
var
  lwId: LongWord;
  Client: IClientSocketConnection;
//@  Cipher: ICipher; <--- PEDORRO!!
begin
  DoStartup;
  Client := (Event.Sender as IClientSocketConnection);

  if Client.IsRequested then
  begin
    if FBlind.Request(IEblisProtocol, lwId) then
    begin
      FEblis := SilProtocol.Tk.Eblis(lwId, Event.Sender);
      if not FEblis.Negotiate(FEblisKey) then
        raise Sil.Error.Create('Falló: Eblis.Negotiate');
    end else
      raise Sil.Error.Create('fue');

    if FBlind.Request(IOuchProtocol, lwId) then
    begin
      FReply := TOuchProtocol.Create(lwId, Client);
      Sil.Sink.Connect(FReply, Self);
      FRequest := FReply as IOuchRequest;

      {Cipher := SilCoder.Cipher.SimpleMix;

      if FBlind.Supports(Cipher.Id) then
      begin
        Cipher.Initialize(FEblisKey);
        (FReply as IOuchProtocol).Cipher := Cipher;
      end;}
    end else
      raise Sil.Error.Create('fue');      
  end;
end;

procedure TOuchChannel.OnFailed(const Event: TConnectionFailedEvent);
begin
//  DoRelease;
end;

{ IConnectedEvents }

procedure TOuchChannel.OnDataReceived(const Event: TConnectionDataEvent);
begin
end;

procedure TOuchChannel.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

procedure TOuchChannel.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  DoRelease;
end;

{ IBlindProtocolEvents }

procedure TOuchChannel.OnRequest(var Event: RBlindProtocolIDEvent);
begin
  if Guid.Compare(Event.Obj, IEBlisProtocol) = 0 then
  begin
    if FEblis = nil then
    begin
      FEblis := SilProtocol.Tk.Eblis(Event.Id, FConnection);
      Sil.Sink.Connect(FEblis, Self);
    end else
      Event.Id := (FReply as IFormatedProtocol).ProtocolID;
  end else
  if Guid.Compare(Event.Obj, IOuchProtocol) = 0 then
  begin
    if FReply = nil then
    begin
      FReply := TOuchProtocol.Create(Event.Id, FConnection);
      Sil.Sink.Connect(FReply, Self);
      FRequest := FReply as IOuchRequest;
    end else
      Event.Id := (FReply as IFormatedProtocol).ProtocolID;
  end;
end;

procedure TOuchChannel.OnText(var Event: RBlindTextEvent);
begin
end;

{ IEblisProtocolEvents }

procedure TOuchChannel.OnNegotiate(var Event: REblisNegotiateEvent);
begin
  FEblisKey := Event.Key;
end;

end.
