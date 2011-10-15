unit UmConnection;

interface

uses
  Sil,
  SilLayer,
  SilCoder,
  SilClasses,

  UiKeepAlive,
  UmKeepAlive,
  UiConnection;

type
  TConnection = class;
  TConnectionClient = class;
  TConnectionServer = class;
  TConnectionList = class;

  TConnection = class (
    // extends
    TSilInterfacedObject,
    // implements
    IConnection,
    IKeepAliveEvents,
    ILayerActivationEvents)
  protected
    FList: Pointer;
    FChain: ILayerChain;
    FSlot: ILayerSlot;
    FBlind: IBlindProtocol;
    FKeepAlive: IKeepAlive;
    FCipherKey: String;
  private
    procedure DoReleaseKeepAlive;
  protected
    procedure DoInitialize; virtual;
    procedure DoClean; virtual;
    function DoGetList: IConnectionList;
    procedure DoInitLayers; virtual; abstract;
    function DoAddProt(Id: LongWord; const Protocol, Listener: IUnknown; AsyncDispatch: Boolean): ILayerChain;
    function DoAddProtThruBlind(const Protocol: IInterface; const IID: TGuid; AsyncDispatch: Boolean; const Listener: IUnknown; out Chain: ILayerChain): Boolean;
    procedure FileConnectionSuccess;
    procedure FileConnectionLost;
    procedure FireConnectionFailed;
  protected // IConnection
    function GetChain: ILayerChain;
    function AddProtocol(const Protocol: IUnknown; const IID: TGuid; AsyncDispatch: Boolean; const Listener: IUnknown): Boolean; 
    procedure Start;
    procedure Stop;
    procedure Drop;
    property Chain: ILayerChain read GetChain;
  protected // ILayerActivationEvents
    procedure OnLayerActivated(const Event: RLayerActivated);
    procedure OnLayerDeactivated(const Event: RLayerDeactivated);
  protected // IKeepAliveEvents
    procedure OnKeepAlive(const Sender: IKeepAlive; out Success: Boolean);
  public
    constructor Create(const List: IConnectionList = nil);
    destructor Destroy; override;
  end;

  TConnectionSocket = class (TConnection)
  protected
    FAddress: String;
    FPort: Word;
  public
    constructor Create(const List: IConnectionList; const Address: String; Port: Word);
  end;

  TConnectionClient = class (TConnectionSocket)
  protected
    procedure DoInitLayers; override;
  public
    constructor CreateConnected(const List: IConnectionList; const Chain: ILayerChain);
  end;

  TConnectionServer = class (
    TConnectionSocket,
    ILayerChainSource,
    IBlindProtocolHook,
    IEblisProtocolHook)
  private
    FConnections: IConnectionList;
    FFileServer: IUnknown;
    FEblis: IEblisProtocol;
  protected
    procedure DoInitialize; override;
    procedure DoClean; override;
    procedure DoInitLayers; override;
    procedure DoFileProtocolRequest(var Request: RBlindRequestEvent);
  protected // ILayerChainSource
    procedure CreateChain(const Chain: ILayerChain; const Context: IUnknown = nil);
    procedure DestroyChain(const Chain: ILayerChain; const Context: IUnknown = nil);
  protected // IBlindProtocolHook
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindRequestEvent);
  protected // IEblisProtocolHook
    procedure OnNegotiate(var Event: REblisNegotiateEvent);
  end;

  TConnectionList = class (
    // extends
    TSilInterfaceList,
    // implements
    IConnections,
    IConnectionList)
  private
    procedure DoDropConnections;
  protected // IConnections
    function GetItem(Index: Integer): IConnection;
    function Enumerate(var Enum: IEnumerator; out Item: IConnection): Boolean; reintroduce;
    property Items[Index: Integer]: IConnection read GetItem; default;
  protected // IConnectionList
    function Add(const Value: IConnection): Integer; reintroduce;
    function CreateClient(const Address: String; Port: Word): IConnection;
    procedure Clear; override; 
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SilSiLayerProtocolBlind;

{ TConnectionList }

constructor TConnectionList.Create;
begin
  inherited Create(true);
end;

destructor TConnectionList.Destroy;
begin
  inherited;
end;

function TConnectionList.Add(const Value: IConnection): Integer;
begin
  Result := inherited Add(Value);
end;

function TConnectionList.Enumerate(var Enum: IEnumerator; out Item: IConnection): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TConnectionList.GetItem(Index: Integer): IConnection;
begin
  Result := IConnection(inherited GetItem(Index));
end;

function TConnectionList.CreateClient(const Address: String; Port: Word): IConnection;
begin
  Result := TConnectionClient.Create(Self, Address, Port);
end;

procedure TConnectionList.Clear;
begin
  DoDropConnections;
  inherited;
end;

procedure TConnectionList.DoDropConnections;
var
  Enum: IEnumerator;
  Item: IConnection;
begin
  while Enumerate(Enum, Item) do
    Item.Drop;
end;

{ TConnection }

constructor TConnection.Create(const List: IConnectionList);
begin
  inherited Create;

  FList := Pointer(List);
  DoInitialize;
end;

destructor TConnection.Destroy;
begin
  DoClean;
  inherited;
end;

function TConnection.DoGetList: IConnectionList;
begin
  Result := IConnectionList(FList);
end;

procedure TConnection.DoClean;
begin
  FSlot := nil;
  FBlind := nil;
  FCipherKey := '';

  if Assigned(FChain) then
  begin
    Sil.Sink.Disconnect(FChain, Self);

    FChain.Clear(true);
    FChain := nil;
  end;
end;

procedure TConnection.Drop;
var
  List: IConnectionList;
begin
  DoClean;
  List := DoGetList;
  if Assigned(List) then List.Remove(Self);
end;

function TConnection.DoAddProt(Id: LongWord; const Protocol, Listener: IUnknown; AsyncDispatch: Boolean): ILayerChain;
var
  Params: IParameterList;
begin
  Result := SilLayer.Layer.Chain;
  FSlot.Add(Result);

  Params := Sil.List.Parameters;

  if Str.NotEmpty(FCipherKey) then
  begin
    Params['Cipher'] := SilCoder.Cipher.SimpleMix;
    Params['Key'] := FCipherKey;

    Result.Add(SilLayer.Layer.Cipher(Params));
    Params.Clear;
  end;

  Params['AsyncDispatch'] := AsyncDispatch;
  Params['Id'] := Id;

  Result.Add(SilLayer.Protocol.Imate(Params));
  Result.Add(Protocol);

  if Assigned(Listener) then 
    Result.Add(Listener);

  if FSlot.Control.IsActive then Result.Control.Activate;
end;

function TConnection.DoAddProtThruBlind(const Protocol: IInterface; const IID: TGuid; AsyncDispatch: Boolean; const Listener: IUnknown; out Chain: ILayerChain): Boolean;
var
  Id: LongWord;
  ObjListener: IUnknown;
begin
  Result := FBlind.Request(IID, Id);

  if Assigned(Listener) then
    ObjListener := Listener else
    ObjListener := Self;

  if Result then Chain := DoAddProt(Id, Protocol, ObjListener, AsyncDispatch);
end;

function TConnection.AddProtocol(const Protocol: IInterface; const IID: TGuid; AsyncDispatch: Boolean; const Listener: IUnknown): Boolean;
var
  Chain: ILayerChain;
begin
  Result := DoAddProtThruBlind(Protocol, IID, AsyncDispatch, Listener, Chain);
end;
                                                            
procedure TConnection.DoInitialize;
begin
end;

function TConnection.GetChain: ILayerChain;
begin
  Result := FChain;
end;

procedure TConnection.OnLayerActivated(const Event: RLayerActivated);
begin
end;

procedure TConnection.OnLayerDeactivated(const Event: RLayerDeactivated);
begin
  FileConnectionLost;
  DoClean;
end;

procedure TConnection.OnKeepAlive(const Sender: IKeepAlive; out Success: Boolean);
begin
  try
    DoInitLayers;
    FileConnectionSuccess;
    DoReleaseKeepAlive;
  except
    DoClean;
    FireConnectionFailed;
    Success := false;
  end;
end;

procedure TConnection.Start;
begin
  FKeepAlive := TKeepAlive.Create;
  Sil.Sink.Connect(FKeepAlive, Self);
  FKeepAlive.Start;
end;

procedure TConnection.Stop;
begin
  if Assigned(FKeepAlive) then
  begin
    FKeepAlive.Stop;
    DoReleaseKeepAlive;
  end;

  DoClean;
end;

procedure TConnection.DoReleaseKeepAlive;
begin
  Sil.Sink.Disconnect(FKeepAlive, Self);
end;

procedure TConnection.FileConnectionSuccess;
var
  Enum: IEnumerator;
  Sink: IConnectionEvents;
  Event: RConnectionEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    while Events.Enumerate(Enum, Sink, IConnectionEvents) do
      Sink.OnConnected(Event);
  end;
end;

procedure TConnection.FileConnectionLost;
var
  Enum: IEnumerator;
  Sink: IConnectionEvents;
  Event: RConnectionEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    while Events.Enumerate(Enum, Sink, IConnectionEvents) do
      Sink.OnDisconnected(Event);
  end;

  Start;
end;

procedure TConnection.FireConnectionFailed;
var
  Enum: IEnumerator;
  Sink: IConnectionClientEvents;
  Event: RConnectionEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    while Events.Enumerate(Enum, Sink, IConnectionClientEvents) do
      Sink.OnConnectionFailed(Event);
  end;
end;

{ TConnectionSocket }

constructor TConnectionSocket.Create(const List: IConnectionList; const Address: String; Port: Word);
begin
  FAddress := Address;
  FPort := Port;

  inherited Create(List);
end;

{ TConnectionClient }

constructor TConnectionClient.CreateConnected(const List: IConnectionList; const Chain: ILayerChain);
begin
  FChain := Chain;
  inherited Create(List, '', 0);
end;

procedure TConnectionClient.DoInitLayers;
var
  Eblis: IEblisProtocol;
  Chain: ILayerChain;
  Params: IParameterList;
  MustInit: Boolean;
begin
  try
    MustInit := not Assigned(FChain);

    if MustInit then
    begin
      FChain := SilLayer.Layer.Chain;
      Sil.Sink.Connect(FChain, Self);

      Params := Sil.List.Parameters;
      Params['Host'] := FAddress;
      Params['Port'] := FPort;

      FChain.Add(SilLayer.Device.SocketClient(Params));
    end;

    FSlot := SilLayer.Layer.Slot;
    FChain.Add(SilLayer.Packer.Imate);
    FChain.Add(FSlot);

    if MustInit then
    begin
      FChain.Control.Activate;

      FBlind := SilLayer.Protocol.Blind;
      DoAddProt(0, FBlind, Self, false);

      Eblis := SilLayer.Protocol.Eblis;

      if DoAddProtThruBlind(Eblis, IEblisProtocol, true, nil, Chain) then
      begin
        Eblis.Negotiate(FCipherKey, 10);
        Eblis := nil;

        FSlot.Remove(Chain);
        SilLayer.Tk.DropChain(Chain);
      end;
    end;
  except
    DoClean;
    raise;
  end;
end;

{ TConnectionServer }

procedure TConnectionServer.DoInitialize;
begin
  FConnections := TConnectionList.Create;
end;

procedure TConnectionServer.DoInitLayers;
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['Address'] := FAddress;
  Params['Port'] := FPort;

  FChain := SilLayer.Layer.Chain;
  FChain.Add(SilLayer.Device.SocketServer(Params));
  FChain.Add(Self);
end;

procedure TConnectionServer.CreateChain(const Chain: ILayerChain; const Context: IInterface);
var
  Connection: IConnection;
begin
  Connection := TConnectionClient.CreateConnected(FConnections, Chain);
  FConnections.Add(Connection);
end;

procedure TConnectionServer.DestroyChain(const Chain: ILayerChain; const Context: IInterface);
var
  Enum: IEnumerator;
  Item: IConnection;
begin
  while FConnections.Enumerate(Enum, Item) do
    if Item.Chain.Id = Chain.Id then
    begin
      Item.Drop;
      Break;
    end;
end;

procedure TConnectionServer.DoClean;
begin
  inherited;
  
  FFileServer := nil;
  FEblis := nil;

  if Assigned(FConnections) then
  begin
    FConnections.Clear;
    FConnections := nil;
  end;
end;

procedure TConnectionServer.OnNegotiate(var Event: REblisNegotiateEvent);
begin
  FCipherKey := Event.Key;
end;

procedure TConnectionServer.OnRequest(var Event: RBlindRequestEvent);
begin
  Event.Result := false;

  if Guid.Compare(Event.Protocol, IEblisProtocol) = 0 then
  begin
    if not Assigned(FEblis) then
    begin
      FEblis := SilLayer.Protocol.Eblis;
      DoAddProt(Event.Id, FEblis, Self, false);
    end;

    Event.Result := true;
  end else
    DoFileProtocolRequest(Event);
end;

procedure TConnectionServer.OnText(var Event: RBlindTextEvent);
begin
end;

procedure TConnectionServer.DoFileProtocolRequest(var Request: RBlindRequestEvent);
var
  Enum: IEnumerator;
  Sink: IConnectionServerEvents;
  Event: RConnectionProtocolRequestEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Protocol := Request.Protocol;
    Event.Id := Request.Id;
    Event.Result := Request.Result;

    while Events.Enumerate(Enum, Sink, IConnectionServerEvents) do
      Sink.OnProtocolRequest(Event);

    Request.Result := Event.Result;
  end;
end;

end.
