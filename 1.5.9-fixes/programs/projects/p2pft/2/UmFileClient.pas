unit UmFileClient;

interface

uses
  Sil,
  SilLayer,
  SilUrl,

  UiFileServer;

type
  TFileClient = class (
    TSilLayerImateConnectionPeer,
    IFileClient)
  private
    FUrl: IUrl;
    FProtocol: IClientSideFileProtocol;
    FConnected: IEvent;
  protected // IFileClient
    function GetProtocol: IClientSideFileProtocol;
    procedure Start(const Params: IParameters);
    procedure Stop;
    function WaitConnect(Timeout: LongWord): Boolean;
  protected // ILayerConnectionManager
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain); override;
    procedure Connected(const Connection: ILayerConnection); override;
    procedure Disconnected(const Connection: ILayerConnection); override;
  public
    constructor Create(const Manager: ILayerConnectionManager);
    destructor Destroy; override;
  end;

implementation

{ TFileClient }

constructor TFileClient.Create(const Manager: ILayerConnectionManager);
begin
  inherited Create(Manager);
end;

destructor TFileClient.Destroy;
begin
  inherited;
end;

function TFileClient.GetProtocol: IClientSideFileProtocol;
begin
  Result := FProtocol;
end;

procedure TFileClient.Start(const Params: IParameters);
begin
  FUrl := SilUrl.Url.Create(Params['url']);
  FConnected := Sil.OS.Ipc.Event;

  inherited;
end;

procedure TFileClient.Stop;
begin
  inherited;
end;

procedure TFileClient.Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['host'] := FUrl.Address.Host.Server;
  Params['port'] := FUrl.Address.Host.Port;

  Chain.Add(SilLayer.Device.SocketClient(Params));
end;

procedure TFileClient.Connected(const Connection: ILayerConnection);
var
  Chain: ILayerChain;
begin
  FProtocol := SilLayer.Protocol.FileClient;
  RequestProtocol(FProtocol, IServerSideFileProtocol, Chain, nil, true);

  FConnected.Signal;
end;

procedure TFileClient.Disconnected(const Connection: ILayerConnection);
begin
  FConnected.Reset;
end;

function TFileClient.WaitConnect(Timeout: LongWord): Boolean;
begin
  Result := FConnected.WaitFor(Timeout) = wrSignaled;
end;

end.
