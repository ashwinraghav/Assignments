unit UmPeer;

interface

uses
  Sil,
  SilLayer,

  UiPeer;

type
  TPeer = class (TSilObject, IPeer)
  private
    FProt: IClientSideFileProtocol;
    FChain: ILayerChain;
    FHook: IPeerHook;
    FCurrentPath: String;
  private
    procedure DoGetFiles(const Path: String; out Info: IFileInfoList);
    procedure DoConnected(const Sender: IInterface; Param: Pointer);
  private
    procedure DoClean;
  protected // IPeer
    procedure ReadContent(const Path: String);
    procedure Download(const Path: String);
    procedure Cancel(const Path: String);
  protected // ILayerActivationEvents
    procedure OnLayerActivated(const Event: RLayerActivated);
    procedure OnLayerDeactivated(const Event: RLayerDeactivated);
  public
    constructor Create(const LocalId, Address: String; Port: Integer; const Hook: IPeerHook);
    destructor Destroy; override;
  end;

implementation

uses SilSiLayerProtocolFile, SilLtList, SilOiFile;

{ TPeer }

constructor TPeer.Create(const LocalId, Address: String; Port: Integer; const Hook: IPeerHook);
var
  LayerParams: IParameterList;
begin
  inherited Create;

  FHook := Hook;
  
  LayerParams := Sil.List.Parameters;
  LayerParams['Host'] := Address;
  LayerParams['Port'] := Port;

  FChain := SilLayer.Layer.Chain;
  FProt := SilLayer.Protocol.FileClient;

  FChain.Add(SilLayer.Device.SocketClient(LayerParams));
  FChain.Add(SilLayer.Packer.Imate);
  FChain.Add(SilLayer.Protocol.Imate);
  FChain.Add(FProt);

  Sil.Sink.Connect(FChain, Self);
  FChain.Control.Activate;
end;

destructor TPeer.Destroy;
begin
  if Assigned(FChain) then
  begin
    FChain.Control.Deactivate;
    FChain.Clear(true);

    DoClean;
    FChain := nil;
  end;

  inherited;
end;

procedure TPeer.DoClean;
begin
  Sil.Sink.Disconnect(FChain, Self);
  FProt := nil;
end;

procedure TPeer.OnLayerActivated(const Event: RLayerActivated);
begin
  if Assigned(FProt) then
    Sil.OS.Thread.SyncCall(DoConnected);
end;

procedure TPeer.DoConnected(const Sender: IUnknown; Param: Pointer);
begin
  if Assigned(FHook) then
    FHook.Connected(Self);
end;

procedure TPeer.OnLayerDeactivated(const Event: RLayerDeactivated);
begin

end;

procedure TPeer.DoGetFiles(const Path: String; out Info: IFileInfoList);
var
  Reader: IDirectoryReader;
begin
  Reader := FProt.ReadDirectory(Path);
  while Reader.Read do;
  Info := Reader.List;
end;

procedure TPeer.ReadContent(const Path: String);
begin
  if Str.NotEmpty(Path) then
    FCurrentPath := Path;
end;

procedure TPeer.Download(const Path: String);
begin

end;

procedure TPeer.Cancel(const Path: String);
begin

end;

end.
