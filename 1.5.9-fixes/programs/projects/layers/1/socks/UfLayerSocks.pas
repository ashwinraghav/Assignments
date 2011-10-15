unit UfLayerSocks;

interface

uses
  Sil,
  SilTool, 
  SilLayer,
  SilSmLayerSocketClient;

type
  TSilSharedSocketClientLayer = class(TSilSocketClientLayer)
  private
    function DoCreateDevice(const Parameters: IParameters): ISocketClient;
  public
    constructor Create(const Factory: ISharedFactory; const Owner: IUnknown = nil; const Controller: IUnknown = nil; const Param: IUnknown = nil); override;
  end;

implementation

uses
  SilSmLayerPackerImate;

{ TSilSharedSocketClientLayer }

constructor TSilSharedSocketClientLayer.Create(const Factory: ISharedFactory; const Owner, Controller, Param: IInterface);
var
  Parameters: IParameters;
begin
  inherited Create(Factory, Owner, Controller, Param);
  Sil.Ref.GetInterface(Param, IParameters, Parameters);
  Create(DoCreateDevice(Parameters), Parameters);
end;

function TSilSharedSocketClientLayer.DoCreateDevice(const Parameters: IParameters): ISocketClient;
var
  Host: String;
  Port: Word;
  TypeSpec: TSocketType;
  Protocol: TSocketProtocol;
begin
  Host := Parameters['Host'];
  Port := Parameters['Port'];

  TypeSpec := TSocketType(Sil.Enum.Value(TypeInfo(TSocketType), Parameters['Type'], 'st'));
  Protocol := TSocketProtocol(Sil.Enum.Value(TypeInfo(TSocketProtocol), Parameters['Protocol'], 'sp'));

  if TypeSpec = stUnknown then TypeSpec := stStream;
  if Protocol = spUnknown then Protocol := spTCP;

  Result := Sil.OS.Socket.CreateClient(TypeSpec, Protocol, Host, Port);
end;

initialization
  SilTool.Sv.SharedObject.Register(CLayerSocketClient, TSilSharedSocketClientLayer, SLayerSocketClient);

end.
