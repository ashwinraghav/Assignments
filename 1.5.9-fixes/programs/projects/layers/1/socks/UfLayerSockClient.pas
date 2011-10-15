unit UfLayerSockClient;

interface

uses
  Sil,
  SilTool,
  SilLayer,
  SilSmLayerDeviceSocketClient;

implementation

{ TSilSharedSocketClientLayer }

initialization
  SilTool.Sv.SharedObject.Register(CLayerSocketClient, TSilSocketClientLayer, SLayerSocketClient);

end.
