unit UfLayerSockServer;

interface

uses
  Sil,
  SilTool, 
  SilLayer,
  SilSmLayerDeviceSocketServer;

implementation

{ TSilSharedSocketClientLayer }

initialization
  SilTool.Sv.SharedObject.Register(CLayerSocketServer, TSilSocketServerLayer, SLayerSocketServer);

end.
