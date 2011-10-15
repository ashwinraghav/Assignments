unit UfLayerBlind;

interface

uses
  SilTool, 
  SilLayer;

implementation

uses
  SilSmLayerProtocolBlind;

initialization
  SilTool.Sv.SharedObject.Register(CLayerBlind, TSilBlindProtocol, 'sil.layer.imated.blind');

end.
 