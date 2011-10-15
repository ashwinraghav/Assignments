unit UfLayerImate;

interface

uses
  SilTool, 
  SilLayer;

implementation

uses
  SilSmLayerPackerImate,
  SilSmLayerProtocolImate;
                                                
initialization
  SilTool.Sv.SharedObject.Register(CLayerImatePacker, TSilLayerPackerImate, SLayerImatePacker);
  SilTool.Sv.SharedObject.Register(CLayerImateProtocol, TSilImateProtocolLayer, SLayerImateProtocol);

end.
