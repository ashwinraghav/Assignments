unit SilSiLayerConnectionImate;

interface

uses
  Sil,
  SilSiLayer,
  SilSiLayerConnection;

type
  ILayerImateConnectionManager = interface
    ['{518B8AC9-094F-4888-AFF1-6E4295911445}']
    function GetConnection: ILayerConnection;
    function InsertProtocol(const Protocol: IUnknown; const Listener: IUnknown = nil; const ID: Integer = 0; AsyncDispatch: Boolean = false): ILayerChain;
    function RequestProtocol(const Protocol: IUnknown; const IID: TGuid; out Chain: ILayerChain; const Listener: IUnknown = nil; AsyncDispatch: Boolean = false): Boolean;
    procedure Start(const Params: IParameters = nil);
    procedure Stop;
    property Connection: ILayerConnection read GetConnection;
  end;

implementation

end.
 