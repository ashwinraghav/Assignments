unit UiConnection;

interface

uses
  Sil,
  SilLayer;

type
  IConnection = interface;
  IConnections = interface;
  IConnectionList = interface;
  IConnectionEvents = interface;

  IConnection = interface
    ['{E2A0C1A8-AE92-4B83-9B00-91FD5B2A51EE}']
    function GetChain: ILayerChain;
    function AddProtocol(const Protocol: IUnknown; const IID: TGuid; AsyncDispatch: Boolean = false; const Listener: IUnknown = nil): Boolean;
    procedure Start;
    procedure Stop;
    procedure Drop;
    property Chain: ILayerChain read GetChain;
  end;

  IConnections = interface
    ['{DFBEE1B3-A453-42FD-B980-60847BD53EEC}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IConnection;
    function Locked: ILock;
    function IndexOf(const Value: IUnknown): Integer;
    function Enumerate(var Enum: IEnumerator; out Item: IConnection): Boolean;
    property Items[Index: Integer]: IConnection read GetItem; default;
    property Count: Integer read GetCount;
  end;

  IConnectionList = interface (IConnections)
    ['{87F75335-121A-44CE-BD07-A69B2EEA6662}']
    procedure SetCount(Value: Integer);
    procedure Delete(Index: Integer);
    function Remove(const Value: IUnknown): Integer;
    procedure Clear;
    function Add(const Value: IConnection): Integer;
    function CreateClient(const Address: String; Port: Word): IConnection;
    property Count: Integer read GetCount write SetCount;
  end;

  RConnectionEvent = record
    Sender: IConnection;
  end;

  RConnectionProtocolRequestEvent = record
    Sender: IConnection;
    Protocol: TGuid;
    Id: LongWord;
    Result: Boolean;
  end;

  IConnectionEvents = interface
    ['{30E6698B-5ECA-43B8-9302-0E8AA951A350}']
    procedure OnConnected(const Event: RConnectionEvent);
    procedure OnDisconnected(const Event: RConnectionEvent);
  end;

  IConnectionClientEvents = interface (IConnectionEvents)
    ['{9151E0EB-A394-45FE-8452-50CD73B5ABC0}']
    procedure OnConnectionFailed(const Event: RConnectionEvent);
  end;

  IConnectionServerEvents = interface (IConnectionEvents)
    ['{30E6698B-5ECA-43B8-9302-0E8AA951A350}']
    procedure OnProtocolRequest(var Event: RConnectionProtocolRequestEvent);
  end;

implementation

end.
