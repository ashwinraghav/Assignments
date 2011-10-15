unit UiClient;

interface

uses
  Sil,
  SilLayer,
  UiProtocol;

type
  IClient = interface;
  IClients = interface;
  IClientList = interface;

  IClient = interface
    ['{A1166E90-8B37-4DB1-B990-065F9A89A322}']
    function GetId: TGuid;
    function GetIsLogged: Boolean;
    function GetServerSide: IServerSide;
    function Chain: ILayerChain;
    procedure Logoff;
    property Id: TGuid read GetId;
    property ServerSide: IServerSide read GetServerSide;
    property IsLogged: Boolean read GetIsLogged;
  end;

  IClients = interface
    ['{2AED74DC-4EB9-4E5A-BEE7-311C1BC74818}']
    function GetItem(Index: Integer): IClient;
    function IndexOf(const Value: IUnknown): Integer;
    function GetCount: Integer;
    function First: IClient;
    function Last: IClient;
    function Enumerate(var Enum: IEnumerator; out Item: IClient): Boolean;
    function Locked: ILock;
    function Find(const Id: TGuid; out Value: IClient): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IClient read GetItem; default;
  end;

  IClientList = interface (IClients)
    ['{6A83CA23-44E3-493B-8008-E0F815DC4565}']
    procedure SetItem(Index: Integer; const Value: IClient);
    procedure SetCount(Value: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;
    function Add(const Value: IClient): Integer;
    function Remove(const Value: IClient): Integer;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IClient read GetItem write SetItem; default;
  end;

implementation

end.
