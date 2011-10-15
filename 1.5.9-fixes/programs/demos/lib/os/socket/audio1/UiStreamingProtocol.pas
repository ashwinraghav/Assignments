unit UiStreamingProtocol;

interface

{$include Defines.inc}

uses
  Sil;

type
  IStreamingProtocol = interface;
  IStreamingProtocolEvents = interface;
  ISubscriber = interface;

  IStreamingProtocol = interface
    ['{8CAE1EAC-D3C0-4D31-90AA-3AD018D13287}']
    function SendBuffer(const Buffer; Size: LongWord): LongWord;
    function SendParams(const Params: IParameters = nil): Boolean; overload;
    function SendParams(const Host: ISocketAddress; const Params: IParameters = nil): Boolean; overload;
    function Receive(const Host: ISocketAddress; const Buffer; Size: LongWord): LongWord;
    function Subscribe(const Host: ISocketAddress): Boolean;
    function UnSubscribe(const Host: ISocketAddress): Boolean;
    function RemoveSubscriber(const Host: ISocketAddress): Boolean;
    function AddSubscriber(RemoteId: Integer; const Host: ISocketAddress): Boolean;
  end;

  RStreamingSuscriberEvent = record
    Sender: IStreamingProtocol;
    Id: Integer;
    Host: ISocketAddress;
  end;

  RStreamingParamsEvent = record
    Sender: IStreamingProtocol;
    Subscription: ISubscriber;
    Params: IParameterList;
  end;

  RStreamingDataEvent = record
    Sender: IStreamingProtocol;
    Subscription: ISubscriber;
    Buffer: Pointer;
    Size: LongWord;
  end;

  IStreamingProtocolEvents = interface
    ['{F6F819A3-2A87-42C5-83F8-E4A81BF2C35A}']
    procedure OnSubscriberReceived(var Event: RStreamingSuscriberEvent);
    procedure OnSubscriptionRevoked(var Event: RStreamingSuscriberEvent);
    procedure OnDataReceived(var Event: RStreamingDataEvent);
    procedure OnParamsReceived(var Event: RStreamingParamsEvent);
  end;

  ISubscriber = interface
    ['{77F8E9EA-27B1-4069-9138-B2732EFC23E3}']
    function GetHost: ISocketAddress;
    function GetData: IUnknown;
    procedure SetData(const Value: IUnknown);
    function GetLocalId: Integer;
    procedure SetLocalId(Value: Integer);
    function GetRemoteId: Integer;
    procedure SetRemoteId(Value: Integer);
    property Host: ISocketAddress read GetHost;
    property Data: IUnknown read GetData write SetData;
    property LocalId: Integer read GetLocalId write SetLocalId;
    property RemoteId: Integer read GetRemoteId write SetRemoteId;
  end;

implementation

end.
