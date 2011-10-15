unit UkEngineChannel;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine,
  UiEngine,
  UkOuchChannel;

type
  TOuchEngineChannel = class(
    TOuchChannel,
    IConnectable,
    IEngineChannel )
  private
    FEvents: IEventList;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown); virtual;
    procedure RemoveListener(const Listener: IUnknown); virtual;
  protected // IEngineChannel
    function GetConnection: IClientSocketConnection;
    function GetRequest: IOuchRequest;
    function GetReply: IOuchReply;
  protected
    property Events: IEventList read FEvents;
  end;

implementation

{ TOuchEngineChannel }

procedure TOuchEngineChannel.AddListener(const Listener: IUnknown);
begin
  Sil.Sv.EventCaster.Add(FEvents, Listener);
  Sil.Sink.Connect(Connection, Listener);
end;

procedure TOuchEngineChannel.RemoveListener(const Listener: IUnknown);
begin
  Sil.Sink.Disconnect(Connection, Listener);
  Sil.Sv.EventCaster.Remove(FEvents, Listener);
end;

function TOuchEngineChannel.GetConnection: IClientSocketConnection;
begin
  Result := inherited Connection;
end;

function TOuchEngineChannel.GetReply: IOuchReply;
begin
  Result := inherited Reply;
end;

function TOuchEngineChannel.GetRequest: IOuchRequest;
begin
  Result := inherited Request;
end;

end.
