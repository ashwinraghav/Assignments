unit UmEngineClient;

interface

uses
  Sil,
  UiOuchEngine,
  UiEngine,
  UkEngineChannel;

type
  TOuchEngineClient = class(
    TOuchEngineChannel,
    IEngineClient )
  private
    FListener: Pointer;
  protected // IEngineClient
    function GetListener: IEngineListener;
    function GetChannel: IEngineChannel;
  protected
    property Listener: IEngineListener read GetListener;
  public
    constructor Create(const Listener: IEngineListener; const Connection: IClientSocketConnection);
    destructor Destroy; override;
  end;

implementation

{ TOuchEngineClient }

constructor TOuchEngineClient.Create(const Listener: IEngineListener; const Connection: IClientSocketConnection);
begin
  inherited Create(Connection);
  FListener := Pointer(Listener);
end;

destructor TOuchEngineClient.Destroy;
begin
  FListener := nil;
  inherited;
end;

function TOuchEngineClient.GetChannel: IEngineChannel;
begin
  Result := Self;
end;

function TOuchEngineClient.GetListener: IEngineListener;
begin
  Result := IEngineListener(FListener);
end;

end.
