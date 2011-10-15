unit UkEngineObject;

interface

uses
  Sil,
  UiOuchEngine;

type
  TOuchEngineObject = class(TInterfacedObject)
  private
    FEngine: Pointer;
    function GetEngine: IOuchEngine;
  protected
    property Engine: IOuchEngine read GetEngine;
  public
    constructor Create(const Engine: IOuchEngine);
    destructor Destroy; override;
  end;

implementation

{ TOuchEngineObject }

constructor TOuchEngineObject.Create(const Engine: IOuchEngine);
begin
  inherited Create;
  FEngine := Pointer(Engine);
end;

destructor TOuchEngineObject.Destroy;
begin
  FEngine := nil;
  inherited;
end;

function TOuchEngineObject.GetEngine: IOuchEngine;
begin
  Result := IOuchEngine(FEngine);
end;

end.
