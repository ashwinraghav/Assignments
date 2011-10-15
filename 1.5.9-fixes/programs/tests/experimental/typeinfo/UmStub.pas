unit UmStub;

interface

uses
  Sil,
  SilBeTypeInfo,
  SilLiTypeInfo,
  UiStub;

type
  TSilInterfaceStub = class(TSilObject)
  private
    FInterface: Pointer;      
  protected // 

  public
    constructor Create(const TypeInfo: ITypeInterface);
    destructor Destroy; override;
  end;

implementation

uses
  UfStub;

{ TSilInterfaceStub }

constructor TSilInterfaceStub.Create(const TypeInfo: ITypeInterface);
begin
  inherited Create;
  
end;

destructor TSilInterfaceStub.Destroy;
begin

  inherited;
end;

end.
