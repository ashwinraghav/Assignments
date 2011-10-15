unit UmEngineGroupData;

interface

uses
  Sil,
  UiOuchEngine;

type
  ROuchGroupData = record
    ID: TGUID;
    Parent: TGUID;
    Name: string;
    Ptr: Pointer;
  end;
  
  TOuchEngineGroupData = class(
    TInterfacedObject,
    IConnectable,
    IOuchGroupData,
    IOuchGroupDef )
  private
    FEvents: IEventList;
    FData: ROuchGroupData;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown);
    procedure RemoveListener(const Listener: IUnknown);
  protected // IOuchGroupData
    function GetID: TGUID;
    function GetParent: TGUID;
    function GetName: string;
    function GetPtr: Pointer;
  protected // IOuchGroupDef
    procedure SetID(const Value: TGUID);
    procedure SetParent(const Value: TGUID);
    procedure SetName(const Value: string);
    procedure SetPtr(const Value: Pointer);
  public
    constructor Create(const GroupID: TGUID; const Parent: TGUID; const Name: string);
    destructor Destroy; override;
  end;

implementation

{ TOuchEngineGroupData }

constructor TOuchEngineGroupData.Create(const GroupID, Parent: TGUID; const Name: string);
begin
  inherited Create;
  FData.ID := GroupID;
  FData.Parent := Parent;
  FData.Name := Name;
end;

destructor TOuchEngineGroupData.Destroy;
begin
  inherited;
end;

procedure TOuchEngineGroupData.AddListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Add(FEvents, Listener);
end;

procedure TOuchEngineGroupData.RemoveListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Remove(FEvents, Listener);
end;

function TOuchEngineGroupData.GetID: TGUID;
begin
  Result := FData.ID;
end;

function TOuchEngineGroupData.GetName: string;
begin
  Result := FData.Name;
end;

function TOuchEngineGroupData.GetParent: TGUID;
begin
  Result := FData.Parent;
end;

function TOuchEngineGroupData.GetPtr: Pointer;
begin
  Result := FData.Ptr;
end;

procedure TOuchEngineGroupData.SetID(const Value: TGUID);
begin
  FData.ID := Value;
end;

procedure TOuchEngineGroupData.SetName(const Value: string);
begin
  FData.Name := Value;
end;

procedure TOuchEngineGroupData.SetParent(const Value: TGUID);
begin
  FData.Parent := Value;
end;

procedure TOuchEngineGroupData.SetPtr(const Value: Pointer);
begin
  FData.Ptr := Value;
end;

end.
