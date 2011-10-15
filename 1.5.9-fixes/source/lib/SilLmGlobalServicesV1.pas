{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilLmGlobalServicesV1;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDelphiObject,
  SilLkAggregated,

  SilLiLock,
  SilLiEnumerator,
  SilLiStringList,
  SilLiGlobalServices;

type
  PGlobalServiceClass = ^RGlobalServiceClass;
  PGlobalServiceItem = ^RGlobalServiceItem;

  TGlobalServiceListV1 = class (
    TSilAggregatedObject,
    IGlobalServicesV1 )
  private
    FServices: Pointer;
    FItem: IStringList;
    FClass: IStringList;
    function DoGetServices: IGlobalServiceListV2;
  private
    function DoNewItem(Service: GlobalServiceType): PGlobalService;
    function DoFindItem(const ID: TGUID; out Item: PGlobalService): Boolean;
    function DoGetItem(Service: GlobalServiceType): PGlobalService; overload;
    procedure DoAddItem(Item: PGlobalService);
    procedure DoDestroyItem(Item: PGlobalService);
    function DoNewClass(Service: PGlobalService): GlobalServiceType;
    function DoFindClass(const ID: TGUID; out Item: GlobalServiceType): Boolean;
    function DoGetClass(Service: PGlobalService): GlobalServiceType;
    procedure DoDestroyClass(Service: GlobalServiceType);
    function DoFindPtr(const List: IStringList; const ID: TGUID; out Ptr): Boolean;
  protected // IGlobalServicesV1
    procedure Register(Service: GlobalServiceType);
    procedure Unregister(Service: GlobalServiceType);
    function Find(Service: GlobalServiceType): Boolean; overload;
    function Find(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil): Boolean; overload;
    procedure Add(Service: GlobalServiceType; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil); 
    procedure Remove(Service: GlobalServiceType);
    procedure Get(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer = nil);
    procedure Release(Service: GlobalServiceType; Obj: PUnknown); reintroduce; 
    function Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Obj: IUnknown): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Obj: GlobalServiceType): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Ref: RServiceRefV1): Boolean; overload;
  public
    constructor Create(const Controller: IUnknown); reintroduce;  
    destructor Destroy; override;
  public
    property Services: IGlobalServiceListV2 read DoGetServices; 
  end;

  RGlobalServiceItem = record
    Base: RGlobalService;
    Item: GlobalServiceType;
    GUID: TGUID;
    Owner: TGlobalServiceListV1; 
  end;

  RGlobalServiceClass = record
    Base: RObjectClass;
    Self: record end;
    Item: PGlobalService; // ocupa el lugar del Create que en este caso no se va a usar
    ID: function(Self: GlobalServiceType): TGUID;
  end;

implementation

uses
  SilBkTool,
  SilBtError,
  SilBtStr,
  SilLtLock,
  SilLtList,
  SilOtTool;

type
  GlobalServiceFriendClass = class of GlobalServiceFriend;
  GlobalServiceFriend = class(GlobalService);

function GetClass(Service: GlobalServiceType): PGlobalServiceClass;
begin
  Result := PGlobalServiceClass(PChar(Service) - SizeOf(Result.Base));
end;

function GetItem(Service: PGlobalService): PGlobalServiceItem;
begin
  Result := PGlobalServiceItem(Service);
end;

function ServiceID(Self: GlobalServiceType): TGUID;
begin
  Result := GetClass(Self).Item.ID^;
end;

procedure ServiceInitialize(Self: PGlobalServiceItem; const Services: IGlobalServicesV2);
begin
  Self.Owner.DoAddItem(@Self.Base)
end;

procedure ServiceFinalize(Self: PGlobalServiceItem; const Services: IGlobalServicesV2);
begin
  Self.Owner.DoDestroyItem(@Self.Base)
end;

function ServiceCreate(Self: PGlobalServiceItem; const Services: IGlobalServicesV2): IUnknown;
begin
  Result := GlobalServiceFriendClass(Self.Item).Create();
end;                                           

{ TGlobalServiceListV1 }

constructor TGlobalServiceListV1.Create(const Controller: IUnknown);
begin
  inherited Create(Controller, True);
  FServices := Pointer(Controller as IGlobalServiceListV2);
  FItem := ListTool.StringList();
  FClass := ListTool.StringList();
end;

destructor TGlobalServiceListV1.Destroy;
var
  Enum: IEnumerator;
  Item: string;
  Service: PGlobalService;
begin
  Locked;

  with FClass do
    while Enumerate(Enum, Item) do
      DoDestroyClass(Ptrs[Enum.Iteration]);

  with FItem do
    while Enumerate(Enum, Item) do
    begin
      Service := Ptrs[Enum.Iteration];
      Services.Unregister(Service, Service.Module);
    end;

  FClass := nil;
  FItem := nil;
  FServices := nil;
  inherited;
end;

procedure TGlobalServiceListV1.Register(Service: GlobalServiceType);
begin
  if not Self.Services.Find(Service.ID) then
    Self.Services.Register(DoGetItem(Service), FindHInstance(Service));
end;

procedure TGlobalServiceListV1.Unregister(Service: GlobalServiceType);
var
  Item: IGlobalServiceV2;
begin
  if Self.Services.Find(Service.ID, Item) then
    Self.Services.Unregister(Item.Service, FindHInstance(Service));
end;

function TGlobalServiceListV1.Enumerate(var Enum: IEnumerator; out Obj: IInterface): Boolean;
begin
  Result := Self.Services.Enumerate(Enum, IUnknown, Obj);
end;

function TGlobalServiceListV1.Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean;
begin
  Result := Self.Services.Enumerate(Enum, IID, Obj);
end;

function TGlobalServiceListV1.Enumerate(var Enum: IEnumerator; out Obj: GlobalServiceType): Boolean;
var
  Item: IGlobalServiceV2;
begin
  Result := Self.Services.Enumerate(Enum, Item);
  if Result then Obj := DoGetClass(Item.Service);
end;

function TGlobalServiceListV1.Enumerate(var Enum: IEnumerator; out Ref: RServiceRefV1): Boolean;
begin
  Result := False;
end;

function TGlobalServiceListV1.Find(Service: GlobalServiceType): Boolean;
begin
  Result := Self.Services.Find(Service.ID);
end;

function TGlobalServiceListV1.Find(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer): Boolean;
begin
  if ID = nil then ID := Obj;
  Result := Self.Services.Find(Service.ID, IID, Obj, FindHInstance(ID));
end;

procedure TGlobalServiceListV1.Add(Service: GlobalServiceType; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; ID: Pointer);
begin
  if ID = nil then ID := Obj;
  Self.Services.Register(DoGetItem(Service), Instance, IID, Obj, FindHInstance(ID));
end;

procedure TGlobalServiceListV1.Remove(Service: GlobalServiceType);
var
  Item: PGlobalService;
begin
  if DoFindItem(Service.ID, Item) then
    Self.Services.Remove(Item, FindHInstance(Service));
end;

procedure TGlobalServiceListV1.Get(Service: GlobalServiceType; const IID: TGuid; Obj: PUnknown; ID: Pointer);
var
  Module: LongWord; 
begin
  Module := FindHInstance(Obj);
  if Module = 0 then Module := FindHInstance(Service);
  if Module = 0 then Module := FindHInstance(ID);
  Self.Services.Get(DoGetItem(Service), IID, Obj, Module);
end;

procedure TGlobalServiceListV1.Release(Service: GlobalServiceType; Obj: PUnknown);
var
  Module: LongWord; 
begin
  Module := FindHInstance(Obj);
  if Module = 0 then Module := FindHInstance(Service);
  Self.Services.Release(Service.ID, Obj, Module);
end;

function TGlobalServiceListV1.DoNewItem(Service: GlobalServiceType): PGlobalService;
var
  Ref: PGlobalServiceItem;
begin
  Ref := System.New(PGlobalServiceItem);

  Ref.GUID := Service.ID;
  Ref.Owner := Self;
  Ref.Item := Service;
  Ref.Base.ID := @Ref.GUID;
  Ref.Base.Name := GetClass(Service).Base.ClassName;
  Ref.Base.Module := FindHInstance(Service);

  @Ref.Base.Initialize := @ServiceInitialize;
  @Ref.Base.Finalize := @ServiceFinalize;
  @Ref.Base.Create := @ServiceCreate;
  @Ref.Base.Destroy := nil;
  @Ref.Base.RefAdded := nil;
  @Ref.Base.RefRemoved := nil;
  @Ref.Base.LinkAdded := nil;
  @Ref.Base.LinkRemoved := nil;

  Result := @Ref.Base;
end;

function TGlobalServiceListV1.DoFindItem(const ID: TGUID; out Item: PGlobalService): Boolean;
var
  Index: Integer;
begin
  Index := FItem.IndexOf(Guid.ToStr(ID));

  Result := FItem.ValidIndex(Index);
  if Result then
    Item := FItem.Ptrs[Index];
end;

function TGlobalServiceListV1.DoGetItem(Service: GlobalServiceType): PGlobalService;
begin
  if not DoFindItem(Service.ID, Result) then
    Result := DoNewItem(Service);
end;

procedure TGlobalServiceListV1.DoAddItem(Item: PGlobalService);
begin
  FItem.Add(Guid.ToStr(Item.ID^), Item);
end;

procedure TGlobalServiceListV1.DoDestroyItem(Item: PGlobalService);
var
  Ref: PGlobalServiceItem;
begin
  if FItem.Remove(Guid.ToStr(Item.ID^)) >= 0 then
  begin
    Ref := GetItem(Item);
    Dispose(Ref);
  end;
end;

function TGlobalServiceListV1.DoNewClass(Service: PGlobalService): GlobalServiceType;
var
  Ref: PGlobalServiceClass; 
begin
  Ref := New(PGlobalServiceClass);
  
  Ref.Base.SelfPtr := Ref;
  Ref.Base.IntfTable := nil;
  Ref.Base.AutoTable := nil;
  Ref.Base.InitTable := nil;
  Ref.Base.TypeInfo := nil;
  Ref.Base.FieldTable := nil;
  Ref.Base.MethodTable := nil;
  Ref.Base.DynamicTable := nil;
  Ref.Base.ClassName := Service.Name;
  Ref.Base.InstanceSize := 4;
  Ref.Base.Parent := SilBkTool.Tool;
  Ref.Base.SafeCallException := nil;
  Ref.Base.AfterConstruction := nil;
  Ref.Base.BeforeDestruction := nil;
  Ref.Base.Dispatch := nil;
  Ref.Base.DefaultHandler := nil;
  Ref.Base.NewInstance := nil;
  Ref.Base.FreeInstance := nil;
  Ref.Base.Destroy := nil;
  Ref.Item := Service;
  Ref.ID := ServiceID;

  Result := @Ref.Self;
  
  FClass.Add(Guid.ToStr(Service.ID^), Result);
end;

function TGlobalServiceListV1.DoFindClass(const ID: TGUID; out Item: GlobalServiceType): Boolean;
var
  Ref: PGlobalService;
begin
  Result := DoFindItem(ID, Ref);
  if not Result then
    Result := DoFindPtr(FClass, ID, Item) else
    Item := GetItem(Ref).Item;
end;

function TGlobalServiceListV1.DoGetClass(Service: PGlobalService): GlobalServiceType;
begin
  if not DoFindClass(Service.ID^, Result) then
    Result := DoNewClass(Service);
end;

procedure TGlobalServiceListV1.DoDestroyClass(Service: GlobalServiceType);
var
  Ref: PGlobalServiceClass;
begin
  if FClass.Remove(Guid.ToStr(Service.ID)) >= 0 then
  begin
    Ref := GetClass(Service);
    Dispose(Ref);
  end;
end;

function TGlobalServiceListV1.DoFindPtr(const List: IStringList; const ID: TGUID; out Ptr): Boolean;
var
  Index: Integer;
begin
  Index := List.IndexOf(Guid.ToStr(ID));
  Result := List.ValidIndex(Index);
  if Result then
    Pointer(Ptr) := List.Ptrs[Index];
end;

function TGlobalServiceListV1.DoGetServices: IGlobalServiceListV2;
begin
  Result := IGlobalServiceListV2(FServices);
end;

end.
