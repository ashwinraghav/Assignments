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

unit SilLmGlobalServicesV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeError,
  SilLkAggregated,

  SilLiLock,
  SilLiEnumerator,
  SilLiStringList,
  SilLiPointerList,

  SilLeGlobalServicesV2,
  SilLiGlobalServicesV2;

type
  TGlobalServiceListV2 = class (
    TSilAggregatedObject,
    IGlobalServicesV2,
    IGlobalServiceListV2 )
  private
    FList: IGlobalListV2;
  private
    function DoGet(Service: PGlobalService; Module: LongWord): IGlobalServiceV2;
    function DoAdd(Service: PGlobalService; const Instance: IUnknown; Module: LongWord): IGlobalServiceV2;
    function DoRemove(const Item: IGlobalServiceV2; Module: LongWord): Boolean;
    function DoFind(const ID: TGUID; out Service: IGlobalServiceV2): Boolean;
    procedure DoRelease(const Item: IGlobalServiceV2; Obj: PUnknown; Module: LongWord);
    procedure DoDelete(const Item: IGlobalServiceV2; Module: LongWord);
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IGlobalServicesV2
    function Find(const ID: TGUID): Boolean; overload;
    function Find(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean; overload;
    procedure Link(const ID: TGUID; const ParentID: TGUID);
    procedure Unlink(const ID: TGUID; const ParentID: TGUID);
    procedure Get(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Release(const ID: TGUID; Obj: PUnknown; Module: LongWord); reintroduce; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean; overload;
  protected // IGlobalServiceListV2
    procedure Register(Service: PGlobalService; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Register(Service: PGlobalService; const Instance: IUnknown; Module: LongWord); overload;
    procedure Register(Service: PGlobalService; Module: LongWord); overload;
    procedure Unregister(const ID: TGUID; Module: LongWord); overload;
    procedure Unregister(Service: PGlobalService; Module: LongWord); overload;
    procedure Unregister(Module: LongWord); overload;
    procedure Remove(const ID: TGUID; Module: LongWord); overload;
    procedure Remove(Service: PGlobalService; Module: LongWord); overload;
    procedure Get(Service: PGlobalService; const IID: TGuid; Obj: PUnknown; Module: LongWord); overload;
    procedure Release(Service: PGlobalService; Obj: PUnknown; Module: LongWord); reintroduce; overload;
    function Enumerate(var Enum: IEnumerator; out Service: IGlobalServiceV2): Boolean; overload;
    function Find(const ID: TGUID; out Service: IGlobalServiceV2): Boolean; overload;
    function Get(const ID: TGUID): IGlobalServiceV2; overload; 
  public
    constructor Create(const Controller: IUnknown; Kind: TGlobalServiceKind); reintroduce; 
    destructor Destroy; override;
  end;

implementation

uses
  SilBtError,
  SilBtStr,
  SilLtConnection,
  SilOtTool,
  SilLfGlobalServiceV2,
  SilLmGlobalListV2;

{ TGlobalServiceListV2 }
                                                                      
constructor TGlobalServiceListV2.Create(const Controller: IUnknown; Kind: TGlobalServiceKind); 
begin
  inherited Create(Controller, True);
  FList := TSilGlobalListV2.Create(Self, Kind);
end;

destructor TGlobalServiceListV2.Destroy;
begin
  Locked();

  (*)
  while Enumerate(Enum, Item) do
    DoRemoveClass(Item);
  (*)
  
  FList := nil;
  inherited;
end;

function TGlobalServiceListV2.Enumerate(var Enum: IEnumerator; const IID: TGuid; out Obj): Boolean;
var
  Item: IGlobalServiceV2;
begin
  repeat
    Result := Assigned(FList) and FList.Enumerate(Enum, Item);
  until not Result or (Item.IsCreated and (Item.Instance.QueryInterface(IID, Obj) = 0));
end;

function TGlobalServiceListV2.Find(const ID: TGUID): Boolean;
var
  Item: IGlobalServiceV2;
begin
  Locked();
  Result := DoFind(ID, Item);
end;

function TGlobalServiceListV2.Find(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord): Boolean;
var
  Item: IGlobalServiceV2;
begin
  Locked();
  Result := Assigned(Obj^)
        or (DoFind(ID, Item) and Item.AddRef(IID, Obj, Module));
end;

procedure TGlobalServiceListV2.Link(const ID, ParentID: TGUID);
(*)
var
  Links: IPointerList;
  Service, Parent: PGlobalService;
(*)
begin
(*)
  if DoFindClass(ParentID, Parent) and DoFindClass(ID, Service) then
  begin
    Links := DoGetLinks(ParentID);
    Links.Add(Service);
  end;
(*)  
end;

procedure TGlobalServiceListV2.Unlink(const ID, ParentID: TGUID);
(*)
var
  Links: IPointerList;
  Service, Parent: PGlobalService;
(*)  
begin
(*)
  if DoFindClass(ParentID, Parent) and DoFindClass(ID, Service) then
  begin
    Links := DoGetLinks(ParentID);
    if (Links.Remove(Service) <> -1) and (Links.Count = 0) then
      DoRemoveLinks(ParentID);
  end;
(*)  
end;

procedure TGlobalServiceListV2.Get(const ID: TGUID; const IID: TGuid; Obj: PUnknown; Module: LongWord);
begin
  Locked();
  if not Assigned(Obj^) then
    Get(Get(ID).Service, IID, Obj, Module);
end;

procedure TGlobalServiceListV2.Release(const ID: TGUID; Obj: PUnknown; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(ID, Item) then
    DoRelease(Item, Obj, Module);
end;

procedure TGlobalServiceListV2.Unregister(const ID: TGUID; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(ID, Item) then
    DoDelete(Item, Module);
end;

procedure TGlobalServiceListV2.Register(Service: PGlobalService; Module: LongWord);
begin
  Locked();
  DoGet(Service, Module);
end;

procedure TGlobalServiceListV2.Unregister(Service: PGlobalService; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(Service.ID^, Item) then
    DoDelete(Item, Module);
end;

procedure TGlobalServiceListV2.Unregister(Module: LongWord);
begin
  Locked();
  if Assigned(FList) then
    FList.Remove(Module);
end;

procedure TGlobalServiceListV2.Register(Service: PGlobalService; const Instance: IUnknown; const IID: TGuid; Obj: PUnknown; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  Item := DoAdd(Service, Instance, Module);
  if not Item.AddRef(IID, Obj, Module) then
    raise Error.Create('LA INTERFASE SOLICITADA NO ES SOPORTADA POR EL SERVICIO INDICADO'); { TODO : !!!! }
end;

procedure TGlobalServiceListV2.Register(Service: PGlobalService; const Instance: IInterface; Module: LongWord);
begin
  Locked();  
  DoAdd(Service, Instance, Module);
end;

procedure TGlobalServiceListV2.Remove(const ID: TGUID; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(ID, Item) then
    DoRemove(Item, Module);
end;

procedure TGlobalServiceListV2.Remove(Service: PGlobalService; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(Service.ID^, Item) then
    DoRemove(Item, Module);
end;

procedure TGlobalServiceListV2.Get(Service: PGlobalService; const IID: TGuid; Obj: PUnknown; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if not Assigned(Obj^) then
  begin
    Item := DoGet(Service, Module);
    if not Item.AddRef(IID, Obj, Module) then
      raise Error.Create('LA INTERFASE SOLICITADA NO ES SOPORTADA POR EL SERVICIO INDICADO'); { TODO : !!!! }
  end;
end;

procedure TGlobalServiceListV2.Release(Service: PGlobalService; Obj: PUnknown; Module: LongWord);
var
  Item: IGlobalServiceV2;
begin
  Locked();
  if DoFind(Service.ID^, Item) then
    DoRelease(Item, Obj, Module);
end;

function TGlobalServiceListV2.Enumerate(var Enum: IEnumerator; out Service: IGlobalServiceV2): Boolean;
begin
  Result := Assigned(FList) and FList.Enumerate(Enum, Service);
end;

function TGlobalServiceListV2.Find(const ID: TGUID; out Service: IGlobalServiceV2): Boolean;
begin
  Result := DoFind(ID, Service);
end;

function TGlobalServiceListV2.Get(const ID: TGUID): IGlobalServiceV2;
begin
  if not Find(ID, Result) then
    raise Error.Create('NO HAY REGISTRADO NINGUN SERVICIO CON ID = %s', [Guid.ToStr(ID)]);  { TODO : !!!! }
end;

procedure TGlobalServiceListV2.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  Sink.Connect(FList, Listener, KeepRef);
end;

procedure TGlobalServiceListV2.RemoveListener(const Listener: IInterface);
begin
  Sink.Disconnect(FList, Listener);
end;

function TGlobalServiceListV2.DoGet(Service: PGlobalService; Module: LongWord): IGlobalServiceV2;
begin
  if Assigned(FList) then
    Result := FList.Get(Service, Module) else
    raise Error.Create('NO HA SIDO INICIALIZADA LA LISTA');  { TODO : !!!! }
end;

function TGlobalServiceListV2.DoAdd(Service: PGlobalService; const Instance: IInterface; Module: LongWord): IGlobalServiceV2;
begin
  Result := DoGet(Service, Module);
  Result.Instance := Instance;
end;

function TGlobalServiceListV2.DoRemove(const Item: IGlobalServiceV2; Module: LongWord): Boolean;
begin
  Result := Item.IsCreated and Item.References.Remove(Module);
  if Result then
    Item.Instance := nil;
end;

function TGlobalServiceListV2.DoFind(const ID: TGUID; out Service: IGlobalServiceV2): Boolean;
begin
  Result := Assigned(FList) and FList.Find(ID, Service);
end;

procedure TGlobalServiceListV2.DoRelease(const Item: IGlobalServiceV2; Obj: PUnknown; Module: LongWord);
begin
  if Item.DropRef(Obj, Module) then
    DoRemove(Item, Module);
end;

procedure TGlobalServiceListV2.DoDelete(const Item: IGlobalServiceV2; Module: LongWord);
begin
  if Assigned(FList) then
    FList.Remove(Item, Module);
end;

end.
