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

unit SilLmGlobalListV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  
  SilLiEnumerator,
  SilLeGlobalServicesV2,
  SilLiGlobalServicesV2,
  SilLmStringList;

type
  TSilGlobalListV2 = class(
    TSilStringList,
    IGlobalListV2 )
  private
    FOwner: Pointer;
    FHook: Pointer;
    FKind: TGlobalServiceKind;
  private
    procedure DoCheck(Service: PGlobalService; Module: LongWord);
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IGlobalListV2
    function GetHook: Pointer;
    function GetOwner: IGlobalServiceListV2;
    function Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean; reintroduce; overload; 
    function Get(const ID: TGUID): IGlobalServiceV2; overload;
    function Get(Index: Integer): IGlobalServiceV2; overload;
    function Get(Service: PGlobalService; Module: LongWord): IGlobalServiceV2; overload;
    function Find(const ID: TGUID; out Item: IGlobalServiceV2): Boolean; overload;
    function Find(Service: PGlobalService; out Item: IGlobalServiceV2): Boolean; overload;
    function Add(Service: PGlobalService; Module: LongWord): IGlobalServiceV2; reintroduce; 
    function Remove(const ID: TGUID; Module: LongWord): Integer; overload;
    function Remove(Service: PGlobalService; Module: LongWord): Integer; overload;
    function Remove(const Item: IGlobalServiceV2; Module: LongWord): Integer; overload;
    function Remove(Module: LongWord): Integer; overload; 
  public
    constructor Create(const List: IGlobalServiceListV2; Kind: TGlobalServiceKind);
    destructor Destroy; override;
  public
    property Owner: IGlobalServiceListV2 read GetOwner; 
  end;
  
implementation

uses
  SilOtTool,
  SilBtError,
  SilBtInterfacePtr,
  SilLtReference,
  SilLmGlobalServiceV2;

type
  TNullName = string[5];

const
  CNull: TNullName = 'null';  

{ TSilGlobalListV2 }

constructor TSilGlobalListV2.Create(const List: IGlobalServiceListV2; Kind: TGlobalServiceKind);
begin
  inherited Create(False, InterfaceHandler);
  FKind := Kind;
  MakeRef(List, @FOwner);
end;

destructor TSilGlobalListV2.Destroy;
begin
  DropRef(@FOwner);
  inherited;
end;

function TSilGlobalListV2.GetHook: Pointer;
begin
  Result := FHook;
end;

function TSilGlobalListV2.GetOwner: IGlobalServiceListV2;
begin
  Result := IGlobalServiceListV2(FOwner);
end;

function TSilGlobalListV2.Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean;
var
  Name: string;
begin
  Result := inherited Enumerate(Enum, Name);
  if Result then Item := Get(Enum.Iteration);
end;

function TSilGlobalListV2.Find(Service: PGlobalService; out Item: IGlobalServiceV2): Boolean;
begin
  Result := Find(Service.ID^, Item);
end;

function TSilGlobalListV2.Find(const ID: TGUID; out Item: IGlobalServiceV2): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(Guid.ToStr(ID));
  Result := ValidIndex(Index);
  if Result then Item := Get(Index);
end;

function TSilGlobalListV2.Get(const ID: TGUID): IGlobalServiceV2;
begin
  if not Find(ID, Result) then
    raise Error.Create('NO HAY UN SERVICIO IDENTIFICADO POR %s', [Guid.ToStr(ID)]);
end;

function TSilGlobalListV2.Get(Index: Integer): IGlobalServiceV2;
begin
  Result := IUnknown(GetPtr(Index)) as IGlobalServiceV2;
end;

function TSilGlobalListV2.Get(Service: PGlobalService; Module: LongWord): IGlobalServiceV2;
begin
  if not Find(Service, Result) then
    Result := Add(Service, Module);
end;

function TSilGlobalListV2.Add(Service: PGlobalService; Module: LongWord): IGlobalServiceV2;
begin
  DoCheck(Service, Module);
  Result := TSilGlobalServiceV2.Create(Self, Service, Module, FKind);
  inherited Add(Guid.ToStr(Service.ID^), @Result);
end;

function TSilGlobalListV2.Remove(const Item: IGlobalServiceV2; Module: LongWord): Integer;
begin
  if Item.Service.Module = Module then
  begin
    Result := IndexOfPtr(@Item);
    if (Result <> -1) then
      Delete(Result);
  end else
    Result := -1;
end;

function TSilGlobalListV2.Remove(Service: PGlobalService; Module: LongWord): Integer;
begin
  Result := Remove(Service.ID^, Module);
end;

function TSilGlobalListV2.Remove(const ID: TGUID; Module: LongWord): Integer;
var
  Item: IGlobalServiceV2;
begin
  if Find(ID, Item) then
    Result := Remove(Item, Module) else
    Result := -1;
end;

function TSilGlobalListV2.Remove(Module: LongWord): Integer;
var
  Item: IGlobalServiceV2;
  Index: Integer;
begin
  Result := 0;
  Index := Count - 1;
  while Index >= 0 do
  begin
    Item := Get(Index);
    try
      if Item.IsCreated and (Item.References.Count > 0) then
        Item.References.Remove(Module);

      if Item.Service.Module = Module then
        Remove(Item, Module);

      Dec(Index);
      Inc(Result);
    finally
      Item := nil;
    end;
  end;
end;

procedure TSilGlobalListV2.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  Ref.ExtractInterface(Listener, IGlobalServicesHookV2, FHook);
end;

procedure TSilGlobalListV2.RemoveListener(const Listener: IInterface);
begin
  FHook := nil;
end;

procedure TSilGlobalListV2.DoCheck(Service: PGlobalService; Module: LongWord);
begin
  Error.Check(Assigned(Service), 'El servicio especificado no está asignado', ESilGlobalServices);
  Error.Check(Assigned(Service.ID), 'El GUID del servicio especificado no está asignado', ESilGlobalServices);
  if Service.Name = nil then Service.Name := @CNull;
  if Service.Module = 0 then Service.Module := FindHInstance(Service.ID);
  if Service.Module = 0 then Service.Module := FindHInstance(Service);
  if Service.Module = 0 then Service.Module := Module;
  if Service.Module = 0 then Service.Module := FindHInstance(Self.ClassType);
end;

end.
 