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

unit SilLkContainer;

{$INCLUDE Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiLock,
  SilLiParameters,
  SilLiContainerTypes,
  SilLiContainer,
  SilLiContainerBase;

type
  TSilContainer = class(
    TSilContainerType,
    ISynchronizable,
    IContainerTypes,
    IContainerItemsStatic,
    IContainerItemsDynamic,
    IContainerCursors,
    IContainerPointers,
    IContainerPointersStatic,
    IContainerPointersDynamic,
    IContainerStatic,
    IContainerDynamic )
  private
    FHandler: ITypeHandler;
    FCursorClass: TSilCursorClass;
  protected
    FBase: IBaseContainer;
  protected
    function ISynchronizable.Lock = Locked;
  protected
    function GetISynchronizable: ISynchronizable;
  protected // IContainerItemsStatic
    function GetCount: Integer;
    function GetHandler: ITypeHandler;
    function GetAllocator: ITypeAllocator;
    function GetFirstItem: HItem;
    function GetLastItem: HItem;
    function IsValid(const Item: HItem): Boolean;
    procedure Check(const Item: HItem);
  protected // IContainerItemsDynamic
    procedure SetCount(Value: Integer);
    procedure Clear;
    procedure Delete(Item: HItem); overload; virtual;
    procedure Delete(const Cursor: IContainerCursor); overload; virtual;
    procedure Delete(First, Last: HItem); overload; virtual;
    procedure Delete(const First, Last: IContainerCursor); overload; virtual;
    procedure Exchange(Item1, Item2: HItem); overload;
    procedure Exchange(const Item1, Item2: IContainerCursor); overload;
    procedure Move(Current, Target: HItem); overload;
    procedure Move(const Current, Target: IContainerCursor); overload;
  protected // IContainerCursors
    function IContainerCursors.Create = CreateCursor;
  protected // IContainerPointers
    function IContainerPointers.Create = CreateCursor;
    function Find(Data: HData; const Kind: TGUID; out Value; const Comparator: ITypeComparator = nil; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
  protected // IContainerPointersStatic
    function IContainerPointersStatic.Create = CreateCursor;
    function IContainerPointersStatic.Get = CursorStatic;
    function GetFirstStaticCursor: IContainerPointerStatic;
    function GetLastStaticCursor: IContainerPointerStatic;
    function CursorGet(const Cursor: IContainerCursor): HData;
    function CursorStatic(const Item: HItem; Delta: Integer = 1): IContainerPointerStatic; overload;
  protected // IContainerPointersDynamic
    function IContainerPointersDynamic.Create = CreateCursor;
    function IContainerPointersDynamic.Get = CursorDynamic;
    function GetFirstDynamicCursor: IContainerPointerDynamic;
    function GetLastDynamicCursor: IContainerPointerDynamic;
    procedure CursorPut(const Cursor: IContainerCursor; Source: HData);
    function CursorDynamic(const Item: HItem; Delta: Integer = 1): IContainerPointerDynamic; overload;
  protected
    function CreateCursor(const Item: HItem; const Kind: TGUID; out Value; Delta: Integer = 1): Boolean; overload;
  protected // IContainerStatic
    function GetItemsStatic: IContainerItemsStatic;
    function GetCursorsStatic: IContainerPointersStatic;
    function HandleGet(Item: HItem): HData;
    function Cursor(Data: HData; Param: Pointer = nil; Delta: Integer = 1): IContainerPointerStatic; overload;
    function Handle(Data: HData; Param: Pointer = nil): HItem;
    function Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean; overload; 
    function Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean; overload;
  protected // IContainerDynamic
    function GetItemsDynamic: IContainerItemsDynamic;
    function GetCursorsDynamic: IContainerPointersDynamic;
    function Add(Source: HData): HItem; overload;
    function Add(Count: Integer = 1): HItem; overload; 
    function Add(Count: Integer; Source: HData): HItem; overload; 
    function Add(out Data: HData; Count: Integer; Source: HData = nil): HItem; overload;   
    function Insert(Item: HItem; Source: HData; Count: Integer = 1): HItem; overload; 
    function Insert(out Data: HData; Item: HItem; Count: Integer = 1; Source: HData = nil): HItem; overload;   
    procedure HandlePut(Item: HItem; Source: HData);
    function Remove(Data: HData; Param: Pointer): HItem; overload;
  protected
    property Base: IBaseContainer read FBase;
    property Handler: ITypeHandler read FHandler;
    property First: HItem read GetFirstItem;
    property Last: HItem read GetLastItem;
    property CursorClass: TSilCursorClass read FCursorClass write FCursorClass;
  public
    constructor Create(
            AClass: TSilBaseContainerClass;
      const Handler: ITypeHandler;
      const Options: IParameters = nil;
      const Owner: IUnknown = nil;
      const Controller: IUnknown = nil;
            Param: Pointer = nil); override;
    destructor Destroy; override;
  end;

  TSilCursor = class(
    TSilCursorType,
    IContainerCursor,
    IContainerCursorStatic,
    IContainerCursorDynamic,
    IContainerPointerStatic,
    IContainerPointerDynamic )
  private
    FOwner: TSilContainer;
    FHandler: ITypeHandler;
    FBase: IBaseContainer;
    FStep: Integer;
  protected
    FItem: HItem;
  protected // IContainerCursor
    function GetIsValid: Boolean;
    function GetItem: HItem;
    procedure SetItem(Value: HItem);
    function GetCursor: IContainerCursor;
    function GetThis: IContainerCursor;
    function IsEqual(const Other: IContainerCursor): Boolean;
    function Next(Delta: Integer): Boolean;  
    function Clone(Step: Integer = 1): IContainerCursor; overload;
    function Clone(const Kind: TGUID; out Obj; Step: Integer = 1): Boolean; overload;
  protected // IContainerCursorStatic
    function GetOwnerStatic: IContainerStatic;
    function GetThisStatic: IContainerCursorStatic;
  protected // IContainerCursorDynamic
    function GetOwnerDynamic: IContainerDynamic;
    function GetThisDynamic: IContainerCursorDynamic;
  protected // IContainerPointerStatic
    function GetCursorStatic: IContainerCursorStatic;
    function GetThisStaticData: IContainerPointerStatic;
    function GetData: HData;
    function Read(out Data): Boolean;
    function Get(out Data: HData): Boolean; 
  protected // IContainerPointerDynamic
    function GetCursorDynamic: IContainerCursorDynamic;
    function GetThisDynamicData: IContainerPointerDynamic;
    procedure SetData(const Data: HData);
    function Write(const Data): Boolean;
    function Put(const Data: HData): Boolean;
    function Insert(Data: HData): Boolean;
  protected
    property Owner: TSilContainer read FOwner;
    property Handler: ITypeHandler read FHandler;
    property Step: Integer read FStep;
    property Cursor: IContainerCursor read GetCursor;
    property IsValid: Boolean read GetIsValid;
  public
    constructor Create(List: Pointer; Item: HItem = HNull; Delta: Integer = 1); override; 
    destructor Destroy; override;
  end;

implementation

uses
  SilBtError,
  SilLdBaseList,
  SilLtContainer,
  SilLmContainerTypes;

{ TSilContainer }

constructor TSilContainer.Create(AClass: TSilBaseContainerClass; const Handler: ITypeHandler; const Options: IParameters; const Owner: IUnknown; const Controller: IUnknown; Param: Pointer);
begin
  inherited Create(Controller, Owner, Param);
  ContainerKind.Create(IBaseContainer, FBase, AClass, Handler, Options);
  FHandler := FBase.Handler;
  FCursorClass := TSilCursor;
end;

destructor TSilContainer.Destroy;
begin
  Clear;
  FBase := nil;
  FHandler := nil;
  inherited;
end;

function TSilContainer.GetISynchronizable: ISynchronizable;
begin
  Result := Self;
end;

function TSilContainer.GetCount: Integer;
begin
  Result := FBase.Count;
end;

function TSilContainer.GetHandler: ITypeHandler;
begin
  Result := FHandler;
end;

function TSilContainer.GetAllocator: ITypeAllocator;
begin
  Result := FBase.Allocator;
end;

function TSilContainer.GetFirstItem: HItem;
begin
  Result := FBase.First;
end;

function TSilContainer.GetLastItem: HItem;
begin
  Result := FBase.Last;
end;

function TSilContainer.IsValid(const Item: HItem): Boolean;
begin
  Result := FBase.IsValid(Item);
end;

procedure TSilContainer.Check(const Item: HItem);
begin
  if not IsValid(Item) then
    Error.Throw(SListIndexError, [Item, FBase.Count], ESilListError);
end;

procedure TSilContainer.SetCount(Value: Integer);
begin
  FBase.Count := Value;
end;

procedure TSilContainer.Clear;
begin
  FBase.Clear;
end;

procedure TSilContainer.Delete(Item: HItem);
begin
  Check(Item);
  FBase.Delete(Item, Item);
end;

procedure TSilContainer.Delete(const Cursor: IContainerCursor);
begin
  Delete(Cursor.Item);
end;

procedure TSilContainer.Delete(First, Last: HItem);
begin
  Check(First);
  Check(Last);
  FBase.Delete(First, Last);
end;

procedure TSilContainer.Delete(const First, Last: IContainerCursor);
begin
  Delete(First.Item, Last.Item);
end;

procedure TSilContainer.Exchange(Item1, Item2: HItem);
begin
  Check(Item1);
  Check(Item2);
  FBase.Exchange(Item1, Item2);
end;

procedure TSilContainer.Exchange(const Item1, Item2: IContainerCursor);
begin
  Exchange(Item1.Item, Item2.Item);
end;

procedure TSilContainer.Move(const Current, Target: IContainerCursor);
begin
  Move(Current.Item, Target.Item);
end;

function TSilContainer.CreateCursor(const Item: HItem; const Kind: TGUID; out Value; Delta: Integer = 1): Boolean;
begin
  Result := FCursorClass.Create(Self, Item, Delta).GetInterface(Kind, Value);
end;

function TSilContainer.Find(Data: HData; const Kind: TGUID; out Value; const Comparator: ITypeComparator; Param: Pointer; Delta: Integer): Boolean; 
var
  Item: HItem;
begin
  Result := FBase.Find(Data, @Item, Comparator, Param) and CreateCursor(Item, Kind, Value, Delta);
end;

function TSilContainer.GetFirstStaticCursor: IContainerPointerStatic;
begin
  CreateCursor(First, IContainerPointerStatic, Result, +1);
end;

function TSilContainer.GetLastStaticCursor: IContainerPointerStatic;
begin
  CreateCursor(Last, IContainerPointerStatic, Result, -1);
end;

function TSilContainer.CursorGet(const Cursor: IContainerCursor): HData;
begin
  Result := HandleGet(Cursor.Item);
end;

function TSilContainer.CursorStatic(const Item: HItem; Delta: Integer = 1): IContainerPointerStatic;
begin
  CreateCursor(Item, IContainerPointerStatic, Result, Delta)
end;

function TSilContainer.GetFirstDynamicCursor: IContainerPointerDynamic;
begin
  CreateCursor(First, IContainerPointerDynamic, Result, +1);
end;

function TSilContainer.GetLastDynamicCursor: IContainerPointerDynamic;
begin
  CreateCursor(Last, IContainerPointerDynamic, Result, -1);
end;

procedure TSilContainer.CursorPut(const Cursor: IContainerCursor; Source: HData);
begin
  HandlePut(Cursor.Item, Source);
end;

function TSilContainer.CursorDynamic(const Item: HItem; Delta: Integer = 1): IContainerPointerDynamic;
begin
  CreateCursor(Item, IContainerPointerDynamic, Result, Delta)
end;

function TSilContainer.GetItemsStatic: IContainerItemsStatic;
begin
  Result := Self;
end;

function TSilContainer.GetCursorsStatic: IContainerPointersStatic;
begin
  Result := Self;
end;

function TSilContainer.HandleGet(Item: HItem): HData;
begin
  Check(Item);
  Result := FBase.GetData(Item);
end;

function TSilContainer.Cursor(Data: HData; Param: Pointer; Delta: Integer): IContainerPointerStatic;
begin
  Result := CursorStatic(Handle(Data, Param), Delta);
end;

function TSilContainer.Handle(Data: HData; Param: Pointer): HItem;
begin
  if not FBase.Find(Data, @Result, nil, Param) then
    Result := HNull;
end;

function TSilContainer.Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean; 
begin
  Result := FBase.Find(Data, Item, Comparator, Param);
end;

function TSilContainer.Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer): Boolean;
begin
  Result := FBase.Find(Data, @Item, Comparator, Param);
end;

function TSilContainer.GetItemsDynamic: IContainerItemsDynamic;
begin
  Result := Self;
end;

function TSilContainer.GetCursorsDynamic: IContainerPointersDynamic;
begin
  Result := Self;
end;

function TSilContainer.Add(Source: HData): HItem;
begin
  Result := FBase.Add(Source);
end;

function TSilContainer.Add(Count: Integer): HItem;
begin
  Result := FBase.Add(nil, Count);
end;

function TSilContainer.Add(Count: Integer; Source: HData): HItem;
begin
  Result := FBase.Add(Source, Count);
end;

function TSilContainer.Add(out Data: HData; Count: Integer; Source: HData): HItem;
begin
  Result := FBase.Add(Source, Count, @Data)
end;

function TSilContainer.Insert(Item: HItem; Source: HData; Count: Integer): HItem;
begin
  Result := FBase.Insert(Source, Count, Item);
end;

function TSilContainer.Insert(out Data: HData; Item: HItem; Count: Integer; Source: HData): HItem;
begin
  Result := FBase.Insert(Source, Count, Item, @Data);
end;

procedure TSilContainer.HandlePut(Item: HItem; Source: HData);
begin
  Check(Item);
  FHandler.Copy(FBase.GetData(Item), Source);
end;

function TSilContainer.Remove(Data: HData; Param: Pointer): HItem;
begin
  Result := Handle(Data, Param);
  if Result <> HNull then Delete(Result);
end;

procedure TSilContainer.Move(Current, Target: HItem);
begin
  Check(Current);
  Check(Target);
  FBase.Move(Current, Target);
end;

{ TSilCursor }

constructor TSilCursor.Create(List: Pointer; Item: HItem; Delta: Integer);
begin
  inherited Create;
  FOwner := List;
  FBase := FOwner.Base;
  FHandler := FOwner.Handler;
  FItem := Item;
  FStep := Delta;
end;

destructor TSilCursor.Destroy;
begin
  FHandler := nil;
  FBase := nil;
  FOwner := nil;
  inherited;
end;

function TSilCursor.GetItem: HItem;
begin
  Result := FItem;
end;

procedure TSilCursor.SetItem(Value: HItem);
begin
  FItem := Value;
end;

function TSilCursor.GetCursor: IContainerCursor;
begin
  Result := Self;
end;

function TSilCursor.GetThis: IContainerCursor;
begin
  Result := Self;
end;

function TSilCursor.GetIsValid: Boolean;
begin
  Result := (FItem <> HNull) and FOwner.IsValid(FItem);
end;

function TSilCursor.IsEqual(const Other: IContainerCursor): Boolean;
begin
  Result := (Self.IsValid = Other.IsValid) and (FItem = Other.Item);
end;

function TSilCursor.Next(Delta: Integer): Boolean;
begin
  Result := FBase.GetNext(FItem, Delta * FStep);
end;

function TSilCursor.Clone(Step: Integer): IContainerCursor;
begin
  Clone(IContainerCursor, Result, Step);
end;

function TSilCursor.Clone(const Kind: TGUID; out Obj; Step: Integer): Boolean;
begin
  Result := FOwner.CreateCursor(FItem, Kind, Obj, Step * FStep);
end;

function TSilCursor.GetOwnerStatic: IContainerStatic;
begin
  Result := FOwner;
end;

function TSilCursor.GetThisStatic: IContainerCursorStatic;
begin
  Result := Self;
end;

function TSilCursor.GetOwnerDynamic: IContainerDynamic;
begin
  Result := FOwner;
end;

function TSilCursor.GetThisDynamic: IContainerCursorDynamic;
begin
  Result := Self;
end;

function TSilCursor.GetCursorStatic: IContainerCursorStatic;
begin
  Result := Self;
end;

function TSilCursor.GetThisStaticData: IContainerPointerStatic;
begin
  Result := Self;
end;

function TSilCursor.GetData: HData;
begin
  Result := FOwner.HandleGet(FItem);
end;

function TSilCursor.Read(out Data): Boolean;
begin
  Result := GetIsValid;
  if Result then FHandler.Copy(@Data, FOwner.HandleGet(FItem));
end;

function TSilCursor.Get(out Data: HData): Boolean;
begin
  Result := IsValid;
  if Result then Data := FOwner.HandleGet(FItem);
end;

function TSilCursor.GetCursorDynamic: IContainerCursorDynamic;
begin
  Result := Self;
end;

function TSilCursor.GetThisDynamicData: IContainerPointerDynamic;
begin
  Result := Self;
end;

procedure TSilCursor.SetData(const Data: HData);
begin
  FOwner.HandlePut(FItem, Data)
end;

function TSilCursor.Write(const Data): Boolean;
begin
  Result := IsValid;
  if Result then FHandler.Copy(FOwner.HandleGet(FItem), @Data);
end;

function TSilCursor.Put(const Data: HData): Boolean;
begin
  Result := IsValid;
  if Result then FOwner.HandlePut(FItem, Data)
end;

function TSilCursor.Insert(Data: HData): Boolean;
begin
  Result := IsValid;
  if Result then FOwner.Insert(FItem, Data)
end;

end.
