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

unit SilLtContainer;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypeInfo,
  SilLiEnumerator,
  SilLiInterfaceList,
  SilLiPointerList,
  SilLiParameters,
  SilLiContainer,
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLiContainerList,
  SilLiContainerVector,
  SilLiContainerStrings;

type
  ContainerClass = class of ContainerKind;
  
  Container = class (SilBkTool.Tool)
    class procedure Create(Container: TSilContainerClass; const IID: TGUID; out Instance; Kind: ContainerClass = nil; const Handler: ITypeHandler = nil; const Options: IParameters = nil); overload;
    class function Create(Kind: ContainerClass = nil; const Handler: ITypeHandler = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Kind: ContainerClass = nil; const Compare: ITypeComparator = nil; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: Pointer; Kind: ContainerClass = nil; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: TTypeCompare; Kind: ContainerClass = nil; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Kind: ContainerClass = nil; const Compare: ITypeComparator = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: Pointer; Kind: ContainerClass = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: TTypeCompare; Kind: ContainerClass = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Strings(Kind: ContainerClass = nil; const Handler: ITypeHandler = nil; const Options: IParameters = nil): IStringsDynamic;
    class function Locked(LockedValue: Boolean = True; const Parameters: IParameterList = nil): IParameterList; overload;
  end;

  Handler = class(SilBkTool.Tool)
    class function Check(const Handler: ITypeHandler): ITypeHandler;
    class function Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil): ITypeinfoHandler; overload;
    class function Create(Size: Integer; const Compare: ITypeComparator = nil; const Hook: ITypeHandler = nil): ITypeHandler; overload;
    class function Compose(const Handlers: array of ITypeHandler; const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil): ITypeHandler;
    class function Allocator(const Handler: ITypeHandler; Offset: Integer = 0): ITypeAllocator;
  end;

  Compare = class(SilBkTool.Tool)
    class function Create(Compare: Pointer): ITypeComparator; overload;
    class function Create(Compare: TTypeCompare): ITypeComparator; overload;
  end;

  Sequence = class(SilBkTool.Tool)
    class function Check(const Owner: IContainerStatic; const Sequence: IContainerSequence): IContainerSequence; overload;
    class function Check(const Owner: IContainerStatic; const Sequence: IContainerSequenceStatic): IContainerSequenceStatic; overload;
    class function Check(const Owner: IContainerDynamic; const Sequence: IContainerSequenceDynamic): IContainerSequenceDynamic; overload;
    class function Create(const Owner: IContainerStatic; Start: HItem = HNull; Delta: Integer = 1): IContainerSequenceStatic; overload;
    class function Create(const Owner: IContainerDynamic; Start: HItem = HNull; Delta: Integer = 1): IContainerSequenceDynamic; overload;
    class function Create(const Owner: IContainerStatic; const Kind: TGUID; Start: HItem = HNull; Delta: Integer = 1): IContainerSequenceStatic; overload;
    class function Create(const Owner: IContainerDynamic; const Kind: TGUID; Start: HItem = HNull; Delta: Integer = 1): IContainerSequenceDynamic; overload;
    class function Create(const Owner: IContainerStatic; const First, Last: IContainerCursor; const Kind: TGUID): IContainerSequenceStatic; overload;
    class function Create(const Owner: IContainerDynamic; const First, Last: IContainerCursor; const Kind: TGUID): IContainerSequenceDynamic; overload;
    class function Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const Sequence: IContainerSequence; Locked: Boolean = False): Boolean; overload;
    class function Enumerator(const Sequence: IContainerSequenceStatic; out Enum: IEnumerator; Locked: Boolean = False): Boolean; overload;
    class function Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; Start: HItem = HNull; Delta: Integer = 1; Locked: Boolean = False): Boolean; overload;
    class function Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const Kind: TGUID; Start: HItem = HNull; Delta: Integer = 1; Locked: Boolean = False): Boolean; overload;
    class function Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const First, Last: IContainerCursor; const Kind: TGUID; Locked: Boolean = False): Boolean; overload;     
    class function Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const Sequence: IContainerSequence): Boolean; overload;
    class function Start(const Sequence: IContainerSequenceStatic; out Enum: IEnumerator; out Data: HData): Boolean; overload;
    class function Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; Start: HItem = HNull; Delta: Integer = 1): Boolean; overload;
    class function Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const Kind: TGUID; Start: HItem = HNull; Delta: Integer = 1): Boolean; overload;
    class function Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const First, Last: IContainerCursor; const Kind: TGUID): Boolean; overload;
    class function Start(const Owner: IEnumerable; out Enum: IEnumerator; out Data: HData; Locked: Boolean = False): Boolean; overload;
    class function Next(var Enum: IEnumerator; out Data: HData): Boolean;
    class function Get(const Enum: IEnumerator; out Data: HData): Boolean;
  end;

  Vector = class(Container)
    class procedure Create(Container: TSilContainerClass; const IID: TGUID; out Instance; const Handler: ITypeHandler = nil; const Options: IParameters = nil); overload;
    class function Create(const Handler: ITypeHandler = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: Pointer; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: TTypeCompare; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: TTypeCompare; const Options: IParameters; Size: Integer = 0): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; const Options: IParameters; Size: Integer = 0): IContainerDynamic; overload;
    class function Create(Size: Integer; const Compare: ITypeComparator = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: Pointer; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: TTypeCompare; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; const Options: IParameters): IContainerDynamic; overload;
    class function Strings(const Handler: ITypeHandler = nil; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(TypeInfo: PTypeInfo; Comparer: Pointer; Size: Integer = 0; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(TypeInfo: PTypeInfo; Comparer: TTypeCompare; Size: Integer = 0; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(TypeInfo: PTypeInfo; const Options: IParameters; Size: Integer = 0): IStringsDynamic; overload;
    class function Strings(Size: Integer; const Compare: ITypeComparator = nil; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(Size: Integer; Comparer: Pointer; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(Size: Integer; Comparer: TTypeCompare; const Options: IParameters = nil): IStringsDynamic; overload;
    class function Strings(Size: Integer; const Options: IParameters): IStringsDynamic; overload;
    class function InterfaceList(Locked: Boolean = false): IInterfaceList;
    class function Interfaces(const Comparer: ITypeComparator; Locked: Boolean = false): IInterfaceList; overload;
    class function Interfaces(Locked: Boolean = false): IInterfaceList; overload;
    class function PointerList(Locked: Boolean = false): IPointerList;
    class function Pointers(const Comparer: ITypeComparator; Locked: Boolean = false): IPointerList; overload;
    class function Pointers(Locked: Boolean = false): IPointerList; overload;
  end;

  List = class(Container)
    class procedure Create(Container: TSilContainerClass; const IID: TGUID; out Instance; const Handler: ITypeHandler = nil; const Options: IParameters = nil); overload;
    class function Create(const Handler: ITypeHandler = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: Pointer; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(TypeInfo: PTypeInfo; Comparer: TTypeCompare; Size: Integer = 0; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; const Compare: ITypeComparator = nil; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: Pointer; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Create(Size: Integer; Comparer: TTypeCompare; const Options: IParameters = nil): IContainerDynamic; overload;
    class function Strings(const Handler: ITypeHandler = nil; const Options: IParameters = nil): IStringsDynamic;
    class function InterfaceList(Locked: Boolean = false): IInterfaceList;
    class function Interfaces(const Comparer: ITypeComparator; Locked: Boolean = false): IInterfaceList; overload;
    class function Interfaces(Locked: Boolean = false): IInterfaceList; overload;
    class function PointerList(Locked: Boolean = false): IPointerList;
    class function Pointers(const Comparer: ITypeComparator; Locked: Boolean = false): IPointerList; overload;
    class function Pointers(Locked: Boolean = false): IPointerList; overload;
  end;

  ContainerKind = class(SilBkTool.Tool)
    class function Kind: TSilBaseContainerClass; virtual; abstract;
    class function Check(Kind: ContainerClass): ContainerClass; overload;
    class function Check(Kind: TSilBaseContainerClass): TSilBaseContainerClass; overload;
    class procedure Create(const IID: TGUID; out Instance; Kind: TSilBaseContainerClass = nil; const Handler: ITypeHandler = nil; const Options: IParameters = nil; const Controller: IUnknown = nil); overload;
    class procedure Create(const IID: TGUID; out Instance; const Handler: ITypeHandler = nil; const Options: IParameters = nil; const Controller: IUnknown = nil); overload; virtual;
  end;

type
  ckList = class(ContainerKind)
    class function Kind: TSilBaseContainerClass; override;
    class function Create(const Handler: ITypeHandler = nil; const Options: IParameters = nil; const Controller: IUnknown = nil): IBaseList; overload;
  end;

type
  ckVector = class(ContainerKind)
    class function Kind: TSilBaseContainerClass; override;
    class function Create(const Handler: ITypeHandler = nil; const Options: IParameters = nil; const Controller: IUnknown = nil): IBaseVector; overload;
  end;

implementation

uses
  SilOtTool,
  SilBtError,
  SilLtList,
  SilLkContainer,
  SilLmContainerList,
  SilLmContainerVector,
  SilLmContainerTypes,
  SilLmContainerStrings,
  SilLmContainerSequence,
  SilLmContainerInterfaces,
  SilLmContainerPointers,
  SilLmContainerEnumerator;

{ Container }

class procedure Container.Create(Container: TSilContainerClass; const IID: TGUID; out Instance; Kind: ContainerClass; const Handler: ITypeHandler; const Options: IParameters);
var
  _Object: TSilContainerType;
begin
  _Object := Container.Create(Kind.Kind, Handler, Options);
  try
    Error.Check(_Object.GetInterface(IID, Instance), 'Interface %s no soportada', [Guid.ToStr(IID)]);
  except
    _Object.Free;
    raise;
  end;
end;

class function Container.Create(Kind: ContainerClass; const Handler: ITypeHandler; const Options: IParameters): IContainerDynamic;
begin
  Create(TSilContainer, IContainerDynamic, Result, Kind, Handler, Options);
end;

class function Container.Create(
        TypeInfo: PTypeInfo;
        Kind: ContainerClass;
  const Compare: ITypeComparator;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(Kind, Handler.Create(TypeInfo, Compare, Size), Options);
end;

class function Container.Create(
        Size: Integer;
        Kind: ContainerClass;
  const Compare: ITypeComparator;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(Kind, Handler.Create(Size, Compare), Options);
end;

class function Container.Create(
        TypeInfo: PTypeInfo;
        Comparer: Pointer;
        Kind: ContainerClass;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(TypeInfo, Kind, Compare.Create(Comparer), Size, Options);
end;

class function Container.Create(
        TypeInfo: PTypeInfo;
        Comparer: TTypeCompare;
        Kind: ContainerClass;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(TypeInfo, Kind, Compare.Create(Comparer), Size, Options);
end;

class function Container.Create(
        Size: Integer;
        Comparer: Pointer;
        Kind: ContainerClass;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(Size, Kind, Compare.Create(Comparer), Options);
end;

class function Container.Create(
        Size: Integer;
        Comparer: TTypeCompare;
        Kind: ContainerClass;
  const Options: IParameters): IContainerDynamic;
begin
  Result := Create(Size, Kind, Compare.Create(Comparer), Options);
end;

class function Container.Strings(Kind: ContainerClass; const Handler: ITypeHandler; const Options: IParameters): IStringsDynamic;
begin
  Create(TSilStringContainer, IStringsDynamic, Result, Kind, Handler, Options);
end;

class function Container.Locked(LockedValue: Boolean; const Parameters: IParameterList): IParameterList;
begin
  Result := ListTool.Params.Check(Parameters);
  Result['Locked'] := LockedValue;
end;

{ Handler }

class function Handler.Check(const Handler: ITypeHandler): ITypeHandler;
begin
  if not Assigned(Handler) then
    Result := Create(SizeOf(Pointer)) else
    Result := Handler;
end;

class function Handler.Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler): ITypeinfoHandler;
begin
  Result := TSilTypeinfoHandler.CreateHandler(TypeInfo, Compare, Size, Hook);
end;

class function Handler.Create(Size: Integer; const Compare: ITypeComparator; const Hook: ITypeHandler): ITypeHandler;
begin
  Result := TSilListHandler.Create(Compare, Size, Hook);
end;

class function Handler.Compose(const Handlers: array of ITypeHandler; const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler): ITypeHandler;
begin
  Result := TSilTypeCompositeHandler.Create(Handlers, Compare, Size, Hook);
end;

class function Handler.Allocator(const Handler: ITypeHandler; Offset: Integer): ITypeAllocator;
begin
  Result := TSilTypeAllocator.Create(Handler, Offset);
end;

{ Compare }

class function Compare.Create(Compare: Pointer): ITypeComparator;
begin
  Result := TSilTypeNestedCompare.Create(Compare);
end;

class function Compare.Create(Compare: TTypeCompare): ITypeComparator;
begin
  Result := TSilTypeMethodCompare.Create(Compare);
end;

{ ContainerKind }
                                                                                                 
class function ContainerKind.Check(Kind: ContainerClass): ContainerClass;
begin
  if Assigned(Kind) then
    Result := Kind else
    Result := ckVector;
end;

class function ContainerKind.Check(Kind: TSilBaseContainerClass): TSilBaseContainerClass;
begin
  if Assigned(Kind) then
    Result := Kind else
    Result := TSilBaseVector;
end;

class procedure ContainerKind.Create(const IID: TGUID; out Instance; Kind: TSilBaseContainerClass; const Handler: ITypeHandler; const Options: IParameters; const Controller: IUnknown);
var
  _Object: TSilBaseContainerType;
begin
  _Object := Check(Kind).Create(SilLtContainer.Handler.Check(Handler), Options, Controller);
  try
    Error.Check(_Object.GetInterface(IID, Instance), 'Interface %s no soportada', [Guid.ToStr(IID)]);
  except
    _Object.Free;
    raise;
  end;
end;

class procedure ContainerKind.Create(const IID: TGUID; out Instance; const Handler: ITypeHandler; const Options: IParameters; const Controller: IUnknown);
begin
  Create(IID, Instance, Kind, Handler, Options);
end;

{ ckList }

class function ckList.Kind: TSilBaseContainerClass;
begin
  Result := TSilBaseList;
end;

class function ckList.Create(const Handler: ITypeHandler; const Options: IParameters; const Controller: IUnknown): IBaseList;
begin
  Create(IBaseList, Result, Handler, Options, Controller);
end;

{ ckVector }

class function ckVector.Kind: TSilBaseContainerClass;
begin
  Result := TSilBaseVector;
end;

class function ckVector.Create(const Handler: ITypeHandler; const Options: IParameters; const Controller: IUnknown): IBaseVector;
begin
  Create(IBaseVector, Result, Handler, Options, Controller);
end;

{ Sequence }

class function Sequence.Check(const Owner: IContainerStatic; const Sequence: IContainerSequence): IContainerSequence;
begin
  ASSERT(Assigned(Owner));
  if not Assigned(Sequence) then
    Result := Create(Owner) else
    Result := Sequence;
end;

class function Sequence.Check(const Owner: IContainerStatic; const Sequence: IContainerSequenceStatic): IContainerSequenceStatic;
begin
  ASSERT(Assigned(Owner));
  if not Assigned(Sequence) then
    Result := Create(Owner) else
    Result := Sequence;
end;

class function Sequence.Check(const Owner: IContainerDynamic; const Sequence: IContainerSequenceDynamic): IContainerSequenceDynamic;
begin
  ASSERT(Assigned(Owner));
  if not Assigned(Sequence) then
    Result := Create(Owner) else
    Result := Sequence;
end;

class function Sequence.Create(const Owner: IContainerStatic; Start: HItem; Delta: Integer): IContainerSequenceStatic;
begin
  Result := Create(Owner, IContainerCursor, Start, Delta);
end;

class function Sequence.Create(const Owner: IContainerDynamic; Start: HItem; Delta: Integer): IContainerSequenceDynamic;
begin
  Result := Create(Owner, IContainerCursor, Start, Delta);
end;

class function Sequence.Create(const Owner: IContainerStatic; const Kind: TGUID; Start: HItem; Delta: Integer): IContainerSequenceStatic;
begin
  Result := TSilSequenceLiteSimpleStatic.Create(Owner, Kind, Start, Delta);
end;

class function Sequence.Create(const Owner: IContainerDynamic; const Kind: TGUID; Start: HItem; Delta: Integer): IContainerSequenceDynamic;
begin
  Result := TSilSequenceLiteSimpleDynamic.Create(Owner, Kind, Start, Delta);
end;

class function Sequence.Create(const Owner: IContainerStatic; const First, Last: IContainerCursor; const Kind: TGUID): IContainerSequenceStatic;
begin
  Result := TSilSequenceRangeStatic.Create(Owner, First, Last, Kind);
end;

class function Sequence.Create(const Owner: IContainerDynamic; const First, Last: IContainerCursor; const Kind: TGUID): IContainerSequenceDynamic;
begin
  Result := TSilSequenceRangeDynamic.Create(Owner, First, Last, Kind);
end;

class function Sequence.Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const Sequence: IContainerSequence; Locked: Boolean): Boolean;
begin
  Result := Assigned(Owner) and (Owner.Items.Count > 0);
  if Result then
  begin
    Enum := TSilContainerEnumerator.Create(Owner, Check(Owner, Sequence), Locked);
    Result := Enum.HasMore;
  end;
end;

class function Sequence.Enumerator(const Sequence: IContainerSequenceStatic; out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(Sequence);
  if Result then
  begin
    Enum := TSilContainerEnumerator.Create(Sequence, Locked);
    Result := Enum.HasMore;
  end;
end;

class function Sequence.Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; Start: HItem; Delta: Integer; Locked: Boolean): Boolean;
begin
  Result := Enumerator(Owner, Enum, Create(Owner, Start, Delta), Locked);
end;

class function Sequence.Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const Kind: TGUID; Start: HItem; Delta: Integer; Locked: Boolean): Boolean;
begin
  Result := Enumerator(Owner, Enum, Create(Owner, Kind, Start, Delta), Locked);
end;

class function Sequence.Enumerator(const Owner: IContainerStatic; out Enum: IEnumerator; const First, Last: IContainerCursor; const Kind: TGUID; Locked: Boolean): Boolean;
begin
  Result := Enumerator(Owner, Enum, Create(Owner, First, Last, Kind), Locked);
end;

class function Sequence.Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const Sequence: IContainerSequence): Boolean;
begin
  Result := Assigned(Owner) and Enumerator(Owner, Enum, Sequence);
  if Result then 
    Get(Enum, Data);
end;

class function Sequence.Start(const Sequence: IContainerSequenceStatic; out Enum: IEnumerator; out Data: HData): Boolean;
begin
  Result := Enumerator(Sequence, Enum);
  if Result then
    Get(Enum, Data);
end;

class function Sequence.Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; Start: HItem; Delta: Integer): Boolean;
begin
  Result := Assigned(Owner) and Self.Start(Owner, Enum, Data, Create(Owner, Start, Delta));
end;

class function Sequence.Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const Kind: TGUID; Start: HItem; Delta: Integer): Boolean;
begin
  Result := Assigned(Owner) and Self.Start(Owner, Enum, Data, Create(Owner, Kind, Start, Delta));
end;

class function Sequence.Start(const Owner: IContainerStatic; out Enum: IEnumerator; out Data: HData; const First, Last: IContainerCursor; const Kind: TGUID): Boolean;
begin
  Result := Assigned(Owner) and Self.Start(Owner, Enum, Data, Create(Owner, First, Last, Kind));
end;

class function Sequence.Start(const Owner: IEnumerable; out Enum: IEnumerator; out Data: HData; Locked: Boolean): Boolean;
begin
  Result := Assigned(Owner) and Owner.GetEnumerator(Enum, Locked);
  if Result then Get(Enum, Data);
end;

class function Sequence.Next(var Enum: IEnumerator; out Data: HData): Boolean;
begin
  if Assigned(Enum) then
    Result := Enum.Next else
    Result := False;
    
  if Result then
    Get(Enum, Data) else
    Enum := nil;
end;

class function Sequence.Get(const Enum: IEnumerator; out Data: HData): Boolean;
begin
  if Assigned(Enum) then
    Enum.Get(Data) else
    Data := nil;
  Result := Assigned(Data);
end;

{ Vector }

class procedure Vector.Create(
        Container: TSilContainerClass;
  const IID: TGUID;
    out Instance;
  const Handler: ITypeHandler;
  const Options: IParameters);
begin
  inherited Create(Container, IID, Instance, ckVector, Handler, Options);
end;

class function Vector.Create(
  const Handler: ITypeHandler;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(ckVector, Handler);
end;

class function Vector.Create(
        TypeInfo: PTypeInfo;
  const Compare: ITypeComparator;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, ckVector, Compare, Size, Options);
end;

class function Vector.Create(
        TypeInfo: PTypeInfo;
        Comparer: Pointer;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, Comparer, ckVector, Size, Options);
end;

class function Vector.Create(
        TypeInfo: PTypeInfo;
        Comparer: TTypeCompare;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, Comparer, ckVector, Size, Options);
end;

class function Vector.Create(
        TypeInfo: PTypeInfo;
        Comparer: TTypeCompare;
  const Options: IParameters;
        Size: Integer): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, Comparer, ckVector, Size, Options);
end;

class function Vector.Create(
        TypeInfo: PTypeInfo;
  const Options: IParameters;
        Size: Integer): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, ckVector, nil, Size, Options);
end;

class function Vector.Create(
        Size: Integer;
  const Compare: ITypeComparator;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, ckVector, Compare, Options);
end;

class function Vector.Create(
        Size: Integer;
        Comparer: Pointer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, Comparer, ckVector, Options);
end;

class function Vector.Create(
        Size: Integer;
        Comparer: TTypeCompare;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, Comparer, ckVector, Options);
end;

class function Vector.Create(
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, ckVector, nil, Options);
end;

class function Vector.Strings(const Handler: ITypeHandler; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler, Options);
end;

class function Vector.Strings(TypeInfo: PTypeInfo; const Compare: ITypeComparator; Size: Integer; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(TypeInfo, Compare, Size), Options);
end;

class function Vector.Strings(TypeInfo: PTypeInfo; Comparer: Pointer; Size: Integer; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(TypeInfo, Compare.Create(Comparer), Size), Options);
end;

class function Vector.Strings(TypeInfo: PTypeInfo; Comparer: TTypeCompare; Size: Integer; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(TypeInfo, Compare.Create(Comparer), Size), Options);
end;

class function Vector.Strings(TypeInfo: PTypeInfo; const Options: IParameters; Size: Integer): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(TypeInfo, nil, Size), Options);
end;

class function Vector.Strings(Size: Integer; const Compare: ITypeComparator; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(Size, Compare), Options);
end;

class function Vector.Strings(Size: Integer; Comparer: Pointer; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(Size, Compare.Create(Comparer)), Options);
end;

class function Vector.Strings(Size: Integer; Comparer: TTypeCompare; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(Size, Compare.Create(Comparer)), Options);
end;

class function Vector.Strings(Size: Integer; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckVector, Handler.Create(Size), Options);
end;

class function Vector.InterfaceList(Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckVector.Kind, Locked);
end;

class function Vector.Interfaces(const Comparer: ITypeComparator; Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckVector.Kind, Locked, Comparer);
end;

class function Vector.Interfaces(Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckVector.Kind, Locked);
end;

class function Vector.PointerList(Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckVector.Kind, Locked);
end;

class function Vector.Pointers(const Comparer: ITypeComparator; Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckVector.Kind, Locked, Comparer);
end;

class function Vector.Pointers(Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckVector.Kind, Locked);
end;

{ List }

class procedure List.Create(
        Container: TSilContainerClass;
  const IID: TGUID;
    out Instance;
  const Handler: ITypeHandler;
  const Options: IParameters);
begin
  inherited Create(Container, IID, Instance, ckList, Handler, Options);
end;

class function List.Create(
  const Handler: ITypeHandler;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(ckList, Handler);
end;

class function List.Create(
        TypeInfo: PTypeInfo;
  const Compare: ITypeComparator;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, ckList, Compare, Size, Options);
end;

class function List.Create(
        TypeInfo: PTypeInfo;
        Comparer: Pointer;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, Comparer, ckList, Size, Options);
end;

class function List.Create(
        TypeInfo: PTypeInfo;
        Comparer: TTypeCompare;
        Size: Integer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(TypeInfo, Comparer, ckList, Size, Options);
end;

class function List.Create(
        Size: Integer;
  const Compare: ITypeComparator;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, ckList, Compare, Options);
end;

class function List.Create(
        Size: Integer;
        Comparer: Pointer;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, Comparer, ckList, Options);
end;

class function List.Create(
        Size: Integer;
        Comparer: TTypeCompare;
  const Options: IParameters): IContainerDynamic;
begin
  Result := inherited Create(Size, Comparer, ckList, Options);
end;

class function List.Strings(const Handler: ITypeHandler; const Options: IParameters): IStringsDynamic;
begin
  Result := inherited Strings(ckList, Handler, Options);
end;

class function List.InterfaceList(Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckList.Kind, Locked);
end;

class function List.Interfaces(const Comparer: ITypeComparator; Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckList.Kind, Locked, Comparer);
end;

class function List.Interfaces(Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceListContainer.Create(ckList.Kind, Locked);
end;

class function List.PointerList(Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckList.Kind, Locked);
end;

class function List.Pointers(const Comparer: ITypeComparator; Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckList.Kind, Locked, Comparer);
end;

class function List.Pointers(Locked: Boolean): IPointerList;
begin
  Result := TSilContainerPointers.Create(ckList.Kind, Locked);
end;

end.
