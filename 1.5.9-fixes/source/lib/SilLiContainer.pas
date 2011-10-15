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

unit SilLiContainer;

{$INCLUDE Defines.inc}

interface

uses
  SilLiLock,
  SilLiContainerTypes;

type
  IContainerTypes = interface;
  IContainerItemsStatic = interface;
  IContainerItemsDynamic = interface;
  IContainerCursors = interface;
  IContainerPointers = interface;
  IContainerPointersStatic = interface;
  IContainerPointersDynamic = interface;
  IContainerCursor = interface;
  IContainerCursorStatic = interface;
  IContainerCursorDynamic = interface;
  IContainerPointerStatic = interface;
  IContainerPointerDynamic = interface;

  IContainerTypes = interface
    ['{94D07808-0A91-4354-9851-DACF90AC76EA}']
    function GetISynchronizable: ISynchronizable;
    function GetHandler: ITypeHandler;
    function GetAllocator: ITypeAllocator;
    property Synchronized: ISynchronizable read GetISynchronizable;
    property Handler: ITypeHandler read GetHandler;
    property Allocator: ITypeAllocator read GetAllocator;
  end;

  IContainerItemsStatic = interface
    ['{A5DE3BD8-9804-42FB-9E1F-938386471A9F}']
    function GetCount: Integer;
    function GetFirstItem: HItem;
    function GetLastItem: HItem;
    function IsValid(const Item: HItem): Boolean;
    procedure Check(const Item: HItem);
    property Count: Integer read GetCount;
    property First: HItem read GetFirstItem;
    property Last: HItem read GetLastItem;
  end;

  IContainerItemsDynamic = interface (IContainerItemsStatic)
    ['{EC8B45D4-E958-453C-B1E8-3627FB17D69E}']
    procedure Clear;
    procedure SetCount(Value: Integer);
    procedure Delete(Item: HItem); overload;
    procedure Delete(const Cursor: IContainerCursor); overload;
    procedure Delete(First, Last: HItem); overload;
    procedure Delete(const First, Last: IContainerCursor); overload;
    procedure Exchange(Item1, Item2: HItem); overload;
    procedure Exchange(const Item1, Item2: IContainerCursor); overload;
    procedure Move(Current, Target: HItem); overload;
    procedure Move(const Current, Target: IContainerCursor); overload;
    property Count: Integer read GetCount write SetCount;
  end;

  IContainerCursors = interface
    ['{A9A12896-EC88-4EA7-B512-AE8A956C7355}']
    function Create(const Item: HItem; const Kind: TGUID; out Value; Delta: Integer = 1): Boolean; overload;
  end;

  IContainerPointers = interface (IContainerCursors)
    ['{9B32354F-5875-4973-BA15-AA5C4C8D7C64}']
    function Find(Data: HData; const Kind: TGUID; out Value; const Comparator: ITypeComparator = nil; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
  end;

  IContainerPointersStatic = interface (IContainerPointers)
    ['{977D71EE-7495-4867-BFC4-599087859787}']
    function GetFirstStaticCursor: IContainerPointerStatic;
    function GetLastStaticCursor: IContainerPointerStatic;
    function CursorGet(const Cursor: IContainerCursor): HData;
    function Get(const Item: HItem; Delta: Integer = 1): IContainerPointerStatic; overload;
    property First: IContainerPointerStatic read GetFirstStaticCursor;
    property Last: IContainerPointerStatic read GetLastStaticCursor;
    property Value[const Cursor: IContainerCursor]: HData read CursorGet; default;
  end;

  IContainerPointersDynamic = interface (IContainerPointers)
    ['{977D71EE-7495-4867-BFC4-599087859787}']
    function GetFirstDynamicCursor: IContainerPointerDynamic;
    function GetLastDynamicCursor: IContainerPointerDynamic;
    function CursorGet(const Cursor: IContainerCursor): HData;
    function Get(const Item: HItem; Delta: Integer = 1): IContainerPointerDynamic; overload;
    procedure CursorPut(const Cursor: IContainerCursor; Source: HData);
    property First: IContainerPointerDynamic read GetFirstDynamicCursor;
    property Last: IContainerPointerDynamic read GetLastDynamicCursor;
    property Value[const Cursor: IContainerCursor]: HData read CursorGet write CursorPut; default;
  end;

  IContainerLookupStatic = interface
    ['{2C86E9E7-E015-4583-9B60-5374C32DDC35}']

  end;

  IContainerLookupDynamic = interface (IContainerLookupStatic)
    ['{EE03B72B-06C9-4F80-B307-B96D3446D87B}']

  end;

  IContainerStatic = interface (IContainerTypes)
    ['{951209CA-A142-40E4-B911-7D9637010080}']
    function GetItemsStatic: IContainerItemsStatic;
    function GetCursorsStatic: IContainerPointersStatic;
    function HandleGet(Item: HItem): HData;
    function Cursor(Data: HData; Param: Pointer = nil; Delta: Integer = 1): IContainerPointerStatic;
    function Handle(Data: HData; Param: Pointer = nil): HItem;
    function Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean; overload; 
    function Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean; overload;
    property Items: IContainerItemsStatic read GetItemsStatic;
    property Cursors: IContainerPointersStatic read GetCursorsStatic;
    property Value[Item: HItem]: HData read HandleGet; default;
  end;

  IContainerDynamic = interface (IContainerStatic)
    ['{7C684288-7301-4D6C-A2CF-CA167A0D756B}']
    function GetItemsDynamic: IContainerItemsDynamic;
    function GetCursorsDynamic: IContainerPointersDynamic;
    function Add(Source: HData): HItem; overload;
    function Add(Count: Integer; Source: HData): HItem; overload;
    function Add(Count: Integer = 1): HItem; overload;
    function Add(out Data: HData; Count: Integer = 1; Source: HData = nil): HItem; overload;
    function Insert(Item: HItem; Source: HData; Count: Integer = 1): HItem; overload;
    function Insert(out Data: HData; Item: HItem; Count: Integer = 1; Source: HData = nil): HItem; overload;
    procedure HandlePut(Item: HItem; Source: HData);
    function Remove(Data: HData; Param: Pointer = nil): HItem; overload;
    property Items: IContainerItemsDynamic read GetItemsDynamic;
    property Cursors: IContainerPointersDynamic read GetCursorsDynamic;
    property Value[Item: HItem]: HData read HandleGet write HandlePut; default;
  end;

  IContainerCursor = interface
    ['{350DAC15-F7A2-11D3-9CA8-00C0DFE46337}']
    function GetIsValid: Boolean;
    function GetItem: HItem;
    procedure SetItem(Value: HItem);
    function GetCursor: IContainerCursor;
    function GetThis: IContainerCursor;
    function IsEqual(const Other: IContainerCursor): Boolean;
    function Next(Delta: Integer = 1): Boolean;
    function Clone(Step: Integer = 1): IContainerCursor; overload;
    function Clone(const Kind: TGUID; out Obj; Step: Integer = 1): Boolean; overload;
    property IsValid: Boolean read GetIsValid;
    property Item: HItem read GetItem write SetItem;
    property Cursor: IContainerCursor read GetCursor;
    property This: IContainerCursor read GetThis;
  end;

  IContainerCursorStatic = interface (IContainerCursor)
    ['{64AAFFE8-AE1A-43DA-A1CE-0E13C59B9469}']
    function GetOwnerStatic: IContainerStatic;
    function GetThisStatic: IContainerCursorStatic;
    property Owner: IContainerStatic read GetOwnerStatic;
    property This: IContainerCursorStatic read GetThisStatic;
  end;

  IContainerCursorDynamic = interface (IContainerCursorStatic)
    ['{4B948076-5B1B-4D4B-98A3-CC96EB39CBCC}']
    function GetOwnerDynamic: IContainerDynamic;
    function GetThisDynamic: IContainerCursorDynamic;
    property Owner: IContainerDynamic read GetOwnerDynamic;
    property This: IContainerCursorDynamic read GetThisDynamic;
  end;

  IContainerPointerStatic = interface (IContainerCursor)
    ['{A2D6CCA8-F7A0-4225-A82E-6661FC78FE68}']
    function GetOwnerStatic: IContainerStatic;
    function GetCursorStatic: IContainerCursorStatic;
    function GetThisStaticData: IContainerPointerStatic;
    function GetData: HData;
    function Read(out Data): Boolean; 
    function Get(out Data: HData): Boolean;
    property Owner: IContainerStatic read GetOwnerStatic;
    property Cursor: IContainerCursorStatic read GetCursorStatic;
    property This: IContainerPointerStatic read GetThisStaticData;
    property Data: HData read GetData;
  end;

  IContainerPointerDynamic = interface (IContainerPointerStatic)
    ['{B41E75D5-EF18-4EE5-8544-126DE7B90427}']
    function GetOwnerDynamic: IContainerDynamic;
    function GetCursorDynamic: IContainerCursorDynamic;
    function GetThisDynamicData: IContainerPointerDynamic;
    procedure SetData(const Data: HData);
    function Write(const Data): Boolean;
    function Put(const Data: HData): Boolean;
    function Insert(Data: HData): Boolean;
    property Owner: IContainerDynamic read GetOwnerDynamic;
    property Cursor: IContainerCursorDynamic read GetCursorDynamic;
    property This: IContainerPointerDynamic read GetThisDynamicData;
    property Data: HData read GetData write SetData;
  end;

  IContainerCursorOrdered = interface (IContainerCursor)
    ['{786BCA47-44C6-407B-ADEE-574DA2C9363B}']
    function GetThisOrdered: IContainerCursorOrdered;
    function GetPosition: Integer;
    procedure SetPosition(Value: Integer);
    function Compare(const Cursor: IContainerCursorOrdered): Integer;
    property This: IContainerCursorOrdered read GetThisOrdered;
    property Position: Integer read GetPosition write SetPosition;
  end;

  IContainerCursorOrderedStatic = interface (IContainerCursorOrdered)
    ['{5635627A-F7C0-4A99-9BF5-642356808199}']
    function GetOwnerStatic: IContainerStatic;
    function GetCursorStatic: IContainerCursorStatic;
    function GetThisStaticOrdered: IContainerCursorOrderedStatic;
    property Owner: IContainerStatic read GetOwnerStatic;
    property Cursor: IContainerCursorStatic read GetCursorStatic;
    property This: IContainerCursorOrderedStatic read GetThisStaticOrdered;
  end;

  IContainerCursorOrderedDynamic = interface (IContainerCursorOrderedStatic)
    ['{F630D797-8ED9-4C42-BC9A-758F776A3363}']
    function GetOwnerDynamic: IContainerDynamic;
    function GetCursorDynamic: IContainerCursorDynamic;
    function GetThisDynamicOrdered: IContainerCursorOrderedDynamic;
    property Owner: IContainerDynamic read GetOwnerDynamic;
    property Cursor: IContainerCursorDynamic read GetCursorDynamic;
    property This: IContainerCursorOrderedDynamic read GetThisDynamicOrdered;
  end;

  IContainerSequence = interface
    ['{4586A847-7BF8-4EDA-9920-221418F4E9F9}']
    function GetIteration: Integer;
    function Get(var Cursor): Boolean; overload;
    procedure Detach;
    property Iteration: Integer read GetIteration;
  end;

  IContainerSequenceStatic = interface (IContainerSequence)
    ['{20489B7D-F70A-48FA-980C-474A1EC869CD}']
    function GetOwnerStatic: IContainerStatic;
    property Owner: IContainerStatic read GetOwnerStatic;
  end;

  IContainerSequenceDynamic = interface (IContainerSequenceStatic)
    ['{0A8FA18B-C230-4F5D-BBC4-968B3E17E357}']
    function GetOwnerDynamic: IContainerDynamic;
    property Owner: IContainerDynamic read GetOwnerDynamic;
  end;

  IVectorStatic = interface (IContainerStatic)
    ['{FE5E5016-1ED9-4379-8C96-09BFA4D0E1F3}']
    function GetCapacity: Integer;
    property Capacity: Integer read GetCapacity;
    property Data[Item: HItem]: HData read HandleGet; default;
  end;

  IVectorDynamic = interface (IContainerDynamic)
    ['{C6EF91B2-02F6-45B4-85B1-1263BCF409A3}']
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Data[Item: HItem]: HData read HandleGet write HandlePut; default;
  end;

implementation
end.
