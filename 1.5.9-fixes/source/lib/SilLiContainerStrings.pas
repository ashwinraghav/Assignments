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

unit SilLiContainerStrings;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilLiContainer,
  SilLiContainerTypes;

type
  IStringsStatic = interface;
  IStringsDynamic = interface;
  IStringCursorsStatic = interface;
  IStringCursorsDynamic = interface;
  IStringPointerStatic = interface;
  IStringPointerDynamic = interface;
  
  PStringData = ^RStringData;
  RStringData = packed record
    Text: string;
    Data: record end;
  end;

  IStringsStatic = interface
    ['{C2529BE9-C0A1-4D67-97EF-9B6F4E613B9B}']
    function GetItemsStatic: IContainerItemsStatic;
    function GetBaseStatic: IContainerStatic;
    function GetCursorsStatic: IStringCursorsStatic;
    function HandleGet(Item: HItem): PStringData;
    function CursorGet(const Cursor: IContainerCursor): PStringData;
    function Cursor(Data: HData; Param: Pointer = nil; Delta: Integer = 1): IStringPointerStatic; overload;
    function GetCursorItem(const Cursor: IContainerCursor): string;
    function GetCursorData(const Cursor: IContainerCursor): HData;
    function GetHandleItem(Item: HItem): string;
    function GetHandleData(Item: HItem): HData;
    function Handle(const Value: string; Param: Pointer = nil): HItem; overload;
    function Handle(Data: HData; Param: Pointer = nil): HItem; overload;
    function Find(const Text: string; out Item: HItem; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Item: HItem; const Compare: TTypeCompare; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer = nil): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; const Compare: TTypeCompare; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(const Text: string; out Value: IStringPointerStatic; const Comparator: ITypeComparator; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Item: HItem; Param: Pointer = nil): Boolean; overload;
    function Find(Data: HData; out Item: HItem; const Compare: TTypeCompare; Param: Pointer = nil): Boolean; overload;
    function Find(Data: HData; out Item: HItem; const Comparator: ITypeComparator; Param: Pointer = nil): Boolean; overload;
    function Find(Data: HData; out Value: IStringPointerStatic; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Value: IContainerCursorStatic; const Compare: TTypeCompare; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    function Find(Data: HData; out Value: IContainerCursorStatic; const Comparator: ITypeComparator; Param: Pointer = nil; Delta: Integer = 1): Boolean; overload;
    property Items: IContainerItemsStatic read GetItemsStatic;
    property Base: IContainerStatic read GetBaseStatic;
    property Cursors: IStringCursorsStatic read GetCursorsStatic;
    property Value[Item: HItem]: PStringData read HandleGet; default;
    property Item[Item: HItem]: string read GetHandleItem;
    property Ptrs[Item: HItem]: HData read GetHandleData;
    property Text[const Cursor: IContainerCursor]: string read GetCursorItem;
    property Data[const Cursor: IContainerCursor]: HData read GetCursorData;
  end;

  IStringsDynamic = interface (IStringsStatic)
    ['{B8570DA1-D6E2-498A-806A-3C26212DD973}']
    function GetItemsDynamic: IContainerItemsDynamic;
    function GetBaseDynamic: IContainerDynamic;
    function GetCursorsDynamic: IStringCursorsDynamic;
    function Add(const Text: string; Source: HData = nil): HItem;
    function Insert(const Text: string; Source: HData = nil; Item: HItem = HNull): HItem;
    procedure PutCursorItem(const Cursor: IContainerCursor; const Text: string);
    procedure PutCursorData(const Cursor: IContainerCursor; Source: HData);
    procedure PutHandleItem(Item: HItem; const Text: string);
    procedure PutHandleData(Item: HItem; Source: HData);
    procedure Put(Item: HItem; const Text: string; Source: HData);
    procedure HandlePut(Item: HItem; Value: PStringData);
    function Remove(const Text: string; Param: Pointer = nil): HItem; overload;
    property Items: IContainerItemsDynamic read GetItemsDynamic;
    property Cursors: IStringCursorsDynamic read GetCursorsDynamic;
    property Base: IContainerDynamic read GetBaseDynamic;
    property Item[Item: HItem]: PStringData read HandleGet write HandlePut;
    property Value[Item: HItem]: string read GetHandleItem write PutHandleItem; default;
    property Ptrs[Item: HItem]: HData read GetHandleData write PutHandleData;
    property Text[const Cursor: IContainerCursor]: string read GetCursorItem write PutCursorItem;
    property Data[const Cursor: IContainerCursor]: HData read GetCursorData write PutCursorData;
  end;

  IStringCursorsStatic = interface (IContainerCursors)
    ['{F4DB31CA-7AFC-4499-A308-010FA19478C1}']
    function GetFirstCursorStatic: IStringPointerStatic;
    function GetLastCursorStatic: IStringPointerStatic;
    function Cursor(const Value: string; Param: Pointer = nil; Delta: Integer = 1): IStringPointerStatic; overload;
    property First: IStringPointerStatic read GetFirstCursorStatic;
    property Last: IStringPointerStatic read GetLastCursorStatic;
  end;

  IStringCursorsDynamic = interface (IContainerCursors)
    ['{CFDC53E8-390E-4814-A289-08AF4BCA1F86}']
    function GetFirstCursorDynamic: IStringPointerDynamic;
    function GetLastCursorDynamic: IStringPointerDynamic;
    function Cursor(const Value: string; Param: Pointer = nil; Delta: Integer = 1): IStringPointerDynamic; overload;
    property First: IStringPointerDynamic read GetFirstCursorDynamic;
    property Last: IStringPointerDynamic read GetLastCursorDynamic;
  end;

  IStringPointerStatic = interface (IContainerCursor)
    ['{BCB145B0-10BF-4C0E-9F76-7C5F5B309C0B}']
    function GetOwnerStatic: IStringsStatic;
    function GetCursorStatic: IContainerCursorStatic;
    function GetThisStaticString: IStringPointerStatic;
    function GetText: string;
    function GetData: HData;
    function Read(out Data): Boolean;
    function Get(out Data: PStringData): Boolean; 
    property Owner: IStringsStatic read GetOwnerStatic;
    property Cursor: IContainerCursorStatic read GetCursorStatic;
    property This: IStringPointerStatic read GetThisStaticString;
    property Text: string read GetText;
    property Data: HData read GetData;
  end;

  IStringPointerDynamic = interface (IStringPointerStatic)
    ['{B41E75D5-EF18-4EE5-8544-126DE7B90427}']
    function GetOwnerDynamic: IStringsDynamic;
    function GetCursorDynamic: IContainerCursorDynamic;
    function GetThisDynamicString: IStringPointerDynamic;
    procedure SetText(const Text: string);
    procedure SetData(Data: HData);
    function Write(const Data): Boolean;
    function Put(const Data: PStringData): Boolean; overload;
    function Put(const Text: string; Source: HData = nil): Boolean; overload;
    function Insert(const Data: PStringData): Boolean; overload;
    function Insert(const Text: string; Source: HData = nil): Boolean; overload;
    property Owner: IStringsDynamic read GetOwnerDynamic;
    property Cursor: IContainerCursorDynamic read GetCursorDynamic;
    property This: IStringPointerDynamic read GetThisDynamicString;
    property Text: string read GetText write SetText;
    property Data: HData read GetData write SetData;
  end;

implementation
end.
