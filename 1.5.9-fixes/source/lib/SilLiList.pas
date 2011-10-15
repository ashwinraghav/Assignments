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

unit SilLiList;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLiLock,
  SilLiCompare,
  SilLiEnumerator;

type
(*)
  IItemHandler = interface;
  IItemList = interface;

  IListBase = interface(IItemization)
    ['{A434F137-B194-11D4-989F-00104B0FA1EF}']
    function GetCount: Integer;
    function GetHandler: IItemHandler;
    function Locked: ILock;
    procedure Sort(const Comparator: IComparator);
    property Handler: IItemHandler read GetHandler;
    property Count: Integer read GetCount;
  end;

  IList = interface(IListBase)
    ['{1BE5FCEE-67F3-4B47-AA0F-C6C7DA7A8208}']
    procedure SetCount(Value: Integer);
    procedure Clear;
    property Count: Integer read GetCount write SetCount;
  end;

  IItemGetter = interface
    ['{D54E9654-60A0-4F21-8288-0E9C62FEC271}']
    function GetPtr(Index: Integer): Pointer;
    property Ptr[Indx: Integer]: Pointer read GetPtr;
  end;

  IItemSetter = interface(IItemGetter)
    ['{A6BFA389-94AE-45C7-BE16-4DB04B98D5A1}']
    procedure SetPtr(Index: Integer; const Value: Pointer);
    property Ptr[Indx: Integer]: Pointer read GetPtr write SetPtr;
  end;

  IItems = interface
    ['{BE535A33-42B4-4862-ADC1-C622186B9811}']
    procedure Check(Index: Integer);
    function IsValid(Index: Integer): Boolean;
    function ItemFind(const Comparable: IComparable; const Item; var Index: Integer): Boolean;
    procedure ItemGet(Index: Integer; var Item);
  end;

  IItemList = interface(IItems)
    ['{F48E7268-0B93-4880-90C8-7161A5AD5B7E}']
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure ItemInsert(Index: Integer; const Item);
    function ItemRemove(const Item): Integer;
    function ItemAdd(const Item): Integer;
    procedure ItemSet(Index: Integer; const Item);
  end;
(*)

  IList = interface(IItemization)
    ['{A434F137-B194-11D4-989F-00104B0FA1EF}']
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function ItemPtr(Index: Integer): Pointer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Locked: ILock;
    function HasLockable: Boolean;
    procedure Exchange(Index1, Index2: Integer);
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    property Count: Integer read GetCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  IItems = interface(IList)
    ['{BE535A33-42B4-4862-ADC1-C622186B9811}']
    procedure ItemGet(Index: Integer; var Item);
  end;

  IListItems = interface(IItems)
    ['{F48E7268-0B93-4880-90C8-7161A5AD5B7E}']
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure ItemInsert(Index: Integer; const Item);
    function ItemRemove(const Item): Integer;
    function ItemAdd(const Item): Integer;
    procedure ItemSet(Index: Integer; const Item);
  end;

  IItemHandler = interface
    ['{B0D998E0-EBA4-4A41-BECC-3C8CE92C7C3E}']
    function GetDataType: HandlerType;
    function GetDataInfo: Pointer;
    property DataType: HandlerType read GetDataType;
    property DataInfo: Pointer read GetDataInfo;
  end;

  IListEvents = interface
    ['{0BE43A32-B3F9-11D4-989F-00104B0FA1EF}']
    procedure OnListAdd(const List: IList; Index: Integer);
    procedure OnListInsert(const List: IList; Index: Integer);
    procedure OnListDelete(const List: IList; Index: Integer);
  end;

implementation
end.
 