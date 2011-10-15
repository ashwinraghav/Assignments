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

unit SilVectorSiVector;

interface

{$include Sil.inc}

type
  IVectorComparator = interface;
  IVectorComparable = interface;

  IVectorEnumerator = interface;
  IVectorPointerEnumerator = interface;
  IVectorStringEnumerator = interface;
  IVector = interface;
  IVectorPointer = interface;
  IVectorString = interface;
  IVectorInterface = interface;

  IVectorComparator = interface
    ['{78CF7D9E-8AEA-4016-B3A7-36BEC5320E9D}']
    function CompareItems(const Vector: IVector; index1, index2: integer): integer;
  end;

  IVectorComparable = interface
    ['{1F9B0FE9-EAD0-45B9-B60D-4A76D5284727}']
    function CompareValue(const Vector: IVector; index: integer; const value): integer;
  end;

  IVectorEnumerator = interface
    ['{E3033E90-9969-4DFB-B09B-951B9CDA817C}']
    procedure Reset;
    function GetIndex: integer;
    procedure Purge;
    property Index: integer read GetIndex;
  end;

  IVectorPointerEnumerator = interface (IVectorEnumerator)
    ['{D4A89FE7-4292-4CFA-99AE-9A6B256FE8C9}']
    function Enumerate(out Value: pointer): boolean;
  end;

  IVectorStringEnumerator = interface (IVectorEnumerator)
    ['{D4A89FE7-4292-4CFA-99AE-9A6B256FE8C9}']
    function Enumerate(out Value: string): boolean;
  end;

  IVectorInterfaceEnumerator = interface (IVectorEnumerator)
    ['{D4A89FE7-4292-4CFA-99AE-9A6B256FE8C9}']
    function Enumerate(out Value): boolean;
  end;

  IVector = interface
    ['{C675B69F-BF4A-4C8B-B993-70DD585BF3E4}']
    function GetCount: integer;
    procedure SetCount(Value: integer);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Exchange(Index1, Index2: integer);
    procedure Sort(const Comparator: IVectorComparator = nil);
    function Search(const Value; const Comparable: IVectorComparable = nil): integer;
    procedure Lock;
    procedure Unlock;
    property Count: integer read GetCount write SetCount;
  end;

  IVectorPointer = interface (IVector)
    ['{15CDF9D3-BC6B-4441-83B0-C0D5804A6122}']
    function GetItem(Index: integer): pointer;
    procedure SetItem(Index: integer; Value: pointer);
    function Add(Value: pointer): integer;
    function Remove(Value: pointer): integer;
    procedure Insert(Index: integer; Value: pointer);
    function IndexOf(Value: pointer): integer;
    function First: pointer;
    function Last: pointer;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorPointerEnumerator;
    property Items[Index: integer]: pointer read GetItem write SetItem; default;
  end;

  IVectorInterface = interface (IVector)
    ['{15CDF9D3-BC6B-4441-83B0-C0D5804A6122}']
    function GetItem(Index: integer): IInterface;
    procedure SetItem(Index: integer; const Value: IInterface);
    function Add(const Value: IInterface): integer;
    function Remove(const Value: IInterface): integer;
    procedure Insert(Index: integer; const Value: IInterface);
    function IndexOf(const Value: IInterface): integer;
    function First: IInterface;
    function Last: IInterface;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorInterfaceEnumerator;
    property Items[Index: integer]: IInterface read GetItem write SetItem; default;
  end;

  IVectorString = interface (IVector)
    ['{15CDF9D3-BC6B-4441-83B0-C0D5804A6122}']
    function GetIgnoreCase: boolean;
    procedure SetIgnoreCase(Value: boolean);
    function GetItem(Index: integer): string;
    procedure SetItem(Index: integer; const Value: string);
    function GetPtr(Index: integer): pointer;
    procedure SetPtr(Index: integer; Value: pointer);
    function Add(const Value: string; Ptr: pointer = nil): integer;
    function Remove(const Value: string): integer;
    procedure Insert(Index: integer; const Value: string; Ptr: pointer = nil);
    function IndexOf(const Value: string): integer;
    function IndexOfPtr(Value: pointer): integer;
    function First: string;
    function Last: string;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorStringEnumerator;
    property Items[Index: integer]: string read GetItem write SetItem; default;
    property Ptrs[Index: integer]: pointer read GetPtr write SetPtr;
    property IgnoreCase: boolean read GetIgnoreCase write SetIgnoreCase;
  end;

implementation

end.
