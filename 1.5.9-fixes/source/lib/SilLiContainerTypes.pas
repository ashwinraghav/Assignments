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

unit SilLiContainerTypes;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBeTypeInfo,
  SilBeError,
  SilLiParameters,
  SilLkObject,
  SilLkAggregable;

type
  HItem = type Integer;
  HData = type Pointer;

type
  PItem = ^HItem;   
  PData = ^HData;
    
const
  HNull = HItem(-1);

type
  TListDuplicates = (
      duIgnore,
      duAccept,
      duError
    );

type
  ESilListError = class(ESilException);

type
  RSortOptions = record
    Sorted: Boolean;
    Duplicates: TListDuplicates;
  end;

type
  TTypeCompare = function (Data1, Data2: HData; Param: Pointer): Integer of object;

  ITypeComparator = interface
    ['{A92DF7FE-83D4-4191-A388-A5BCB9D2081B}']
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer;
  end;

  ITypeHandler = interface (ITypeComparator)
    ['{6BB0C982-A761-47D6-BEEA-7F6295AC3C5B}']
    function GetSize: Integer;
    procedure Initialize(Data: HData; Count: Integer = 1);
    procedure Finalize(Data: HData; Count: Integer = 1);
    procedure Copy(Data: HData; Source: HData; Count: Integer = 1; Step: Integer = 1);
    property Size: Integer read GetSize;
  end;

  ITypeAllocator = interface
    ['{6A91F22C-3457-4C73-85FE-19AEACD2B9CE}']
    function GetHandler: ITypeHandler;
    function Get(Count: Integer = 1): HData;
    procedure Release(var Data: HData; Count: Integer = 1);
    property Handler: ITypeHandler read GetHandler;
  end;

  ITypeinfoHandler = interface(ITypeHandler)
    ['{55CEC0C6-4F30-4F38-9741-5F1FC753CAC3}']
    function GetTypeInfo: PTypeInfo;
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  TTypeHandlerList = array of ITypeHandler;

type
  TSilBaseContainerClass = class of TSilBaseContainerType;
  TSilBaseContainerType = class(TSilAggregableObject)
  public 
    constructor Create(
        const Handler: ITypeHandler;
        const Options: IParameters = nil;
        const Controller: IUnknown = nil); reintroduce; overload; virtual; abstract; 
  end;

type
  TSilCursorClass = class of TSilCursorType;
  TSilCursorType = class(TSilObject)
  public
    constructor Create(List: Pointer; Item: HItem = HNull; Delta: Integer = 1); overload; virtual; abstract;
  end;

type
  TSilContainerClass = class of TSilContainerType;
  TSilContainerType = class(TSilAggregableObject)
    constructor Create(
              Container: TSilBaseContainerClass;
        const Handler: ITypeHandler;
        const Options: IParameters = nil;
        const Owner: IUnknown = nil;
        const Controller: IUnknown = nil;
              Param: Pointer = nil); reintroduce; overload; virtual; abstract;
  end;

implementation
end.
