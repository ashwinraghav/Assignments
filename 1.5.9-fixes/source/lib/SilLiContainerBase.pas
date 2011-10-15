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

unit SilLiContainerBase;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLiLock,
  SilLiParameters,
  SilLiContainerTypes;

type
  IBaseContainer = interface
    ['{6DBA7ADC-0102-4A65-AE6B-01A437DB3ECF}']
    function GetILockingSet: ILockingSet; 
    function GetHandler: ITypeHandler;
    function GetAllocator: ITypeAllocator;
    function GetScratch: HData;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetFirstHandle: HItem;
    function GetLastHandle: HItem;
    function GetData(const Item: HItem): HData;
    function IsValid(const Item: HItem): Boolean;
    procedure Clear;
    function Add(Source: HData; Count: Integer = 1; Return: PData = nil): HItem;
    function Insert(Source: HData; Count: Integer = 1; Item: HItem = 0; Return: PData = nil): HItem;
    procedure Delete(First, Last: HItem);
    procedure Exchange(Item1, Item2: HItem);
    procedure Move(Current, Target: HItem);
    function Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean;
    function GetNext(var Item: HItem; Delta: Integer = 1): Boolean;
    property Locking: ILockingSet read GetILockingSet; 
    property Handler: ITypeHandler read GetHandler;
    property Allocator: ITypeAllocator read GetAllocator;
    property Scratch: HData read GetScratch;
    property Count: Integer read GetCount write SetCount;
    property First: HItem read GetFirstHandle;
    property Last: HItem read GetLastHandle;
  end;

implementation
end.
 
