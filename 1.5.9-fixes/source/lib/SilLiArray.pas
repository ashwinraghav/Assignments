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

unit SilLiArray;

{$I Defines.inc}

interface

uses
  SilLiCompare,
  SilLiContainer;

type

// usings

  IPointer = SilLiContainer.IPointer;

// interfases

  IArrayEvents = interface(IContainerEvents)
    ['{350DAC11-F7A2-11D3-9CA8-00C0DFE46337}']
    function OnCalcDelta(ACount: Integer): Integer;
  end;

  IArray = interface(IContainer)
    ['{350DAC17-F7A2-11D3-9CA8-00C0DFE46337}']
    //- methods
    function Get(AIndex: Integer): IPointer;
    procedure Put(AIndex: Integer; const Item);
    procedure Pack(const Comparator: IComparator; const Item);
    function Insert(const Item; Index: Integer = -1): IPointer; overload;
    procedure Delete(FromIndex, ToIndex: Integer);
    //- property accessors
    function GetCount: Integer;
    procedure SetCount(NewCount: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function GetBuffer: Pointer;
    //- properties
    property Count: Integer read GetCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Buffer: Pointer read GetBuffer;
    property Items[AIndex: Integer]: IPointer read Get; default;
  end;

implementation

end.

