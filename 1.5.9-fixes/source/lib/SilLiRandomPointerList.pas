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

unit SilLiRandomPointerList;

{$I Defines.inc}

interface

uses
  SilLiPointerList;

type
  IRandomPointerList = interface( IPointerList )
    ['{82A053C1-8F33-11D4-B9C3-006008AE4EDF}']
    function GetFirstIndex: integer;
    function GetLastIndex: integer;
    function GetUsedCount: integer;
    function GetUsedItem(Index: integer): Pointer;
    function GetUsed(Index: integer; var Position: integer): Pointer; 
    procedure SetUsedItem(Index: integer; const Value: Pointer);
    function GetUsedIndex(Index: integer): integer;
    function FirstFree: Integer;
    property FirstIndex: integer read GetFirstIndex;
    property LastIndex: integer read GetLastIndex;
    property UsedCount: integer read GetUsedCount;
    property UsedItems[Index: integer]: Pointer read GetUsedItem write SetUsedItem;
    property UsedIndex[Index: integer]: integer read GetUsedIndex;
  end;

implementation

end.
