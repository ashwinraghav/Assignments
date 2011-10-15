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

unit SilLiStringList;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLiList;

type
  PStringItem = ^RStringItem;

  RStringItem = record
    Buffer: String;
    Ptr: Pointer;
  end;

{ IStrings }

  IStrings = interface (IList)
    ['{4AD6F6B5-79BF-4264-815E-1314B1F20C88}']
    function GetItem(Index: Integer): String;
    function GetPtr(Index: Integer): Pointer;
    function DoGetText: String;
    function GetIgnoreCase: Boolean;
    function IndexOf(const Value: String): Integer;
    function IndexOfMask(const Value: String): Integer;
    function IndexOfPtr(Value: Pointer): Integer;
    function First: String;
    function Last: String;
    function Enumerate(var Enum: IEnumerator; out Item: string): Boolean; overload;
    property Items[index: Integer]: String read GetItem; default;
    property Ptrs[index: Integer]: Pointer read GetPtr;
    property Text: String read DoGetText;
    property IgnoreCase: Boolean read GetIgnoreCase;
  end;

{ IStringList }

  IStringList = interface (IStrings)
    ['{6C7D14C4-FE0E-11D3-913F-00C0261013CD}']
    procedure SetItem(Index: Integer; const Value: String);
    procedure SetPtr(Index: Integer; Value: Pointer);
    procedure DoSetText(const Value: String);
    procedure SetIgnoreCase(Value: Boolean);
    function Add(const Value: String; Ptr: Pointer = nil): Integer;
    procedure AddStrings(const Source: IStringList);
    function AddText(const Value: String; const Separator: string = ''): Integer;
    function GetText(const Separator: string = ''): string;
    procedure Insert(Index: Integer; const Value: String; Ptr: Pointer = nil);
    function Remove(const Value: String; All: Boolean = false): Integer;
    property Items[index: Integer]: String read GetItem write SetItem; default;
    property Ptrs[index: Integer]: Pointer read GetPtr write SetPtr;
    property Text: String read DoGetText write DoSetText;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
  end;

implementation
end.
 