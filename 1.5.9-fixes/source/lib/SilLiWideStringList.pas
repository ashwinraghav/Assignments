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

unit SilLiWideStringList;

{$I Defines.inc}

interface

uses
  SilLiList;

type

{ IWideStringList }

  IWideStringList = interface (IList)
    ['{6C7D14C4-FE0E-11D3-913F-00C0261013CD}']
    function GetItem(Index: Integer): WideString;
    procedure SetItem(Index: Integer; const Value: WideString);
    function GetPtr(Index: Integer): Pointer;
    procedure SetPtr(Index: Integer; Value: Pointer);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(Value: Boolean);
    function Add(const Value: WideString; Ptr: Pointer = nil): Integer;
    procedure AddStrings(const Source: IWideStringList);
    function IndexOf(const Value: WideString): Integer;
    function IndexOfPtr(Value: Pointer): Integer;
    procedure Insert(Index: Integer; const Value: WideString; Ptr: Pointer = nil);
    function First: WideString;
    function Last: WideString;
    property Items[index: Integer]: WideString read GetItem write SetItem; default;
    property Ptrs[index: Integer]: Pointer read GetPtr write SetPtr;
    property Text: WideString read GetText write SetText;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
  end;

implementation

end.
 