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

unit SilLiPointerList;

{$I Defines.inc}

interface

uses
  SilLiList;

type
  TFreeMethod = (fmNone, fmFreeMem, fmDispose, fmFreeObject, fmRelease); // obsoleto

  IPointerList = interface (IList)
    ['{4B22A481-EBA6-11D3-9870-00104B0FA1EF}']
    function Add(Item: Pointer): Integer;
    procedure AddList(const Source: IPointerList);
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Remove(Item: Pointer): Integer;
    function First: Pointer;
    function Last: Pointer;
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; Value: Pointer);
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

implementation

end.
 
