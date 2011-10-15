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

unit SilLiInterfaceList;

{$I Defines.inc}

interface

uses
  SilLiList;

type

{ IInterfaceList }

  IInterfaceList = interface (IList)
    ['{4B22A481-EBA6-11D3-9870-00104B0FA1EF}']
    function Add(const Item: IUnknown): Integer;
    procedure AddList(const Source: IInterfaceList);
    function IndexOf(const Item: IUnknown): Integer;
    procedure Insert(Index: Integer; const Item: IUnknown);
    function Remove(const Item: IUnknown): Integer;
    function First: IUnknown;
    function Last: IUnknown;
    function GetItem(Index: Integer): IUnknown;
    procedure SetItem(Index: Integer; const Value: IUnknown);
    property Items[Index: Integer]: IUnknown read GetItem write SetItem; default;
  end;

implementation

end.
 