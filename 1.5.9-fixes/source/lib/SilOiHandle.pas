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

unit SilOiHandle;

interface

{$I Defines.inc}

uses
  SilOeTypes,
  SilLiLock;

type
  IHandle = interface
    ['{8FE2B641-0B31-11D4-987A-00104B0FA1EF}']
    function GetIsValid: Boolean;
    function GetValue: THandle;
    function GetIsOwned: Boolean;
    procedure Close;
    property IsValid: Boolean read GetIsValid;
    property Value: THandle read GetValue;
    property IsOwned: Boolean read GetIsOwned;
  end;

  IHandleList = interface
    ['{E9A20461-0BCE-11D4-987A-00104B0FA1EF}']
    function GetCount: Integer;
    function First: IHandle;
    function Last: IHandle;
    function GetItem(Index: Integer): IHandle;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IHandle read GetItem; default;
  end;

  IHandledObject = interface
    ['{7B1A3537-D6DF-4F29-BAD4-975D3D849430}']
    function GetHandle: IHandle;
    property Handle: IHandle read GetHandle;
  end;

  IHandled = interface(IHandledObject)
    ['{FD203306-0B70-11D4-9155-00C0261013CD}']
  end;

implementation
end.
