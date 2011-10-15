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

unit SilLiEventList;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLiEnumerator,
  SilLiConnection;

type
  IEvents = interface (IConnectable)
    ['{50496090-84B8-41E4-91D7-DECAD12FC4EC}']
    function GetCount: Integer;
    function Locked: ILock;
    function Enumerate(var Enum: IEnumerator; out Item; const IID: TGuid): Boolean;
    property Count: Integer read GetCount;
  end;

type
  IEventList = interface (IEvents)
    ['{03FD6A6B-DF39-40A2-ACDD-A49FCC83EE24}']
    procedure AddList(const List: IEvents; KeepRef: Boolean = true);
    procedure Clear;
  end;

type
  IConnections = interface (IConnectable)
    ['{2AABE846-162C-4E31-8ABF-FBB9A9A2282D}']
    function GetHasConnections: Boolean;
    function GetEventList: IEventList;
    property HasConnections: Boolean read GetHasConnections;
    property Events: IEventList read GetEventList;
  end;
  
implementation
end.
 