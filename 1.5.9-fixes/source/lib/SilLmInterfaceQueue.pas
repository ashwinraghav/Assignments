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

unit SilLmInterfaceQueue;

{$I Defines.inc}

interface

uses
  SilLiInterfaceQueue,
  SilLkQueue;

type
  TInterfaceQueue = class(
    TSilQueue,
    IInterfaceQueue)
  protected
    function Put(const Item: IUnknown): Boolean; reintroduce;
    function Get(const IID: TGUID; out Obj; TimeOut: LongWord; AvoidMsgLock: Boolean): Boolean; reintroduce;
  public
    constructor Create(const InitialCount: Integer; const MaxCount: Integer; const Name: PChar);
  end;

implementation

uses
  SilLtReference,
  SilBtInterfacePtr;

{ TInterfaceQueue }

constructor TInterfaceQueue.Create(const InitialCount: Integer; const MaxCount: Integer; const Name: PChar);
begin
  inherited Create(InitialCount, MaxCount, InterfaceHandler);
end;

function TInterfaceQueue.Get(const IID: TGUID; out Obj; TimeOut: LongWord; AvoidMsgLock: Boolean): Boolean;
var
  Item: IUnknown;
begin
  Result := inherited Get(Item, Timeout, AvoidMsgLock);
  if Result then Reference.GetInterface(Item, IID, Obj);
end;

function TInterfaceQueue.Put(const Item: IUnknown): Boolean;
begin
  Result := inherited Put(Item);
end;

end.
