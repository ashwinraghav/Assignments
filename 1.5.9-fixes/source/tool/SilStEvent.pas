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

unit SilStEvent;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilLiInterfaceList,
  SilLiEventList,
  SilBkTool;

type
  EventsClass = class of EventsTool;
  EventsTool = class(Tool)
    class procedure Add(var Events: IInterfaceList; const Adapter: IUnknown; const IID: TGUID); overload;
    class procedure Remove(const Events: IInterfaceList; const Adapter: IUnknown; const IID: TGUID); overload;
    class procedure Add(var Events: IEventList; const Listener: IUnknown; KeepRef: Boolean = True); overload;
    class procedure Remove(const Events: IEventList; const Listener: IUnknown); overload;
    class procedure Execute(const Events: IInterfaceList; Method: TMethod; const Event);
    class procedure Clear(var Events: IEventList);
  end;

implementation

uses
  SilLtTool,
  SilLtList,
  SilLtReference,
  SilLiEnumerator;

{ Listener }

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class procedure EventsTool.Add(var Events: IEventList; const Listener: IUnknown; KeepRef: Boolean);
begin
  if not Assigned(Events) then Events := ListTool.EventList;
  Events.AddListener(Listener, KeepRef);
end;

class procedure EventsTool.Remove(const Events: IEventList; const Listener: IUnknown);
begin
  if Assigned(Events) then Events.RemoveListener(Listener);
end;

class procedure EventsTool.Add(var Events: IInterfaceList; const Adapter: IUnknown; const IID: TGUID);
var
  Listener: IUnknown;
begin
  if not Reference.GetInterface(Adapter, IID, Listener) then Exit;
  if Events = nil then Events := ListTool.InterfaceList(true);

  Events.Locked;
  if Events.IndexOf(Listener) = -1 then Events.Add(Listener);
end;

class procedure EventsTool.Remove(const Events: IInterfaceList; const Adapter: IUnknown; const IID: TGUID);
var
  Listener: IUnknown;
begin
  if Events = nil then Exit;
  if Reference.GetInterface(Adapter, IID, Listener) then Events.Remove(Listener);
end;

class procedure EventsTool.Execute(const Events: IInterfaceList; Method: TMethod; const Event);
var
  e: IEnumerator;
  Item: IUnknown;
begin
  if Events = nil then Exit;

  while Events.Enumerate(e, Item) do
    asm
      push    ebx
      mov     ebx, Method.Code
      mov     eax, Method.Data
      mov     edx, Item
      mov     ecx, Event
      call    ebx
      pop     ebx
    end;
end;

class procedure EventsTool.Clear(var Events: IEventList);
begin
  if Events <> nil then
  try
    Events.Clear;
  finally
    Events := nil;
  end;
end;

end.
