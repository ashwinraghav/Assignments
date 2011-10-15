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

unit SilLtConnections;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypes,
  SilLiInterfaceList,
  SilLiEventList;

type
  SilConnectionsClass = class of SilConnections;

  SilConnections = class(Tool)
    class function Create(const Controller: IUnknown): IUnknown;
    class procedure Add(var List: IEventList; const Sink: IUnknown); overload;
    class procedure Remove(const List: IEventList; const Sink: IUnknown); overload;
    class procedure Clear(var List: IEventList);
    class procedure Add(var List: IInterfaceList; const Sink: IUnknown; const IID: TGUID); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class procedure Remove(const List: IInterfaceList; const Sink: IUnknown; const IID: TGUID); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class procedure Execute(const List: IInterfaceList; Method: TMethod; const Event); {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
  end;

implementation

uses
  SilLtTool,
  SilLtList,
  SilLtReference,
  SilLiEnumerator,
  SilLmConnections;

{ SilConnections }


class function SilConnections.Create(const Controller: IInterface): IUnknown;
begin
  Result := TSilConnections.Create(Controller);
end;

class procedure SilConnections.Add(var List: IEventList; const Sink: IUnknown);
begin
  if List = nil then List := ListTool.EventList;
  List.Locked;
  if List.IndexOf(Sink) = -1 then List.Add(Sink);
end;

class procedure SilConnections.Remove(const List: IEventList; const Sink: IUnknown);
begin
  if List <> nil then List.Remove(Sink);
end;

class procedure SilConnections.Clear(var List: IEventList);
begin
  if List <> nil then
  try
    List.Clear;
  finally
    List := nil;
  end;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class procedure SilConnections.Add(var List: IInterfaceList; const Sink: IUnknown; const IID: TGUID);
var
  Listener: IUnknown;
begin
  if not Reference.GetInterface(Sink, IID, Listener) then Exit;
  if List = nil then List := ListTool.InterfaceList(true);

  List.Locked;
  if List.IndexOf(Listener) = -1 then List.Add(Listener);
end;

class procedure SilConnections.Remove(const List: IInterfaceList; const Sink: IUnknown; const IID: TGUID);
var
  Listener: IUnknown;
begin
  if List = nil then Exit;
  if Reference.GetInterface(Sink, IID, Listener) then List.Remove(Listener);
end;

class procedure SilConnections.Execute(const List: IInterfaceList; Method: TMethod; const Event);
var
  e: IEnumerator;
  Item: IUnknown;
begin
  if List = nil then Exit;

  while List.Enumerate(e, Item) do
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

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}                   

end.
