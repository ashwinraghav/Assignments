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

unit SilStTool;

{$I Defines.inc}

interface

uses
  SilBkTool,
  (*)SilSiPacketCompletion,
  SilStAbstractConnection,
  SilStSerialConnection,
  SilStSocketConnection,(*)
  SilStConfiguration,
  SilStDispatcher,
  SilStEvent,
  SilStThreadPool,
  SilStResourcePool,
  SilStMessageQueue,
  SilStSharedObject,
  SilStTimedRun,

  SilSiConfig, // <-- no me convence!
  SilStConfig;

type
  Sv = class(Tool)
    class function Dispatcher: DispatcherClass;
    (*)class function Connection: SvcConnectionClass;
    class function PacketCompleter: IPacketCompletion;
    class function Socket: SvcSocketConnectionClass;
    class function Serial: SvcSerialConnectionClass;(*)
    class function Configuration: SvcConfigurationClass;
    class function ThreadPool: SvcThreadPoolClass;
    class function ResourcePool: SvcResourcePoolClass;
    class function MessageQueue: SvcMessageQueueClass;
    class function Events: EventsClass;
    class function EventCaster: EventsClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function SharedObject: SvcSharedObjectClass;
    class function TimedRun: SvcTimedRunClass;
    class function Config: SilConfigType;
  end;

implementation

(*)uses
  SilSmPacketCompleter;(*)

{ Sv }

class function Sv.Configuration: SvcConfigurationClass;
begin
  Result := SilStConfiguration.Configuration;
end;

class function Sv.Dispatcher: DispatcherClass;
begin
  Result := SilStDispatcher.DispatcherTool;
end;

(*)class function Sv.Connection: SvcConnectionClass;
begin
  Result := SilStAbstractConnection.Connection;
end;

class function Sv.PacketCompleter: IPacketCompletion;
begin
  Result := TPacketCompleter.Create;
end;

class function Sv.Serial: SvcSerialConnectionClass;
begin
  Result := SilStSerialConnection.SerialConnection;
end;

class function Sv.Socket: SvcSocketConnectionClass;
begin
  Result := SilStSocketConnection.SocketConnection;
end;(*)

class function Sv.ThreadPool: SvcThreadPoolClass;
begin
  Result := SilStThreadPool.ThreadPool;
end;

class function Sv.ResourcePool: SvcResourcePoolClass;
begin
  Result := SilStResourcePool.ResourcePool;
end;

class function Sv.MessageQueue: SvcMessageQueueClass;
begin
  Result := SilStMessageQueue.SvcMessageQueue;
end;

class function Sv.Events: EventsClass;
begin
  Result := SilStEvent.EventsTool;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function Sv.EventCaster: EventsClass;
begin
  Result := Self.Events;
end;

class function Sv.SharedObject: SvcSharedObjectClass;
begin
  Result := SilStSharedObject.SharedObject;
end;

class function Sv.TimedRun: SvcTimedRunClass;
begin
  Result := SilStTimedRun.TimedRun;
end;


class function Sv.Config: SilConfigType;
begin
  Result := SilStConfig.SilConfigTool;
end;

end.

