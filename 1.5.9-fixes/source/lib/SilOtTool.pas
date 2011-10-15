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

unit SilOtTool;

interface

{$INCLUDE Defines.inc}

uses
  SilBkTool,
  SilOsTool,

  SilOtSerialPort,
  SilOtSharedMemory,
  SilOtSocket,
  SilOtWindow,
  SilOtEventQueue,
  SilOtModule,

  SilOtSynchronization;

type
  OS = class(Tool)
    class function TimeZone: OsTimeZoneClass;
    class function Handle: OSHandleClass;
    class function FileSystem: OsFileClass;
    class function Fsys: OsFileClass;
    class function Window: OSWindowClass;
    class function ToolWindow: OSToolWindowClass;
    class function Timer: OSTimerClass;
    class function Thread: OsThreadClass;
    class function Task: OsTaskClass;
    class function Messenger: OsMessengerClass;
    class function Process: OSProcessClass;
    class function IPC: OSIPCClass;
    class function Socket: OSSocketClass;
    class function SharedLibrary: OSSharedLibraryClass;
    class function Wait: OSWaitClass;
    class function EventQueue: OSEventQueueClass;
    class function SerialPort: OSSerialPortClass;
    class function Environment: OSEnvironmentClass;
    class function Version: OSVersionClass;
    class function Locale: OSLocaleClass;
    class function Module: OSModuleClass;
    class function Locked: OSLockedClass;
    class function Error: OSErrorClass;
    class function Registry: OsRegistryClass;
    class function Performance: OsPerformanceClass;
    class function Computer: OsComputerClass;
    class function Pipe: OSPipeClass;
    class function Resource: OsResourceClass;
    // deprecated
    class function SharedMemory: OSSharedMemoryClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Event: OSEventClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Mutex: OSMutexClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Semaphore: OSSemaphoreClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function CriticalSection: OSCriticalSectionClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Configuration: OsRegistryClass; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
  end;

type
  Guid = SilOsTool.OsGuid;

type
  Wstr = SilOsTool.OsWstr;

implementation

{ OS }

class function OS.TimeZone: OsTimeZoneClass;
begin
  Result := SilOsTool.OsTimeZone;
end;

class function OS.Environment: OSEnvironmentClass;
begin
  Result := SilOsTool.OsEnvironment;
end;

class function OS.FileSystem: OsFileClass;
begin
  Result := SilOsTool.OsFile;
end;

class function OS.Fsys: OsFileClass;
begin
  Result := SilOsTool.OsFile;
end;

class function OS.Thread: OSThreadClass;
begin
  Result := SilOsTool.OsThread;
end;

class function OS.Task: OsTaskClass;
begin
  Result := SilOsTool.OsTask;
end;

class function OS.Messenger: OsMessengerClass;
begin
  Result := SilOsTool.OsMessenger;
end;

class function OS.Wait: OSWaitClass;
begin
  Result := SilOsTool.OsWait;
end;

class function OS.IPC: OSIPCClass;
begin
  Result := SilOsTool.OsIpc;
end;

class function OS.SharedLibrary: OSSharedLibraryClass;
begin
  Result := SilOsTool.OsSharedLibrary;
end;

class function OS.Locked: OSLockedClass;
begin
  Result := SilOsTool.OsLocked;
end;

class function OS.Locale: OSLocaleClass;
begin
  Result := SilOsTool.OsLocale;
end;

class function OS.Handle: OSHandleClass;
begin
  Result := SilOsTool.OsHandle;
end;

class function OS.Timer: OSTimerClass;
begin
  Result := SilOsTool.OsTimer;
end;

class function OS.Version: OSVersionClass;
begin
  Result := SilOsTool.OsVersion;
end;

class function OS.Process: OSProcessClass;               
begin
  Result := SilOsTool.OsProcess;
end;

class function OS.Error: OSErrorClass;
begin
  Result := SilOsTool.OsError;
end;

class function OS.Module: OSModuleClass;
begin
  Result := SilOsTool.OsModule;
end;

class function OS.EventQueue: OSEventQueueClass;
begin
  Result := SilOsTool.OsEventQueue;
end;

class function OS.Socket: OSSocketClass;
begin
  Result := SilOsTool.OsSocket;
end;

class function OS.Registry: OsRegistryClass;
begin
  Result := SilOsTool.OsRegistry;
end;

class function OS.Performance: OsPerformanceClass;
begin
  Result := SilOsTool.OsPerformance;
end;

class function OS.Computer: OsComputerClass;
begin
  Result := SilOsTool.OsComputer;
end;

class function OS.SerialPort: OSSerialPortClass;
begin
  Result := SilOtSerialPort.SerialPort;
end;

class function OS.ToolWindow: OSToolWindowClass;
begin
  Result := SilOtWindow.ToolWindow;
end;

class function OS.Window: OSWindowClass;
begin
  Result := SilOtWindow.Window;
end;

class function OS.Pipe: OSPipeClass;
begin
  Result := SilOsTool.OSPipe;
end;

class function OS.Resource: OsResourceClass;
begin
  Result := SilOsTool.OsResource;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function OS.CriticalSection: OSCriticalSectionClass;
begin
  Result := SilOtSynchronization.CriticalSection;
end;

class function OS.Event: OSEventClass;
begin
  Result := SilOtSynchronization.Event;
end;

class function OS.Mutex: OSMutexClass;
begin
  Result := SilOtSynchronization.Mutex;
end;

class function OS.Semaphore: OSSemaphoreClass;
begin
  Result := SilOtSynchronization.Semaphore;
end;

class function OS.SharedMemory: OSSharedMemoryClass;
begin
  Result := SilOtSharedMemory.SharedMemory;
end;

class function OS.Configuration: OsRegistryClass;
begin
  Result := Self.Registry;
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

end.
