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

unit SilOsTool;

{$I Defines.inc}

interface

uses
  SilOsError,
  SilOsSocket,

  SilOtLocked,
  SilOtHandle,
  SilOtGuid,
  SilOtLang,
  SilOtThread,
{$IFNDEF FP20}
  SilOtMessenger,
  SilOtTask,
  SilOtPipe,
{$ENDIF}
  SilOtProcess,
  SilOtTimer,
  SilOtTimeZone,
  SilOtWStr,
  SilOtFile,
  SilOtSharedLibrary,
  SilOtWait,
  SilOtIpc,
  SilOtEnvironment,
  SilOtVersion,
  SilOtModule,
  SilOtEventQueue,
  SilOtRegistry,
  SilOtPerformance,
  SilOtComputer,
  SilOtResource;

type
  OsError                             = SilOsError.OsError;
  OsErrorClass                        = class of OsError;

type
  OsSocket                            = SilOsSocket.OsSocket;
  OsSocketClass                       = class of OsSocket;

type
  OsLocked                            = class(SilOtLocked.SilLockedTool);
  OsLockedClass                       = class of OsLocked;

type
  OsGuid                              = class(SilOtGuid.OsGuid);
  OsGuidClass                         = class of OsGuid;

type
  OsHandle                            = class(SilOtHandle.LinuxHandleTool);
  OsHandleClass                       = class of OsHandle;

type
  OsLocale                            = class(SilOtLang.LinuxLocaleTool);
  OsLocaleClass                       = class of OsLocale;

type
  OsTimer                             = class(SilOtTimer.LinuxTimerTool);
  OsTimerClass                        = class of OsTimer;

type
  OsThread                            = class(SilOtThread.LinuxThread);
  OsThreadClass                       = class of OsThread;

{$IFNDEF FP20}
type
  OsTask                              = class(SilOtTask.LinuxTask);
  OsTaskClass                         = class of OsTask;

type
  OsMessenger                         = class(SilOtMessenger.LinuxMessenger);
  OsMessengerClass                    = class of OsMessenger;

type
  OSPipe                              = class(SilOtPipe.LinuxNamedPipe);
  OSPipeClass                         = class of OSPipe;
{$ENDIF}

type
  OsProcess                           = class(SilOtProcess.LinuxProcessTool);
  OsProcessClass                      = class of OsProcess;

type
  OsTimeZone                          = class(SilOtTimeZone.LinuxTimeZone);
  OsTimeZoneClass                     = class of OsTimeZone;

type
  OsWStr                              = class(SilOtWStr.LinuxWStr);
  OsWStrClass                         = class of OsWStr;

type
  OsFile                              = class(SilOtFile.OsFile);
  OsFileClass                         = class of OsFile;

type
  OsSharedLibrary                     = class(SilOtSharedLibrary.LinuxSharedLibrary);
  OsSharedLibraryClass                = class of OsSharedLibrary;

type
  OsWait                              = class(SilOtWait.LinuxWaitTool);
  OsWaitClass                         = class of OsWait;

type
  OsIpc                               = class(SilOtIpc.LinuxIpcTool);
  OsIpcClass                          = class of OsIpc;

type
  OsEnvironment                       = class(SilOtEnvironment.LinuxEnvironment);
  OsEnvironmentClass                  = class of OsEnvironment;

{$IFNDEF FP20}
type
  OsVersion                           = class(SilOtVersion.LinuxVersionTool);
  OsVersionClass                      = class of OsVersion;
{$ENDIF}

type
  OsModule                            = class(SilOtModule.LinuxModuleTool);
  OsModuleClass                       = class of OsModule;

type
  OsEventQueue                        = class(SilOtEventQueue.LinuxEventQueueTool);
  OsEventQueueClass                   = class of OsEventQueue;

type
  OsRegistry                          = class(SilOtRegistry.SilLinuxRegistryTool);
  OsRegistryClass                     = class of OsRegistry;

type
  OsPerformance                       = class(SilOtPerformance.SilLinuxPerformanceTool);
  OsPerformanceClass                  = class of OsPerformance;

type
  OsComputer                          = class(SilOtComputer.LinuxComputerTool);
  OsComputerClass                     = class of OsComputer;

type
  OsResource                          = class(SilOtResource.LinuxResourceTool);
  OsResourceClass                     = class of OsResource;

implementation
end.
