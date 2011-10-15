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
  SilOtMessenger,
  SilOtTask,
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
  SilOtPipe,
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
  OsGuid                              = class(SilOtGuid.WindowsGuid);
  OsGuidClass                         = class of OsGuid;

type
  OsHandle                            = class(SilOtHandle.WindowsHandleTool);
  OsHandleClass                       = class of OsHandle;

type
  OsLocale                            = class(SilOtLang.WindowsLocaleTool);
  OsLocaleClass                       = class of OsLocale;

type
  OsTimer                             = class(SilOtTimer.WindowsTimerTool);
  OsTimerClass                        = class of OsTimer;

type
  OsThread                            = class(SilOtThread.WindowsThread);
  OsThreadClass                       = class of OsThread;

type
  OsTask                              = class(SilOtTask.WindowsTask);
  OsTaskClass                         = class of OsTask;

type
  OsMessenger                         = class(SilOtMessenger.WindowsMessenger);
  OsMessengerClass                    = class of OsMessenger;

type
  OsProcess                           = class(SilOtProcess.WindowsProcessTool);
  OsProcessClass                      = class of OsProcess;

type
  OsTimeZone                          = class(SilOtTimeZone.WindowsTimeZone);
  OsTimeZoneClass                     = class of OsTimeZone;

type
  OsWStr                              = class(SilOtWStr.WindowsWStr);
  OsWStrClass                         = class of OsWStr;

type
  OsFile                              = class(SilOtFile.WindowsFile);
  OsFileClass                         = class of OsFile;

type
  OsSharedLibrary                     = class(SilOtSharedLibrary.WindowsSharedLibrary);
  OsSharedLibraryClass                = class of OsSharedLibrary;

type
  OsWait                              = class(SilOtWait.WindowsWaitTool);
  OsWaitClass                         = class of OsWait;

type
  OsIpc                               = class(SilOtIpc.WindowsIpcTool);
  OsIpcClass                          = class of OsIpc;

type
  OsEnvironment                       = class(SilOtEnvironment.WindowsEnvironment);
  OsEnvironmentClass                  = class of OsEnvironment;

type
  OsVersion                           = class(SilOtVersion.WindowsVersionTool);
  OsVersionClass                      = class of OsVersion;

type
  OsModule                            = class(SilOtModule.WindowsModuleTool);
  OsModuleClass                       = class of OsModule;

type
  OsEventQueue                        = class(SilOtEventQueue.WindowsEventQueueTool);
  OsEventQueueClass                   = class of OsEventQueue;

type
  OsRegistry                          = class(SilOtRegistry.SilWindowsRegistryTool);
  OsRegistryClass                     = class of OsRegistry;

type
  OsPerformance                       = class(SilOtPerformance.SilWindowsPerformanceTool);
  OsPerformanceClass                  = class of OsPerformance;

type
  OsComputer                          = class(SilOtComputer.WindowsComputerTool);
  OsComputerClass                     = class of OsComputer;

type
  OSPipe                              = class(SilOtPipe.WindowsNamedPipe);
  OSPipeClass                         = class of OSPipe;

type
  OsResource                          = class(SilOtResource.WindowsResourceTool);
  OsResourceClass                     = class of OsResource;

implementation
end.
