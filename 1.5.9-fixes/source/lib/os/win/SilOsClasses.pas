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

unit SilOsClasses;

{$I Defines.inc}

interface

uses
  SilOmThread,
  SilOmThreadList,
  SilOmProcess,
  SilOmFile,
  SilOmFileInfo,
  SilOkFileInfo,
  SilOmToolWindow,
  SilOmSharedLibrary,
  SilOmIpcEvent,
  SilOmIpcMutex,
  SilOmIpcSemaphore,
  SilOmIpcCriticalSection,
  SilOmFilesystemNotifier,
  SilOmSharedMemory,
  SilOmEnvVariable,
  SilOmEnvVariables,
  SilOmTimer,
  SilOmVersion,
  SilOmModule,
  SilOmEventQueue,
  SilOmSocket,
  SilOmRegistry,
  SilOmRegKeys,
  SilOmRegKey,
  SilOmRegValues,
  SilOmDirectoryReader,
  SilOmPerformance,
  SilOmComputer;

type
  TSilOsThread                            = class(SilOmThread.TSilWindowsThread);
  TSilOsThreadList                        = class(SilOmThreadList.TSilWindowsThreadList);

type
  TSilOsProcess                           = class(SilOmProcess.TSilWindowsProcess);
  
type
  TSilOsFile                              = class(SilOmFile.TSilWindowsFile);
  TSilOsFileInfo                          = class(SilOmFileInfo.TSilWindowsFileInfo);

type
  TSilOsToolWindow                        = class(SilOmToolWindow.TSilWindowsToolWindow);

type
  TSilOsSharedLibrary                     = class(SilOmSharedLibrary.TSilWindowsSharedLibrary);

type
  TSilOsEvent                             = class(SilOmIpcEvent.TSilWindowsEvent);

type
  TSilOsMutex                             = class(SilOmIpcMutex.TSilWindowsMutex);

type
  TSilOsSemaphore                         = class(SilOmIpcSemaphore.TSilWindowsSemaphore);

type
  TSilOsCriticalSection                   = class(SilOmIpcCriticalSection.TSilWindowsCriticalSection);

type
  TSilOsFilesystemNotifier                = class(SilOmFilesystemNotifier.TSilWindowsFilesystemNotifier);

type
  TSilOsSharedMemory                      = class(SilOmSharedMemory.TSilWindowsSharedMemory);

type
  TSilOsEnvironmentVariable               = class(SilOmEnvVariable.TSilWindowsEnvironmentVariable);
  TSilOsEnvironmentVariables              = class(SilOmEnvVariables.TSilWindowsEnvironmentVariables);
  
type
  TSilOsTimer                             = class(SilOmTimer.TSilWindowsTimer);

type
  TSilOsVersionInfo                       = class(SilOmVersion.TSilWindowsVersionInfo);
  TSilOsVersionTags                       = class(SilOmVersion.TSilWindowsVersionTags);
  TSilOsStandardTags                      = class(SilOmVersion.TSilWindowsStandardTags);
  TSilOsVersionNumber                     = class(SilOmVersion.TSilWindowsVersionNumber);

type
  TSilOsModule                            = class(SilOmModule.TSilWindowsModule);
  TSilOsModuleList                        = class(SilOmModule.TSilWindowsModuleList);

type
  TSilOsEventQueue                        = class(SilOmEventQueue.TSilWindowsEventQueue);

type
  TSilOsSocket                            = class(TSilWindowsSocket);
  TSilOsSocketProtocol                    = class(TSilWindowsSocketProtocol);
  TSilOsSocketService                     = class(TSilWindowsSocketService);
  TSilOsSocketHandle                      = class(TSilWindowsSocketHandle);
  TSilOsSocketLocalAddress                = class(TSilWindowsSocketLocalAddress);
  TSilOsSocketRemoteAddress               = class(TSilWindowsSocketRemoteAddress);

type
  TSilOsRegistryKeys                      = class(SilOmRegKeys.TSilWindowsRegistryKeys);
  TSilOsRegistryKey                       = class(SilOmRegKey.TSilWindowsRegistryKey);
  TSilOsRegistryValues                    = class(SilOmRegValues.TSilWindowsRegistryValues);
  TSilOsRegistry                          = class(SilOmRegistry.TSilWindowsRegistry);

type
  TSilOsDirectoryReader                   = class(SilOmDirectoryReader.TSilWindowsDirectoryReader);

type
  TSilOsPerformance                       = class(SilOmPerformance.TSilWindowsPerformance);

type
  TSilOsLocalComputer                     = class(SilOmComputer.TSilWindowsLocalComputer);
  TSilOsRemoteComputer                    = class(SilOmComputer.TSilWindowsRemoteComputer);

implementation
end.
