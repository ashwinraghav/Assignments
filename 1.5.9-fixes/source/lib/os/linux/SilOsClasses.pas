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
  TSilOsThread                            = class(SilOmThread.TSilLinuxThread);
  TSilOsThreadList                        = class(SilOmThreadList.TSilLinuxThreadList);

type
  TSilOsProcess                           = class(SilOmProcess.TSilLinuxProcess);

type
  TSilOsFile                              = class(SilOmFile.TSilOsFile);
  TSilOsFileInfo                          = class(SilOmFileInfo.TSilLinuxFileInfo);

type
  TSilOsToolWindow                        = class(SilOmToolWindow.TSilLinuxToolWindow);

type
  TSilOsSharedLibrary                     = class(SilOmSharedLibrary.TSilLinuxSharedLibrary);

type
  TSilOsEvent                             = class(SilOmIpcEvent.TSilLinuxEvent);

type
  TSilOsMutex                             = class(SilOmIpcMutex.TSilLinuxMutex);

type
  TSilOsSemaphore                         = class(SilOmIpcSemaphore.TSilLinuxSemaphore);

type
  TSilOsCriticalSection                   = class(SilOmIpcCriticalSection.TSilLinuxCriticalSection);

type
  TSilOsFilesystemNotifier                = class(SilOmFilesystemNotifier.TSilLinuxFilesystemNotifier);

type
  TSilOsSharedMemory                      = class(SilOmSharedMemory.TSilLinuxSharedMemory);

type
  TSilOsEnvironmentVariable               = class(SilOmEnvVariable.TSilLinuxEnvironmentVariable);
  TSilOsEnvironmentVariables              = class(SilOmEnvVariables.TSilLinuxEnvironmentVariables);

type
  TSilOsTimer                             = class(SilOmTimer.TSilLinuxTimer);

{$IFNDEF FP20}
type
  TSilOsVersionInfo                       = class(SilOmVersion.TSilLinuxVersionInfo);
  TSilOsVersionTags                       = class(SilOmVersion.TSilLinuxVersionTags);
  TSilOsStandardTags                      = class(SilOmVersion.TSilLinuxStandardTags);
  TSilOsVersionNumber                     = class(SilOmVersion.TSilLinuxVersionNumber);
{$ENDIF}

type
  TSilOsModule                            = class(SilOmModule.TSilLinuxModule);
  TSilOsModuleList                        = class(SilOmModule.TSilLinuxModuleList);

type
  TSilOsEventQueue                        = class(SilOmEventQueue.TSilLinuxEventQueue);

type
  TSilOsSocket                            = class(TSilLinuxSocket);
  TSilOsSocketProtocol                    = class(TSilLinuxSocketProtocol);
  TSilOsSocketService                     = class(TSilLinuxSocketService);
  TSilOsSocketHandle                      = class(TSilLinuxSocketHandle);
  TSilOsSocketLocalAddress                = class(TSilLinuxSocketLocalAddress);
  TSilOsSocketRemoteAddress               = class(TSilLinuxSocketRemoteAddress);

type
  TSilOsRegistryKeys                      = class(SilOmRegKeys.TSilLinuxRegistryKeys);
  TSilOsRegistryKey                       = class(SilOmRegKey.TSilLinuxRegistryKey);
  TSilOsRegistryValues                    = class(SilOmRegValues.TSilLinuxRegistryValues);
  TSilOsRegistry                          = class(SilOmRegistry.TSilLinuxRegistry);

type
  TSilOsDirectoryReader                   = class(SilOmDirectoryReader.TSilLinuxDirectoryReader);

type
  TSilOsPerformance                       = class(SilOmPerformance.TSilLinuxPerformance);

type
  TSilOsLocalComputer                     = class(SilOmComputer.TSilLinuxLocalComputer);
  TSilOsRemoteComputer                    = class(SilOmComputer.TSilLinuxRemoteComputer);

implementation
end.
