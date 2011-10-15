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

unit SilOmFilesystemNotifier;

{$I Defines.inc}

interface

uses
  SilOsTypes,

  SilOeWait,
  SilOeFilesystemNotifier,
  
  SilOiHandle,
  SilOiIpc,

  SilOkFilesystemNotifier;

type
  TSilLinuxFilesystemNotifier = class(TSilFilesystemNotifier)
  protected // TSilFilesystemNotifier
		function DoWait(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult; override;
  protected // TSilFilesystemNotifier
    function DoOpen(const PathName: String; WatchSubtree: Boolean; NotifyFilter: TFilesystemChangeFilters): IHandle; override;
    procedure DoClose; reintroduce; override;
    procedure DoReset; override;
  end;

implementation

uses
  SysUtils,
  
  SilOfWait,
  SilOfFilesystemNotifier,
  SilOsError;

{ TSilLinuxFilesystemNotifier }

function TSilLinuxFilesystemNotifier.DoWait(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
begin(*)
  Result := SilOfWait.Single(Self.Handle.Value, Timeout, AvoidMsgLock);
(*)end;

function TSilLinuxFilesystemNotifier.DoOpen(const PathName: String; WatchSubtree: Boolean; NotifyFilter: TFilesystemChangeFilters): IHandle;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxFilesystemNotifier.DoOpen']);
(*)
  Result := DoCreateHandle(Windows.FindFirstChangeNotification(PChar(PathName), WatchSubtree, FiltersToValue(NotifyFilter)), False);
(*)end;

procedure TSilLinuxFilesystemNotifier.DoClose;
begin(*)
  Windows.FindCloseChangeNotification(Self.Handle.Value);
(*)end;

procedure TSilLinuxFilesystemNotifier.DoReset;
begin(*)
  if not FindNextChangeNotification(Self.Handle.Value) then
    OsError.Check(GetLastError, 'TSilLinuxFilesystemNotifier.WaitNext [FindNextChangeNotification]');
(*)end;

end.
