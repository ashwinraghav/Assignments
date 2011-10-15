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

unit SilOfProcess;

{$INCLUDE Defines.inc}

interface

uses
  SysUtils,
  SilOsTypes,
  SilOeProcess;

function PriorityToWindows(
           const Value: TProcessPriority): Integer;

function WindowsToPriority(
           const Value: Integer): TProcessPriority;

(*)function Execute(const FileName, StartDir: String;
  out ProcessInfo: TProcessInformation;
  Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): boolean;(*)

implementation

uses
  SilOsError;

(*)const
  GProcessPriorities: array [TProcessPriority] of Integer =
    (
      IDLE_PRIORITY_CLASS,
      NORMAL_PRIORITY_CLASS,
      HIGH_PRIORITY_CLASS,
      REALTIME_PRIORITY_CLASS
    );(*)

(*)const
  GProcessVisibility: array [TProcessVisibility] of Integer =
    (
      SW_HIDE,
      SW_SHOWNORMAL,
      SW_SHOWMINIMIZED,
      SW_SHOWMAXIMIZED,
      SW_SHOWNOACTIVATE
    );(*)

function PriorityToWindows(const Value: TProcessPriority): Integer;
begin
  raise Exception.CreateFmt('%s: not implemented', ['PriorityToWindows']);
(*)  Result := GProcessPriorities[Value];(*)
end;

function WindowsToPriority(const Value: Integer): TProcessPriority;
begin
(*)  for Result := Low(Result) to High(Result) do
    if GProcessPriorities[Result] = Value then
      Exit;
  Result := ppIdle;(*)
end;

(*)function Execute(const FileName, StartDir: String; out ProcessInfo: TProcessInformation; Visibility: TProcessVisibility = pvNormal; Timeout: LongWord = 0): boolean;
var
  StartupInfo: TStartupInfo;
  PStart: PChar;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);

  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := GProcessVisibility[Visibility];

  if Length(StartDir) > 0 then
    PStart := PChar(StartDir) else
    PStart := nil;

  OsError.Check(CreateProcess(nil, PChar(FileName), nil, nil, false, 0, nil, pStart, StartupInfo, ProcessInfo), 'Windows.CreateProcess');

  WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
  if Timeout <> 0 then
    WaitForSingleObject(ProcessInfo.hProcess, Timeout);

  //Result := ProcessInfo.hProcess;
  Result := true;
end;(*)

end.
