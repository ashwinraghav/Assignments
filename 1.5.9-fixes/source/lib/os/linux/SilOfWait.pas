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

unit SilOfWait;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilOcTypes,
  SilOeTypes,
  SilOeWait;

function Multiple(const Handles: array of THandle; Timeout: Cardinal = INFINITE; WaitAll: Boolean = True; AvoidMsgLock: Boolean = False): Integer;
function Single(const Handle: THandle; Timeout: Cardinal; AvoidMsgLock: Boolean = False): TSyncWaitResult;
procedure Sleep(const Timeout: Cardinal; AvoidMsgLock: Boolean = False);
function NonlockingWait(const Handles: array of THandle; Timeout: Cardinal; WaitAll: Boolean): Integer;

function Map(Value: LongWord; const Count: LongWord = 1): TSyncWaitResult;

implementation

uses
  Libc;

function Multiple(const Handles: array of THandle; Timeout: Cardinal; WaitAll: Boolean; AvoidMsgLock: Boolean): Integer;
begin
  raise Exception.CreateFmt('%s: not implemented', ['SilOfWait.Multiple']);
(*)  if AvoidMsgLock then
    Result := NonlockingWait(Handles, Timeout, WaitAll) else
    Result := Windows.WaitForMultipleObjects(Length(Handles), @Handles[0], WaitAll, TimeOut);(*)
end;

function Single(const Handle: THandle; Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
begin
  raise Exception.CreateFmt('%s: not implemented', ['SilOfWait.Single']);
(*)  if AvoidMsgLock then
    Result := Map(Multiple([Handle], Timeout, false, AvoidMsgLock)) else
    Result := Map(Windows.WaitForSingleObject(Handle, Timeout));(*)
end;

procedure Sleep(const Timeout: Cardinal; AvoidMsgLock: Boolean = False);
begin
  Libc.usleep(Timeout * 1000);
end;

function NonlockingWait(const Handles: array of THandle; Timeout: Cardinal; WaitAll: Boolean): Integer;
(*)var
  Msg: TMsg;
  PHandles: Pointer;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['SilOfWait.NonlockingWait']);
(*)  Result := 0;
  PHandles := @Handles[0];

  PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);

  while true do
  begin
    Result := Windows.MsgWaitForMultipleObjects(Length(Handles), PHandles^, WaitAll, Timeout, QS_ALLINPUT);
    if Result = WAIT_OBJECT_0 + Length(Handles) then
    begin
      if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end else
      Break;
  end;(*)
end;

function Map(Value: LongWord; const Count: LongWord): TSyncWaitResult;
begin
  raise Exception.CreateFmt('%s: not implemented', ['SilOfWait.Map']);
(*) case Value of
    WAIT_ABANDONED:     Result := wrAbandoned;
    WAIT_OBJECT_0:      Result := wrSignaled;
    WAIT_TIMEOUT:       Result := wrTimeout;
    WAIT_FAILED:        Result := wrError;
    else
      if (Value > WAIT_OBJECT_0) and (Value < WAIT_OBJECT_0 + Count) then
        Result := wrSignaled else
      if Value = WAIT_OBJECT_0 + Count then
        Result := wrMessage else
        Result := wrError;
  end;(*)
end;

end.
