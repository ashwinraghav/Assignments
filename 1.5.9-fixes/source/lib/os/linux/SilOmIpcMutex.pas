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

unit SilOmIpcMutex;

{$I Defines.inc}

interface

uses
  Libc,

  SilOeWait,

  SilOsTypes,
  SilOiIpc,
  SilOiHandle,

  SilOkIpcMutex;

type
  TSilLinuxMutex = class(TSilMutex)
  private
    FSection: TPthreadMutex;
    procedure DoInit(Shared: Boolean);
  protected
    function DoCreateMutex(const Name: PChar; InitialOwner: Boolean): IHandle; override;
    function DoOpenMutex(const Name: PChar): IHandle; override;
    procedure DoRelease; override;
    procedure DoClose; override;
  protected // TSilWaitableObject
    function DoWait(const Timeout: Cardinal = INFINITE; AvoidMsgLock: Boolean = false): TSyncWaitResult; override;
  end;

implementation

{ TSilLinuxMutex }

procedure TSilLinuxMutex.DoInit(Shared: Boolean);
var
  Attribute: TMutexAttribute;
  Status: Integer;
begin
  Status := Libc.pthread_mutexattr_init(Attribute);

  if Status = 0 then
    try
      Status := Libc.pthread_mutexattr_settype(Attribute, PTHREAD_MUTEX_RECURSIVE);

      if Shared and (Status = 0) then
        Status := Libc.pthread_mutexattr_setpshared(Attribute, PTHREAD_PROCESS_SHARED);

      if Status = 0 then
        Status := Libc.pthread_mutex_init(FSection, Attribute);
    finally
      Libc.pthread_mutexattr_destroy(Attribute);
    end;
end;

procedure TSilLinuxMutex.DoClose;
begin
  Libc.pthread_mutex_destroy(FSection);
end;

function TSilLinuxMutex.DoCreateMutex(const Name: PChar; InitialOwner: Boolean): IHandle;
begin
  DoInit(Assigned(Name));
  Result := DoCreateHandle(THandle(@FSection), false);
end;

function TSilLinuxMutex.DoOpenMutex(const Name: PChar): IHandle;
begin
  DoInit(Assigned(Name));
  Result := DoCreateHandle(THandle(@FSection), false);
end;

procedure TSilLinuxMutex.DoRelease;
begin
  Libc.pthread_mutex_unlock(FSection);
end;

function TSilLinuxMutex.DoWait(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
var
  Spec: Libc.timespec;
begin
  if Timeout <> INFINITE then
  begin
    Spec.tv_sec := Timeout div 1000;
    Spec.tv_nsec := Timeout mod 1000 * 1000;

    if Libc.pthread_mutex_timedlock(@FSection, @Spec) = 0 then
      Result := wrSignaled else
      Result := wrTimeout;
  end else
  begin
    Libc.pthread_mutex_lock(FSection);
    Result := wrSignaled;
  end;
end;

end.
