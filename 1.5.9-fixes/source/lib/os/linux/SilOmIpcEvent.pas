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

unit SilOmIpcEvent;

{$I Defines.inc}

interface

uses
  libc,

  SilOeWait,
  SilOiHandle,
  SilOiIpc,
  SilOiWait,
  SilOsTypes,

  SilOkIpcEvent;

type
  TSilLinuxEvent = class(TSilEvent)
  private
    FSem: libc.TSemaphore;
    FManualReset: ICriticalSection;
    FOpened: Boolean;
  protected // TSilEvent
    function DoCreate(ManualReset: Boolean; InitialState: Boolean; const Name: PChar): IHandle; override;
    function DoOpen(const Name: PChar): IHandle; override;
    procedure DoClose; override;
    procedure DoSignal; override;
    procedure DoReset; override;
    procedure DoPulse; override;
  protected // TSilWaitableObject
    function DoWait(const Timeout: Cardinal = INFINITE; AvoidMsgLock: Boolean = false): TSyncWaitResult; override;
  end;

implementation

uses
  SysUtils,
  SilOmIpcCriticalSection;

{ TSilLinuxEvent }

function TSilLinuxEvent.DoCreate(ManualReset, InitialState: Boolean; const Name: PChar): IHandle;
begin
  if ManualReset then
    FManualReset := TSilLinuxCriticalSection.Create;

  FillChar(FSem, SizeOf(FSem), 0);

  if Assigned(Name) then
    raise Exception.Create('TSilLinuxEvent.DoCreate: not implemented')
  else
    libc.sem_init(@FSem, Ord(false), Ord(InitialState));

  Result := DoCreateHandle(THandle(@FSem), false);
end;

function TSilLinuxEvent.DoOpen(const Name: PChar): IHandle;
begin
  raise Exception.Create('TSilLinuxEvent.DoOpen: not implemented');
end;

procedure TSilLinuxEvent.DoClose;
begin
  if Assigned(FManualReset) then
    FManualReset := nil;

  if FOpened then
    libc.sem_close(@FSem)
  else
    libc.sem_destroy(@FSem);
end;

procedure TSilLinuxEvent.DoPulse;
begin
  DoSignal;
  DoReset;
end;

procedure TSilLinuxEvent.DoReset;
begin
  while libc.sem_trywait(@FSem) = 0 do;
end;

procedure TSilLinuxEvent.DoSignal;
var
  Status: Integer;
begin
  if Assigned(FManualReset) then FManualReset.Lock;
  try
    libc.sem_getvalue(@FSem, @Status);
    if Status = 0 then
      libc.sem_post(@FSem);
  finally
    if Assigned(FManualReset) then FManualReset.Unlock;
  end;
end;

function TSilLinuxEvent.DoWait(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;

  procedure doaddmsec(tp: Ptimeval; msec: Integer);
  begin
    msec := tp.tv_usec + (msec * 1000);
    tp.tv_sec := tp.tv_sec + (msec div 1000000);
    tp.tv_usec := msec mod 1000000;
  end;

var
  Spec: libc.timespec;
  tp: libc.TTimeval;
begin
  if Timeout <> INFINITE then
  begin
    libc.gettimeofday(@tp, nil);
    doaddmsec(@tp, Timeout);

    Spec.tv_sec := tp.tv_sec;
    Spec.tv_nsec := tp.tv_usec * 1000;

    if libc.sem_timedwait(@FSem, @Spec) = 0 then
      Result := wrSignaled
    else
      Result := wrTimeout;
  end else
  begin
    libc.sem_wait(@FSem);
    Result := wrSignaled;
  end;

  if (Result = wrSignaled) and Assigned(FManualReset) then
  begin
    FManualReset.Lock;
    try
      while libc.sem_trywait(@FSem) = 0 do;
      libc.sem_post(@FSem);
    finally
      FManualReset.Unlock;
    end;
  end;
end;

end.
