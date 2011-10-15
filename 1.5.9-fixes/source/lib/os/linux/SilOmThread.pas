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

unit SilOmThread;

{$I Defines.inc}

interface

uses
  SilBeError,
  SilLiEnumerator,
  SilLiEventList,
  SilOeWait,
  SilOiHandle,
  SilOiThread,
  SilOiIpc,
  SilOiWait,

  SilOkThread;

type
  TSilLinuxThread = class (TSilThread, IIpcObject)
  private
    FSuspended: ICriticalSection;
    class function DoGetCurrent: IThread;
  protected // IWaitable
    function WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean = false): TSyncWaitResult; override;
  protected // IIpcObject
    function GetSignaled: Boolean;
  protected
    function GetTermination: IIpcObject; override;
    function DoResume: Boolean; override;
    function DoSuspend: Boolean; override;
    procedure DoApplyPriority; override;
    function DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle; override;
  public
    class function GetCurrent: IThread; override;
    destructor Destroy; override;
  end;

implementation

uses
  Libc,

  SilOmIpcCriticalSection,
  SilOtWindow,
  SilOtTool,
  SilOfThread,
  SilOfWait,
  SilOfThreadList,
  SilOmHandle;

(*)const
  ThreadPriorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);(*)

function ThreadProc(Thread: TSilLinuxThread): Integer; forward;

{ TSilLinuxThread }

function TSilLinuxThread.DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle;
var
  Id: LongInt;
begin
  if CreateSuspended then
  begin
    FSuspended := TSilLinuxCriticalSection.Create;
    FSuspended.Lock;
  end;

  Result := TSilHandle.Create(BeginThread(@ThreadProc, Pointer(Self), Id), false);
  ThreadID := Id;
end;

function TSilLinuxThread.DoResume: Boolean;
begin
  Result := true;

  if Assigned(FSuspended) then
  begin
    FSuspended.Unlock;
    FSuspended := nil;
  end else
    ResumeThread(Handle.Value);
end;

function TSilLinuxThread.DoSuspend: Boolean;
begin
  Result := true;

  if not Assigned(FSuspended) then
    SuspendThread(Handle.Value);
end;

procedure TSilLinuxThread.DoApplyPriority;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxThread.DoApplyPriority']);
end;

function TSilLinuxThread.WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
var
  ID: Cardinal;
  ResPtr: Pointer;
begin
  if (Timeout <> 0) and (Timeout <> $ffffffff) then
    raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxThread.WaitFor']);

  if (FFinished.WaitFor(0) = wrTimeout) and (SilOfThread.GetCurrentId <> ThreadID) then
  begin
    ID := Handle.Value;
    Libc.pthread_join(ID, ResPtr);
    FExitCode := LongWord(ResPtr);
    Result := wrSignaled;
  end else
    Result := wrError;
end;

function TSilLinuxThread.GetSignaled: Boolean;
begin
  Result := FTermination.IsSignaled;
end;

function TSilLinuxThread.GetTermination: IIpcObject;
begin
  Result := Self;
end;

class function TSilLinuxThread.GetCurrent: IThread;
begin
  Result := DoGetCurrent();
end;

class function TSilLinuxThread.DoGetCurrent: IThread;
var
  //iPriority: Integer;
  //FindPriority: TThreadPriority;
  CurrID: LongWord;
  NewThread: TSilLinuxThread;
begin
  CurrID := SilOfThread.GetCurrentId;

  if not GetList.FindThreadID(CurrID, Result) then
  begin
    if CurrID = SilOfThread.MainThreadID then
      NewThread := TSilLinuxThread.Create('Main', TSilHandle.Create(SilOfThread.GetCurrentId, false)) else
      NewThread := TSilLinuxThread.Create('', TSilHandle.Create(SilOfThread.GetCurrentId, false));

    NewThread.ThreadID := CurrID;
    //iPriority := GetThreadPriority(GetCurrentThread);

    {for FindPriority := Low(ThreadPriorities) to High(ThreadPriorities) do
      if ThreadPriorities[FindPriority] = iPriority then
      begin
        NewThread.Priority := FindPriority;
        Break;
      end;}

    Result := NewThread;
  end;
end;

function ThreadProc(Thread: TSilLinuxThread): Integer;
var
  Hold: IThread;
  Suspended: ICriticalSection;

  function DoRun: Integer;
  begin
    Thread.DoFireEnter;
    Thread.DoExecute;
    Result := Thread.ReturnValue;

    Thread.DoFireExit;
    Thread.FTermination.Signal;
  end;

  procedure DoRunException(e: Exception);
  begin
    Thread.DoFireUnhandledException(e);
    Thread.FTermination.Signal;
  end;

  procedure DoThreadEnd;
  begin
    Thread.Detach;
    SilOfThreadList.RemoveThread(Thread);
    Thread.FFinished.Signal;
    Hold := nil;
  end;

begin
  Result := -1;
  Hold := Thread; // mantiene una referencia por seguridad
  Suspended := Thread.FSuspended;

  if Assigned(Suspended) then
  begin
    Suspended.Lock;
    Suspended := nil;
    Thread.FSuspended := nil;
  end;

  try
    try
      Result := DoRun;
    except
      on e: Exception do
        DoRunException(e);
    end;
  finally
    try
      DoThreadEnd;
    except
      // do nothing
    end;

    EndThread(Result);
  end;
end;

destructor TSilLinuxThread.Destroy;
begin
  inherited;
end;

end.
