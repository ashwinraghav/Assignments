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

  SilOfThreadList,
  SilOkThread;

type
  TSilWindowsThread = class (TSilThread)
  private
    class function DoGetCurrent: IThread;
  protected // IWaitable
    function WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean = false): TSyncWaitResult; override;
  protected
    function DoResume: Boolean; override;
    function DoSuspend: Boolean; override;
    procedure DoApplyPriority; override;
    function DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle; override;
  public
    class function GetCurrent: IThread; override;
  end;

implementation

uses
  Windows, Messages,

  SilOtWindow,
  SilOtTool,
  SilOfThread,
  SilOfWait,
  SilOmObjectHandle;


const
  ThreadPriorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function ThreadProc(Thread: TSilWindowsThread): Integer; forward;

{ TSilWindowsThread }

function TSilWindowsThread.DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle;
var
  Flags: DWORD;
begin
  Flags := CREATE_SUSPENDED;
  Result := TSilObjectHandle.Create(BeginThread(nil, 0, @ThreadProc, Pointer(Self), Flags, ThreadID), true);
end;

function TSilWindowsThread.DoResume: Boolean;
begin
  Result := ResumeThread(Self.Handle.Value) = 1;
end;

function TSilWindowsThread.DoSuspend: Boolean;
begin
  Result := SuspendThread(Self.Handle.Value) = 0;
end;

procedure TSilWindowsThread.DoApplyPriority;
begin
  SetThreadPriority(Handle.Value, ThreadPriorities[Self.Priority]);
end;

function TSilWindowsThread.WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
begin
  if (FFinished.WaitFor(0) = wrTimeout) and (GetCurrentThreadId <> ThreadID) then
  begin
    Result := SilOfWait.Single(Self.Handle.Value, Timeout, AvoidMsgLock);
    GetExitCodeThread(Handle.Value, FExitCode);
  end else
    Result := wrError;
end;

class function TSilWindowsThread.GetCurrent: IThread;
begin
  Result := DoGetCurrent();
end;

class function TSilWindowsThread.DoGetCurrent: IThread;
var
  iPriority: Integer;
  FindPriority: TThreadPriority;
  CurrID: LongWord;
  NewThread: TSilWindowsThread;
begin
  CurrID := GetCurrentThreadId;

  if not GetList.FindThreadID(CurrID, Result) then
  begin
    if CurrID = MainThreadID then
      NewThread := TSilWindowsThread.Create('Main', TSilObjectHandle.Create(GetCurrentThread, false)) else
      NewThread := TSilWindowsThread.Create('', TSilObjectHandle.Create(GetCurrentThread, false));

    NewThread.ThreadID := CurrID;
    iPriority := GetThreadPriority(GetCurrentThread);

    for FindPriority := Low(ThreadPriorities) to High(ThreadPriorities) do
      if ThreadPriorities[FindPriority] = iPriority then
      begin
        NewThread.Priority := FindPriority;
        Break;
      end;

    Result := NewThread;
  end;
end;

function ThreadProc(Thread: TSilWindowsThread): Integer;
var
  Hold: IThread;
begin
  Result := -1;
  Hold := Thread; // mantiene una referencia por seguridad

  try
    try
      Thread.DoFireEnter;
      Thread.DoExecute;
      Result := Thread.ReturnValue;
    except
      on e: Exception do
      begin
        try
          Thread.DoFireUnhandledException(e);
          Thread.FTermination.Signal;
        except end;
      end;
    end;

    try
      Thread.DoFireExit;
      Thread.FTermination.Signal;
    except end;
  finally
    try
      Thread.Detach;
      RemoveThread(Thread);
      Thread.FFinished.Signal;
      Hold := nil;
    except end;

    EndThread(Result);
  end;
end;

end.


