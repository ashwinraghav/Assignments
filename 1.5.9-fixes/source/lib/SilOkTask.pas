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

unit SilOkTask;

{$I Defines.inc}

interface

uses
  SilOiTask,
  SilOiThread,
  SilOiIpc,

  SilOsMessenger;

type
  TSilTask = class(
    TSilOsMessenger,
    ITask,
    IThreadHook,
    IRunnable )
  private
    FThread: IThread;
    FInitialized: IEvent;
    FFinalized: IEvent;
  protected
    FHook: Pointer;
  private
    procedure DoClearConnections;
  protected
    function DoFireInitialize(const Thread: IThread): Boolean;
    procedure DoFireFinalize(const Thread: IThread);
    procedure DoFireSuspended(const Thread: IThread);
    procedure DoFireResumed(const Thread: IThread);
  protected // IRunnable
    procedure Run(const Thread: IThread); virtual;
  protected // IThreadHook
    function Initialize(const Thread: IThread): Boolean; virtual;
    procedure Finalize(const Thread: IThread); virtual;
    procedure Suspended(const Thread: IThread); virtual;
    procedure Resumed(const Thread: IThread); virtual;
  protected // ITask
    function GetThread: IThread;
    function GetInitialized: IIpcObject;
    function GetTerminated: IIpcObject;
    function GetHook: ITaskHook;
    procedure SetHook(const Value: ITaskHook);
    procedure Terminate;
    procedure Flush;
  protected // virtuals
    procedure DoRun(const Thread: IThread); virtual; abstract;
    procedure DoInitialize(const Thread: IThread); virtual; abstract;
    procedure DoFinalize(const Thread: IThread); virtual; abstract;
    procedure DoSuspended(const Thread: IThread); virtual;
    procedure DoResumed(const Thread: IThread); virtual;
    procedure DoTerminate; virtual; abstract;
    procedure DoFlush; virtual; abstract;
  public
    constructor Create(const Owner: IUnknown; const Name: string; const CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
    property Thread: IThread read FThread;
    property Initialized: IIpcObject read GetInitialized;
    property Terminated: IIpcObject read GetTerminated;
  end;

implementation

uses
  SilBeError,
  SilOsTypes,
  SilBcDebug,
  SilBgDebug,
  SilBtVart,
  SilLiEnumerator,
  SilLtTrace,
  SilLtReference,
  SilOiHandle,
  SilOeMessenger,
  SilOtTool;

{ TSilTask }

constructor TSilTask.Create(const Owner: IUnknown; const Name: string; const CreateSuspended: Boolean);
var
  Dummy: IHandle;
begin
  inherited Create(Dummy, Owner);
  FInitialized := Os.IPC.Event();
  FFinalized := Os.IPC.Event(true, true);
  FThread := Os.Thread.Spawn(Name, Self, CreateSuspended);
  if not CreateSuspended then
    FInitialized.WaitFor(INFINITE, True);
end;

destructor TSilTask.Destroy;
begin
  DoClearConnections;
  FThread := nil;
  FInitialized := nil;
  FFinalized := nil;
  inherited;
end;

procedure TSilTask.Run(const Thread: IThread);
begin
  FFinalized.Reset;
  FInitialized.Signal;
  try
    DoRun(Thread);
  except on E: Exception do
  end;
end;

function TSilTask.Initialize(const Thread: IThread): Boolean;
begin
  try
    Thread.Data['TASK'] := ITask(Self);
    DoInitialize(Thread);
    DoFireInitialize(Thread);
    Result := True;
  except on Ex: Exception do
    begin
      Result := False;
    end;
  end;
end;

procedure TSilTask.Finalize(const Thread: IThread);
begin
  try
    try
      try
        DoFireFinalize(Thread);
        try
          Sending.Lock;
          DoFinalize(Thread);
        finally
          Sending.Unlock;
        end;
      finally
        Thread.Data['TASK'] := Vart.Unassigned;
      end;
    except
    end;
  finally
    FFinalized.Signal;
  end;
end;

procedure TSilTask.Suspended(const Thread: IThread);
begin
  DoSuspended(Thread);
end;

procedure TSilTask.Resumed(const Thread: IThread);
begin
  DoResumed(Thread);
end;

procedure TSilTask.DoSuspended(const Thread: IThread);
begin
  DoFireSuspended(Thread);
end;

procedure TSilTask.DoResumed(const Thread: IThread);
begin
  DoFireResumed(Thread);
end;

procedure TSilTask.Terminate;
begin
  try
    FInitialized.Reset;
    DoTerminate;
    FFinalized.WaitFor(INFINITE, True);
  except end;
end;

function TSilTask.GetThread: IThread;
begin
  Result := FThread;
end;

function TSilTask.GetInitialized: IIpcObject;
begin
  Result := FInitialized;
end;

function TSilTask.GetTerminated: IIpcObject;
begin
  Result := FThread.Termination;
end;

function TSilTask.DoFireInitialize(const Thread: IThread): Boolean;
var
  Hook: IThreadHook;
  Enum: IEnumerator;
begin
  Result := True;
  try
    if Ref.GetInterface(IUnknown(Self.Owner), IThreadHook, Hook) then
      Hook.Initialize(Thread);
    if HasConnections then with Self.Events do
      while Enumerate(Enum, Hook, IThreadHook) do
        Hook.Initialize(Thread);
  except on Ex: Exception do
  end;
end;

procedure TSilTask.DoFireFinalize(const Thread: IThread);
var
  Hook: IThreadHook;
  Enum: IEnumerator;
begin
  try
    if Ref.GetInterface(IUnknown(Self.Owner), IThreadHook, Hook) then
      Hook.Finalize(Thread);
    if HasConnections then with Self.Events do
      while Enumerate(Enum, Hook, IThreadHook) do
        Hook.Finalize(Thread);
  except on Ex: Exception do
  end;
end;

procedure TSilTask.DoFireSuspended(const Thread: IThread);
var
  Hook: IThreadHook;
  Enum: IEnumerator;
begin
  try
    if Ref.GetInterface(IUnknown(Self.Owner), IThreadHook, Hook) then
      Hook.Suspended(Thread);
    if HasConnections then with Self.Events do
      while Enumerate(Enum, Hook, IThreadHook) do
        Hook.Suspended(Thread);
  except on Ex: Exception do
  end;
end;

procedure TSilTask.DoFireResumed(const Thread: IThread);
var
  Hook: IThreadHook;
  Enum: IEnumerator;
begin
  try
    if Ref.GetInterface(IUnknown(Self.Owner), IThreadHook, Hook) then
      Hook.Resumed(Thread);
    if HasConnections then with Self.Events do
      while Enumerate(Enum, Hook, IThreadHook) do
        Hook.Resumed(Thread);
  except on Ex: Exception do
  end;
end;

procedure TSilTask.Flush;
begin
  DoFlush;
end;

function TSilTask.GetHook: ITaskHook;
begin
  Result := ITaskHook(FHook);
end;

procedure TSilTask.SetHook(const Value: ITaskHook);
begin
  FHook := Pointer(Value);
end;

procedure TSilTask.DoClearConnections;
begin
  if HasConnections then
    Events.Clear;
end;

end.
