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

unit SilSmThreadPool;

{$I Defines.inc}

interface

uses
  SilOiThread,
  SilOiIpc,
  SilOiWait,
  SilLmInterfaceList,

  SilOsClasses;

type
  TThreadPool = class;

{ TCachedThread }

  TCachedThread = class (
    // extends
    TSilOsThread,
    // implements
    IThread,
    ICachedThread )
  private
    FActivation: IEvent;
    FSleeping: IEvent;
    FThreadPool: TThreadPool;
    FFinalize: Boolean;
  private
    function DoUpdateList: Boolean;
  protected // IThread
    function GetTermination: IIpcObject; override;
  protected // ICachedThread
    function GetActivation: IEvent;
  protected
    procedure DoExecute; override;
  protected
    function WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean = false): TSyncWaitResult; override;
    procedure Kill;
  public
    constructor Create(ThreadPool: TThreadPool);
    destructor Destroy; override;
  end;

{ TThreadPool }

  TThreadPool = class (
    // extenfs
    TSilInterfaceList,
    // implements
    IThreadManager,
    IThreads)
  private
    FCacheSize: LongWord;
    FRemoveTimeout: LongWord;
    FEmpty: IEvent;
  private
    function DoUpdateList(NewCount: Integer = -1): Boolean;
    function DoCreateNewThread: ICachedThread;
    function DoGetInactiveThread: ICachedThread;
  protected // IThreadManager
    function GetCacheSize: LongWord;
    procedure SetCacheSize(Value: LongWord);
    function GetRemoveTimeout: LongWord;
    procedure SetRemoveTimeout(Value: LongWord);
    function GetThreads: IThreads;
  protected // IThreads
    function GetDestroying: Boolean;
    function IThreads.GetItem = DoGetItem;
    function DoGetItem(const Index: Variant): IThread;
    function IThreads.First = DoFirst;
    function DoFirst: IThread;
    function IThreads.Last = DoLast;
    function IThreads.IndexOf = DoIndexOf;
    function DoIndexOf(const Item: IThread): Integer;
    function DoLast: IThread;
    function FindThreadID(ID: LongWord; out Thread: IThread): Boolean;
  public
    class function Global: IThreadManager; 
    function Spawn(const Runnable: IRunnable; Suspended: Boolean = false): IThread; overload;
    function Spawn(const Name: String; const Runnable: IRunnable; Suspended: Boolean = false): IThread; overload;
    function Add(const Item: IUnknown): Integer; override;
    procedure Delete(Index: Integer); override;
    constructor Create; reintroduce; 
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilLiEnumerator,
  SilLtReference,
  SilLtGlobal,
  SilOsTypes,
  SilOeWait,
  SilOtTool,
  SilStTool,
  SilLtLock,
  SilBtVart;

const
  CThreadPool: TGUID = '{6991B6D7-7219-455F-83FA-360235A2698E}';

type
  ThreadPool = class(GlobalService)
    class function ID: TGuid; override;
    class function Create: IUnknown; override; 
  end;  

var
  MThreadPool: IThreadManager = nil; 

{ ThreadPool }

class function ThreadPool.Create: IUnknown;
begin
  Result := TThreadPool.Create();
end;

class function ThreadPool.ID: TGuid;
begin
  Result := CThreadPool;
end;

{ TCachedThread }

constructor TCachedThread.Create(ThreadPool: TThreadPool);
begin
  inherited Create(Self, true);
  FThreadPool := ThreadPool;
  FActivation := OS.IPC.Event();
  FSleeping := OS.IPC.Event();
end;

destructor TCachedThread.Destroy;
begin
  Kill;
  FSleeping := nil;
  inherited;
  FActivation := nil;
end;

procedure TCachedThread.DoExecute;
var
  Hook: IThreadHook;
begin
  if FTermination.WaitFor(0) <> wrSignaled then
  begin
    FFinalize := false;

    repeat
      FActivation.WaitFor(INFINITE);
      if FFinalize then Break;

      if (FRunnable <> nil) and (not DoGetHook(Hook) or Hook.Initialize(Self)) then
      begin
        Hook := nil;
        FSleeping.Reset;
        Runnable.Run(Self);

        if DoGetHook(Hook) then Hook.Finalize(Self);
        Detach;
        FSleeping.Signal;
      end else
      begin
        if DoGetHook(Hook) then Hook.Finalize(Self);
        Detach;
      end;
    until DoUpdateList;
  end;

  FThreadPool.Remove(ICachedThread(Self));
end;

function TCachedThread.DoUpdateList: Boolean;
begin
  FThreadPool.Locked;
  FActivation.Reset;
  Result := FThreadPool.DoUpdateList;
end;

function TCachedThread.WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean): TSyncWaitResult;
begin
  if (FSleeping <> nil) and not FFinalize then
    Result := FSleeping.WaitFor(Timeout, AvoidMsgLock) else
    Result := inherited WaitFor(Timeout, AvoidMsgLock);
end;

procedure TCachedThread.Kill;
begin
  FFinalize := true;
  FActivation.Signal;
  FTermination.Signal;
  Resume;
end;

function TCachedThread.GetActivation: IEvent;
begin
  Result := FActivation;
end;

function TCachedThread.GetTermination: IIpcObject;
begin
  Result := FActivation;
end;

{ TThreadPool }

function TThreadPool.GetCacheSize: LongWord;
begin
  Result := FCacheSize;
end;

function TThreadPool.DoUpdateList(NewCount: Integer): Boolean;
var
  e: IEnumerator;
  Thread: ICachedThread;
begin
  Result := false;
  if NewCount < 0 then NewCount := FCacheSize;

  if NewCount < Count then
  begin
    while Enumerate(e, Thread) do
      if Thread.Activation.WaitFor(0) = wrTimeout then
      begin
        Result := true;
        Thread.Kill;
        Thread := nil;
        if NewCount >= Count then Break;
      end;
  end else
    while NewCount > Count do DoCreateNewThread;
end;

function TThreadPool.DoCreateNewThread: ICachedThread;
begin
  Result := TCachedThread.Create(Self);
  Add(Result);
end;

procedure TThreadPool.SetCacheSize(Value: LongWord);
begin
  if FCacheSize <> Value then
  begin
    DoUpdateList(Value);
    FCacheSize := Value
  end;
end;

function TThreadPool.GetRemoveTimeout: LongWord;
begin
  Result := FRemoveTimeout;
end;

procedure TThreadPool.SetRemoveTimeout(Value: LongWord);
begin
  FRemoveTimeout := Value;
end;

function TThreadPool.DoGetInactiveThread: ICachedThread;
var
  e: IEnumerator;
begin
  while Enumerate(e, Result) do
    if Result.Activation.WaitFor(0) = wrTimeout then
      Exit;

  Result := DoCreateNewThread;
end;

function TThreadPool.Spawn(const Name: String; const Runnable: IRunnable; Suspended: Boolean = false): IThread; 
var
  Thread: ICachedThread;
begin
  Locked;
  Thread := DoGetInactiveThread;
  Thread.Name := Name;
  Thread.Runnable := Runnable;
  if Suspended and not Thread.IsSuspended then Thread.Suspend;
  Thread.Activation.Signal;
  if not Suspended then Thread.Resume;
  Result := Thread;
end;

function TThreadPool.Spawn(const Runnable: IRunnable; Suspended: Boolean): IThread;
begin
  Result := Spawn(Str.Null, Runnable, Suspended);
end;

class function TThreadPool.Global: IThreadManager;
begin
  if not Assigned(MThreadPool) then
    SilLtGlobal.Global.Services.Get(ThreadPool, IThreadManager, @MThreadPool);
  Result := MThreadPool;
end;

constructor TThreadPool.Create;
begin
  inherited Create(true);
  FEmpty := OS.IPC.Event(true, true);
end;

destructor TThreadPool.Destroy;
begin
  SetCacheSize(0);
  FEmpty.WaitFor(INFINITE, true);
  SilLtLock.Lock.Take(Self);
  inherited;
end;

function TThreadPool.DoFirst: IThread;
begin
  Result := IThread(inherited First);
end;

function TThreadPool.DoGetItem(const Index: Variant): IThread;
var
  e: IEnumerator;
  sName: String;
begin
  if Vart.VType(Index) = varString then
  begin
    sName := Index;
    while Enumerate(e, Result) do
      if Result.Name = sName then Exit;
    Result := nil;
  end else
    Result := IThread(inherited GetItem(Index));
end;

function TThreadPool.DoLast: IThread;
begin
  Result := IThread(inherited Last);
end;

function TThreadPool.GetDestroying: Boolean;
begin
  Result := false;
end;

function TThreadPool.GetThreads: IThreads;
begin
  Result := Self;
end;

function TThreadPool.DoIndexOf(const Item: IThread): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TThreadPool.Add(const Item: IUnknown): Integer;
begin
  try
    Lock;
    Result := inherited Add(Item);
    if Count = 1 then FEmpty.Reset;
  finally
    UnLock;
  end;
end;

procedure TThreadPool.Delete(Index: Integer);
begin
  try
    Lock;
    inherited Delete(Index);
    if Count = 0 then FEmpty.Signal;
  finally
    UnLock;
  end;
end;

function TThreadPool.FindThreadID(ID: LongWord; out Thread: IThread): Boolean;
var
  i: Integer;
  Item: Pointer;
begin
  Result := false;

  try
    Lock;

    for i := 0 to Count - 1 do
    begin
      Item := ItemPtr(i);
      if IThread(Item).ThreadID = ID then
      begin
        Thread := IThread(Item);
        Result := true;
        Exit;
      end;
    end;
  finally
    Unlock;
  end;
end;

initialization
finalization
  MThreadPool := nil;

end.
