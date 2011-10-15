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

unit SilLmReadWriteLock;

{$I Defines.inc}

interface

{$I Defines.inc}

uses
  SilLkInterfaced,
  SilLiLock,
  SilLiReadWriteLock,
  SilOiIpc;

type
  TActiveThreadRecord = record
    ThreadID: Integer;
    RecursionCount: Integer;
  end;
  TActiveThreadArray = array of TActiveThreadRecord;

  TReadWriteLock = class (
    // extends
    TSilInterfacedObject,
    // implements
    IReadWriteLock,
    ILockable)
	private
    FLock: ILockable;
    FReadExit: IEvent;
    FCount: Integer;
    FSaveReadCount: Integer;
    FActiveThreads: TActiveThreadArray;
    FWriteRequestorID: Integer;
    FReallocFlag: Integer;
    FWriting: Boolean;
    function WriterIsOnlyReader: Boolean;
  protected // IReadWriteLock
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
	protected // ILockable
		procedure ILockable.Lock = BeginWrite;
		procedure ILockable.Unlock = EndWrite;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilAfLockedIncrement,
  SilOsTypes,
  SilOtTool;

{ TReadWriteLock }

constructor TReadWriteLock.Create;
begin
  inherited Create;

  FLock := OS.Ipc.CriticalSection();
  FReadExit := OS.Ipc.Event(true, true);
  SetLength(FActiveThreads, 4);
end;

destructor TReadWriteLock.Destroy;
begin
  BeginWrite;
  inherited;
end;

function TReadWriteLock.WriterIsOnlyReader: Boolean;
var
  I, Len: Integer;
begin
  Result := False;
  if FWriteRequestorID = 0 then Exit;
  I := 0;
  Len := High(FActiveThreads);
  while (I < Len) and
    ((FActiveThreads[I].ThreadID = 0) or (FActiveThreads[I].ThreadID = FWriteRequestorID)) do
    Inc(I);
  Result := I >= Len;
end;

procedure TReadWriteLock.BeginWrite;
begin
  FLock.Lock;
  if not FWriting then
  begin
    FWriteRequestorID := OS.Thread.ID;
    if not WriterIsOnlyReader then FReadExit.WaitFor(INFINITE);
    FSaveReadCount := FCount;
    FCount := 0;
    FWriteRequestorID := 0;
    FWriting := True;
  end;
  Inc(FCount);
end;

procedure TReadWriteLock.EndWrite;
begin
  Dec(FCount);
  if FCount = 0 then
  begin
    FCount := FSaveReadCount;
    FSaveReadCount := 0;
    FWriting := False;
  end;
  FLock.Unlock;
end;

procedure TReadWriteLock.BeginRead;
var
  I: Integer;
  ThreadID: Integer;
  ZeroSlot: Integer;
  AlreadyInRead: Boolean;
begin
  ThreadID := OS.Thread.ID;

  while LockedExc(FReallocFlag, ThreadID) <> 0 do OS.Wait.Sleep(0);

  try
    I := 0;
    while (I < High(FActiveThreads)) and (FActiveThreads[I].ThreadID <> ThreadID) do
      Inc(I);
    AlreadyInRead := I < High(FActiveThreads);
    if AlreadyInRead then
    begin
      if not FWriting then
      begin
        LockedInc(FCount);
        Inc(FActiveThreads[I].RecursionCount);
      end;
    end
  finally
    FReallocFlag := 0;
  end;
  if not AlreadyInRead then
  begin
    FLock.Lock;
    try
      if not FWriting then
      begin
        if LockedInc(FCount) = 1 then
          FReadExit.Reset;
        I := 0;
        ZeroSlot := -1;
        while (I < High(FActiveThreads)) and (FActiveThreads[I].ThreadID <> ThreadID) do
        begin
          if (FActiveThreads[I].ThreadID = 0) and (ZeroSlot < 0) then ZeroSlot := I;
          Inc(I);
        end;
        if I >= High(FActiveThreads) then
        begin
          if ZeroSlot < 0 then
          begin
            while LockedExc(FReallocFlag, ThreadID) <> 0 do OS.Wait.Sleep(0);
            try
              SetLength(FActiveThreads, High(FActiveThreads) + 3);
            finally
              FReallocFlag := 0;
            end;
          end
          else
            I := ZeroSlot;

          FActiveThreads[I].ThreadID := ThreadID;
          FActiveThreads[I].RecursionCount := 1;
        end
        else
          Inc(FActiveThreads[I].RecursionCount);
      end;
    finally
      FLock.Unlock;
    end;
  end;
end;

procedure TReadWriteLock.EndRead;
var
  I, ThreadID, Len: Integer;
begin
  if not FWriting then
  begin
    I := 0;
    ThreadID := OS.Thread.ID;
    while LockedExc(FReallocFlag, ThreadID) <> 0 do OS.Wait.Sleep(0);
    try
      Len := High(FActiveThreads);
      while (I < Len) and (FActiveThreads[I].ThreadID <> ThreadID) do Inc(I);

      Dec(FActiveThreads[I].RecursionCount);
      if FActiveThreads[I].RecursionCount = 0 then
        FActiveThreads[I].ThreadID := 0;
    finally
      FReallocFlag := 0;
    end;
    if (LockedDec(FCount) = 0) or WriterIsOnlyReader then
      FReadExit.Signal;    
  end;
end;

end.
