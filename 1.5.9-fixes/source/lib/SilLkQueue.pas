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

unit SilLkQueue;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLkInterfaced,
  SilLiLinkedList,
  SilLiQueue,
  SilOiWait,
  SilOiIpc;

type
  RQueueParams = record
    InitialCount: Integer;
    MaxCount: Integer;
    MaxSize: Integer;
  end;

  TSilQueue = class(
    TSilInterfacedObject,
    IQueue )
  private
    FItems: ILinkedList;
    FSemaphore: ISemaphore;
    FParams: RQueueParams;
    FCanceled: Boolean;
  protected
    procedure SetMaxSize(const Value: Integer);
    function GetMaxSize: Integer;
    function Put(const Item): Boolean; virtual;
    function Get(out Item; TimeOut: LongWord; AvoidMsgLock: Boolean): Boolean; virtual;
    procedure Cancel(Clear: Boolean = false); overload;
    procedure Cancel(out List: ILinkedList); overload;
    procedure Reset;
  protected
    function Top(out Item): Boolean;
  public
    constructor Create(const InitialCount: Integer; const MaxCount: Integer; TypeHandler: HandlerType);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtTool,
  SilLtList,
  SilOtTool,
  SilLtReference;

{ TSilQueue }

constructor TSilQueue.Create(const InitialCount, MaxCount: Integer; TypeHandler: HandlerType);
begin
  inherited Create;

  FParams.InitialCount := InitialCount;
  FParams.MaxCount := MaxCount;
  FParams.MaxSize := 0;
  
  FItems := ListTool.LinkedList(false, TypeHandler);

  Reset;
end;

destructor TSilQueue.Destroy;
begin
  Cancel(True); 
  FItems := nil;
  inherited;
end;

procedure TSilQueue.Cancel(out List: ILinkedList); 
begin
  Locked; 

  List := FItems;
  FCanceled := true;
  
  if Assigned(FSemaphore) then
  begin
    FSemaphore.Release;
    FSemaphore := nil;
  end;
end;

procedure TSilQueue.Cancel(Clear: Boolean);
var
  List: ILinkedList;
begin
  Locked;

  Cancel(List);

  if Clear then
    FItems.Clear;
end;

function TSilQueue.GetMaxSize: Integer;
begin
  Result := FParams.MaxSize;
end;

procedure TSilQueue.Reset;
var
  InitialCount: Integer; 
begin
  Locked;

  if Assigned(FSemaphore) then
    Cancel(false);

  if not Assigned(FSemaphore) then
  begin
    if Assigned(FItems) and (FItems.Count > 0) then
      InitialCount := FItems.Count else
      InitialCount := FParams.InitialCount;

    FSemaphore := Os.Ipc.Semaphore(InitialCount, FParams.MaxCount);
  end;

  FCanceled := false;
end;

procedure TSilQueue.SetMaxSize(const Value: Integer);
begin
  FParams.MaxSize := Value;
end;

function TSilQueue.Put(const Item): Boolean;
var
  Ptr: Pointer;
begin
  Locked;
  Result := Assigned(FSemaphore) and not FCanceled;

  if Result then
  begin
    FItems.Add(Item);

    if (FParams.MaxSize > 0) and (FItems.Count > FParams.MaxSize) then
    begin
      FItems.GetFirst(Ptr);
      FItems.Remove(Ptr);
    end else
      FSemaphore.Release;
  end;
end;

function TSilQueue.Get(out Item; TimeOut: LongWord; AvoidMsgLock: Boolean): Boolean;
var
  Ref: IUnknown;
  Semaphore: ISemaphore;
begin
  Semaphore := FSemaphore;
  Ref := Self;
  Result := False;

  if not Assigned(Semaphore) then
    Exit;
  
  if not FCanceled then
    Semaphore.WaitFor(TimeOut, AvoidMsgLock);

  Locked;
  Result := FItems.Count > 0;

  if Result and FItems.GetFirst(Item) then
    FItems.Remove(Item);
end;

function TSilQueue.Top(out Item): Boolean;
begin
  Locked;
  Result := (FItems.Count > 0) and FItems.GetFirst(Item);
end;

end.
