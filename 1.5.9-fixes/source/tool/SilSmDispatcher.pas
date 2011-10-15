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

unit SilSmDispatcher;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilOsTypes,
  SilOiThread,
  SilOiIpc,
  SilLiPointerQueue,
  SilLiReference,
  SilSiDispatcher;

type
  PDispatchBlock = ^RDispatchBlock;
  RDispatchBlock = packed record
    Msg: RDispatcherMessage;
    Event: IEvent;
    Dispatcher: IDispatchable;
  end;

  TSilDispatcher = class (TSilObject, IRunnable, IDispatcher)
  private
    FQueue: IPointerQueue;
    FThread: IThread;
    FStarted: IEvent;
  protected
    procedure Run(const Thread: IThread);
  protected
    function Send(const Dispatcher: IDispatchable; Id: Word; Param1: Integer; const Param2: IUnknown; Timeout: LongWord = INFINITE): Integer;
    function Post(const Dispatcher: IDispatchable; Id: Word; Param1: Integer; const Param2: IUnknown): Boolean;
    procedure Terminate;
  public
    constructor Create(const Name: String);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilLtTrace,
  SilLtList,
  SilOtTool,
  SilOtIpc;

{ TSilDispatcher }

constructor TSilDispatcher.Create(const Name: String);
begin
  inherited Create;
  FQueue := ListTool.PointerQueue;
  FThread := OS.Thread.Spawn(Str.Iif(Str.IsAssigned(Name), Name, 'dispatcher'), Self);
  FStarted := OS.Ipc.Event;
end;

destructor TSilDispatcher.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TSilDispatcher.Terminate;
begin
  if Assigned(FQueue) then
  begin
    FQueue.Cancel;
    FThread.Termination.WaitFor(INFINITE, true);

    FThread := nil;
    FStarted := nil;
    FQueue := nil;
  end;
end;

function DoSetMsg(const Dispatcher: IDispatchable; Id: Word; Param1: Integer; const Param2: IInterface): PDispatchBlock;
begin
  New(Result);
  Result.Msg.Id := Id;
  Result.Msg.Param1 := Param1;
  Result.Msg.Param2 := Param2;
  Result.Dispatcher := Dispatcher;
end;

function TSilDispatcher.Post(const Dispatcher: IDispatchable; Id: Word; Param1: Integer; const Param2: IInterface): Boolean;
var
  Block: PDispatchBlock;
begin
  Result := Assigned(FThread);
  if not Result then Exit;

  Block := DoSetMsg(Dispatcher, Id, Param1, Param2);
  Result := FQueue.Put(Block);
  if not Result then Dispose(Block);
end;

function TSilDispatcher.Send(const Dispatcher: IDispatchable; Id: Word; Param1: Integer; const Param2: IInterface; Timeout: LongWord): Integer;
var
  Block: PDispatchBlock;
  Event: IEvent;
begin
  Result := 0;
  if not Assigned(FThread) then Exit;

  Event := OS.Ipc.Event;

  Block := DoSetMsg(Dispatcher, Id, Param1, Param2);
  Block.Event := Event;
  Block.Msg.Result := @Result;

  if FQueue.Put(Block) then
    Event.WaitFor(Timeout) else
    Dispose(Block);              
end;

procedure TSilDispatcher.Run(const Thread: IThread);
var
  Block: PDispatchBlock;
begin
  while FQueue.Get(Block) do
  try
    try
      Block.Dispatcher.Dispatch(Block.Msg);
    finally
      try
        if Assigned(Block.Event) then Block.Event.Signal;
      except
        Trace.Exception('TSilDispatcher.Run: Event.Signal');
      end;
      Dispose(Block);
    end;
  except
    Trace.Exception('TSilDispatcher.Run');
  end;
end;

end.
