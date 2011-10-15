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

unit SilOkEventQueue;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilLiReference,
  SilLkInterfaced,

  SilLiConnection,
  SilLiEventList,
  SilOiHandle,
  SilOiIpc,
  SilOiThread,
  SilOiEventQueue;

type
  IEventPacket = interface
    ['{CBEB43C7-2E89-4247-B4F1-D0701E6DC376}']
    function GetEvent: TEvent;
    function GetParams: TEventParams;
    property Event: TEvent read GetEvent;
    property Params: TEventParams read GetParams;
  end;

  TSilEventQueue = class (
    // extends
    TSilInterfacedObject,
    // implements
    IEventQueue,
    IRunnable,
    IConnectable,
    IThreadHook)
  private
    FSink: Pointer;
    FInstance: Pointer;
    FCreationThread: IThread;
    FProcessingThread: IThread;
    FSynchronize: Boolean;
    FThreadHook: Pointer;
    FStarted: IEvent;
    FInitialized: Boolean;
    function DoGetInstance: IReferenceable;
    function GetSink: IEventSink;
  private
    function DoCallInitialize(const Thread: IThread): Boolean;
    procedure DoCallFinalize(const Thread: IThread);
    procedure DoCallSuspended(const Thread: IThread);
    procedure DoCallResumed(const Thread: IThread);
    //procedure DoCallProcess(const Sender: IUnknown; const Ref);
    function DoGetConnectable: IConnectable;
  protected // IEventQueue
    function Post(Event: TEvent; Params: array of Variant): Boolean;
    function Send(Event: TEvent; Params: array of Variant; Timeout: LongWord): Integer;
    procedure Cancel;
    function GetThread: IThread;
  protected // IRunnable
    procedure IRunnable.Run = DoThreadRun;
  protected // IThreadHook
    function Initialize(const Thread: IThread): Boolean;
    procedure Finalize(const Thread: IThread);
    procedure Suspended(const Thread: IThread);
    procedure Resumed(const Thread: IThread);
  protected
    property Connectable: IConnectable read DoGetConnectable implements IConnectable;
  protected
    procedure DoThreadRun(const Thread: IThread); virtual; abstract;
    function DoThreadInitialize(const Thread: IThread): Boolean; virtual; abstract;
    procedure DoThreadFinalize(const Thread: IThread); virtual; abstract;
    procedure DoThreadSuspended(const Thread: IThread); virtual;
    procedure DoThreadResumed(const Thread: IThread); virtual;
    procedure DoThreadTerminate; virtual; abstract;
    function DoPostPacket(const Packet: IEventPacket): Boolean; virtual; abstract;
    function DoSendPacket(const Packet: IEventPacket; Timeout: Longword): Integer; virtual; abstract;
  protected
    function DoProcess(const Packet: IEventPacket): Integer;
  public
    constructor Create(const ASink: IUnknown; Synchronize: Boolean; const ThreadName: String); overload; virtual;
    destructor Destroy; override;
    property Sink: IEventSink read GetSink;
    property Instance: IReferenceable read DoGetInstance;
  end;

  TEventPacket = class(
    TSilInterfacedObject,
    IEventPacket )
  private
    FEvent: TEvent;
    FParams: TEventParams;
  protected  // IEventPacket
    function GetEvent: TEvent;
    function GetParams: TEventParams;
  public
    constructor Create(AEvent: TEvent; const Params: array of Variant);
    destructor Destroy; override;
    property Event: TEvent read GetEvent;
    property Params: TEventParams read GetParams;
  end;

implementation

uses
  SilBtStr,
  SilLtLock,
  SilLtReference,
  SilOtTool;

{ TSilEventQueue }

constructor TSilEventQueue.Create(const ASink: IUnknown; Synchronize: Boolean; const ThreadName: String);
var
  Hook: IThreadHook;
  Instance: IReferenceable;
  Sink: IEventSink;
begin
  inherited Create;

  FStarted := OS.IPC.Event();

  if Ref.GetInterface(ASink, IEventSink, Sink) then
    FSink := Pointer(Sink) else
  if Ref.GetInterface(ASink, IReferenceable, Instance) then
    FInstance := Pointer(Instance);

  if Ref.GetInterface(ASink, IThreadHook, Hook) then
    FThreadHook := Pointer(Hook);

  Self.Locked;

  FSynchronize := Synchronize;
  FCreationThread := Os.Thread.Current;
  FProcessingThread := OS.Thread.Spawn(Str.IIf(Str.IsEmpty(ThreadName), ClassName, ThreadName), Self);

  FStarted.WaitFor(INFINITE, true);
end;

destructor TSilEventQueue.Destroy;
begin
  Self.Locked;

  FThreadHook := nil;

  FSink := nil;
  FProcessingThread := nil;
  inherited;
end;

function TSilEventQueue.Post(Event: TEvent; Params: array of Variant): Boolean;
begin
  Self.Locked;
  Result := FInitialized and DoPostPacket(TEventPacket.Create(Event, Params));
end;

function TSilEventQueue.Send(Event: TEvent; Params: array of Variant; Timeout: LongWord): Integer;
begin
  Self.Locked;
  if FInitialized then
    Result := DoSendPacket(TEventPacket.Create(Event, Params), Timeout) else
    Result := 0;
end;

procedure TSilEventQueue.Cancel;
var
  Temp: ILock;
begin
  Temp := Self.Locked;
  DoThreadTerminate();
  Temp.Release;

  if FProcessingThread <> nil then
  begin
    OS.Wait.Single(FProcessingThread.Termination, INFINITE, True);
    FProcessingThread.Detach;
    FProcessingThread := nil;
  end;
end;

function TSilEventQueue.Initialize(const Thread: IThread): Boolean;
begin
  Result := DoThreadInitialize(Thread);
  if Result then
    Result := DoCallInitialize(Thread);

  FInitialized := Result;

  FStarted.Signal;
end;

procedure TSilEventQueue.Finalize(const Thread: IThread);
begin
  DoCallFinalize(Thread);

  Self.Locked;

  DoThreadFinalize(Thread);
  FThreadHook := nil;
  FInitialized := False;

  //Thread.Termination.Signal;
end;

procedure TSilEventQueue.Resumed(const Thread: IThread);
begin
  DoThreadResumed(Thread);
  DoCallResumed(Thread);
end;

procedure TSilEventQueue.Suspended(const Thread: IThread);
begin
  DoThreadSuspended(Thread);
  DoCallSuspended(Thread);
end;

function TSilEventQueue.DoCallInitialize(const Thread: IThread): Boolean;
begin
  if FThreadHook <> nil then
    Result := IThreadHook(FThreadHook).Initialize(Thread) else
    Result := True;
end;

procedure TSilEventQueue.DoCallFinalize(const Thread: IThread);
begin
  if FThreadHook <> nil then
    IThreadHook(FThreadHook).Finalize(Thread);
end;

procedure TSilEventQueue.DoCallResumed(const Thread: IThread);
begin
  if FThreadHook <> nil then
    IThreadHook(FThreadHook).Resumed(Thread);
end;

procedure TSilEventQueue.DoCallSuspended(const Thread: IThread);
begin
  if FThreadHook <> nil then
    IThreadHook(FThreadHook).Suspended(Thread);
end;

function TSilEventQueue.GetThread: IThread;
begin
  Result := FProcessingThread;
end;

function TSilEventQueue.DoGetConnectable: IConnectable;
begin
  Result := FProcessingThread as IConnectable;
end;

procedure TSilEventQueue.DoThreadResumed(const Thread: IThread);
begin
end;

procedure TSilEventQueue.DoThreadSuspended(const Thread: IThread);
begin
end;

function TSilEventQueue.DoProcess(const Packet: IEventPacket): Integer;
var
  Msg: TEventQueueMessage;
begin
  Msg.Event := Packet.Event;
  Msg.Params := Packet.Params;
  Msg.Result := 0;
{
  if FSynchronize then
    FCreationThread.SyncCall(DoCallProcess, Msg) else
    FProcessingThread.SyncCall(DoCallProcess, Msg);
}
  if Assigned(FSink) then
    Sink.Process(Msg.Event, Msg.Params, Msg.Result) else
  if Assigned(FInstance) then
    Instance.Dispatch(Msg);

  Result := Msg.Result;
end;

{procedure TSilEventQueue.DoCallProcess(const Sender: IUnknown; const Ref);
var
  Msg: ^TEventQueueMessage;
begin
  Msg := @Ref;

  if Assigned(FSink) then
    Sink.Process(Msg.Event, Msg.Params, Msg.Result) else
  if Assigned(FInstance) then
    Instance.Dispatch(Msg^);
end;}

function TSilEventQueue.DoGetInstance: IReferenceable;
begin
  Result := IReferenceable(FInstance);
end;

function TSilEventQueue.GetSink: IEventSink;
begin
  Result := IEventSink(FSink);
end;

{ TEventPacket }

constructor TEventPacket.Create(AEvent: TEvent; const Params: array of Variant);
var
  i: Integer;
begin
  inherited Create;
  FEvent := AEvent;
  SetLength(FParams, Length(Params));
  for i := 0 to High(Params) do FParams[i] := Params[i];
end;

destructor TEventPacket.Destroy;
begin
  SetLength(FParams, 0);
  inherited;
end;

function TEventPacket.GetEvent: TEvent;
begin
  Result := FEvent;
end;

function TEventPacket.GetParams: TEventParams;
begin
  Result := FParams;    
end;

end.
