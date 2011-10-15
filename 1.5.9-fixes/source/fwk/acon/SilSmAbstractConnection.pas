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

unit SilSmAbstractConnection;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLmInterfaceList,
  SilLmPointerList,
  SilSmThreadPool,
  SilSmPacketReader,
  SilSiAbstractConnection,
  SilLiLock,
  SilLiConnection,
  SilLiFiler,
  SilLiList,
  SilLiEventList,
  SilLiStream,
  SilOiThread,
  SilOiTimer,
  SilOiIpc;

type

{ TAbstractConnection }
     
  TAbstractConnection = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAbstractConnection,
    IRunnable,
    IThreadHook,
    ITimerEvents)
  private
    procedure DoTimerStart(const Sender: IUnknown; Param: Pointer);
    procedure DoTimerStop;
  protected
    FConnected: Boolean;
    FConnectionLost: Boolean;
    FBuffer: String;
    FPacketReader: IPbPacketReader;
    FPacketCompletion: IPacketCompletion;
    FThreadContext: IThread;
    FDataEvents: Boolean;
    FReader: IStream;
    FWriter: IStream;
    FReady: IEvent;
    FTerminated: IEvent;
    FRetryTimer: ITimer;
    FRetryInterval: LongWord;
    FRetryCount: LongWord;
    FCopyEventsOnDisconnect: Boolean;
  protected
    procedure DoClearListeners; virtual;
    procedure DoRemoveFromList;
    procedure DoAddToList;
    procedure SetStream(const Reader: IStream; const Writer: IStream = nil);
  protected
    procedure FireConnected; virtual;
    procedure FireFailed; virtual;
    procedure FireDataReceived(Buffer: PChar; Size: LongWord); virtual;
    procedure FireDataSent(Buffer: PChar; Size: LongWord); virtual;
    procedure FireDisconnected; virtual;
    procedure FireConnectionRetry(var Cancel: Boolean); virtual;
    procedure FireConnecting; virtual;
  protected // IAbstractConnection
    function GetConnected: Boolean; virtual;
    function GetPacketCompletion: IPacketCompletion;
    procedure SetPacketCompletion(const Value: IPacketCompletion);
    function GetBufferSize: LongWord; virtual;
    function GetReady: IEvent;
    function GetRetryInterval: LongWord;
    procedure SetRetryInterval(Value: LongWord);
    procedure CopyEventsOnDisconnect(Value: Boolean);
    function GetRetryCount: LongWord;
    procedure SetRetryCount(Value: LongWord);
    function InternalBuffer: PChar; virtual;
    function Initialize: Boolean; virtual;
    function Connect(const ThreadName: string = ''): Boolean; virtual;
    procedure Disconnect(Wait: Boolean = false);
    procedure DoDisconnect; virtual; abstract;
    procedure Cleanup; virtual;
    function Thread: IThread;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    function ReadStr(var Buffer: String): LongWord; virtual;
    function WriteStr(const Buffer: String): LongWord; virtual;
    procedure SpawnThread(ThreadName: string = '');
    function WaitDataInput: Boolean;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IRunnable
    function IThreadHook.Initialize = InitializeThread;
    function InitializeThread(const Context: IThread): Boolean; virtual;
    procedure Finalize(const Context: IThread); virtual;
    procedure Suspended(const Context: IThread); virtual;
    procedure Resumed(const Context: IThread); virtual;
    procedure Run(const Context: IThread); virtual;
  protected // ITimerEvents
    procedure OnTick(const Event: RTimerEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TConnectionList }

  TConnectionList = class (
    // extends
    TSilInterfaceList,
    // implements
    IList,
    IConnectionList)
  private
    FAlone: IEvent;
    FThreadManager: IThreadManager;
  protected // IConnectionList
    function GetThreadManager: IThreadManager;
    procedure SetThreadManager(const Value: IThreadManager);
    procedure DisconnectAll;
    function First: IAbstractConnection;
    function Last: IAbstractConnection;
    function GetItem(Index: Integer): IAbstractConnection;
    function Add(const Item: IAbstractConnection): Integer; reintroduce;
    function IndexOf(const Item: IAbstractConnection): Integer; reintroduce;
    function Remove(const Item: IAbstractConnection): Integer; reintroduce;
  public
    constructor Create; 
    destructor Destroy; override;
  end;

function List: IConnectionList;

implementation

uses
  SilLtGlobal,
  SilLtTool,
  SilLtList,
  SilOtTool,
  SilStTool,
  SilLtReference,
  SilLiEnumerator,
  SilLiInterfaceList,
  SilBtStr,
  SilOsTypes;

const
  CConnectionList: TGUID = '{C402DCC3-1FEF-4AA4-A16A-962F45304C90}';

type
  ConnectionList = class(GlobalService)
    class function ID: TGuid; override; 
    class function Create: IUnknown; override; 
  end;    

{ ConnectionList }

class function ConnectionList.Create: IUnknown;
begin
  Result := TConnectionList.Create();  
end;

class function ConnectionList.ID: TGuid;
begin
  Result := CConnectionList;  
end;

var
  MList: IConnectionList = nil;

procedure CheckList;
begin
  if not Assigned(MList) then
    Global.Services.Get(ConnectionList, IConnectionList, @MList); 
end;

function List: IConnectionList;
begin
  CheckList;
  Result := MList;
end;

{ TAbstractConnection }

constructor TAbstractConnection.Create;
begin
  List;
  inherited Create;
  FReady := OS.Ipc.Event();
  FTerminated := OS.Ipc.Event(true, true);
  FRetryCount := 0;
  FCopyEventsOnDisconnect := true;

  DoAddToList; //!
end;

destructor TAbstractConnection.Destroy;
begin
  DoTimerStop;
  Disconnect;
  DoRemoveFromList;

  if Assigned(FThreadContext) then
    FThreadContext.Detach;

  FReader := nil;
  FWriter := nil;

  inherited;
end;

function TAbstractConnection.GetConnected: Boolean;
begin
  Result := false;
end;

function TAbstractConnection.Connect(const ThreadName: string): Boolean;
begin
  Result := true;
  SpawnThread(ThreadName);
end;

function TAbstractConnection.GetPacketCompletion: IPacketCompletion;
begin
  Result := FPacketCompletion;
end;

procedure TAbstractConnection.SetPacketCompletion(const Value: IPacketCompletion);
begin
  if Assigned(Value) then
  begin
    FPacketCompletion := Value;
    if Assigned(FReader) then
      FPacketReader := TPacketReader.Create(FReader, Value, GetBufferSize) else
      FPacketReader := nil;
  end else
  begin
    FPacketReader := nil;
    FPacketCompletion := nil;
  end;
end;

function TAbstractConnection.GetBufferSize: LongWord;
begin
  Result := 8012;
end;

procedure TAbstractConnection.DoAddToList;
begin
  CheckList;
  if MList.IndexOf(Self) = -1 then MList.Add(Self);
end;

procedure TAbstractConnection.DoRemoveFromList;
begin
  CheckList;
  if MList <> nil then MList.Remove(Self);
end;

procedure TAbstractConnection.Disconnect(Wait: Boolean);
begin
  FConnected := false;
  FConnectionLost := false;

  DoDisconnect;

  if Wait then
    OS.Wait.Single(FTerminated, INFINITE, true);
end;

function TAbstractConnection.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := 0;

  if {FConnected and} (FReader <> nil) then
  begin
    Result := FReader.Read(Buffer, Count);
    if (Result = LongWord(-1)) and (Count <> Result) then Result := 0;
    if FPacketReader = nil then FireDataReceived(PChar(@Buffer), Result);
  end;
end;

function TAbstractConnection.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := 0;

  if FConnected and (FWriter <> nil) then
  begin
    try
      Lock;
      Result := FWriter.Write(Buffer, Count);
      if (Result = LongWord(-1)) and (Count <> Result) then Result := 0;
    finally
      Unlock;
      FireDataSent(PChar(@Buffer), Result);
    end;
  end;
end;

procedure TAbstractConnection.AddListener(const Listener: IUnknown; KeepRef: Boolean);
begin
  inherited;
  if not FDataEvents and Reference.Supports(Listener, IConnectedEvents) then FDataEvents := true;
end;

procedure TAbstractConnection.RemoveListener(const Listener: IUnknown);
var
  e: IEnumerator;
  Item: IUnknown;
begin
  inherited;

  if Events <> nil then
  begin
    while Events.Enumerate(e, Item, IConnectedEvents) do
    begin
      FDataEvents := true;
      Break;
    end;
  end else
    FDataEvents := false;
end;

procedure TAbstractConnection.FireConnected;
var
  n: IEnumerator;
  l: IConnectingEvents;
  Event: TConnectionEvent;
  Events: IEvents;
begin
  if not HasConnections then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  Events.Enumerate(n, l, IConnectingEvents);
  n := nil;
  if l <> nil then l.OnConnected(Event);
end;

procedure TAbstractConnection.FireFailed;
var
  n: IEnumerator;
  l: IConnectingEvents;
  Event: TConnectionFailedEvent;
  Events: IEvents;
begin
  if not HasConnections then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  while Events.Enumerate(n, l, IConnectingEvents) do l.OnFailed(Event);
end;

procedure TAbstractConnection.FireConnecting;
var
  n: IEnumerator;
  l: IConnectionTrialEvents;
  Event: TConnectionEvent;
  Events: IEvents;
begin
  if not HasConnections then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  while Events.Enumerate(n, l, IConnectionTrialEvents) do l.OnConnecting(Event);
end;

procedure TAbstractConnection.FireConnectionRetry(var Cancel: Boolean);
var
  n: IEnumerator;
  l: IConnectionTrialEvents;
  Event: RConnectionRetryEvent;
  Events: IEvents;
begin
  Cancel := false;
  if not HasConnections then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  Event.CancelRetry := false;
  while Events.Enumerate(n, l, IConnectionTrialEvents) do l.OnRetryConnect(Event);
  Cancel := Event.CancelRetry;
end;

procedure TAbstractConnection.FireDataReceived(Buffer: PChar; Size: LongWord);
var
  n: IEnumerator;
  l: IConnectedEvents;
  Event: TConnectionDataEvent;
  Events: IEvents;
begin
  if not HasConnections or not FDataEvents then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  Event.Buffer := Buffer;
  Event.Size := Size;
  while Events.Enumerate(n, l, IConnectedEvents) do l.OnDataReceived(Event);
end;

procedure TAbstractConnection.FireDataSent(Buffer: PChar; Size: LongWord);
var
  n: IEnumerator;
  l: IConnectedEvents;
  Event: TConnectionDataEvent;
  Events: IEvents;
begin
  if not HasConnections or not FDataEvents then Exit;
  Events := Self.Events;

  Event.Sender := Self;
  Event.Buffer := Buffer;
  Event.Size := Size;
  while Events.Enumerate(n, l, IConnectedEvents) do l.OnDataSent(Event);
end;

procedure TAbstractConnection.FireDisconnected;
var
  n: IEnumerator;
  l: IConnectedEvents;
  Event: TConnectionBreakEvent;
  Events: IEventList;
begin
  if not HasConnections or not FDataEvents then Exit;

  if FCopyEventsOnDisconnect then
  begin
    Events := ListTool.EventList;

    with Self.Events.Locked do
    begin
      Events.AddList(Self.Events);
      Release;
    end;
  end else
    Events := Self.Events;

  Event.Sender := Self;
  Event.ConnectionLost := FConnectionLost;
  while Events.Enumerate(n, l, IConnectedEvents) do
    l.OnDisconnected(Event);
end;

function TAbstractConnection.Initialize: Boolean;
begin
  Result := false;
end;

function TAbstractConnection.ReadStr(var Buffer: String): LongWord;
var
  lwSize: LongWord;
begin
  if {FConnected and} (FReader <> nil) then
  begin
    lwSize := FReader.Size;
    if lwSize < 1 then lwSize := GetBufferSize;

    if FPacketReader <> nil then
    begin
      FPacketReader.BufferSize := lwSize;

      if not FPacketReader.Read(FBuffer) then
      begin
        FBuffer := Str.Null;
        Result := 0;
      end else
        Result := Length(FBuffer);

      FireDataReceived(PChar(FBuffer), Result);
    end else
    begin
      SetLength(FBuffer, lwSize);
      Result := Read(FBuffer[1], Length(FBuffer));
    end;

    if Result < 1 then FBuffer := Str.Null else
    if Result < Str.Len(FBuffer) then SetLength(FBuffer, Result);
  end else
    Result := 0;

  Buffer := FBuffer;
end;

function TAbstractConnection.WriteStr(const Buffer: String): LongWord;
begin
  Result := Write(Buffer[1], Length(Buffer));
end;

function TAbstractConnection.InternalBuffer: PChar;
begin
  Result := PChar(FBuffer);
end;

function TAbstractConnection.Thread: IThread;
begin
  Result := FThreadContext;
end;

procedure TAbstractConnection.SpawnThread(ThreadName: string);
begin
  CheckList;
  
  if FThreadContext = nil then
    //if MList <> nil then
      //FThreadContext := MList.ThreadManager.Spawn(ThreadName, Self) else
      FThreadContext := OS.Thread.Spawn(ThreadName, Self);
end;

function TAbstractConnection.WaitDataInput: Boolean;
var
  sBuffer: String;
begin
  Result := ReadStr(sBuffer) > 0;
end;

function TAbstractConnection.InitializeThread(const Context: IThread): Boolean;
begin
  FTerminated.Reset;

  FConnectionLost := true;
  FireConnecting;
  Result := Initialize;

  if not Result then
  begin
    FTerminated.Signal;
    FireFailed;
    DoRemoveFromList;
  end;
end;

procedure TAbstractConnection.Run(const Context: IThread);
begin
  FReady.Signal;
  FConnected := true;
  FireConnected;

  while GetConnected do
    WaitDataInput;

  FReady.Reset;
  FireDisconnected;
end;

procedure TAbstractConnection.Finalize(const Context: IThread);
var
  bCancel, bLost: Boolean;
begin
  bLost := FConnectionLost;
  if FConnected then Disconnect;
  FThreadContext := nil;

  if (FRetryInterval <> 0) and bLost then
  begin
    FireConnectionRetry(bCancel);
    //bCancel := false;
    if not bCancel then
    begin
      OS.Thread.SyncCall(DoTimerStart);
      Exit;
    end;
  end;

  FTerminated.Signal;
  DoRemoveFromList;
end;

procedure TAbstractConnection.Suspended(const Context: IThread);
begin
end;

procedure TAbstractConnection.Resumed(const Context: IThread);
begin
end;

procedure TAbstractConnection.DoClearListeners;
begin
  if Events <> nil then Events.Clear;
end;

function TAbstractConnection.GetReady: IEvent;
begin
  Result := FReady;
end;

procedure TAbstractConnection.Cleanup;
begin
  DoTimerStop;
  DoClearListeners;
end;

procedure TAbstractConnection.DoTimerStop;
begin
  FRetryInterval := 0;
  FRetryTimer := nil;
end;

procedure TAbstractConnection.DoTimerStart(const Sender: IUnknown; Param: Pointer);
begin
  FRetryTimer := OS.Timer.Create(1, FRetryInterval, Self, false);
  if FRetryCount > 0 then FRetryTimer.TickCount := FRetryCount;
  FRetryTimer.Enabled := true;
end;

procedure TAbstractConnection.OnTick(const Event: RTimerEvent);
begin
  FRetryTimer := nil;
  if not GetConnected then Connect;
end;

function TAbstractConnection.GetRetryInterval: LongWord;
begin
  Result := FRetryInterval;
end;

procedure TAbstractConnection.SetRetryInterval(Value: LongWord);
begin
  FRetryInterval := Value;
end;

function TAbstractConnection.GetRetryCount: LongWord;
begin
  Result := FRetryCount;
end;

procedure TAbstractConnection.SetRetryCount(Value: LongWord);
begin
  FRetryCount := Value;
end;

procedure TAbstractConnection.SetStream(const Reader, Writer: IStream);
begin
  FReader := Reader;
  if Writer = nil then
    FWriter := FReader else
    FWriter := Writer;
end;

procedure TAbstractConnection.CopyEventsOnDisconnect(Value: Boolean);
begin
  FCopyEventsOnDisconnect := Value;
end;

{ TConnectionList }

constructor TConnectionList.Create;
begin
  inherited Create(true);
  FThreadManager := TThreadPool.Global;
  FAlone := OS.IPC.Event(true, true);
end;

destructor TConnectionList.Destroy;
begin
  DisconnectAll;
  FThreadManager := nil;
  inherited;
end;

procedure TConnectionList.DisconnectAll;
var                                
  e: IEnumerator;
  i: IAbstractConnection;
begin
  while Enumerate(e, i) do
  begin
    i.Cleanup;
    if i.Thread <> nil then i.Disconnect else Remove(i);
  end;

  FAlone.WaitFor(INFINITE, true);
end;

function TConnectionList.First: IAbstractConnection;
begin
  Result := GetItem(0);
end;

function TConnectionList.GetItem(Index: Integer): IAbstractConnection;
begin
  Result := IAbstractConnection(inherited GetItem(Index));
end;

function TConnectionList.GetThreadManager: IThreadManager;
begin
  Result := FThreadManager;
end;

function TConnectionList.Add(const Item: IAbstractConnection): Integer;
begin
  Locked;
  Result := inherited Add(Item);
  if Count > 0 then FAlone.Reset;
end;

function TConnectionList.Remove(const Item: IAbstractConnection): Integer;
begin
  Locked;
  Result := inherited Remove(Item);
  if Count = 0 then FAlone.Signal;
end;

function TConnectionList.IndexOf(const Item: IAbstractConnection): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TConnectionList.Last: IAbstractConnection;
begin
  Result := GetItem(Count - 1);
end;

procedure TConnectionList.SetThreadManager(const Value: IThreadManager);
begin
  FThreadManager := Value;
end;

end.
