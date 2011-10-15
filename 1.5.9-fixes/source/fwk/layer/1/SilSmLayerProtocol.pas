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

unit SilSmLayerProtocol;

interface

{$include Defines.inc}

uses
  Sil,
  SilScLayer,
  SilSiLayer,
  SilSmLayer;

type
  RProtocolParams = record
    ReplyTimeout: LongWord;
    AsyncDispatch: Boolean;
  end;

  TSilProtocolLayer = class (
    TSilLayer,
    IRunnable,
    ILayerProtocol,
    IThreadEvents)
  private
    FThread: IThread;
    FReceiveThreadId: LongWord;
    FDispatch: IInterfaceQueue;
    FWaits: IInterfaceList;
    FLink: Pointer;
    FHook: IUnknown;
    FThreadHook: ILayerThreadHook;
    FForeignThread: IThread;
    FControl: ILayerProtocolControl;
    FParams: RProtocolParams;
    FInitialized: Boolean;
    procedure DoThreadInit;
    procedure DoThreadEnd;
    procedure DoThreadExcept(e: Exception);
  protected // IThreadEvents
    procedure OnEnter(const Thread: IThread);
    procedure OnExit(const Thread: IThread);
    procedure OnUnhandledException(const Thread: IThread; Error: Exception);
  protected // ILayerTerminal
    function GetLink: ILayerLink;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // ILayerControl
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
    procedure Receive(const Command: ILayerCommand); override;
  protected // ILayerProtocol
    function CreatePacket(Data: Integer = -1; Flags: LongWord = 0): IPacket; virtual;
    procedure SendPacket(const Packet: IPacket; const Context: IUnknown; const Params: IParameterList); overload; virtual;
    procedure SendPacket(const Packet: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList); overload; virtual;
    procedure SendPacket(const Packet: IPacket; out Source: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList); overload; virtual;
    procedure WaitPacket(const Request: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord);
    function ComparePacket(const Wait: ILayerWaitPacket; const Command: ILayerCommand): Boolean; virtual;
    function IsValidMessage(const Command: ILayerCommand): Boolean; virtual;
    function ExtractPacket(const Packet: IPacket): IPacket; virtual;
    procedure DispatchMessage(const Msg: ILayerCommand); virtual; abstract;
    function GetHook(out Hook: IUnknown): Boolean;
  protected
    function CreateWait(const Request: IPacket; Data: Integer): ILayerWaitPacket;
    procedure DoWait(const Wait: ILayerWaitPacket; Timeout: LongWord; out Source: IPacket; out Reply: IPacket);
    procedure DoCancelWaits;
  protected
    property ProtocolControl: ILayerProtocolControl read FControl;
    property Link: ILayerLink read GetLink;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

  EProtocolLayerError = class(Exception)
  private
    FPacket: IPacket;
  public
    constructor Create(const Msg: String; const Packet: IPacket);
    property Packet: IPacket read FPacket;
  end;

implementation

uses
  SilLayer, SilLiParameters;

type
  TSilWaitPacket = class (
    TSilObject,
    ILayerWaitPacket )
  private
    FEvent: IEvent;
    FRequest: IPacket;
    FData: Integer;
    FSource: IPacket;
    FPacket: IPacket;
  protected // ILayerWaitPacket
    function GetEvent: IEvent;
    function GetPacket: IPacket;
    procedure SetPacket(const Value: IPacket);
    function GetSource: IPacket;
    procedure SetSource(const Value: IPacket);
    function GetRequest: IPacket;
    function GetData: Integer;
    procedure Signal;
  protected 
    property Event: IEvent read GetEvent;
    property Packet: IPacket read GetPacket write SetPacket;
    property Source: IPacket read GetSource write SetSource;
    property Request: IPacket read GetRequest;
    property Data: Integer read GetData;
  public
    constructor Create(const Request: IPacket; Data: Integer);
    destructor Destroy; override;
  end;

{ TSilProtocolLayer }

constructor TSilProtocolLayer.Create(const Parameters: IParameters; const Controller: IInterface);
begin
  inherited;
  FParams.ReplyTimeout := 60000;
  FParams.AsyncDispatch := False;
end;

destructor TSilProtocolLayer.Destroy;
begin
  FThreadHook := nil;
  FForeignThread := nil;
  inherited;
end;

function TSilProtocolLayer.GetLink: ILayerLink;
begin
  Result := ILayerLink(FLink);
end;

procedure TSilProtocolLayer.OnEnter(const Thread: IThread);
begin
end;

procedure TSilProtocolLayer.OnExit(const Thread: IThread);
begin
  DoThreadEnd;
  Sil.Sink.Disconnect(FForeignThread, Self);
end;

procedure TSilProtocolLayer.OnUnhandledException(const Thread: IThread; Error: Exception);
begin
end;

procedure TSilProtocolLayer.DoThreadInit;
begin
  if FThreadHook <> nil then
    try
      FThreadHook.InitializeThread(Self);
    except
      on e: Exception do DoThreadExcept(e);
    end;
end;

procedure TSilProtocolLayer.DoThreadEnd;
begin
  if FThreadHook <> nil then
    try
      FThreadHook.FinalizeThread(Self);
    except
      on e: Exception do DoThreadExcept(e);
    end;
end;

procedure TSilProtocolLayer.DoThreadExcept(e: Exception);
begin
  if FThreadHook <> nil then
    try
      FThreadHook.UnhandledException(Self, e);
    except end;
end;

procedure TSilProtocolLayer.Run(const Thread: IThread);
var
  Command: ILayerCommand;
  Obj: ILayerProtocolControl;
begin
  try
    if Link.Chain.GetLast(ILayerProtocolControl, Obj) then
      Ref.GetInterface(Obj, ILayerThreadHook, FThreadHook);

    DoThreadInit;

    while FDispatch.Get(ILayerCommand, Command) do
      try
        try
          DispatchMessage(Command);
        finally
          Command := nil;
        end;
      except
        on e: Exception do
        begin
          DoThreadExcept(e);

          if Debug.Check(dlException, CDebugLayer) then
            Sil.Trace.Exception(ClassName + '.Run');
        end;
      end;

    DoThreadEnd;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.Run');
  end;
end;

procedure TSilProtocolLayer.LayerActivate(const Link: ILayerLink; const Context: IUnknown);
var
  Obj: IUnknown;
begin
  ChangeRef(Link, @FLink);
  FControl := nil;

  if Link.Chain.GetLast(ILayerProtocolControl, Obj) then
  begin
    Link.Chain.GetAfter(Obj, FHook);
    Ref.Get(Obj, ILayerProtocolControl, FControl);
  end else
  if Debug.Check(dlWarning, CDebugLayer) then
    Sil.Trace.Log(ClassName + '.LayerActivate no hook');

  if Assigned(FControl) then
  begin
    FControl.Activate(Self, Context);
    Parameters.Merge(FControl.Parameters);
  end;

  FParams.AsyncDispatch := Parameters.Get('AsyncDispatch', False);
  FParams.ReplyTimeout := Parameters.Get('ReplyTimeout', 60000);

  if not FInitialized then
  begin
    FInitialized := true;
    FWaits := Sil.List.InterfaceList(true);

    if FParams.AsyncDispatch then
    begin
      FDispatch := Sil.List.InterfaceQueue;
      FThread := Sil.OS.Thread.Spawn('LayProt', Self, true);
    end;
  end;

  if Assigned(FThread) and FThread.IsSuspended then FThread.Resume;
end;

procedure TSilProtocolLayer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  Sil.List.CancelQueue(FDispatch);
  DoCancelWaits;
  Sil.OS.Thread.Wait(FThread);

  if Assigned(FControl) then FControl.Deactivate(Self, Context);

  // FDispatch := nil; <-- dont nil, because it is used in another thread on asyncdispatch
  FThread := nil;

  FHook := nil;
  DropRef(@FLink);
  FWaits := nil;
  FInitialized := false;
end;

procedure TSilProtocolLayer.Receive(const Command: ILayerCommand);
var
  Enum: IEnumerator;
  Item: ILayerWaitPacket;
  Hybrid: ILayerCommand;
  Obj: ILayerProtocolControl;
begin
  Error.Check(FInitialized, ClassName + '.Receive: not initialized');

  if not FParams.AsyncDispatch and (FForeignThread = nil) then
  begin
    if Link.Chain.GetLast(ILayerProtocolControl, Obj) then
      Ref.GetInterface(Obj, ILayerThreadHook, FThreadHook);

    FForeignThread := Sil.OS.Thread.Current;
    Sil.Sink.Connect(FForeignThread, Self);
    DoThreadInit;
  end;

  FReceiveThreadId := Sil.OS.Thread.Current.ThreadID;
  Command.Packet.Buffer.Position := 0;

  if Debug.Check(dlInOut, CDebugLayer) then
    Sil.Trace.Log(ClassName + '.Receive: protocol=[%s]', [Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);

  if not IsValidMessage(Command) then Exit;

  try
    while FWaits.Enumerate(Enum, Item) do
      if ComparePacket(Item, Command) then
      begin
        FWaits.Delete(Enum.Iteration);

        Item.Source := Command.Packet;
        Item.Packet := ExtractPacket(Command.Packet);
        Item.Signal;

        Item := nil;
        Enum := nil;

        Exit;
      end;

    Hybrid := Cmd.Create(Command, Command.Packet);

    if FParams.AsyncDispatch then
      FDispatch.Put(Hybrid)
    else
      DispatchMessage(Hybrid);
  except
    on e: Exception do
    begin
      DoThreadExcept(e);

      if Debug.Check(dlException, CDebugLayer) then
        Sil.Trace.Exception(ClassName + '.Receive');
    end;
  end;
end;

function TSilProtocolLayer.CreatePacket(Data: Integer; Flags: LongWord): IPacket;
begin
  Result := Sil.Stream.Raw.Packet;
end;

procedure TSilProtocolLayer.SendPacket(const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
begin
  try
    if Assigned(Link) then
    begin
      Packet.Buffer.Position := 0;
      if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.SendPacket: protocol=[%s]', [Sil.Mem.Dump(Packet.Buffer.Memory, Packet.Buffer.Size, 1, '')]);
      Cmd.Write(Link, Packet, Context, Params);
    end else
      raise Sil.Error.Create('no se encontro el dispositivo');
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Log(ClassName + '.SendPacket packet');

    raise;
  end;
end;

procedure TSilProtocolLayer.SendPacket(const Packet: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList);
var
  Source: IPacket;
begin
  SendPacket(Packet, Source, Reply, Data, Timeout, Context, Params);
end;

procedure TSilProtocolLayer.SendPacket(const Packet: IPacket; out Source: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList);
var
  Wait: ILayerWaitPacket;
begin
  try
    if FReceiveThreadId = Sil.OS.Thread.Current.ThreadID then
      raise Error.Create('no se puede esperar respuesta en el thread que recibe');

    if Assigned(Packet) then
    begin
      Wait := CreateWait(Packet, Data);
      SendPacket(Packet, Context, Params);

      DoWait(Wait, Timeout, Source, Reply);
    end else
      raise Sil.Error.Create('paquete nil');
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Log(ClassName + '.SendPacket packet reply');

    raise;
  end;
end;

procedure TSilProtocolLayer.WaitPacket(const Request: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord);
var
  Source: IPacket;
  Wait: ILayerWaitPacket;
begin
  Wait := CreateWait(Request, Data);
  DoWait(Wait, Timeout, Source, Reply);
end;

function TSilProtocolLayer.ComparePacket(const Wait: ILayerWaitPacket; const Command: ILayerCommand): Boolean;
begin
  Result := true;
end;

function TSilProtocolLayer.IsValidMessage(const Command: ILayerCommand): Boolean;
begin
  Result := true;
end;

function TSilProtocolLayer.ExtractPacket(const Packet: IPacket): IPacket;
begin
  Result := Packet;
end;

function TSilProtocolLayer.GetHook(out Hook: IUnknown): Boolean;
begin
  Hook := FHook;
  Result := Assigned(Hook);
end;

function TSilProtocolLayer.CreateWait(const Request: IPacket; Data: Integer): ILayerWaitPacket;
begin
  Result := TSilWaitPacket.Create(Request, Data);
  if Assigned(FWaits) then FWaits.Add(Result);
end;

procedure TSilProtocolLayer.DoWait(const Wait: ILayerWaitPacket; Timeout: LongWord; out Source: IPacket; out Reply: IPacket);
begin
  try
    if (Timeout = INFINITE) and (FParams.ReplyTimeout <> INFINITE) then
      Timeout := FParams.ReplyTimeout;

    if Wait.Event.WaitFor(Timeout) = wrSignaled then
    begin
      Source := Wait.Source;
      Reply := Wait.Packet;
    end else
    begin
      FWaits.Remove(Wait);
      raise EProtocolLayerError.Create('tiempo de espera excedido', nil);
    end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Log(ClassName + '.DoWait');

    raise;
  end;
end;

procedure TSilProtocolLayer.DoCancelWaits;
var
  Enum: IEnumerator;
  Item: ILayerWaitPacket;
  Waits: IInterfaceList;
begin
  Waits := FWaits;

  if Assigned(Waits) then
  begin
    while Waits.Enumerate(Enum, Item) do
      Item.Signal;

    Waits.Clear;
  end;
end;

{ TSilWaitPacket }

constructor TSilWaitPacket.Create(const Request: IPacket; Data: Integer);
begin
  inherited Create;
  FRequest := Request;
  FData := Data;
  FEvent := Sil.OS.Ipc.Event;
end;

destructor TSilWaitPacket.Destroy;
begin
  FEvent := nil;
  FRequest := nil;
  inherited;
end;

function TSilWaitPacket.GetEvent: IEvent;
begin
  Result := FEvent;
end;

function TSilWaitPacket.GetPacket: IPacket;
begin
  Result := FPacket;
end;

procedure TSilWaitPacket.SetPacket(const Value: IPacket);
begin
  FPacket := Value;
end;

procedure TSilWaitPacket.Signal;
begin
  FEvent.Signal;
end;

function TSilWaitPacket.GetRequest: IPacket;
begin
  Result := FRequest;
end;

function TSilWaitPacket.GetSource: IPacket;
begin
  Result := FSource;
end;

procedure TSilWaitPacket.SetSource(const Value: IPacket);
begin
  FSource := Value;
end;

function TSilWaitPacket.GetData: Integer;
begin
  Result := FData;
end;

{ EProtocolLayerError }

constructor EProtocolLayerError.Create(const Msg: String; const Packet: IPacket);
begin
  inherited Create(Msg);
  FPacket := Packet;
end;

end.
