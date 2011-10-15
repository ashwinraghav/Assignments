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

unit SilSmLayerDeviceSocketDatagram;

interface

{$include Defines.inc}

uses
  Sil,
  SilScLayer,
  SilSiLayer,
  SilSmLayer;

const
  THREAD_LISTENER   = 1024;
  THREAD_DISPATCHER = 1025;

type
  TSilSocketDatagramLayer = class (
    TSilLayer,
    IDispatchable,
    ILayerTerminal )
  private
    FLink: Pointer;
    FListener: IThread;
    FDispatcher: IThread;
    FDevice: ISocketClient;
    FDestination: ISocketAddressDef;
    FReadBlockSize: LongWord;
    FQueue: IInterfaceQueue;
    FStarted: IEvent;
    FReadLock: IMutex;
    FReadTimeout: LongWord;
    FReceiveEnabled: Boolean;
  private
    procedure DoDecodeParams;
    procedure DoDisconnect;
    function DoRead(out Command: ILayerCommand): Boolean;
  protected // threads
    procedure ThreadListener(var Msg: RThreadRunMessage); message THREAD_LISTENER;
    procedure ThreadDispatcher(var Msg: RThreadRunMessage); message THREAD_DISPATCHER;
  protected // ILayer
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
    function GetIsActive: Boolean; override;
  protected // ILayerTerminal
    procedure StartLayer(const Context: IUnknown);
    procedure StopLayer(const Context: IUnknown);
    function GetLink: ILayerLink;
  protected // TSilLayer
    procedure Write(const Command: ILayerCommand); override;
    procedure Read(const Command: ILayerCommand); override;
  protected
    property Link: ILayerLink read GetLink;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilOcSocket,
  SilStLayer;

{ TSilSocketDatagramLayer }

constructor TSilSocketDatagramLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited Create(Parameters, Controller);
  FStarted := Sil.OS.Ipc.Event;
  FReadBlockSize := 1024 * 4;
end;

destructor TSilSocketDatagramLayer.Destroy;
begin
  FStarted := nil;
  inherited;
end;

procedure TSilSocketDatagramLayer.ThreadDispatcher(var Msg: RThreadRunMessage);
var
  Command: ILayerCommand;
begin
  try
    while FQueue.Get(ILayerCommand, Command) do
      try
        if Debug.Check(dlInOut, CDebugLayer) then
          Sil.Trace.Log(ClassName + '.Receive: datagram=[%s]', [Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);
        Cmd.Receive(Command);
      except
        if Debug.Check(dlException, CDebugLayer) then
          Sil.Trace.Exception(ClassName + '.ThreadDispatcher');
      end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.ThreadDispatcher');
  end;
end;

procedure TSilSocketDatagramLayer.ThreadListener(var Msg: RThreadRunMessage);
var
  Command: ILayerCommand;
begin
  try
    FStarted.Signal;

    try
      while DoRead(Command) do
      begin
        if Debug.Check(dlConnection, CDebugLayer) then
          Sil.Trace.Log(ClassName + '.ThreadListener client data arrival');
          
        FQueue.Put(Command);
      end;
    finally
      FStarted.Reset;
    end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.ThreadDispatcher');
  end;
end;

procedure TSilSocketDatagramLayer.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
  MakeRef(Link, @FLink);
  FQueue := Sil.List.InterfaceQueue;
  FReadLock := Sil.OS.Ipc.Mutex;
  DoDecodeParams;
end;

procedure TSilSocketDatagramLayer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  if Assigned(FDispatcher) then
  begin
    if FReceiveEnabled then
    begin
      Sil.List.CancelQueue(FQueue);
      Sil.OS.Thread.Wait(FDispatcher);
      FDispatcher := nil;
      if Assigned(FDevice) then
        FDevice.Disconnect;
      FDevice := nil;
      Sil.OS.Thread.Wait(FListener);
      FListener := nil;
    end else
      FStarted.Reset;

    FReadLock := nil;
    FQueue := nil;

    DropRef(@FLink);
  end;
end;

function TSilSocketDatagramLayer.GetIsActive: Boolean;
begin
  Result := FStarted.IsSignaled;
end;

procedure TSilSocketDatagramLayer.StartLayer(const Context: IInterface);
begin
  try
    if FReceiveEnabled and not Assigned(FDispatcher) then
    begin
      FDispatcher := Sil.OS.Thread.Spawn(THREAD_DISPATCHER, 'dispatcher', Self);
      FListener := Sil.OS.Thread.Spawn(THREAD_LISTENER, 'listener', Self);
      FStarted.WaitFor;
    end else
      FStarted.Signal;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.StartLayer');
  end;
end;

procedure TSilSocketDatagramLayer.StopLayer(const Context: IInterface);
begin
end;

function TSilSocketDatagramLayer.GetLink: ILayerLink;
begin
  Result := ILayerLink(FLink);
end;

procedure TSilSocketDatagramLayer.Write(const Command: ILayerCommand);
var
  Address: ISocketAddressDef;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Write');

  if Assigned(FDevice) then
  begin
    if not Ref.Get(Command.Context, ISocketAddress, Address) then
      Address := FDestination;

    FDevice.Stream.WriteTo(Command.Packet.Buffer.Memory^, Command.Packet.Buffer.Size, Address);

    if Debug.Check(dlInOut, CDebugLayer) then
      Sil.Trace.Log(ClassName + '.Write: datagram=[to: %s content: %s]', [Address.Format(CAddressLong), Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);
  end else
    inherited;
end;

procedure TSilSocketDatagramLayer.Read(const Command: ILayerCommand);
var
  Buffer: String;
  lwSize: LongWord;
  Address: ISocketAddress;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Read');

  if Assigned(FDevice) then
  begin
    lwSize := 0;
    if Command.Packet.Buffer.Size <> 0 then
      SetLength(Buffer, Command.Packet.Buffer.Size) else
      SetLength(Buffer, FReadBlockSize);

    while Assigned(FDevice) and FDevice.Handle.IsValid and (lwSize = 0) do
      try
        if FReadLock.WaitFor(FReadTimeout) <> wrSignaled then
          raise Sil.Error.Create('El tiempo de espera por arrivo de datos fue superado');

        try
          lwSize := FDevice.Stream.ReadFrom(Buffer[1], Length(Buffer), Address);
        finally
          FReadLock.Release;
        end;

        if (lwSize = 0) {and (Sil.OS.Socket.LastError = ECONNRESET)} then Continue;

        if Debug.Check(dlInOut, CDebugLayer) and (lwSize <> 0) then
          Sil.Trace.Log(ClassName + '.Read: datagram=[from: %s content: %s]', [Address.Format(CAddressLong), Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);
      except
        if Debug.Check(dlException, CDebugLayer) then Sil.Trace.Exception(ClassName + '.Read');
        raise;
      end;

    if lwSize > 0 then
    begin
      Command.Packet.Buffer.Write(Buffer[1], lwSize);
      Command.Context := Address;
    end else
      DoDisconnect;
  end else
    inherited;
end;

procedure TSilSocketDatagramLayer.DoDecodeParams;
var
  Value: Variant;
  LocalAddress, RemoteAddress: string;
  LocalPort, RemotePort: Word;
begin
  FReceiveEnabled := Parameters.Get('ReceiveEnabled', False);
  FReadTimeout := Parameters.Get('ReadTimeout', INFINITE);

  RemoteAddress := Parameters.Get('Address', '');
  RemoteAddress := Parameters.Get('RemoteAddress', RemoteAddress);
  LocalAddress := Parameters.Get('LocalAddress', '');
  RemotePort := Parameters.Get('Port', 0);
  RemotePort := Parameters.Get('RemotePort', RemotePort);
  LocalPort := Parameters.Get('LocalPort', 0);

  if (RemotePort = 0) and (LocalPort <> 0) then RemotePort := LocalPort;
  FReceiveEnabled := FReceiveEnabled or (LocalPort <> 0) or Str.NotEmpty(LocalAddress);

  if Sil.Str.IsAssigned(RemoteAddress) and (RemotePort <> 0) then
    FDestination := Sil.Os.Socket.IP.Create(RemoteAddress, RemotePort);

  if not Parameters.Find('Device', Value) then
  begin
    FDevice := Sil.Os.Socket.CreateClient(stDatagram, spUdp);

    if FReceiveEnabled then
      FDevice.Bind(LocalAddress, LocalPort);

  end else
    FDevice := Sil.Vart.ToUnknown(Value) as ISocketClient;

  FReadBlockSize := Parameters.Get('BufferSize', FDevice.Parameters.ReceiveBufferSize);
end;

procedure TSilSocketDatagramLayer.DoDisconnect;
var
  Link: ILayerLink;
  Chain: ILayerChain;
  Control: ILayerLinkControl;
begin
  if Debug.Check(dlConnection, CDebugLayer) then Sil.Trace.Log(ClassName + '.DoCheck desconectado');

  if Assigned(FDispatcher) and IsActive then
  begin
    if Assigned(Link) then
    begin
      Chain := Link.Chain;

      if Assigned(Chain) then
      begin
        Control := Chain.Control;

        if Assigned(Control) then
          Control.Deactivate(Link, true);
      end;
    end;
  end;
end;

function TSilSocketDatagramLayer.DoRead(out Command: ILayerCommand): Boolean;
begin
  try
    Command := Cmd.Create(Link, Sil.Stream.Raw.Packet);
    Read(Command);
    Result := Command.Packet.Buffer.Size > 0;
  except
    if Debug.Check(dlException, CDebugLayer) then Sil.Trace.Exception(ClassName + '.DoRead');
    Result := False;
  end;
end;

end.

