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

unit SilSmAsciiCommandSink;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOcTypes,
  SilOeWait,
  SilOiIpc,
  SilSiAsciiProtocol,
  SilSiAsciiCommandSink,
  SilSiAbstractConnection;

type
  PPacketItem = ^RPacketItem;
  RPacketItem = record
    Buffer: String;
    Next: PPacketItem;
  end;

  TAsciiCommandSink = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAsciiCommandSink,
    IAsciiProtocolEvents,
    IConnectedEvents)
  private
    FCommand: Pointer;
    FConnection: IAbstractConnection;
    FReadEvent: IEvent;
    FPacket: PPacketItem;
    FTimeout: LongWord;
  protected // IAsciiProtocolEvents
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  protected // IAsciiCommandSink
    procedure Drop;
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent);
    procedure OnDataSent(const Event: TConnectionDataEvent);
    procedure OnDisconnected(const Event: TConnectionBreakEvent);
  public
    constructor Create(const Command: IUnknown; const Connection: IAbstractConnection);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtError,
  SilOtTool,
  SilLtConnection;

{ TAsciiCommandSink }

constructor TAsciiCommandSink.Create(const Command: IUnknown; const Connection: IAbstractConnection);
begin
  inherited Create;

  FReadEvent := OS.IPC.Event();
  FCommand := Pointer(Command);
  FConnection := Connection;
  FTimeout := INFINITE;

  Sink.Connect(Command, Self);
  Sink.Connect(FConnection, Self);
end;

destructor TAsciiCommandSink.Destroy;
begin
  Drop;
  inherited;
end;

procedure TAsciiCommandSink.Drop;
var
  PNext: PPacketItem;
begin
  if FConnection <> nil then
  begin
    Sink.Disconnect(IUnknown(FCommand), Self);
    Sink.Disconnect(FConnection, Self);
    FCommand := nil;
    FConnection := nil;
  end;

  while FPacket <> nil do
  begin
    PNext := FPacket.Next;
    Dispose(FPacket);
    FPacket := PNext;
  end;
end;

procedure TAsciiCommandSink.OnDataReceived(const Event: TConnectionDataEvent);
var
  Packet: PPacketItem;
begin
  New(Packet);
  Packet.Next := nil;
  SetString(Packet.Buffer, Event.Buffer, Event.Size);

  if FPacket <> nil then
    FPacket.Next := Packet else
    FPacket := Packet;

  FReadEvent.Signal;
end;

procedure TAsciiCommandSink.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

procedure TAsciiCommandSink.OnDisconnected(const Event: TConnectionBreakEvent);
begin
end;

procedure TAsciiCommandSink.OnReadLine(var Event: RReadLineEvent);
var
  PNext: PPacketItem;
begin
  if FPacket = nil then
  begin
    if (FConnection.Thread <> nil) and not FConnection.Thread.IsCurrent then
    begin
      if FReadEvent.WaitFor(FTimeout, true) <> wrSignaled then
        Error.Throw('TAsciiCommandSink.OnReadLine:timeout(%s)', [ClassName]);
    end else
      FConnection.WaitDataInput;
  end;

  FReadEvent.Reset;

  if FPacket <> nil then
  begin
    Event.Text := FPacket.Buffer;
    Event.Result := Length(Event.Text) > 0;

    PNext := FPacket.Next;
    Dispose(FPacket);
    FPacket := PNext;
  end;
end;

procedure TAsciiCommandSink.OnWriteLine(var Event: RWriteLineEvent);
begin
  Event.Result := FConnection.WriteStr(Event.Text) > 0;
end;

end.
