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

unit SilSmIcmpEcho;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiIcmpProtocol,
  SilSiIcmpEcho;

type
  TSilIcmpEcho = class (
    TSilObject,
    IIcmpEcho )
  private
    FCounter: IPerformanceCounter;
    FSocket: ISocketClient;
    FProtocol: IIcmpProtocol;
    FSequence: Integer;
  protected // IIcmpEcho
    function Ping(const Host: String; Delay: PDouble = nil; Timeout: LongWord = 2000; DataSize: LongWord = 20): Boolean; overload;
    function Ping(const Socket: ISocketClient; var Dest: ISocketAddress; const Query: REcho): REcho; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmIcmpProtocol;

{ TSilIcmpEcho }

constructor TSilIcmpEcho.Create;
begin
  inherited Create;
  FCounter := Sil.Os.Performance.Create;
  FSocket := Sil.Os.Socket.CreateClient(stRaw, spICMP);
  FProtocol := TIcmpProtocol.Create;
  FSequence := 0;
end;

destructor TSilIcmpEcho.Destroy;
begin
  FSocket := nil;
  FCounter := nil;
  FProtocol := nil;
  inherited;
end;

function TSilIcmpEcho.Ping(const Socket: ISocketClient; var Dest: ISocketAddress; const Query: REcho): REcho;
var
  Packet: IPacket;
  Elap: TDateTime;
  Size: LongWord;
begin
  Packet := FProtocol.Echo.Pack(Query);
  Socket.Stream.WriteTo(Packet.Buffer.Memory^, Packet.Buffer.Size, Dest);
  Elap := DateTime.Now + DateTime.FromMilisecs(Socket.Parameters.ReadTimeout);

  while true do
  begin
    Packet.Buffer.Size := 1024;
    Sil.Mem.Clear(Packet.Buffer.Memory^, Packet.Buffer.Size);
    Size := Socket.Stream.ReadFrom(Packet.Buffer.Memory^, Packet.Buffer.Size, Dest);

    if Size <> 0 then
    begin
      Result := FProtocol.Echo.Unpack(Packet);
      if Query.Id = Result.Id then Break;
    end;

    if (DateTime.Now > Elap) or (Size = 0) then
    begin
      Dest := nil;
      Break;
    end;
  end;
end;

function TSilIcmpEcho.Ping(const Host: String; Delay: PDouble; Timeout, DataSize: LongWord): Boolean;
var
  Dest: ISocketAddress;
  Query, Reply: REcho;
begin
  FSocket.Parameters.ReadTimeout := Timeout;

  Dest := Sil.Os.Socket.IP.Create(Host, 0);

  Query.Id := Sil.Os.Process.Current.PID;
  Query.Sequence := Sil.Os.Locked.Increment(FSequence);
  Query.Data := Sil.Str.Replicate('X', DataSize);

  if Assigned(Delay) then FCounter.Reset;

  Reply := Ping(FSocket, Dest, Query);

  if Assigned(Delay) then Delay^ := FCounter.ToMilliseconds();

  Result := Assigned(Dest);
end;

end.
