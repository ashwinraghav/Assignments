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

unit SilSmLayerPackerImate;

interface

{$include Defines.inc}

uses
  Sil,
  SilSeLayerPacket,
  SilSiLayer,
  SilSmLayerPacker;

type
  TSilLayerPackerImate = class (
    // extend
    TSilLayerPacker,
    //implements
    ILayerOperation)
  protected
    procedure DoReceive(const Command: ILayerCommand); override;
    function DoGetPacket(const Packet: IPacket): Boolean; override;
  protected
    procedure Write(const Command: ILayerCommand); override;
    function Duplicate(out Obj: IUnknown; const Context: IUnknown = nil): Boolean; override;
  end;

implementation

uses
  SilStLayer,
  SilScLayer,
  SilSmLayerSlot,
  SilSmLayerLink,
  SilSmLayerCommand, SilLiStream;

{ TSilLayerPackerImate }

const
  HeaderSize = SizeOf(RPacketHeader);

function TSilLayerPackerImate.DoGetPacket(const Packet: IPacket): Boolean;
var
  PBuf: PPacketHeader;
begin
  Result := false;

  if Length(Buffer) >= HeaderSize then
  begin
    PBuf := PPacketHeader(PChar(Buffer));

    if Mem.Crc16(PBuf^, HeaderSize) = 0 then
    begin
      if Length(Buffer) >= Integer(HeaderSize + PBuf.Size) then
      begin
        Packet.Buffer.Write(Buffer[1], HeaderSize + PBuf.Size);
        DoBufferExtract(Packet.Buffer.Size);
        Result := true;
      end;
    end else
    begin
      DoResetBuffer; // buffer corrupto

      if Debug.Check(dlPacket, CDebugLayer) then
        Sil.Trace.Log(ClassName + '.DoGetPacket %s', [Str.IIf(Result, 'ok', 'corrupt')]);
    end;
  end;
end;

procedure TSilLayerPackerImate.DoReceive(const Command: ILayerCommand);
var
  FormattedPacket: IPacket;
  lwProtocol: LongWord;
  Buf: PChar;
begin
  lwProtocol := PPacketHeader(Command.Packet.Buffer.Memory).Protocol;

  FormattedPacket := Sil.Stream.Raw.Packet;
  FormattedPacket.Buffer.Size := PPacketHeader(Command.Packet.Buffer.Memory).Size;
  Command.Packet.Buffer.Position := SizeOf(RPacketHeader);

  Buf := FormattedPacket.Buffer.Current;
  Command.Packet.Buffer.Read(Buf^, FormattedPacket.Buffer.Size);

  inherited DoReceive(Cmd.Create(Command, FormattedPacket, lwProtocol));
end;

procedure TSilLayerPackerImate.Write(const Command: ILayerCommand);
var
  Header: RPacketHeader;
  FormattedPacket: IPacket;
begin
  Header.Protocol := Command.Caller.Operation.Id;
  Header.Size := Command.Packet.Buffer.Size;
  Header.Crc := Mem.Crc16(Header, SizeOf(Header) - SizeOf(Header.Crc));

  FormattedPacket := Sil.Stream.Raw.Packet;
  FormattedPacket.Buffer.Write(Header, SizeOf(Header));
  FormattedPacket.Buffer.Write(Command.Packet.Buffer.Memory^, Command.Packet.Buffer.Size);

  inherited Write(Cmd.Create(Command, FormattedPacket));
end;

function TSilLayerPackerImate.Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean;
begin
  Obj := TSilLayerPackerImate.Create(Parameters);
  Result := true;
end;

end.
