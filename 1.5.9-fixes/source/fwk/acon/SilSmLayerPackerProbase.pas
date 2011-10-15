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

unit SilSmLayerPackerProbase;

interface

{$include Defines.inc}
{$WARN SYMBOL_DEPRECATED OFF} 

uses
  Sil,
  SilSeLayerPacket,
  SilSiLayer,
  SilSiProtocolPacket,
  SilSmLayerPacker;

type
  TSilLayerPackerProbase = class (
    // extend
    TSilLayerPacker,
    //implements
    ILayerOperation)
  private
    function DoConvertRead(const ProbasePacket: IProtocolPacket): IPacket;
    function DoConvertWrite(const Command: ILayerCommand): IProtocolPacket;
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
  SilLeTypeReader,
  SilSeLayerProtocol,
  SilSmProtocolPacket;

{ TSilLayerPackerProbase }

const
  HeaderSize = SizeOf(TProtocolHeader);

function TSilLayerPackerProbase.DoGetPacket(const Packet: IPacket): Boolean;
var
  PBuf: PProtocolHeader;
begin
  Result := false;

  if Length(Buffer) >= HeaderSize then
  begin
    PBuf := PProtocolHeader(PChar(Buffer));

    if Length(Buffer) >= HeaderSize + Integer(PBuf.DataSize) then
    begin
      Packet.Buffer.Write(Buffer[1], HeaderSize + PBuf.DataSize);
      DoBufferExtract(Packet.Buffer.Size);
      Result := true;
    end;
  end;
end;

function TSilLayerPackerProbase.DoConvertRead(const ProbasePacket: IProtocolPacket): IPacket;
var
  PHeader: PProtocolHeader;
  PacketHdr: RPacketHeader;
  ProtocolHdr: RImateProtocolHeader;
  VType: Byte;
  Writer: IWriter;
  Data: IPacketData;
begin
  PHeader := PProtocolHeader(ProbasePacket.GetBufferPtr);

  PacketHdr.Protocol := PHeader.ProtoID;
  PacketHdr.Size := PHeader.HeaderSize + PHeader.DataSize - SizeOf(PacketHdr);
  PacketHdr.Crc := 0;

  ProtocolHdr.Id := PHeader.DataID;
  ProtocolHdr.Version := PHeader.ProtoVer;
  ProtocolHdr.Flags := PHeader.Flags;
  ProtocolHdr.Size := PHeader.DataSize;

  Result := Sil.Stream.Typed.Packet;
  Result.Writer.Write(PacketHdr, SizeOf(PacketHdr));
  Result.Writer.Write(ProtocolHdr, SizeOf(ProtocolHdr));

  Writer := Result.Writer;
  Data := ProbasePacket.Data;

  while true do
  begin
    VType := ProbasePacket.Data.ReadValueType;
    if VType = 0 then Break;

    case VType of
      SilSiPacketBuilder.tiInteger:    Writer.WriteInteger(Data.ReadInteger);
      SilSiPacketBuilder.tiBoolean:    Writer.WriteBoolean(Data.ReadBoolean);
      SilSiPacketBuilder.tiByte:       Writer.WriteByte(Data.ReadByte);
      SilSiPacketBuilder.tiDouble:     Writer.WriteDouble(Data.ReadFloat);
      SilSiPacketBuilder.tiAnsiString: Writer.WriteString(Data.ReadString);
      SilSiPacketBuilder.tiWideString: Writer.WriteWideString(Data.ReadWideString);
      SilSiPacketBuilder.tiLargeInt:   Writer.WriteLargeInt(Data.ReadLargeInt);
      SilSiPacketBuilder.tiWord:       Writer.WriteWord(Data.ReadWord);
      SilSiPacketBuilder.tiLongWord:   Writer.WriteLongWord(Data.ReadLongWord);
      SilSiPacketBuilder.tiShortInt:   Writer.WriteInteger(Data.ReadShortInt);
      SilSiPacketBuilder.tiSmallInt:   Writer.WriteInteger(Data.ReadSmallInt);
      SilSiPacketBuilder.tiSingle:     Writer.WriteSingle(Data.ReadSingle);
      SilSiPacketBuilder.tiVariant:    Writer.WriteVariant(Data.ReadVariant);
      // guid no es compatible
      else                              raise Error.Create('tipo desconocido ', [VType]);
    end;
  end;
end;

procedure TSilLayerPackerProbase.DoReceive(const Command: ILayerCommand);
var
  ProbasePacket: IProtocolPacket;
  ImatePacket, FormattedPacket: IPacket;
  lwProtocol: LongWord;
  Buf: PChar;
begin
  with Command.Packet.Buffer do
    ProbasePacket := TProtocolPacket.Create(Memory, Size);

  ImatePacket := DoConvertRead(ProbasePacket);
  lwProtocol := PPacketHeader(ImatePacket.Buffer.Memory).Protocol;

  FormattedPacket := Sil.Stream.Raw.Packet;
  FormattedPacket.Buffer.Size := PPacketHeader(ImatePacket.Buffer.Memory).Size + SizeOf(LongWord);
  ImatePacket.Buffer.Position := SizeOf(RPacketHeader);

  FormattedPacket.Writer.WriteLongWord(lwProtocol);
  Buf := FormattedPacket.Buffer.Current;
  ImatePacket.Buffer.Read(Buf^, FormattedPacket.Buffer.Size);

  inherited DoReceive(Cmd.Create(Command, FormattedPacket));
end;

function TSilLayerPackerProbase.DoConvertWrite(const Command: ILayerCommand): IProtocolPacket;
var
  ProtocolHdr: PImateProtocolHeader;
  Reader: IReader;
  Data: IPacketData;
  VType: Byte;
begin
  ProtocolHdr := PImateProtocolHeader(Command.Packet.Buffer.Memory);

  Result := TProtocolPacket.Create;
  Result.ProtoID := Command.Caller.Operation.Id;
  Result.ProtoVer := 0;
  Result.HeaderVer := 0;
  Result.SessionID := 0;
  Result.Flags := TProtocolPacketFlags(ProtocolHdr.Flags);
  Result.DataID := ProtocolHdr.Id;

  Reader := Command.Packet.Reader;
  Data := Result.Data;
  Command.Packet.Buffer.Position := SizeOf(RImateProtocolHeader);

  while Command.Packet.Buffer.Remaining > 0 do
  begin
    VType := Byte(Command.Packet.Buffer.Current^);

    case VType of
      SilLeTypeReader.tiInteger:    Data.WriteInteger(Reader.ReadInteger);
      SilLeTypeReader.tiBoolean:    Data.WriteBoolean(Reader.ReadBoolean);
      SilLeTypeReader.tiByte:       Data.WriteByte(Reader.ReadByte);
      SilLeTypeReader.tiDouble:     Data.WriteFloat(Reader.ReadDouble);
      SilLeTypeReader.tiDate:       Data.WriteFloat(Reader.ReadDate);
      SilLeTypeReader.tiAnsiString: Data.WriteString(Reader.ReadString);
      SilLeTypeReader.tiWideString: Data.WriteWideString(Reader.ReadWideString);
      SilLeTypeReader.tiLargeInt:   Data.WriteLargeInt(Reader.ReadLargeInt);
      SilLeTypeReader.tiWord:       Data.WriteWord(Reader.ReadWord);
      SilLeTypeReader.tiLongWord:   Data.WriteLongWord(Reader.ReadLongWord);
      SilLeTypeReader.tiShortInt:   Data.WriteShortInt(Reader.ReadInteger);
      SilLeTypeReader.tiSmallInt:   Data.WriteSmallInt(Reader.ReadInteger);
      SilLeTypeReader.tiSingle:     Data.WriteSingle(Reader.ReadSingle);
      SilLeTypeReader.tiVariant:    Data.WriteVariant(Reader.ReadVariant);
      SilLeTypeReader.tiGuid:       Data.WriteGuid(Reader.ReadGuid);
      else                          raise Error.Create('tipo desconocido ', [VType]);
    end;
  end;
end;

procedure TSilLayerPackerProbase.Write(const Command: ILayerCommand);
var
  ProbasePacket: IProtocolPacket;
  FormattedPacket: IPacket;
  Buffer: String;
begin
  ProbasePacket := DoConvertWrite(Command);

  if ProbasePacket.Build(Buffer) then
  begin
    FormattedPacket := Sil.Stream.Raw.Packet;
    FormattedPacket.Buffer.Write(Buffer[1], Length(Buffer));
    inherited Write(Cmd.Create(Command, FormattedPacket));
  end else
    raise Error.Create('imposible armar paquete');
end;

function TSilLayerPackerProbase.Duplicate(out Obj: IInterface; const Context: IInterface): Boolean;
begin
  Obj := TSilLayerPackerProbase.Create(Parameters);
  Result := true;
end;

end.
