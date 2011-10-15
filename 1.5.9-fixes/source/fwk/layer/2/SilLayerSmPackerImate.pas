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

unit SilLayerSmPackerImate;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSkLayer;

type
  PImateProtocol = ^RImateProtocol;
  RImateProtocol = packed record
    ProtocolId: longword;
    DataSize: integer;
    HeaderCrc16: word;
  end;

const
  protocol_header_size = sizeof(RImateProtocol);

type
  TSilLayerPackerImate = class (TSilLayer)
  private
    fbuffer: IMemoryStream;
    FHeader: RImateProtocol;
    procedure DoInit;
    procedure DoAppendBuffer(const Packet: ILayerToken);
    function DoExtractPacket(out Packet: ILayerToken): Boolean;
    function DoPutHeader(const Packet: ILayerToken): Boolean;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TSilLayerPackerImate }

constructor TSilLayerPackerImate.Create;
begin
  inherited;
  DoInit;
end;

destructor TSilLayerPackerImate.Destroy;
begin
  inherited;
end;

procedure TSilLayerPackerImate.DoInit;
begin
  fbuffer := Sil.Stream.Memory;
end;

procedure TSilLayerPackerImate.DoActivate(const Context: IInterface);
begin
  FHeader.ProtocolId := 0;
  FHeader.DataSize := 0;
  FHeader.HeaderCrc16 := 0;
end;

procedure TSilLayerPackerImate.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  DoInit;
end;

function TSilLayerPackerImate.DoGetIsActive: Boolean;
begin
  result := fbuffer <> nil;
end;

procedure TSilLayerPackerImate.DoAppendBuffer(const Packet: ILayerToken);
begin
  with packet.buffer do
    fbuffer.write(memory^, size);
end;

function TSilLayerPackerImate.DoExtractPacket(out Packet: ILayerToken): Boolean;
var
  header: PImateProtocol;
  sizeb, sizep: integer;
  buft: pchar;
begin
  Result := false;
  sizeb := fbuffer.size;

  if sizeb >= protocol_header_size then
  begin
    header := PImateProtocol(pchar(fbuffer.memory));

    if Mem.Crc16(header^, protocol_header_size) = 0 then
    begin
      result := sizeb - protocol_header_size >= header.DataSize;

      if result then
      begin
        packet := GetStack.GetToken;
        sizep := protocol_header_size + header.DataSize;

        packet.Buffer.Size := sizep;
        move(fbuffer.memory^, packet.Buffer.Memory^, sizep);

        if sizeb > sizep then
        begin
          sizeb := sizeb - sizep;
          getmem(buft, sizeb);
          move((fbuffer.memory + sizep)^, buft, sizeb);
          fbuffer.size := sizeb;
          fbuffer.Position := 0;
          move(buft^, fbuffer.memory^, sizeb);
          freemem(buft);
        end else
          fbuffer.size := 0;
      end;
    end else
      DoInit; // buffer corrupto; avisar al stack
  end;
end;

function TSilLayerPackerImate.DoPutHeader(const Packet: ILayerToken): Boolean;
var
  buf: pchar;
begin
  fHeader.ProtocolId := Packet.Params.Get('protocolid', 0);
  fheader.DataSize := Packet.Buffer.Size;
  fheader.HeaderCrc16 := Mem.Crc16(fheader, protocol_header_size - SizeOf(fheader.HeaderCrc16));

  Packet.Buffer.Size := protocol_header_size + fheader.DataSize;
  buf := Packet.Buffer.Memory;

  move(buf^, pchar(buf + protocol_header_size)^, fheader.DataSize);
  move(fheader, buf^, protocol_header_size);

  Result := true;
end;

function TSilLayerPackerImate.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
var
  BufferedPacket: ILayerToken;
begin
  DoAppendBuffer(Packet);

  while DoExtractPacket(BufferedPacket) do
  begin
    if Link = nil then
      Link := GetUpper;

    Link.Action.Read(BufferedPacket);
  end;

  Result := false;
end;

function TSilLayerPackerImate.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Link := GetLower;
  Result := DoPutHeader(Packet);
end;

end.
