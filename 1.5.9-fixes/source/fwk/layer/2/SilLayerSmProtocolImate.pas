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

unit SilLayerSmProtocolImate;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSkLayer,
  SilLayerSkProtocol,
  SilLayerSmPackerImate;

type
  PImatePacket = ^RImatePacket;
  RImatePacket = packed record
    Id: word;
    Version: longword;
    Flags: longword;
    Size: integer;
  end;

  RImateMessage = record
    msgid: Cardinal;  //Word; PFC no usa Word
    protocol: RImateProtocol;
    packet: RImatePacket;
    sender: IUnknown;
    reader: IReader;
    token: ILayerToken;
  end;

const
  packet_header_size = sizeof(RImatePacket);
  total_size = protocol_header_size + packet_header_size;

type
  TSilLayerProtocolImate = class (TSilLayerProtocol)
  private
    fprotocolid: longword;
    procedure DoWriteHeader(const buffer: IMemoryStream; id: longword);
  protected
    function DoProtocolMatch(const token: ILayerToken): boolean; override;
    function DoPacketMatch(const token: ILayerToken; const wait: ILayerWait): boolean; override;
    procedure DoDispatchPacket(const token: ILayerToken); override;
  protected
    function CreatePacket(id: longword): ILayerToken;
    procedure SendPacket(const token: ILayerToken); overload;
    procedure SendPacket(const token: ILayerToken; out reply: ILayerToken; id: longword; timeout: longword = 0); overload;
  public
    constructor Create(protocolid: longword); reintroduce;
    destructor destroy; override;
  end;

implementation

{ TSilLayerProtocolImate }

constructor TSilLayerProtocolImate.Create(protocolid: longword);
begin
  inherited Create;
  fprotocolid := protocolid;
end;

destructor TSilLayerProtocolImate.destroy;
begin
  //writeln('TSilLayerProtocolImate.destroy');
  inherited;
end;

function TSilLayerProtocolImate.DoProtocolMatch(const token: ILayerToken): boolean;
begin
  Result :=
    (token.Buffer.Size >= protocol_header_size) and
    (PImateProtocol(token.Buffer.Memory)^.ProtocolId = fprotocolid);
end;

function TSilLayerProtocolImate.DoPacketMatch(const token: ILayerToken; const wait: ILayerWait): boolean;
var
  buf1, buf2: PImatePacket;
begin
  Result := false;

  if token.Buffer.Size >= total_size then
  begin
    buf1 := PImatePacket(token.Buffer.Memory + protocol_header_size);
    buf2 := PImatePacket(wait.buffer.Memory);
    Result := buf1.Id = buf2.Id;
  end;
end;

procedure TSilLayerProtocolImate.DoDispatchPacket(const token: ILayerToken);
var
  msg: RImateMessage;
begin
  token.Configure(Stream.Typed);

  msg.protocol := PImateProtocol(token.Buffer.Memory)^;
  msg.packet := PImatePacket(token.Buffer.Memory + protocol_header_size)^;
  msg.sender := self;
  msg.reader := token.Reader;
  msg.token := token;
  msg.msgid := msg.packet.Id;

  token.Buffer.Position := total_size;

  try
    Dispatch(msg);
  except
    // auto resp except
  end;
end;

procedure TSilLayerProtocolImate.DoWriteHeader(const buffer: IMemoryStream; id: longword);
var
  header: PImatePacket;
begin
  header := PImatePacket(buffer.Memory);
  header.Id := id;
  header.Version := 0;
  header.Flags := 0;
  header.Size := 0;
end;

function TSilLayerProtocolImate.CreatePacket(id: longword): ILayerToken;
begin
  result := GetStack.GetToken;
  result.Configure(Stream.Typed);

  result.Params['protocolid'] := fprotocolid;
  result.Buffer.Size := packet_header_size;
  result.Buffer.Seek(0, soFromEnd);

  DoWriteHeader(result.Buffer, id);
end;

procedure TSilLayerProtocolImate.SendPacket(const token: ILayerToken);
begin
  PImatePacket(token.Buffer.Memory).Size := token.Buffer.Size - packet_header_size;
  GetLower.Action.Write(token);
end;

procedure TSilLayerProtocolImate.SendPacket(const token: ILayerToken; out reply: ILayerToken; id, timeout: longword);
var
  wait: ILayerWait;
  buffer: IMemoryStream;
begin
  buffer := Sil.Stream.Memory(packet_header_size);
  DoWriteHeader(buffer, id);

  wait := DoCreateWait(buffer, timeout);
  SendPacket(token);
  DoWaitPacket(wait, reply);

  reply.Buffer.Position := total_size;
  reply.Configure(Stream.Typed);
end;

end.
