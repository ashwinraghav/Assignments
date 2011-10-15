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

unit SilSmLogSocketClient;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLkLogger,
  SilLiParameters,
  SilLiLoggers,
  SilLiTrace,
  SilLeTrace,
  SilOiSocket;

const
  ID_VERSION = 1;

type
  TSilLogSocketClient = class (TSilObject, ILogginManager)
  private
    FModule: String;
    FFormat: FormatterType;
    FSocket: ISocketClient;
    FAddress: ISocketAddress;
  protected
    procedure Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
    procedure Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
    procedure Remove(const Logger: LoggerType);
  public
    constructor Create(const Address: String; Port: Word; const Module: String; const Format: FormatterType);
    destructor Destroy; override;
  end;

implementation

uses
  SilLiPacket,
  SilLtStream,
  SilBtDateTime,
  SilOtTool;

{ TSilLogSocketClient }

constructor TSilLogSocketClient.Create(const Address: String; Port: Word; const Module: String; const Format: FormatterType);
begin
  inherited Create;

  FSocket := OS.Socket.CreateClient(stDatagram, spUDP);
  FAddress := OS.Socket.IP.Create(Address, Port);
  FModule := OS.FileSystem.ChangeFileExt(Module, '');
  FFormat := Format;
end;

destructor TSilLogSocketClient.Destroy;
begin
  FSocket := nil;
  FAddress := nil;
  inherited;
end;

procedure TSilLogSocketClient.Add(const Logger: LoggerType; const Formatter: FormatterType; const Params: IParameters);
begin
end;

procedure TSilLogSocketClient.Log(const Stamp: TDateTime; const Kind: TTraceKind; const Trace: ITraceData; const Text, Sender, Category: string);
var
  Packet: IPacket;
begin
  Packet := Stream.Typed.Packet;
  Packet.Writer.WriteWord(ID_VERSION);

  Packet.Writer.WriteString(FModule);
  Packet.Writer.Write(Kind, SizeOf(TTraceKind));
  Packet.Writer.WriteLongWord(Trace.Thread.ID);
  Packet.Writer.WriteInteger(Trace.Level);
  Packet.Writer.WriteDate(Stamp);
  Packet.Writer.WriteString(FFormat.Format(Kind, Trace, Text));
  Packet.Writer.WriteString(Sender);
  Packet.Writer.WriteString(Category);

  FSocket.Stream.WriteTo(Packet.Buffer.Memory^, Packet.Buffer.Size, FAddress);
end;

procedure TSilLogSocketClient.Remove(const Logger: LoggerType);
begin
end;

end.
 