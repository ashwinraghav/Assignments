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

unit SilSmProtocolPacket;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilSmPacketCompleter,
  SilSmPacketBuilder,
  SilSiProtocolPacket,
  SilSiPacketBuilder,
  SilSiPacketCompletion;

type
  PDispMessage = ^TDispMessage;
  TDispMessage = packed record
    DataID: Cardinal;
    Packet: IProtocolPacket;
  end;

  TProtocolPacket = class (
    // extends
    TSilInterfacedObject,
    // implements
    IProtocolPacket)
  private
    FHeader: TProtocolHeader;
    FPacketBuilder: IPacketData;
    FValid: Boolean;
  protected
    procedure SetProtoID(const Value: Word);
    function GetProtoID: Word;
    procedure SetHeaderVer(const Value: Word);
    function GetHeaderVer: Word;
    function GetHeaderSize: Cardinal;
    function GetSessionID: Cardinal;
    procedure SetSessionID(const Value: Cardinal);
    procedure SetDataID(const Value: Cardinal);
    function GetDataID: Cardinal;
    function GetData: IPacketData;
    function GetSize: Cardinal;
    procedure SetProtoVer(const Value: Word);
    function GetProtoVer: Word;
    function Build(out Buffer: String): Boolean;
    function GetValid: Boolean;
    function GetFlags: TProtocolPacketFlags;
    procedure SetFlags(Value: TProtocolPacketFlags);
    function GetBufferPtr: Pointer;
  public
    constructor Create(Buffer: PChar = nil; Size: Cardinal = 0); 
    destructor Destroy; override;
  end;

{ TProtocolPacketCompleter }

  TProtocolPacketCompleter = class(TPacketCompleter, IPacketCompletion)
  public
    constructor Create; 
    procedure ExtractHeader; override;
  end;

implementation

{ TProtocolPacket }


function FlagsToLongWord(Flags: TProtocolPacketFlags): LongWord;
begin
  Result := LongWord(Pointer(@Flags)^);
end;

function LongWordToFlags(Flags: LongWord): TProtocolPacketFlags;
begin
  Result := TProtocolPacketFlags(Pointer(@Flags)^);
end;

constructor TProtocolPacket.Create(Buffer: PChar; Size: Cardinal);
begin
  inherited Create;

  if (Buffer <> nil) and (Size >= SizeOf(TProtocolHeader)) then
  begin
    Move(Buffer^, FHeader, SizeOf(TProtocolHeader));
    FPacketBuilder := TPacketBuilder.Create(Buffer + SizeOf(TProtocolHeader), Size - SizeOf(TProtocolHeader));
    FValid := true;
  end else
  begin
    FillChar(FHeader, SizeOf(TProtocolHeader), 0);
    FPacketBuilder := TPacketBuilder.Create;

    FHeader.ProtoVer := 1;
    FHeader.HeaderVer := 1;
    FHeader.HeaderSize := SizeOf(TProtocolHeader);
    FValid := true;
  end;
end;

destructor TProtocolPacket.Destroy;
begin
  FPacketBuilder := nil;
  inherited;
end;

function TProtocolPacket.GetData: IPacketData;
begin
  Result := FPacketBuilder;
end;

function TProtocolPacket.GetDataID: Cardinal;
begin
  Result := FHeader.DataID;
end;

function TProtocolPacket.GetHeaderSize: Cardinal;
begin
  Result := FHeader.HeaderSize;
end;

function TProtocolPacket.GetHeaderVer: Word;
begin
  Result := FHeader.HeaderVer;
end;

function TProtocolPacket.GetProtoID: Word;
begin
  Result := FHeader.ProtoID;
end;

function TProtocolPacket.GetProtoVer: Word;
begin
  Result := FHeader.ProtoVer;
end;

function TProtocolPacket.GetSessionID: Cardinal;
begin
  Result := FHeader.SessionID;
end;

function TProtocolPacket.GetSize: Cardinal;
begin
  Result := FHeader.HeaderSize + FPacketBuilder.Size;
end;

function TProtocolPacket.GetValid: Boolean;
begin
  Result := FValid;
end;

function TProtocolPacket.GetFlags: TProtocolPacketFlags;
begin
  Result := LongWordToFlags(FHeader.Flags);
end;

procedure TProtocolPacket.SetFlags(Value: TProtocolPacketFlags);
begin
  FHeader.Flags := FlagsToLongWord(Value);
end;

procedure TProtocolPacket.SetDataID(const Value: Cardinal);
begin
  FHeader.DataID := Value;
end;

procedure TProtocolPacket.SetHeaderVer(const Value: Word);
begin
  FHeader.HeaderVer := Value;
end;

procedure TProtocolPacket.SetProtoID(const Value: Word);
begin
  FHeader.ProtoID := Value;
end;

procedure TProtocolPacket.SetProtoVer(const Value: Word);
begin
  FHeader.ProtoVer := Value;
end;

procedure TProtocolPacket.SetSessionID(const Value: Cardinal);
begin
  FHeader.SessionID := Value;
end;

function TProtocolPacket.Build(out Buffer: String): Boolean;
begin
  FHeader.DataSize := FPacketBuilder.Size;
  SetLength(Buffer, FHeader.HeaderSize + FHeader.DataSize);
  Result := Length(Buffer) > 0;
  if Result then
  begin
    Move(FHeader, Buffer[1], FHeader.HeaderSize);
    Move(FPacketBuilder.Buffer^, Buffer[1 + FHeader.HeaderSize], FPacketBuilder.Size);
  end;
end;

function TProtocolPacket.GetBufferPtr: Pointer;
begin
  Result := @FHeader;
end;

{ TProtocolPacketCompleter }

constructor TProtocolPacketCompleter.Create;
begin
  inherited;
  FControlSize := SizeOf(TProtocolHeader);
end;

procedure TProtocolPacketCompleter.ExtractHeader;
begin
  if (FPacketSize = 0) and (FCurrentSize >= FControlSize) then
  begin
    FHeaderReceived := true;
    FPacketSize := PProtocolHeader(FBuffer)^.HeaderSize + PProtocolHeader(FBuffer)^.DataSize;
  end;
end;

end.
