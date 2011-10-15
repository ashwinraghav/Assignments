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

unit SilSmPacketCompleter;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilSiPacketCompletion;

type

  PPacketHeader = ^TPacketHeader;
  TPacketHeader = packed record
    Id: Cardinal;    // identificacion del paquete
    Size: Cardinal;  // cantidad total de bytes (incluyendo esta estructura)
  end;

{ TPacketCompleter }

  TPacketCompleter = class(
    TSilInterfacedObject,
    IPacketCompletion )
  protected
    FBuffer: PChar;
    FHeaderReceived: Boolean;
    FControlSize: Cardinal;
    FCurrentSize: Cardinal;
    FPacketSize: Cardinal;
    procedure Reset;
  public
    constructor Create; 
    destructor Destroy; override;
    function HeaderReceived: Boolean; virtual;
    procedure ExtractHeader; virtual;
    function CheckData: Boolean; virtual;
    function Decrease: Boolean; virtual;
    function Buffer: Pointer;
    function Append(const Buffer; const Size: Cardinal): Boolean;
    function GetPacket(var Buffer: PChar; var Size: Cardinal): Boolean; overload; virtual;
    function GetPacket(var Buffer: String): Boolean; overload; virtual;
  end;

implementation

{ TPacketCompleter }

function TPacketCompleter.Append(const Buffer; const Size: Cardinal): Boolean;
begin
  Result := true;{FHeaderReceived or (FControlSize < 1) or (FCurrentSize < FControlSize);
  if not Result then Exit;}

  ReallocMem(FBuffer, FCurrentSize + Size + 1);
  Move(Buffer, FBuffer[FCurrentSize], Size);
  Inc(FCurrentSize, Size);
  FBuffer[FCurrentSize] := #0;
end;

function TPacketCompleter.Buffer: Pointer;
begin
  Result := FBuffer;
end;

function TPacketCompleter.CheckData: Boolean;
begin
  Result := (FPacketSize > 0) and (FCurrentSize >= FPacketSize);
end;

constructor TPacketCompleter.Create;
begin
  inherited Create;
  FBuffer := nil;
  FCurrentSize := 0;
  FPacketSize := 0;
  FControlSize := SizeOf(TPacketHeader);
end;

destructor TPacketCompleter.Destroy;
begin
  Reset;
  inherited Destroy;
end;

function TPacketCompleter.Decrease: Boolean;
var
  pcTemp: PChar;
  iSize: Integer;
begin
  Result := FPacketSize < FCurrentSize;

  if Result then
  begin
    iSize := FCurrentSize - FPacketSize;
    GetMem(pcTemp, iSize);
    Move(FBuffer[FPacketSize], pcTemp[0], iSize);
    FreeMem(FBuffer);

    FBuffer := pcTemp;
    FPacketSize := 0;
    FCurrentSize := iSize;
    FHeaderReceived := false;
  end else
    Reset;
end;

procedure TPacketCompleter.Reset;
begin
  FHeaderReceived := false;
  FCurrentSize := 0;
  FPacketSize := 0;

  if FBuffer <> nil then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
end;

procedure TPacketCompleter.ExtractHeader;
begin
  if {(FPacketSize = 0) and} (FCurrentSize >= SizeOf(TPacketHeader)) then
  begin
    FHeaderReceived := true;
    FPacketSize := PPacketHeader(FBuffer)^.Size;
  end;
end;

function TPacketCompleter.GetPacket(var Buffer: String): Boolean;
begin
  if not FHeaderReceived then ExtractHeader;
  Result := CheckData;

  if Result then
  begin
    SetLength(Buffer, FPacketSize);
    Move(FBuffer^, Buffer[1], FPacketSize);
    Decrease;
  end;
end;

function TPacketCompleter.GetPacket(var Buffer: PChar; var Size: Cardinal): Boolean;
begin
  if not FHeaderReceived then ExtractHeader;
  Result := CheckData;

  if Result then
  begin
    GetMem(Buffer, FPacketSize);
    Move(FBuffer^, Buffer^, FPacketSize);
    Size := FPacketSize;
    Decrease;
  end;
end;

function TPacketCompleter.HeaderReceived: Boolean;
begin
  Result := FHeaderReceived;
end;

end.
