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

unit SilSmPacketReader;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiFiler,
  SilSiPacketCompletion,
  SilLiStream;

type
  TPacketReader = class(
    TSilInterfacedObject,
    IPbPacketReader)
  private
    FBufferSize: LongWord;
    FCompletion: IPacketCompletion;
    FStream: IStream;
  protected // IPacketReader
    function GetBufferSize: LongWord;
    procedure SetBufferSize(Value: LongWord);
    function Read(var Buffer: String): Boolean;
  public
    constructor Create(const Stream: IStream; const Completion: IPacketCompletion; BufferSize: LongWord = 1024);
    destructor Destroy; override;     
  end;

implementation

{ TPacketReader }

constructor TPacketReader.Create(const Stream: IStream; const Completion: IPacketCompletion; BufferSize: LongWord);
begin
  inherited Create;
  FStream := Stream;
  FCompletion := Completion;
  FBufferSize := BufferSize;
end;

destructor TPacketReader.Destroy;
begin
  FCompletion := nil;
  FStream := nil;
  inherited;
end;

function TPacketReader.GetBufferSize: LongWord;
begin
  Result := FBufferSize;
end;

function TPacketReader.Read(var Buffer: String): Boolean;
var
  iRead: Integer;
  Buf: String;
begin
  Result := false;
  if (FStream = nil) or (FCompletion = nil) then Exit;

  if FCompletion.GetPacket(Buffer) then
  begin
    Result := true;
    Exit;
  end;

  SetLength(Buf, FBufferSize);

  repeat
    iRead := FStream.Read(Buf[1], Length(Buf));
    if iRead > 0 then
    begin
      FCompletion.Append(Buf[1], iRead);
      if FCompletion.GetPacket(Buffer) then
      begin
        Result := true;
        Exit;
      end;
    end;
  until iRead < 1;
end;

procedure TPacketReader.SetBufferSize(Value: LongWord);
begin
  FBufferSize := Value;
end;

end.
 