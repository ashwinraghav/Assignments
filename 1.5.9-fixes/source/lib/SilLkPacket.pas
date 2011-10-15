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

unit SilLkPacket;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilLiFiler,
  SilLiPacket,
  SilLkInterfaced,
  SilLjFiler;

type
  TSilPacket = class(TSilInterfacedObject, IPacket)
  private
    FBuffer: IBufferStream;
    FReader: IReader;
    FWriter: IWriter;
  protected // IPacket
    function GetBuffer: IBufferStream;
    function GetSource: IStream;
    function GetReader: IReader;
    function GetWriter: IWriter;
    procedure SetSource(const Source: IStream);
  public
    constructor Create(const Factory: FilerFactoryType; const Source: IStream; Buffered: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtTool,
  SilLmPageStream;

{ TSilPacket }

constructor TSilPacket.Create(const Factory: FilerFactoryType; const Source: IStream; Buffered: Boolean);
begin
  inherited Create;

  if Buffered then
    FBuffer := Tk.BufferStream(Source) else
    FBuffer := TSilPageStream.Create(Source);

  FReader := Factory.Reader(FBuffer);
  FWriter := Factory.Writer(FBuffer);
end;

destructor TSilPacket.Destroy;
begin
  FWriter := nil;
  FReader := nil;
  FBuffer := nil;
  inherited;
end;

function TSilPacket.GetBuffer: IBufferStream;
begin
  Result := FBuffer;
end;

function TSilPacket.GetReader: IReader;
begin
  Result := FReader;
end;

function TSilPacket.GetWriter: IWriter;
begin
  Result := FWriter;   
end;

function TSilPacket.GetSource: IStream;
begin
  Result := FBuffer.Source;
end;

procedure TSilPacket.SetSource(const Source: IStream);
begin
  FBuffer.Source := Source;
end;

end.
