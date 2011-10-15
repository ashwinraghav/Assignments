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

unit SilLtRawFiler;

{$I Defines.inc}

interface

uses
  SilLjFiler,
  SilLiStream,
  SilLiFiler,
  SilLiPacket;

type
  Factory = class (FilerFactory)
    class function Reader(const Stream: IStream): IReader; override;
    class function Writer(const Stream: IStream = nil): IWriter; override;
    class function Packet(const Stream: IStream = nil; Buffered: Boolean = false): IPacket; override;
  end;

implementation

uses
  SilLtTool,
  SilLmReader,
  SilLmWriter,
  SilLmRawPacket;

class function Factory.Packet(const Stream: IStream; Buffered: Boolean): IPacket;
begin
  Result := TSilRawPacket.Create(Stream, Buffered);
end;

class function Factory.Reader(const Stream: IStream): IReader;
begin
  Result := TStreamReader.Create(Stream);
end;

class function Factory.Writer(const Stream: IStream): IWriter;
var
  Stm: IStream;
begin
  if Stream <> nil then
    Stm := Stream else
    Stm := Tk.MemoryStream();
  Result := TStreamWriter.Create(Stm);
end;

end.
