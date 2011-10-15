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

unit SilLiPacket;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilLiFiler;

type
  IPacket = interface
    ['{B34402C6-24C1-43A9-943F-B5ECC0449EFB}']
    function GetBuffer: IBufferStream;
    function GetSource: IStream;
    function GetReader: IReader;
    function GetWriter: IWriter;
    procedure SetSource(const Source: IStream);
    property Buffer: IBufferStream read GetBuffer;
    property Source: IStream read GetSource write SetSource;
    property Reader: IReader read GetReader;
    property Writer: IWriter read GetWriter;
  end;

  IPacketDelivery = interface
    ['{B14FC53C-DE01-40BC-B7F5-8695CBA08F19}']
    procedure Put(const Packet: IPacket);
  end;

implementation
end.

