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

unit SilSiIcmpProtocol;

{$INCLUDE Defines.inc}

interface

uses
  Sil;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;

type
  REcho = packed record
    Id: Word;
    Sequence: Word;
    Data: String;
  end;

type
  IIcmpCommandEcho = interface
    ['{C2804FCF-D206-4535-9C1E-04943D3E5673}']
    function Pack(const Query: REcho): IPacket;
    function Unpack(const Buffer: IPacket): REcho;
  end;

  IIcmpProtocol = interface
    ['{7E937181-0DA6-41BA-A34A-99B362D196A3}']
    function GetEcho: IIcmpCommandEcho;
    property Echo: IIcmpCommandEcho read GetEcho;
  end;

implementation
end.
