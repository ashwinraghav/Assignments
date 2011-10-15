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

unit SilSeLayerProtocol;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer;

type
  TImateProtocolFlag = 0..32;
  TImateProtocolFlags = set of TImateProtocolFlag;

  PImateProtocolHeader = ^RImateProtocolHeader;
  RImateProtocolHeader = packed record
    Id: Word;
    Version: LongWord;
    Flags: LongWord;
    Size: LongWord;
  end;

  PImateProtocolMessage = ^RImateProtocolMessage;
  RImateProtocolMessage = packed record
    Id: Word;             // no se debe cambiar la posicion
    Packet: IPacket;      // item
    Version: LongWord;
    Flags: LongWord;
    Context: IUnknown;
  end;

const
  ifException = TImateProtocolFlag(32);

implementation

end.
