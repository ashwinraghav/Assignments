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

unit SilSiCommunicationQueue;

{$I Defines.inc}

interface

uses
  SilSiProtocolPacket;

type
  TQueueOverflowMode = (omDiscardOld, omIgnoreAppend);

  ICommunicationQueue = interface
    ['{68389B32-7E94-11D4-9897-00104B0FA1EF}']
    function GetName: String;
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
    function GetOverflowMode: TQueueOverflowMode;
    procedure SetOverflowMode(Value: TQueueOverflowMode);
    procedure AppendPacket(const Packet: IProtocolPacket);
    procedure RemovePacket(const Packet: IProtocolPacket);
    function GetPacket(out Packet: IProtocolPacket): Boolean;
    property Name: String read GetName;
    property Size: LongWord read GetSize write SetSize;
    property OverflowMode: TQueueOverflowMode read GetOverflowMode write SetOverflowMode;
  end;

implementation

end.
 