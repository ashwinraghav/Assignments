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

unit SilSiProtocolPacket;

{$I Defines.inc}

interface

uses
  SilSiAbstractConnection,
  SilSiPacketBuilder;

type
  TProtocolPacketFlag = 0..31;
  TProtocolPacketFlags = set of TProtocolPacketFlag;

  PProtocolHeader = ^TProtocolHeader;
  TProtocolHeader = packed record
    ProtoID: Word;
    ProtoVer: Word;
    HeaderVer: Word;
    HeaderSize: Word;
    SessionID: Cardinal;
    Flags: LongWord;
    DataID: Cardinal;
    DataSize: Cardinal;
  end;

const
  pfDelayedWrite  = TProtocolPacketFlag(0);
  pfException     = TProtocolPacketFlag(31);

type
  IProtocolPacket = interface
    ['{62E11902-FC14-11D3-9875-00104B0FA1EF}']
    function GetProtoID: Word;
    procedure SetProtoID(const Value: Word);
    function GetHeaderVer: Word;
    procedure SetProtoVer(const Value: Word);
    function GetProtoVer: Word;
    procedure SetHeaderVer(const Value: Word);
    function GetSessionID: Cardinal;
    procedure SetSessionID(const Value: Cardinal);
    function GetDataID: Cardinal;
    procedure SetDataID(const Value: Cardinal);
    function GetHeaderSize: Cardinal;
    function GetData: IPacketData;
    function GetSize: Cardinal;
    function GetValid: Boolean;
    function GetFlags: TProtocolPacketFlags;
    procedure SetFlags(Value: TProtocolPacketFlags);
    function GetBufferPtr: Pointer;
    function Build(out Buffer: String): Boolean;
    property ProtoID: Word read GetProtoID write SetProtoID;
    property ProtoVer: Word read GetProtoVer write SetProtoVer;
    property HeaderVer: Word read GetHeaderVer write SetHeaderVer;
    property HeaderSize: Cardinal read GetHeaderSize;
    property SessionID: Cardinal read GetSessionID write SetSessionID;
    property DataID: Cardinal read GetDataID write SetDataID;
    property Data: IPacketData read GetData;
    property Size: Cardinal read GetSize;
    property IsValid: Boolean read GetValid;
    property Flags: TProtocolPacketFlags read GetFlags write SetFlags;
  end;

implementation

end.
