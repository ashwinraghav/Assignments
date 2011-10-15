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

unit SilSiPacketBuilder;

{$I Defines.inc}

interface

uses
  SilBeTypes;

const
  tiInteger     = 0;
  tiBoolean     = 1;
  tiByte        = 2;
  tiDouble      = 3;
  tiShort       = 5;
//  tiWideChar    = 9;
  tiAnsiString  = 11;
  tiWideString  = 15;
  tiLargeInt    = 16;
  tiWord        = 20;
  tiLongWord    = 21;
//tiLargeWord   = 22;
  tiShortInt    = 23;
  tiSmallInt    = 24;
  tiSingle      = 25;
  tiVariant     = 26;

type

{ IPacketData }

  IPacketData = interface
    ['{62E11901-FC14-11D3-9875-00104B0FA1EF}']
    procedure Clear;
    procedure ResetRead;
    procedure ResetBuffer(Buffer: PChar; Size: Cardinal);
    procedure WriteBuffer(const Value; Size: Cardinal);
    procedure WriteInteger(const Value: Integer);
    procedure WriteBoolean(const Value: Boolean);
    procedure WriteByte(const Value: Byte);
    procedure WritePChar(const Value: PChar; Size: Cardinal = 0);
    procedure WriteString(const Value: String);
    procedure WriteLargeInt(const Value: LargeInt);
    procedure WriteWord(const Value: Word);
    procedure WriteShort(const Value: SmallInt);
    procedure WriteLongWord(const Value: LongWord);
    procedure WriteShortInt(const Value: ShortInt);
    procedure WriteSmallInt(const Value: SmallInt);
    procedure WriteSingle(const Value: Single);
    procedure WriteFloat(const Value: Double);
    procedure WriteGuid(const Value: TGuid);
    procedure WriteVariant(const Value: Variant);
    procedure WriteWideString(const Value: WideString);
    procedure ReadBuffer(var Value; Size: Cardinal);
    function ReadInteger: Integer;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadPChar(var Size: Cardinal): PChar; overload;
    function ReadPChar: PChar; overload;
    function ReadString: String;
    function ReadLargeInt: LargeInt;
    function ReadWord: Word;
    function ReadShort: SmallInt;
    function ReadLongWord: LongWord;
    function ReadShortInt: ShortInt;
    function ReadSmallInt: SmallInt;
    function ReadSingle: Single;
    function ReadFloat: Double;
    function ReadGuid: TGuid;
    function ReadVariant: Variant;
    function ReadWideString: WideString;
    function ReadValueType: Byte;
    function GetSize: Cardinal;
    function GetBuffer: Pointer;
    property Size: Cardinal read GetSize;
    property Buffer: Pointer read GetBuffer;
  end;

implementation

end.
 