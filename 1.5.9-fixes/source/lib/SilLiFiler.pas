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

unit SilLiFiler;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDataType;

type
  IPbPacketReader = interface
    ['{F2B0B021-4856-11D4-9889-00104B0FA1EF}']
    function GetBufferSize: LongWord;
    procedure SetBufferSize(Value: LongWord);
    function Read(var Buffer: String): Boolean;
    property BufferSize: LongWord read GetBufferSize write SetBufferSize;
  end;

  IReader = interface
    ['{3E6816A3-2035-11D4-AD73-00902794F778}']
    procedure Read(var Buf; Count: Longint); 
    function ReadLn(out Buf: String): Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadChar: Char;
    function ReadWord: Word;
    function ReadDouble: Double;
    function ReadSingle: Single;
    function ReadCurrency: Currency;
    function ReadDate: TDateTime;
    function ReadInteger: Integer;
    function ReadLongWord: LongWord;
    function ReadLargeInt: LargeInt;
    function ReadString(const Size: Integer = -1): string;
    function ReadWideString(const Size: Integer = -1): WideString;
    function ReadVariant: Variant;
    function ReadGuid: TGUID;
    function ReadObject(const Instance: IUnknown = nil): IUnknown;
  end;

  IWriter1 = interface
    ['{3E6816A4-2035-11D4-AD73-00902794F778}']
    procedure Write(const Buf; Count: Longint);
    procedure WriteLn(const Buf: String);
    procedure WriteBoolean(Value: Boolean);
    procedure WriteByte(Value: Byte);
    procedure WriteChar(Value: Char);
    procedure WriteWord(Value: Word);
    procedure WriteDouble(const Value: Double);
    procedure WriteSingle(const Value: Single);
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDate(const Value: TDateTime);
    procedure WriteInteger(Value: Integer);
    procedure WriteLongWord(Value: LongWord);
    procedure WriteLargeInt(Value: LargeInt);
    procedure WriteString(const Value: string);
    procedure WriteWideString(const Value: WideString);
    procedure WriteVariant(const Value: Variant);
    procedure WriteGuid(const Value: TGUID);
    procedure WriteObject(const Value: IUnknown);
  end;

  IWriter = interface
    ['{E950BDBF-D67B-4903-977F-70308FBA53BA}']
    procedure Write(const Buf; Count: Longint);
    procedure WriteLn(const Buf: String);
    procedure WriteBoolean(Value: Boolean);
    procedure WriteByte(Value: Byte);
    procedure WriteChar(Value: Char);
    procedure WriteWord(Value: Word);
    procedure WriteDouble(const Value: Double);
    procedure WriteSingle(const Value: Single);
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDate(const Value: TDateTime);
    procedure WriteInteger(Value: Integer);
    procedure WriteLongWord(Value: LongWord);
    procedure WriteLargeInt(Value: LargeInt);
    procedure WriteString(const Value: string; Count: Integer = -1); overload;
    procedure WriteString(const Value: PChar); overload;
    procedure WriteWideString(const Value: WideString; Count: Integer = -1); overload; 
    procedure WriteWideString(const Value: PWideChar); overload; 
    procedure WriteVariant(const Value: Variant);
    procedure WriteGuid(const Value: TGUID);
    procedure WriteObject(const Value: IUnknown);
  end;

  IValueReader = interface
    ['{73928376-567E-11D4-988A-00104B0FA1EF}']
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer;
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetAsChar: Char;
    function GetAsByte: Byte;
    function GetAsString: String;
    function GetAsWideString: WideString;
    function GetAsInteger: Integer;
    function GetAsLargeInt: LargeInt;
    function GetAsLongWord: LongWord;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsVariant: Variant;
    function GetAsGuid: TGUID;
    function GetAsInterface: IUnknown;
    function GetAsPointer: Pointer;
    property AsChar: Char read GetAsChar;
    property AsByte: Byte read GetAsByte;
    property AsString: String read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    property AsInteger: Integer read GetAsInteger;
    property AsLargeInt: LargeInt read GetAsLargeInt;
    property AsLongWord: LongWord read GetAsLongWord;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsFloat: Double read GetAsFloat;
    property AsCurrency: Currency read GetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime;
    property AsVariant: Variant read GetAsVariant;
    property AsGuid: TGUID read GetAsGuid;
    property AsInterface: IUnknown read GetAsInterface;
    property AsPointer: Pointer read GetAsPointer;
  end;

  IValueAccess = interface (IValueReader)
    ['{73928377-567E-11D4-988A-00104B0FA1EF}']
    procedure SetAsChar(Value: Char);
    procedure SetAsByte(Value: Byte);
    procedure SetAsString(const Value: String);
    procedure SetAsWideString(const Value: WideString);
    procedure SetAsInteger(Value: LongInt);
    procedure SetAsLargeInt(Value: LargeInt);
    procedure SetAsLongWord(Value: LongWord);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsFloat(Value: Double);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsGuid(const Value: TGUID);
    procedure SetAsInterface(const Value: IUnknown);
    procedure SetAsPointer(const Value: Pointer);
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsString: String read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
    property AsLongWord: LongWord read GetAsLongWord write SetAsLongWord;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
    property AsInterface: IUnknown read GetAsInterface write SetAsInterface;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
  end;

  IValueDef = interface (IValueAccess)
    ['{060E859D-C3ED-44F5-A5C9-69B7EF1A5834}']
    function GetDataType: TDataFieldType;
    procedure SetDataType(Value: TDataFieldType);
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
    property DataType: TDataFieldType read GetDataType write SetDataType;
    property Size: LongWord read GetSize write SetSize;
  end;

implementation

end.
