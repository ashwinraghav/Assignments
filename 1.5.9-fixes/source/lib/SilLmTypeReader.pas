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

unit SilLmTypeReader;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBeTypes,
  SilLiFiler,
  SilLiStream;

type
  TTypeStreamReader = class(
    TSilInterfacedObject,
    IReader )
  private
    FStream: IStream;
    procedure DoRead(ValType: Byte; ValSize: Cardinal; var Value);
    procedure DoReadAnsiStr(ValSize: Integer; var Value: String);
    procedure DoReadWideStr(ValSize: Integer; var Value: WideString);
  protected // IReader
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
  public
    constructor Create(const Stream: IStream);
    destructor Destroy; override;
  end;

implementation

uses
  SilBcChr,
  SilBtStr,
  SilBtError,
  SilBtVart,
  SilLdReader,
  SilLeTypeReader;

{ TTypeStreamReader }

constructor TTypeStreamReader.Create(const Stream: IStream);
begin
  inherited Create;
  Error.Check(Stream <> nil, SErrorStreamIsNil);
  FStream := Stream;
end;

destructor TTypeStreamReader.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TTypeStreamReader.DoRead(ValType: Byte; ValSize: Cardinal; var Value);
var
  yType: Byte;

  procedure DoStreamRead(var Buf; Size: LongWord);
  var
    lwCount: LongWord;
  begin
    lwCount := FStream.Read(Buf, Size);
    Error.Check(lwCount >= Size, Str.Format(SErrorStreamReadFailed, [Size, lwCount]));
  end;

begin
  DoStreamRead(yType, 1);
  Error.Check(yType = ValType, SErrorStreamWrongType);
  DoStreamRead(Value, ValSize);
end;

procedure TTypeStreamReader.DoReadAnsiStr(ValSize: Integer; var Value: String);
var
  lwSize: LongWord;
begin
  DoRead(tiAnsiString, 4, lwSize);
  if ValSize = -1 then ValSize := lwSize;
  SetLength(Value, ValSize);
  FStream.Read(Value[1], ValSize);
end;

procedure TTypeStreamReader.DoReadWideStr(ValSize: Integer; var Value: WideString);
var
  lwSize: LongWord;
begin
  DoRead(tiWideString, SizeOf(lwSize), lwSize);
  
  if ValSize = -1 then
    ValSize := lwSize;
    
  if lwSize > 0 then
  begin
    SetLength(Value, ValSize);
    FStream.Read(PWideChar(Value)^, ValSize * 2);
  end;
end;

procedure TTypeStreamReader.Read(var Buf; Count: Integer);
var
  lwCount: LongWord;
begin
  lwCount := FStream.Read(Buf, Count);
  Error.Check(lwCount >= LongWord(Count), Str.Format(SErrorStreamReadFailed, [Count, lwCount]));
end;

function TTypeStreamReader.ReadBoolean: Boolean;
begin
  DoRead(tiBoolean, SizeOf(Boolean), Result);
end;

function TTypeStreamReader.ReadByte: Byte;
begin
  DoRead(tiByte, SizeOf(Byte), Result);
end;

function TTypeStreamReader.ReadChar: Char;
begin
  DoRead(tiChar, SizeOf(Char), Result);
end;

function TTypeStreamReader.ReadCurrency: Currency;
begin
  DoRead(tiCurrency, SizeOf(Currency), Result);
end;

function TTypeStreamReader.ReadDate: TDateTime;
begin
  DoRead(tiDate, SizeOf(TDateTime), Result);
end;

function TTypeStreamReader.ReadDouble: Double;
begin
  DoRead(tiDouble, SizeOf(Double), Result);
end;

function TTypeStreamReader.ReadGuid: TGUID;
begin
  DoRead(tiGuid, SizeOf(TGuid), Result);
end;

function TTypeStreamReader.ReadInteger: Integer;
begin
  DoRead(tiInteger, SizeOf(Integer), Result);
end;

function TTypeStreamReader.ReadLargeInt: LargeInt;
begin
  DoRead(tiLargeInt, SizeOf(LargeInt), Result);
end;

function TTypeStreamReader.ReadLongWord: LongWord;
begin
  DoRead(tiLongWord, SizeOf(Integer), Result);
end;

function TTypeStreamReader.ReadSingle: Single;
begin
  DoRead(tiSingle, SizeOf(Single), Result);
end;

function TTypeStreamReader.ReadWord: Word;
begin
  DoRead(tiWord, SizeOf(Word), Result);
end;

function TTypeStreamReader.ReadString(const Size: Integer): string;
begin
  DoReadAnsiStr(Size, Result);
end;

function TTypeStreamReader.ReadWideString(const Size: Integer): WideString;
begin
  DoReadWideStr(Size, Result);
end;

function TTypeStreamReader.ReadLn(out Buf: String): Boolean;
begin
  Buf := ReadString;
  Result := Length(Buf) > 0;
end;

function TTypeStreamReader.ReadObject(const Instance: IInterface): IUnknown;
begin
end;

function TTypeStreamReader.ReadVariant: Variant;
var
  wType: Word;
begin
  DoRead(tiVariant, SizeOf(Word), wType);

  case wType of
    varEmpty:     Result := Vart.Unassigned;
    varNull:      Result := Vart.Null;
    varSmallint,
    varShortInt,
    varInteger:   Result := ReadInteger;
    varSingle:    Result := ReadSingle;
    varDouble:    Result := ReadDouble;
    varCurrency:  Result := ReadCurrency;
    varDate:      Result := ReadDate;
    varOleStr:    Result := ReadWideString;
    varBoolean:   Result := ReadBoolean;
    //varVariant
    //varUnknown
    //varDecimal
    varByte:      Result := ReadByte;
    varWord:      Result := ReadWord;
    {$IFDEF D60}
    varLongWord:  Result := ReadLongWord;
    varInt64:     Result := ReadLargeInt;
    {$ELSE}
    varLongWord:  Result := Integer(ReadLongWord);
    varInt64:     Result := Integer(ReadLargeInt);
    {$ENDIF}
    varString:    Result := ReadString;
    else          Result := Vart.Unassigned;
  end;
end;

end.
