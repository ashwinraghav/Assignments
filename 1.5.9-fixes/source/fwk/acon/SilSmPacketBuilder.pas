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

unit SilSmPacketBuilder;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBeTypes,
  SilSiPacketBuilder;

type

{ TPacketBuilder }

  TPacketBuilder = class(
    // extends
    TSilInterfacedObject,
    // implements
    IPacketData)
  private
    FSize: Cardinal;
    FBuffer: PChar;
    FIndex: Cardinal;
  protected
    function GetSize: Cardinal;
    function GetBuffer: Pointer;
    procedure DoWrite(ValueType: Byte; ValueSize: Cardinal; const Value);
    procedure DoRead(ValueType: Byte; ValueSize: Cardinal; var Value);
  protected
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
    procedure WriteShort(const Value: SmallInt); // Ver comentario en la implementacion (DAP)
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
    function ReadShort: SmallInt; // Ver writeshort
    function ReadLongWord: LongWord;
    function ReadShortInt: ShortInt;
    function ReadSmallInt: SmallInt;
    function ReadSingle: Single;
    function ReadFloat: Double;
    function ReadGuid: TGuid;
    function ReadVariant: Variant;
    function ReadWideString: WideString;
    function ReadValueType: Byte;
  public
    constructor Create(Buffer: PChar = nil; Size: Cardinal = 0);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtMem,
  SilBtVart,
  SilBtError;

{ TPacketBuilder }

constructor TPacketBuilder.Create(Buffer: PChar; Size: Cardinal);
begin
  inherited Create;
  ResetBuffer(Buffer, Size);
end;

destructor TPacketBuilder.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPacketBuilder.ResetBuffer(Buffer: PChar; Size: Cardinal);
begin
  Clear;

  if (Buffer <> nil) and (Size > 0) then
  begin
    GetMem(FBuffer, Size);
    Move(Buffer^, FBuffer^, Size);
  end else
    FBuffer := nil;

  FSize := Size;
  FIndex := 0;
end;

procedure TPacketBuilder.DoRead(ValueType: Byte; ValueSize: Cardinal; var Value);
var
  bVal: Byte;
begin
  if FIndex + ValueSize > FSize - 1 then Error.Throw('lectura fuera del buffer');

  Mem.Move(FBuffer^, bVal, SizeOf(ValueType), FIndex, 0);
  if bVal <> ValueType then Error.Throw('tipo invalido');
  Inc(FIndex, SizeOf(ValueType));

  Mem.Move(FBuffer^, Value, ValueSize, FIndex, 0);
  Inc(FIndex, ValueSize);
end;

procedure TPacketBuilder.DoWrite(ValueType: Byte; ValueSize: Cardinal; const Value);
var
  iOldSize: Integer;
begin
  iOldSize := FSize;
  Inc(FSize, ValueSize + 1); // 1 = ValueType
  ReallocMem(FBuffer, FSize);
  Mem.Move(ValueType, FBuffer^, SizeOf(ValueType), 0, iOldSize);
  Mem.Move(Value, FBuffer^, ValueSize, 0, iOldSize + 1);
end;

function TPacketBuilder.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TPacketBuilder.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TPacketBuilder.ReadBoolean: Boolean;
begin
  DoRead(tiBoolean, SizeOf(Boolean), Result);
end;

function TPacketBuilder.ReadInteger: Integer;
begin
  DoRead(tiInteger, SizeOf(Integer), Result);
end;

function TPacketBuilder.ReadPChar: PChar;
var
  cSize: Cardinal;
begin
  Result := ReadPChar(cSize);
end;

function TPacketBuilder.ReadPChar(var Size: Cardinal): PChar;
begin
  DoRead(tiAnsiString, SizeOf(Size), Size);
  Result := PChar(FBuffer) + FIndex;
  Inc(FIndex, Size);
  Dec(Size);
end;

function TPacketBuilder.ReadString: String;
var
  PStr: PChar;
  cLen: Cardinal;
begin
  PStr := ReadPChar(cLen);
  SetString(Result, PStr, cLen);
end;

function TPacketBuilder.ReadValueType: Byte;
begin
  Result := 0;
  if FIndex + SizeOf(Result) > FSize - 1 then Exit;
  Mem.Move(FBuffer^, Result, SizeOf(Result), FIndex, 0);
end;

procedure TPacketBuilder.Clear;
begin
  FreeMem(FBuffer);
  ResetRead;
end;

procedure TPacketBuilder.ResetRead;
begin
  FIndex := 0;
end;

procedure TPacketBuilder.WriteBoolean(const Value: Boolean);
begin
  DoWrite(tiBoolean, SizeOf(Boolean), Value);
end;

procedure TPacketBuilder.WriteInteger(const Value: Integer);
begin
  DoWrite(tiInteger, SizeOf(Integer), Value);
end;

procedure TPacketBuilder.WritePChar(const Value: PChar; Size: Cardinal);
var
  PBuf: PChar;
begin
  if Size = 0 then Size := Length(Value);
  Inc(Size); // for null #0

  PBuf := Mem.Alloc(Size + SizeOf(Size));

  try
    Move(Size, PBuf^, SizeOf(Integer));
    Mem.Move(Value^, PBuf^, Size, 0, SizeOf(Integer));
    DoWrite(tiAnsiString, Size + SizeOf(Size), PBuf^);
  finally
    FreeMem(PBuf);
  end;
end;

procedure TPacketBuilder.WriteString(const Value: String);
begin
  WritePChar(PChar(Value), Length(Value));
end;

procedure TPacketBuilder.WriteByte(const Value: Byte);
begin
  DoWrite(tiByte, 1, Value);
end;

function TPacketBuilder.ReadByte: Byte;
begin
  DoRead(tiByte, 1, Result);
end;

procedure TPacketBuilder.ReadBuffer(var Value; Size: Cardinal);
var
  Buf: PChar;
begin
  Buf := ReadPChar(Size);
  Move(Buf^, Value, Size);
end;

procedure TPacketBuilder.WriteBuffer(const Value; Size: Cardinal);
begin
  WritePChar(PChar(@Value), Size);
end;

function TPacketBuilder.ReadFloat: Double;
begin
  DoRead(tiDouble, SizeOf(Double), Result);
end;

function TPacketBuilder.ReadLongWord: LongWord;
begin
  DoRead(tiInteger, SizeOf(Integer), Result);
end;

function TPacketBuilder.ReadWord: Word;
begin
  DoRead(tiWord, SizeOf(Word), Result);
end;

function TPacketBuilder.ReadShort: SmallInt;
begin
  DoRead(tiShort, SizeOf(SmallInt), Result);
end;

function TPacketBuilder.ReadLargeInt: LargeInt;
begin
  DoRead(tiLargeInt, SizeOf(LargeInt), Result);
end;

function TPacketBuilder.ReadShortInt: ShortInt;
begin
  DoRead(tiShortInt, SizeOf(ShortInt), Result);
end;

function TPacketBuilder.ReadSmallInt: SmallInt;
begin
  DoRead(tiSmallInt, SizeOf(SmallInt), Result);
end;

function TPacketBuilder.ReadSingle: Single;
begin
  DoRead(tiSingle, SizeOf(Single), Result);
end;

procedure TPacketBuilder.WriteFloat(const Value: Double);
begin
  DoWrite(tiDouble, SizeOf(Double), Value);
end;

procedure TPacketBuilder.WriteLargeInt(const Value: LargeInt);
begin
  DoWrite(tiLargeInt, SizeOf(LargeInt), Value);
end;

procedure TPacketBuilder.WriteShortInt(const Value: ShortInt);
begin
  DoWrite(tiShortInt, SizeOf(ShortInt), Value);
end;

procedure TPacketBuilder.WriteSmallInt(const Value: SmallInt);
begin
  DoWrite(tiSmallInt, SizeOf(SmallInt), Value);
end;

procedure TPacketBuilder.WriteLongWord(const Value: LongWord);
begin
  DoWrite(tiInteger, SizeOf(Integer), Value);
end;

procedure TPacketBuilder.WriteSingle(const Value: Single);
begin
  DoWrite(tiSingle, SizeOf(Single), Value);
end;

procedure TPacketBuilder.WriteWord(const Value: Word);
begin
  DoWrite(tiWord, SizeOf(Word), Value);
end;

procedure TPacketBuilder.WriteShort(const Value: SmallInt);
begin
 (*)
   Esta funcion se implemento pues el tipo de valor que debe enviar en el caso de un short es
   tiShort (5) y no tiShortInt (23) o tiSmallInt(24). Asi lo requeria el protocolo del pasamandril
   Las demas funciones no se han tocado.
   El READShort tambien se hizo asi.

   DAP (Damian)
 (*)
  DoWrite(tiShort, SizeOf(SmallInt), Value);
end;

function TPacketBuilder.ReadGuid: TGuid;
begin
  ReadBuffer(Result, SizeOf(TGuid));
end;

procedure TPacketBuilder.WriteGuid(const Value: TGuid);
begin
  WriteBuffer(Value, SizeOf(TGuid));
end;

function TPacketBuilder.ReadVariant: Variant;
var
  wType: Word;
begin
  DoRead(tiVariant, SizeOf(Word), wType);

  case wType of
    varEmpty:     Result := Vart.Unassigned;
    varNull:      Result := Vart.Null;
    varSmallint:  Result := ReadSmallInt;
    varInteger:   Result := ReadInteger;
    varSingle:    Result := ReadSingle;
    varCurrency,
    varDouble:    Result := ReadFloat;
    varDate:      Result := ReadFloat;
    varOleStr:    Result := ReadWideString;
    //varDispatch
    //varError
    varBoolean:   Result := ReadBoolean;
    //varVariant
    //varUnknown
    //varDecimal
    varShortInt:  Result := ReadShortInt;
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

procedure TPacketBuilder.WriteVariant(const Value: Variant);
var
  wType: Word;
begin
  wType := Vart.VType(Value);
  DoWrite(tiVariant, SizeOf(Word), wType);

  case wType of
    //varEmpty
    //varNull
    varSmallint:  WriteSmallInt(Value);
    varInteger:   WriteInteger(Value);
    varSingle:    WriteSingle(Value);
    varCurrency,
    varDouble:    WriteFloat(Value);
    varDate:      WriteFloat(Value);
    varOleStr:    WriteWideString(Value);
    //varDispatch
    //varError
    varBoolean:   WriteBoolean(Value);
    //varVariant
    //varUnknown
    //varDecimal
    varShortInt:  WriteShortInt(Value);
    varByte:      WriteByte(Value);
    varWord:      WriteWord(Value);
    varLongWord:  WriteLongWord(Value);
    {$IFDEF D60}
    varInt64:     WriteLargeInt(Value);
    {$ELSE}
    varInt64:     WriteLargeInt(Integer(Value));
    {$ENDIF}
    varString:    WriteString(Value);
  end;
end;

function TPacketBuilder.ReadWideString: WideString;
var
  lwLen: LongWord;
begin
  DoRead(tiWideString, SizeOf(lwLen), lwLen);
  Result := PWideChar(PChar(FBuffer) + FIndex);
  Inc(FIndex, lwLen);
end;

procedure TPacketBuilder.WriteWideString(const Value: WideString);
var
  PBuf: PChar;
  lwLen: LongWord;
begin
  lwLen := Length(Value) * 2;
  Inc(lwLen, 2); // for null #0 #0

  PBuf := Mem.Alloc(lwLen + SizeOf(lwLen));

  try
    Move(lwLen, PBuf^, SizeOf(lwLen));
    Mem.Move(Value[1], PBuf^, lwLen, 0, SizeOf(lwLen));
    DoWrite(tiWideString, lwLen + SizeOf(lwLen), PBuf^);
  finally
    FreeMem(PBuf);
  end;
end;

end.
