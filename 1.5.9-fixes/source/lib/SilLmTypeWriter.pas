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

unit SilLmTypeWriter;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBeTypes,
  SilLiFiler,
  SilLiStream;

type
  TTypeStreamWriter = class(
    TSilInterfacedObject,
    IWriter1,
    IWriter )
  private
    FStream: IStream;
    procedure DoWrite(ValueType: Byte; ValueSize: Cardinal; const Value);
  protected // IWriter1
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
    procedure IWriter1.WriteString = DoWriteString;
    procedure DoWriteString(const Value: string);
    procedure IWriter1.WriteWideString = DoWriteWideString;
    procedure DoWriteWideString(const Value: WideString);
    procedure WriteVariant(const Value: Variant);
    procedure WriteGuid(const Value: TGUID);
    procedure WriteObject(const Value: IUnknown);
  protected // IWriter
    procedure WriteString(const Value: string; Count: Integer = -1); overload; 
    procedure WriteString(const Value: PChar); overload;
    procedure WriteWideString(const Value: WideString; Count: Integer = -1); overload;
    procedure WriteWideString(const Value: PWideChar); overload; 
  public
    constructor Create(const Stream: IStream);
    destructor Destroy; override;
  end;
  
implementation

uses
  SilBcChr,
  SilBtVart,
  SilBtError,
  SilBtStr,
  SilBtInt,
  SilOtTool,
  SilLdReader,
  SilLeTypeReader;

{ TTypeStreamWriter }

constructor TTypeStreamWriter.Create(const Stream: IStream);
begin
  inherited Create;
  Error.Check(Stream <> nil, SErrorStreamIsNil);
  FStream := Stream;
end;

destructor TTypeStreamWriter.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TTypeStreamWriter.DoWrite(ValueType: Byte; ValueSize: Cardinal; const Value);
begin
  FStream.Write(ValueType, 1);
  FStream.Write(Value, ValueSize);
end;

procedure TTypeStreamWriter.Write(const Buf; Count: Integer);
var
  Result: Cardinal;
begin
  Result := FStream.Write(Buf, Count);
  Error.Check(Result >= LongWord(Count), Str.Format(SErrorStreamWriteFailed, [Count, Result]));
end;

procedure TTypeStreamWriter.WriteBoolean(Value: Boolean);
begin
  DoWrite(tiBoolean, SizeOf(Boolean), Value);
end;

procedure TTypeStreamWriter.WriteByte(Value: Byte);
begin
  DoWrite(tiByte, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteChar(Value: Char);
begin
  DoWrite(tiChar, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteCurrency(const Value: Currency);
begin
  DoWrite(tiCurrency, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteDate(const Value: TDateTime);
begin
  DoWrite(tiDate, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteDouble(const Value: Double);
begin
  DoWrite(tiDouble, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteGuid(const Value: TGUID);
begin
  DoWrite(tiGuid, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteInteger(Value: Integer);
begin
  DoWrite(tiInteger, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteLargeInt(Value: LargeInt);
begin
  DoWrite(tiLargeInt, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteLongWord(Value: LongWord);
begin
  DoWrite(tiLongWord, SizeOf(Value), Value);
end;


procedure TTypeStreamWriter.WriteObject(const Value: IInterface);
begin
end;

procedure TTypeStreamWriter.WriteSingle(const Value: Single);
begin
  DoWrite(tiSingle, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.WriteWord(Value: Word);
begin
  DoWrite(tiWord, SizeOf(Value), Value);
end;

procedure TTypeStreamWriter.DoWriteString(const Value: string);
begin
  WriteString(Value, -1);
end;

procedure TTypeStreamWriter.DoWriteWideString(const Value: WideString);
begin
  WriteWideString(Value, -1);
end;

procedure TTypeStreamWriter.WriteLn(const Buf: String);
begin
  DoWriteString(Buf);
end;

procedure TTypeStreamWriter.WriteVariant(const Value: Variant);
var
  wType: Word;
begin
  wType := Vart.VType(Value);
  DoWrite(tiVariant, SizeOf(Word), wType);

  case wType of
    varSmallint,
    varShortInt,
    varInteger:   WriteInteger(Value);
    varSingle:    WriteSingle(Value);
    varDouble:    WriteDouble(Value);
    varCurrency:  WriteCurrency(Value);
    varDate:      WriteDate(Value);
    varOleStr:    DoWriteWideString(Value);
    varBoolean:   WriteBoolean(Value);
    //varVariant
    //varUnknown
    //varDecimal
    varByte:      WriteByte(Value);
    varWord:      WriteWord(Value);
    {$IFDEF D60}
    varLongWord:  WriteLongWord(Value);
    varInt64:     WriteLargeInt(Value);
    {$ELSE}
    varLongWord:  WriteInteger(Value);
    varInt64:     WriteLargeInt(Integer(Value));
    {$ENDIF}
    varString:    DoWriteString(Value);
  end;
end;

procedure TTypeStreamWriter.WriteString(const Value: string; Count: Integer);
var
  lwSize: LongWord;
  Buf: string;
begin
  lwSize := Length(Value);
  if Count = -1 then Count := lwSize;
  DoWrite(tiAnsiString, SizeOf(lwSize), Count);
  if LongWord(Count) > lwSize then
  begin
    SetLength(Buf, Count);
    System.FillChar(Buf[1], Count, 0);
    System.Move(Value[1], Buf[1], lwSize);
  end else
    Buf := Value;
  FStream.Write(Buf[1], Count);
end;

procedure TTypeStreamWriter.WriteString(const Value: PChar);
var
  lwSize: LongWord;
begin
  lwSize := Str.Len(Value);
  DoWrite(tiAnsiString, SizeOf(lwSize), lwSize);
  FStream.Write(Value^, lwSize);
end;

procedure TTypeStreamWriter.WriteWideString(const Value: WideString; Count: Integer);
var
  lwSize: LongWord;
  Buf: WideString;
begin
  lwSize := Length(Value);
  if Count = -1 then Count := lwSize;
  DoWrite(tiWideString, SizeOf(lwSize), Count);
  if LongWord(Count) > lwSize then
  begin
    SetLength(Buf, Count);
    System.FillChar(Buf[1], Count * SizeOf(Value[1]), 0);
    System.Move(Value[1], Buf[1], lwSize * SizeOf(Value[1]));
  end else
    Buf := Value;
  FStream.Write(Buf[1], Count * SizeOf(Value[1]));
end;

procedure TTypeStreamWriter.WriteWideString(const Value: PWideChar);
var
  lwSize: LongWord;
begin
  lwSize := WStr.Len(Value);
  DoWrite(tiWideString, SizeOf(lwSize), lwSize);
  FStream.Write(Value^, lwSize * SizeOf(Value^));
end;

end.
