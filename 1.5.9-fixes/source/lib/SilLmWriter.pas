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

unit SilLmWriter;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBeTypes,
  SilLiFiler,
  SilLiStream;

type
  TStreamWriter = class(
    TSilInterfacedObject,
    IWriter1,
    IWriter )
  private
    FStream: IStream;
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
  SilOtTool,
  SilLdReader;

{ TStreamWriter }

constructor TStreamWriter.Create(const Stream: IStream);
begin
  inherited Create;
  Error.Check(Stream <> nil, SErrorStreamIsNil);
  FStream := Stream;
end;

destructor TStreamWriter.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TStreamWriter.Write(const Buf; Count: Integer);
var
  Result: Cardinal;
begin
  Result := FStream.Write(Buf, Count);
  Error.Check(Result >= LongWord(Count), Str.Format(SErrorStreamWriteFailed, [Count, Result]));
end;

procedure TStreamWriter.WriteLn(const Buf: String);
begin
  WriteString(Buf + ccCRLF);
end;

procedure TStreamWriter.WriteBoolean(Value: Boolean);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteByte(Value: Byte);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteChar(Value: Char);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteCurrency(const Value: Currency);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteDate(const Value: TDateTime);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteDouble(const Value: Double);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteGuid(const Value: TGUID);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteWord(Value: Word);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteInteger(Value: Integer);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteLargeInt(Value: LargeInt);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteLongWord(Value: LongWord);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.WriteSingle(const Value: Single);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamWriter.DoWriteString(const Value: string);
begin
  WriteString(Value, -1);
end;

procedure TStreamWriter.DoWriteWideString(const Value: WideString);
begin
  Write(Value[1], Length(Value) * SizeOf(Value[1]));
end;

procedure TStreamWriter.WriteVariant(const Value: Variant);
var
  Vtype: LongWord;
begin
  Vtype := Vart.VType(Value);
  WriteLongWord(VType);
  case Vtype of
    varBoolean:
      WriteBoolean(Value);
    varByte:
      WriteByte(Value);
    varSmallint, varInteger:
      WriteInteger(Value);
    varSingle:
      WriteSingle(Value);
    varDouble:
      WriteDouble(Value);
    varDate:
      WriteDate(Value);
    varCurrency:
      WriteCurrency(Value);
    varString:
      WriteString(Value);
    varOleStr:
      DoWriteWideString(Value);
    varUnknown, varDispatch:
      WriteObject(Value);
  end;
end;

procedure TStreamWriter.WriteObject(const Value: IUnknown);
begin
end;

procedure TStreamWriter.WriteString(const Value: string; Count: Integer);
var
  Buf: string;
begin
  if Count <> -1 then
  begin
    SetLength(Buf, Count);
    System.FillChar(Buf[1], Count, 0);
    System.Move(Value[1], Buf[1], Length(Value));

    Write(Buf[1], Count);
  end else
    Write(Value[1], Length(Value));
end;

procedure TStreamWriter.WriteString(const Value: PChar);
begin
  Write(Value^, Str.Len(Value) * SizeOf(Value^));
end;

procedure TStreamWriter.WriteWideString(const Value: WideString; Count: Integer);
var
  Buf: WideString;
begin
  if Count <> -1 then
  begin
    SetLength(Buf, Count);
    System.FillChar(Buf[1], Count * SizeOf(Value[1]), 0);
    System.Move(Value[1], Buf[1], Length(Value) * SizeOf(Value[1]));
  end else
  begin
    Buf := Value;
    Count := Length(Buf);
  end;
  Write(Buf[1], Count * SizeOf(Buf[1]));
end;

procedure TStreamWriter.WriteWideString(const Value: PWideChar);
begin
  Write(Value^, WStr.Len(Value) * SizeOf(Value^));
end;

end.
