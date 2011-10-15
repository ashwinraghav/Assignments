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

unit SilLmReader;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBeTypes,
  SilLiFiler,
  SilLiStream;

type
  TStreamReader = class(
    TSilInterfacedObject,
    IReader )
  private
    FStream: IStream;
    procedure ReadAnsiStr(Count: Integer; var Str: string);
    procedure ReadAnsiBuf(var Str: string);
    procedure ReadWideStr(Count: Integer; var Str: WideString);
    procedure ReadWideBuf(var Str: WideString);
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
  SilLdReader;

{ TStreamReader }

constructor TStreamReader.Create(const Stream: IStream);
begin
  inherited Create;
  Error.Check(Stream <> nil, SErrorStreamIsNil);
  FStream := Stream;
end;

destructor TStreamReader.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TStreamReader.Read(var Buf; Count: Integer);
var
  Result: Cardinal;
begin
  Result := FStream.Read(Buf, Count);
  Error.Check(Result >= LongWord(Count), Str.Format(SErrorStreamReadFailed, [Count, Result]));
end;

function TStreamReader.ReadLn(out Buf: String): Boolean;
const
  FindEol: array [0..1] of Char = (ccCR, ccLF);
var
  i: Integer;
  bFound: Byte;
begin
  i := 1;
  bFound := 0;
  Result := false;
  SetLength(Buf, 1024);

  while FStream.Read(Buf[i], 1) > 0 do
  begin
    if Buf[i] = FindEol[bFound] then
    begin
      Inc(bFound);
      if bFound > 1 then
      begin
        Result := true;
        SetLength(Buf, i - 2);
        Exit;
      end;
    end else
    if bFound > 0 then bFound := 0;

    if i >= Length(Buf) then SetLength(Buf, i + 1024);
    Inc(i);
  end;

  if i > 1 then
  begin
    Result := true;
    SetLength(Buf, i - 1);
  end;
end;

function TStreamReader.ReadBoolean: Boolean;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadByte: Byte;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadChar: Char;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadCurrency: Currency;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadDate: TDateTime;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadDouble: Double;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadWord: Word;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadGuid: TGUID;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadInteger: Integer;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadLargeInt: LargeInt;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadLongWord: LongWord;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadSingle: Single;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamReader.ReadString(const Size: Integer): string;
begin
  if Size <> -1 then
    ReadAnsiStr(Size, Result) else
    ReadAnsiBuf(Result);
end;

function TStreamReader.ReadWideString(const Size: Integer): WideString;
begin
  if Size <> -1 then
    ReadWideStr(Size, Result) else
    ReadWideBuf(Result);
end;

function TStreamReader.ReadVariant: Variant;
begin
end;

function TStreamReader.ReadObject(const Instance: IUnknown): IUnknown;
begin
end;

procedure TStreamReader.ReadAnsiBuf(var Str: string);
var
  C: Char;
  N: LongWord;
begin
  Str := '';
  repeat
    N := FStream.Read(C, SizeOf(C));
    if N > 0 then
      Str := Str + C else
      Break;
  until C = #0;
end;

procedure TStreamReader.ReadAnsiStr(Count: Integer; var Str: string);
begin
  SetLength(Str, Count);
  Read(Str[1], Count * SizeOf(Str[1]));
end;

procedure TStreamReader.ReadWideBuf(var Str: WideString);
var
  C: WideChar;
  N: LongWord;
begin
  Str := '';
  repeat
    N := FStream.Read(C, SizeOf(C));
    if N > 0 then
      Str := Str + C else
      Break;
  until C = #0;
end;

procedure TStreamReader.ReadWideStr(Count: Integer; var Str: WideString);
begin
  SetLength(Str, Count);
  Read(Str[1], Count * SizeOf(Str[1]));
end;

end.
