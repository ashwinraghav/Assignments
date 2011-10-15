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

unit SilSmMimeCoder;

interface

{$I Defines.inc}

uses
  Sil,
  SilSiMailClient;

type
  TMimeCoder = class;
  TMimeCoderClass = class of TMimeCoder;

{ TMimeCoder }

  TMimeCoder = class
  public
    class function Name: String; virtual; abstract;
    class function MaxLineSize: Integer; virtual;
    class function EncodeLine(const Buffer: String): String; virtual; abstract;
    class function Encode(const Source: IRandomStream; const Dest: IStringList; Progress: TProgressCallback = nil): String; overload; virtual;
    class function Encode(const Source, Dest: IStream; Progress: TProgressCallback = nil): String; overload; virtual;
    class function DecodeLine(const Buffer: String): String; virtual; abstract;
    class function Decode(const Source: IStringList; const Dest: IRandomStream; Progress: TProgressCallback = nil): Boolean; overload; virtual;
    class function Decode(const Source, Dest: IStream; Progress: TProgressCallback = nil): Boolean; overload; virtual;
  end;

{ TBase64Coder }

  TBase64Coder = class(TMimeCoder)
  public
    class function MaxLineSize: Integer; override;
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Name: String; override;
  end;

{ TUUCoder }

  TUUCoder = class(TMimeCoder)
  public
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Name: String; override;
  end;

{ TISO8859_1Coder }

  TISO8859_1Coder = class(TMimeCoder)
  public
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Decode(const Source: IStringList; const Dest: IRandomStream; Progress: TProgressCallback): Boolean; override;
    class function Name: String; override;
  end;

{ TQuotedPrintableCoder }

  TQuotedPrintableCoder = class(TISO8859_1Coder)
  public
    class function Name: String; override;
  end;

{ TWindows1252Coder }

  TWindows1252Coder = class(TISO8859_1Coder)
  public
    class function Name: String; override;
  end;

{ THexaCoder }

  THexaCoder = class(TMimeCoder)
  public
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Name: String; override;
  end;

{ TDefaultCoder }

  TDefaultCoder = class(TMimeCoder)
  public
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Name: String; override;
  end;

{ TXmlCoder }

  TXmlCoder = class(TMimeCoder)
  public
    class function EncodeLine(const Buffer: String): String; override;
    class function DecodeLine(const Buffer: String): String; override;
    class function Name: String; override;
  end;

implementation

uses SilLiStream;

{ TMimeCoder }

class function TMimeCoder.Decode(const Source: IStringList; const Dest: IRandomStream; Progress: TProgressCallback): Boolean;
var
  i, iCount, iCurr, iFreq: Integer;
  sLine: String;
begin
  iCurr := 0;
  iCount := Source.Count;
  iFreq := iCount div 100;

  if Assigned(Progress) then Progress(iCount, iCurr, pkDecode);

  for i := 0 to iCount - 1 do
  begin
    sLine := DecodeLine(Source[i]);

    if Length(sLine) > 0 then Dest.Write(sLine[1], Length(sLine));
    Inc(iCurr);

    if (iFreq > 0) and (iCurr mod iFreq = 0) then
      if Assigned(Progress) then Progress(iCount, iCurr, pkDecode);
  end;

  if Assigned(Progress) then Progress(1, 1, pkDecode);
  Result := true;
end;

class function TMimeCoder.Decode(const Source, Dest: IStream; Progress: TProgressCallback): Boolean;
var
  Text: ITextStream;
  iCount, iCurr, iFreq: Integer;
  sLine, LineIn: String;
begin
  iCurr := 0;
  iCount := Source.Size;
  iFreq := iCount div 100;

  if not Ref.GetInterface(Source, ITextStream, Text) then
    Text := Sil.Stream.Text(Source);

  if Assigned(Progress) then
    Progress(iCount, iCurr, pkDecode);

  while Text.ReadLn(LineIn) do
  begin
    sLine := DecodeLine(LineIn);

    if Length(sLine) > 0 then Dest.Write(sLine[1], Length(sLine));
    Inc(iCurr);

    if Assigned(Progress) and (iFreq > 0) and (iCurr mod iFreq = 0) then
      Progress(iCount, iCurr, pkDecode);
  end;

  if Assigned(Progress) then Progress(1, 1, pkDecode);
  Result := true;
end;

class function TMimeCoder.Encode(const Source: IRandomStream; const Dest: IStringList; Progress: TProgressCallback): String;
var
  iCount, iCurr, iFreq, iMaxLineSize, iRead: Integer;
  sLine, LineOut: String;
begin
  iCurr := 0;
  iCount := Source.Size;
  iFreq := iCount div 100;

  if Assigned(Progress) then Progress(iCount, iCurr, pkEncode);

  iMaxLineSize := MaxLineSize;
  SetLength(sLine, iMaxLineSize);

  while true do
  begin
    iRead := Source.Read(sLine[1], iMaxLineSize);
    if iRead <= 0 then
      Break else
    if iRead < iMaxLineSize then
      SetLength(sLine, iRead);

    LineOut := EncodeLine(sLine);
    Dest.Add(LineOut);

    if (iFreq > 0) and (iCurr mod iFreq = 0) then
      if Assigned(Progress) then Progress(iCount, Source.Position, pkEncode);
  end;

  if Assigned(Progress) then Progress(1, 1, pkEncode);
end;

class function TMimeCoder.Encode(const Source, Dest: IStream; Progress: TProgressCallback): String;
var
  iCount, iCurr, iFreq, iMaxLineSize, iRead: Integer;
  sLine, LineOut: String;
  Rand: IRandomStream;
begin
  iCurr := 0;
  iCount := Source.Size;
  iFreq := iCount div 100;

  if Assigned(Progress) then
  begin
    Ref.GetInterface(Source, IRandomStream, Rand);
    Progress(iCount, iCurr, pkEncode);
  end;

  iMaxLineSize := MaxLineSize;
  SetLength(sLine, iMaxLineSize);

  while true do
  begin
    iRead := Source.Read(sLine[1], iMaxLineSize);
    if iRead <= 0 then
      Break else
    if iRead < iMaxLineSize then
      SetLength(sLine, iRead);

    LineOut := EncodeLine(sLine);
    Dest.Write(LineOut[1], Length(LineOut));
    Dest.Write(ccCRLF, Length(ccCRLF));

    if Assigned(Rand) and (iFreq > 0) and (iCurr mod iFreq = 0) then
      Progress(iCount, Rand.Position, pkEncode);
  end;

  if Assigned(Progress) then Progress(1, 1, pkEncode);
end;

class function TMimeCoder.MaxLineSize: Integer;
begin
  Result := 58;//76;
end;

{ TBase64Coder }

class function TBase64Coder.MaxLineSize: Integer;
begin
  Result := 54;
end;

class function TBase64Coder.EncodeLine(const Buffer: String): String;
const
  cb64: pchar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  inb: array [0..2] of byte;
  outb: array [0..3] of char;
  pos, rest, len: Integer;

  procedure EncodeBlock;
  begin
    outb[0] := cb64[inb[0] shr 2];
    outb[1] := cb64[((inb[0] and $03) shl 4) or ((inb[1] and $f0) shr 4)];

    if len > 1 then
      outb[2] := cb64[((inb[1] and $0f) shl 2) or ((inb[2] and $c0) shr 6)] else
      outb[2] := '=';

    if len > 2 then
      outb[3] := cb64[inb[2] and $3f] else
      outb[3] := '=';
  end;

var
  Stream: IWriteOnlyStream;
begin
  Stream := Sil.Stream.WriteOnly;
  rest := Length(Buffer);
  pos := 1;

  while rest > 0 do
  begin
    len := Int.Min(3, rest);
    fillchar(inb, sizeof(inb), 0);
    Move(Buffer[pos], inb, len);

    if len > 0 then
    begin
      EncodeBlock;
      Stream.Write(outb, SizeOf(outb));
    end;

    dec(rest, len);
    inc(pos, len);
  end;

  Result := Stream.Buffer;
end;

class function TBase64Coder.DecodeLine(const Buffer: String): String;
const
  cd64: pchar = '|$$$}rstuvwxyz{$$$$$$$>?@ABCDEFGHIJKLMNOPQRSTUVW$$$$$$XYZ[\]^_`abcdefghijklmnopq';
var
  inb: array [0..3] of byte;
  outb: array [0..2] of byte;
  i, pos, len, buflen: Integer;
  v: byte;

  procedure DecodeBlock;
  begin
    outb[0] := inb[0] shl 2 or inb[1] shr 4;
    outb[1] := inb[1] shl 4 or inb[2] shr 2;
    outb[2] := ((inb[2] shl 6) and $c0) or inb[3];
  end;

var
  Stream: IWriteOnlyStream;
begin
  Stream := Sil.Stream.WriteOnly;
  buflen := length(buffer);
  pos := 0;

  while pos < buflen do
  begin
    len := 0;
    i := 0;
    fillchar(inb, sizeof(inb), 0);

    while (pos < buflen) and (i < 4) do
    begin
      v := 0;

      while (pos < buflen) and (v = 0) do
      begin
        inc(pos);
        v := ord(Buffer[pos]);

        if (v < 43) or (v > 122) then
          v := 0 else
          v := ord(cd64[v - 43]);

        if v > 0 then
          if v = ord('$') then
            v := 0 else
            dec(v, 61);
      end;

      if (pos < buflen) or (Buffer[pos] <> '=') then
      begin
        inc(len);
        if v > 0 then inb[i] := v - 1;
      end else
        inb[i] := 0;

      inc(i);
    end;

    if len > 0 then
    begin
      decodeblock;
      Stream.Write(outb, len - 1);
    end;
  end;

  Result := Stream.Buffer;
end;

class function TBase64Coder.Name: String;
begin
  Result := 'base64';
end;

{ TUUCoder }

class function TUUCoder.EncodeLine(const Buffer: String): String;
const
  Codes: String = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
var
  i, iIdx, iLen: Integer;
  DataIn: array[0..2] of Byte;
  DataOut: array[0..80] of Char;
begin
  i := 0;
  iIdx := 1;
  Result := '';
  FillChar(DataOut, SizeOf(DataOut), 0);

  while true do
  begin
    iLen := Int.Min(3, Length(Buffer) - iIdx + 1);
    if iLen < 1 then Break;

    Move(Buffer[iIdx], DataIn, iLen);
    Inc(iIdx, 3);

    DataOut[i + 0] := Codes[((DataIn[0] shr 2) and 63) + 1];

    if iLen > 1 then
    begin
      DataOut[i + 1] := Codes[(((DataIn[0] shl 4) or (DataIn[1] shr 4)) and 63) + 1];
      DataOut[i + 2] := Codes[(((DataIn[1] shl 2) or (DataIn[2] shr 6)) and 63) + 1];
      if iLen > 2 then DataOut[i + 3] := Codes[(DataIn[2] and 63) + 1];
    end else
      DataOut[i + 1] := Codes[(DataIn[0] shl 4) + 1];

    Inc(i, 4);
    if i > 59 then Break;
  end;

  i := Sil.Str.Len(DataOut);
  SetLength(Result, i + 1);
  Result[1] := Codes[Int.Min(Length(Buffer) + 1, 46)];
  Move(DataOut, Result[2], i);
end;

class function TUUCoder.DecodeLine(const Buffer: String): String;
const
  CHARS_PER_LINE = 45;
  Table = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
var
  A24Bits: array[0..8 * CHARS_PER_LINE] of Boolean;
  i, j, k, b: Word;
  LineLen, ActualLen: Byte;

  function p_ByteFromTable(Ch: Char): Byte;
  var
    i: Integer;
  begin
    i := Pos(Ch, Table);

    if i < 1 then
    begin
      if Ch = #32 then
        Result := 0 else
        raise Exception.Create('Error: TUUCoder.DecodeLine');
    end else
      Result := i - 1;
  end;

begin
  if (Sil.Text.Compare(Buffer, 'begin') = 0) or (Sil.Text.Compare(Buffer, 'end') = 0) then
  begin
    Result := '';
    Exit;
  end;

  LineLen := p_ByteFromTable(Buffer[1]);
  ActualLen := 4 * LineLen div 3;

  FillChar(A24Bits, 8 * CHARS_PER_LINE + 1, 0);
  SetLength(Result, LineLen);

  if ActualLen <> (4 * CHARS_PER_LINE div 3) then
    ActualLen := Length(Buffer) - 1;

  k := 0;
  for i := 2 to ActualLen + 1 do
  begin
    b := p_ByteFromTable(Buffer[i]);
    for j := 5 downto 0 do
    begin
      A24Bits[k] := b and (1 shl j) > 0;
      Inc(k);
    end;
  end;

  k := 0;
  for i := 1 to Int.Min(CHARS_PER_LINE, LineLen) do
  begin
    b := 0;
    for j := 7 downto 0 do
    begin
      if A24Bits[k] then b := b or (1 shl j);
      Inc(k);
    end;
    Result[i] := Char(b);
  end;
end;

class function TUUCoder.Name: String;
begin
  Result := 'uuencode';
end;

{ TISO8859_1Coder }

class function TISO8859_1Coder.Name: String;
begin
  Result := 'iso-8859-1';
end;

class function TISO8859_1Coder.Decode(const Source: IStringList; const Dest: IRandomStream; Progress: TProgressCallback): Boolean;
var
  i, iIdx, iCount, iCurr, iFreq: Integer;
  sLine: String;
begin
  iCurr := 0;
  iCount := Source.Count;
  iFreq := iCount div 100;
  if Assigned(Progress) then Progress(iCount, iCurr, pkDecode);

  for i := 0 to iCount - 1 do
  begin
    sLine := DecodeLine(Source[i]);
    iIdx := Length(sLine);

    if (iIdx > 0) and (sLine[iIdx] = #13) then
      SetLength(sLine, iIdx - 1) else
    if i < iCount - 1 then
    begin
      SetLength(sLine, iIdx + 2);
      sLine[iIdx + 1] := #13;
      sLine[iIdx + 2] := #10;
    end;

    if Length(sLine) > 0 then Dest.Write(sLine[1], Length(sLine));
    Inc(iCurr);
    if (iFreq > 0) and (iCurr mod iFreq = 0) then
      if Assigned(Progress) then Progress(iCount, iCurr, pkDecode);
  end;

  if Assigned(Progress) then Progress(1, 1, pkDecode); // ensure 100%
  Result := true;
end;

class function TISO8859_1Coder.DecodeLine(const Buffer: String): String;
const
  sChars: String = '0123456789ABCDEF';
var
  i, iIdx, iAnt, iPos: Integer;
begin
  iIdx := 1;
  iAnt := 1;

  SetLength(Result, Length(Buffer));
  FillChar(Result[1], Length(Buffer), 0);

  repeat
    iPos := Str.Pos('=', Buffer, iAnt);

    if iPos > 0 then
    begin
      if (iPos > iAnt) then
      begin
        Move(Buffer[iAnt], Result[iIdx], iPos - iAnt);
        Inc(iIdx, iPos - iAnt);
      end;

      if iPos < Length(Buffer) - 1 then
      begin
        if (Pos(Buffer[iPos + 1], sChars) > 0) and (Pos(Buffer[iPos + 2], sChars) > 0) then
        begin
          Result[iIdx] := Char(Sil.Str.ToInt('$' + Buffer[iPos + 1] + Buffer[iPos + 2], 32));
          Inc(iIdx);
        end;
      end else
      if iPos = Length(Buffer) then
      begin
        Result[iIdx] := #13;
        Break;
      end;

      iAnt := iPos + 3;
    end else
    begin
      iPos := Length(Buffer) - iAnt + 1;
      if iPos > 0 then Move(Buffer[iAnt], Result[iIdx], iPos);
      Break;
    end;
  until false;

  iIdx := Length(Result);

  for i := iIdx downto 1 do
    if (Result[i] > #31) or (Result[i] = #13) then
    begin
      if i < iIdx then iIdx := i;
      Break;
    end;

  if iIdx <> Length(Result) then SetLength(Result, iIdx);
end;

class function TISO8859_1Coder.EncodeLine(const Buffer: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(Buffer) do
    if not (Buffer[i] in [#32..#127]) or (Buffer[i] = '=') then
      Result := Result + '=' + Int.ToHex(Ord(Buffer[i]), 2) else
      Result := Result + Buffer[i];
end;

{ TQuotedPrintableCoder }

class function TQuotedPrintableCoder.Name: String;
begin
  Result := 'quoted-printable';
end;

{ TWindows1252Coder }

class function TWindows1252Coder.Name: String;
begin
  Result := 'windows-1252';
end;

{ TDefaultCoder }

class function TDefaultCoder.DecodeLine(const Buffer: String): String;
begin
  Result := Buffer;
end;

class function TDefaultCoder.EncodeLine(const Buffer: String): String;
begin
  Result := Buffer;
end;

class function TDefaultCoder.Name: String;
begin
  Result := 'default';
end;

{ THexaCoder }

class function THexaCoder.DecodeLine(const Buffer: String): String;

  function HexToNumber( AChar: Char ): Byte;
  begin
    if ( Ord( '0' ) <= Ord( AChar ) ) and ( Ord( AChar ) <= Ord( '9' ) ) then
      result := Ord( AChar ) - Ord( '0' )
    else if ( Ord( 'A' ) <= Ord( AChar ) ) and ( Ord( AChar ) <= Ord( 'F' ) ) then
      result := $0A + Ord( AChar ) - Ord( 'A' )
    else if ( Ord( 'a' ) <= Ord( AChar ) ) and ( Ord( AChar ) <= Ord( 'f' ) ) then
      result := $0A + Ord( AChar ) - Ord( 'a' )
    else
      raise Exception.Create('Error: THexaCoder.DecodeLine');
  end;

var
  i1, cnt: Integer;
begin
  if ( Length( Buffer ) mod 2 <> 0 ) then
    raise Exception.Create('Error: THexaCoder.DecodeLine');

  cnt := Length( Buffer ) div 2;
  SetLength( result, cnt );
  for i1 := 1 to cnt do
    result[ i1 ] := Char( HexToNumber( Buffer[ i1 * 2 - 1 ] ) * 16 +
      HexToNumber( Buffer[ i1 * 2 ] ) );
end;

class function THexaCoder.EncodeLine(const Buffer: String): String;
const
  sChars: String = '0123456789ABCDEF';
var
  i1, cnt: integer;
begin
  cnt := Length( Buffer );
  SetLength( result, cnt * 2 );
  for i1 := 1 to cnt do
  begin
    result[ i1 * 2 - 1 ] := sChars[ 1 + ( Ord( Buffer[ i1 ] ) and $F0 ) shr 4 ];
    result[ i1 * 2 ] := sChars[ ( 1 + Ord( Buffer[ i1 ] ) and $0F ) ];
  end;
end;

class function THexaCoder.Name: String;
begin
  Result := 'hexa';
end;

{ TXmlCoder }

const
  XmlPredefined: array [1..5] of String = ('&quot;', '&apos;', '&amp;', '&lt;', '&gt;');
  XmlPredChars = '"''&<>';

class function TXmlCoder.EncodeLine(const Buffer: String): String;
var
  i, Idx, Pos, LenRes, LenLit: Integer;
  c: Char;
  Lit: String;
begin
  LenRes := Length(Buffer);
  SetLength(Result, LenRes);
  Idx := 1;

  for i := 1 to Length(Buffer) do
  begin
    c := Buffer[i];
    Pos := Str.Pos(c, XmlPredChars);

    if (Pos > 0) or not (c in [#32..#127]) then
    begin
      if Pos > 0 then
        Lit := XmlPredefined[Pos] else
        Lit := '&#' + Int.ToStr(Ord(c)) + ';';

      LenLit := Length(Lit);
      Inc(LenRes, LenLit - 1);
      SetLength(Result, LenRes);
      Move(Lit[1], Result[Idx], LenLit);
      Inc(Idx, LenLit);
    end else
    begin
      Result[Idx] := c;
      Inc(Idx);
    end;
  end;
end;

class function TXmlCoder.DecodeLine(const Buffer: String): String;

  procedure DoError;
  begin
    raise Exception.Create('Error: TXmlCoder.DecodeLine');
  end;

var
  LenBuf, LenRes: Integer;
  i, Idx, Pos, Ant, Semi: Integer;
  c: Char;
  Item: String;
begin
  LenBuf := Length(Buffer);
  LenRes := LenBuf;
  SetLength(Result, LenBuf);

  Ant := 1;
  Idx := 1;

  repeat
    Pos := Str.Pos('&', Buffer, Ant);

    if Pos > 0 then
    begin
      if (Pos > Ant) then
      begin
        Move(Buffer[Ant], Result[Idx], Pos - Ant);
        Inc(Idx, Pos - Ant);
      end;

      Semi := Str.Pos(';', Buffer, Pos + 1);
      if Semi = 0 then DoError;

      c := #0;
      Item := Str.Copy(Buffer, Pos, Semi - Pos + 1);

      if (Length(Item) > 2) and (Item[2] = '#') then
      begin
        if Chr.Lower(Item[3]) = 'x' then
          Item[3] := '$';

        c := Char(Str.ToInt(Str.Copy(Item, 3, -2)));
      end else
      begin
        for i := Low(XmlPredefined) to High(XmlPredefined) do
          if Sil.Text.Compare(Item, XmlPredefined[i]) = 0 then
          begin
            c := XmlPredChars[i];
            Break;
          end;
      end;

      if c = #0 then DoError;
      Result[Idx] := c;
      Inc(Idx);
      Ant := Semi + 1;
      Dec(LenRes, Length(Item) - 1);
    end else
    begin
      Pos := LenBuf - Ant + 1;
      if Pos > 0 then Move(Buffer[Ant], Result[Idx], Pos);
      Break;
    end;
  until false;

  if LenRes <> Length(Result) then
    SetLength(Result, LenRes);
end;

class function TXmlCoder.Name: String;
begin
  Result := 'XmlCoder';
end;

end.

