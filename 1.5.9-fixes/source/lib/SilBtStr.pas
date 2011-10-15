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

unit SilBtStr;

{$I Defines.inc}

interface

// doc: SilBtStr.dtx

uses
  SilBeTypes,
  SilBkTool;

type
  Str = class(Tool)
    class function Token(const Buffer, Separator: String; var Position: Integer): String;
    class function TokenToArray(const Buffer, Separator: String): TStringArray;
    class function TokenLast(const Buffer, Separator: String; EvenIfFirst: Boolean = true): String;
    class function Enumerate(const Buffer, Separator: String; var Item: string; var Position: Integer): Boolean;
    class function WordWrap(const Buffer: String; const Size: Integer; var Position: Integer): String;
    class function Translate(const Buffer, Substr, Replace: String; IgnoreCase: Boolean = false): String; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // reemplazar por Sil.Str.Replace
    class procedure TranslateChar(var Buffer: String; const Character, Replace: Char); {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // reemplazar por Sil.Str.Replace
    class function Replace(var Buffer: string; const Text: string; Init: Integer = 1; Term: Integer = -1): string; overload;
    class function Replace(const Buffer, Substr, Replace: String; IgnoreCase: Boolean = false): String; overload;
    class function Replace(var Buffer: String; const Character, Replace: Char): String; overload;
    class function Replace(var Buffer: String; const FirstCharacter, LastCharacter, Replace: Char; InRange: Boolean = False): String; overload;
    class function Replace(var Buffer: String; const SubStrList: Array of String; const Replace: String; IgnoreCase: Boolean = false): String; overload;
    class function PadL(const Buffer: String; const Len: Integer; const Character: Char = ' '): String;
    class function PadR(const Buffer: String; const Len: Integer; const Character: Char = ' '): String;
    class function WildCard(const Source, Pattern: String; IgnoreCase: Boolean = false): Boolean;
    class function IsWildCard(Pattern: PChar): Boolean; overload;
    class function IsWildCard(const Pattern: string): Boolean; overload;
    class function IsEmpty(const Buffer: string; Trim: Boolean = false): Boolean;
    class function IfEmpty(const Value: string; const Default: String = ''): String;
    class function NotEmpty(const Buffer: string; Trim: Boolean = false): Boolean;
    class function IsAlpha(const Buffer: string): Boolean;
    class function IsNumber(const Buffer: string): Boolean;
    class function Assigned(const Buffer: string): Boolean; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // reemplazar por Sil.Str.IsAssigned
    class function IsAssigned(const Buffer: string): Boolean;
    class function IsEqual(const S1, S2: string): Boolean;
    class function IsEqualText(const S1, S2: string): Boolean; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // reemplazar por Sil.Text.
    class function Null: string;
    class function ToLower(const Buffer: String): String;
    class function ToUpper(const Buffer: String): String;
    class function Quoted(const Buffer: String; const Quotes: Char = ''''): string; overload;
    class function Quoted(const Buffer: String; const Starter, Terminator: string): string; overload;
    class function IsQuoted(const Buffer: String; const Starter, Terminator: string): Boolean; overload;
    class function Capitalize(const Buffer: String): String;
    class procedure Crypt(pcDest: PChar; const Size: Word; const Key: String);
    class function Replicate(const Substr: String; const Count: Integer): String;
    class function ReplicateChar(const Character: Char; const Len: Integer): String;
    class function Copy(const Buffer: String; Index: Integer; const Count: Integer = -1 ): String;
    class function Left(const Buffer: String; const Count: Integer): String;
    class function Right(const Buffer: String; const Count: Integer): String;
    class function Extract(var Buffer: String; Index: Integer; const Count: Integer = -1 ): String;
    class function Between(const Buffer: string; const Starter, Terminator: string; out Value: string; Init: Integer = 1; Term: PInteger = nil; Start: PInteger = nil): Boolean;
    class function Split(const Buffer: string; const Separator: string; var LeftValue, RightValue: string; ReturnLeft: Boolean = true): Boolean; overload;
    class function Split(const Buffer: string; Position: Integer; var LeftValue, RightValue: string; Length: Integer = 1; ReturnLeft: Boolean = true): Boolean; overload;
    class function Zero(const Number: Integer; const Len: Byte): String;
    class function Pos(const Substr, Buffer: String; const Init: Integer = 1; IgnoreCase: boolean = false): Integer;
    class function GetPos(const Substr, Buffer: String; const Init: Integer; var FoundAt: Integer): Boolean;
    class function LastPos(const Substr, Buffer: String; Init: Integer = 0 ): Integer;
    class function Trim(const Buffer: String; Character: Char; MaxCount: Integer = 0): String; overload;
    class function Trim(const Buffer: String; const Chars: TCharSet = ccSpaceChars; MaxCount: Integer = 0): String; overload;
    class function TrimLeft(const Buffer: String; Character: Char; MaxCount: Integer = 0): String; overload;
    class function TrimLeft(const Buffer: String; const Chars: TCharSet = ccSpaceChars; MaxCount: Integer = 0): String; overload;
    class function TrimRight(const Buffer: String; Character: Char; MaxCount: Integer = 0): String; overload;
    class function TrimRight(const Buffer: String; const Chars: TCharSet = ccSpaceChars; MaxCount: Integer = 0): String; overload;
    class function TrimCtrl(const Buffer: String ): String; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // reemplazar por Sil.Str.Trim que por default trimea todos los ctrls + el spc
    class function Count(const Buffer, Substr: String): Integer;
    class function Len(const Buffer: String): Cardinal; overload;
    class function Len(Buffer: PChar): Cardinal; overload;
    class procedure TranslateAccent(var Buffer: String);
    class function Compare(const Buffer, Compare: String): Integer; overload;
    class function Compare(const Buffer, Compare: String; Count: Integer): Integer; overload;
    class function CompareText(const Buffer, Compare: String; Complete: Boolean = false): Integer; overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // Sil.Text.
    class function CompareText(const Buffer, Compare: String; Count: Integer): Integer; overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // Sil.Text.
    class function TextCompare(const Buffer, Compare: String): Integer; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // Sil.Text.
    class function TextPos(const Substr, Buffer: String; const Init: Integer = 1): Integer; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF} // Sil.Text.
    class function CompareCase(const Buffer, Compare: String; Sensitive: Boolean = false): Integer; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Move(const Source: String; Dest: PChar; const MaxLen: Integer; IncludeTerminator: Boolean = True): Integer;
    class procedure ReverseBuf(var Buffer: String);
    class function Reverse(const Buffer: String): String;
    class function DelimiterPos(const Delimiters, Buffer: String; BegPos, EndPos: Integer): Integer; overload;
    class function DelimiterPos(const Delimiters, Buffer: String; BegPos: Integer = 1): Integer; overload;
    class function LastDelimiter(const Delimiters, Buffer: String): Integer;
    class function NewLength(var Buffer: String; const Add: Integer): Integer;
    class function Add(var Buffer: String; const Substr: String; const Separator: String = ''; AllowDuplicateSeparator: Boolean = True): String; overload;
    class function Add(var Buffer: String; const Substr: String; const Args: array of const; const Separator: String = ''; AllowDuplicateSeparator: Boolean = True): String; overload;
    class function Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr: string; BufferRemainingPtr: PInteger = nil): PChar; overload;
    class function Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr: string; const Separator: String = ''; BufferRemainingPtr: PInteger = nil): PChar; overload;
    class function Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr: string; const Args: array of const; const Separator: String = ''; BufferRemainingPtr: PInteger = nil): PChar; overload;
    class function Delete(var Buffer: String; Index: Integer; Count: Integer = -1): String;
    class function Remove(var Buffer: String; const Substr: String): String;
    class function Insert(var Buffer: String; const Substr: String; Index: Integer = 1): String;
    class procedure DelControlChars(var Buffer: String);
    class function ReplaceControlChars(const Buffer: PChar; Size: LongWord): String;
    class function IIf(const Expr: Boolean; const RTrue: String; RFalse: String = ''): String;
    class function ToCurrency(const Buffer: string): Currency; overload;
    class function ToHex(const Buffer: String): String;
    class function Scan(const Buffer, Format: string; const Args: array of const): Boolean;
    class function Format(const FormatText: string; const Args: array of const): string;
    class function IsByte(const Buffer: string): Boolean;
    class function IsShortint(const Buffer: string): Boolean;
    class function IsWord(const Buffer: string): Boolean;
    class function IsSmallint(const Buffer: string): Boolean;
    class function IsInteger(const Buffer: string): Boolean;
    class function IsLarge(const Buffer: string): Boolean;
    class function IsExtended(const Buffer: string): Boolean;
    class function IsDouble(const Buffer: string): Boolean;
    class function IsSingle(const Buffer: string): Boolean;
    class function IsFloat(const Buffer: string): Boolean;
    class function IsNumeric(const Buffer: string): Boolean;
    class function ToChr(const S: String; const Def: Char = #0): Char;
    class function ToByte(const Buffer: string): Byte; overload;
    class function ToByte(const Buffer: string; Def: Byte): Byte; overload;
    class function ToShort(const Buffer: string): Shortint; overload;
    class function ToShort(const Buffer: string; Def: Shortint): Shortint; overload;
    class function ToSmall(const Buffer: string): Smallint; overload;
    class function ToSmall(const Buffer: string; Def: Smallint): Smallint; overload;
    class function ToWord(const Buffer: string): Word; overload;
    class function ToWord(const Buffer: string; Def: Word): Word; overload;
    class function ToInt(const Buffer: string): Integer; overload;
    class function ToInt(const Buffer: string; Def: Integer): Integer; overload;
    class function ToInt(const Buffer: string; Trim: Boolean; Def: Integer = 0): Integer; overload;
    class function ToLongWord(const Buffer: string): LongWord; overload;
    class function ToLongWord(const Buffer: string; Def: LongWord): LongWord; overload;
    class function ToLarge(const Buffer: string): LargeInt; overload;
    class function ToLarge(const Buffer: string; const Def: LargeInt): LargeInt; overload;
    class function ToFloat(const Buffer: string): Double; overload;
    class function ToFloat(const Buffer: string; const DecimalSeparators: string): Double; overload;
    class function ToFloat(const Buffer: string; Def: Double): Double; overload;
    class function ToCurrency(const Buffer: string; Def: Currency): Currency; overload;
    class function RemoveDelimiters(const Buffer: String; L, R: Char): String;
    class procedure Fill(var Buffer: String; Size: LongWord = 0; Chr: Char = #0);
    class function FirstChar(const Buffer: String): Char;
    class function Char(const Buffer: String; Index: Integer): Char;
    class function LastChar(const Buffer: String): Char;
    class function ArrayAdd(var A: TStringArray; const Value: String): Integer;
    class function ArrayFind(const A: TStringArray; const Value: String): Integer;
    class function ArrayDelete(var A: TStringArray; Index: Integer): Boolean;
    class function ArrayLast(const A: TStringArray): String;
    class procedure ArraySet(var A: TStringArray; const Values: array of String);
    class function Make(Buffer: PChar; Size: LongWord): String;
    class procedure Append(var Buffer: String; const Value; Size: LongWord); overload;
    class procedure Append(var Buffer: String; Value: LargeInt); overload;
    class procedure Append(var Buffer: String; Value: Integer); overload;
    class procedure Append(var Buffer: String; Value: Byte); overload;
    class procedure Append(var Buffer: String; Value: Word); overload;
    class procedure Append(var Buffer: String; const Value: string); overload;
    class function New(const Value: String; Size: Integer = 0): PChar;
    class function Coalesce(const A: Array of string; const Default: String = ''): String;
    class function StartsWith(const substr, buffer: string): boolean;
    class function EndsWith(const substr, buffer: string): boolean;
  end;

implementation

uses
  SysUtils,
{$IFDEF D40}
  SysConst,
{$ENDIF}
//  SilLtList,
//  SilLtTool,
  SilBcChr,
  SilBtInt,
  SilBtChr,
  SilBtText,
  SilBtFloat;

class function Str.Add(var Buffer: String; const Substr: String; const Separator: String; AllowDuplicateSeparator: Boolean): string;
begin
  if (Length(Separator) <> 0) and (Length(Buffer) <> 0) then
  begin
    if AllowDuplicateSeparator or not Text.IsEqual(Separator, Right(Buffer, Length(Separator))) then
      Buffer := Buffer + Separator;
  end;
  Buffer := Buffer + Substr;
  Result := Buffer;
end;

class function Str.Add(var Buffer: String; const Substr: String; const Args: array of const; const Separator: String; AllowDuplicateSeparator: Boolean): String;
begin
  Result := Add(Buffer, SysUtils.Format(Substr, Args), Separator, AllowDuplicateSeparator);
end;

class function Str.Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr: string; BufferRemainingPtr: PInteger): PChar;
var
  Count: Integer;
begin
  Count := Move(Substr, BufferPtr, BufferRemaining, False);
  if System.Assigned(BufferRemainingPtr) then BufferRemainingPtr^ := BufferRemaining - Count;
  Result := BufferPtr + Count;
  Result^ := #0;
end;

class function Str.Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr, Separator: String; BufferRemainingPtr: PInteger): PChar;
begin
  if (Length(Separator) > 0) and (Buffer <> BufferPtr) then
    BufferPtr := Add(Buffer, BufferPtr, BufferSize, BufferRemaining, Separator, @BufferRemaining);
  Result := Add(Buffer, BufferPtr, BufferSize, BufferRemaining, Substr, @BufferRemaining);  
  if System.Assigned(BufferRemainingPtr) then BufferRemainingPtr^ := BufferRemaining;
end;

class function Str.Add(Buffer, BufferPtr: PChar; BufferSize, BufferRemaining: Integer; const Substr: string; const Args: array of const; const Separator: String; BufferRemainingPtr: PInteger): PChar;
begin
  Result := Add(Buffer, BufferPtr, BufferSize, BufferRemaining, Format(Substr, Args), Separator, BufferRemainingPtr);
end;

class function Str.Capitalize(const Buffer: String): String;
var
  i: Integer;
  First: Boolean;
begin
  First := true;
  SetLength(Result, Length(Buffer));

  for i := 1 to Length(Buffer) do
  begin
    if First then
      Result[i] := Chr.Upper(Buffer[i]) else
      Result[i] := Chr.Lower(Buffer[i]);

    First := Buffer[i] in [#32, '-', '.'];
  end;
end;

class function Str.Compare(const Buffer, Compare: String; Count: Integer): Integer;
begin
  Result := Sysutils.StrLComp(PChar(Buffer), PChar(Compare), Count);
end;

class function Str.Compare(const Buffer, Compare: String): Integer;
begin
  Result := Sysutils.CompareStr(Buffer, Compare);
end;

class function Str.Copy(const Buffer: String; Index: Integer; const Count: Integer): String;
var
  iLen: Integer;
begin
  if Count <> 0 then
  begin
    if Index < 0 then Index := Length(Buffer) + Index + 1;
    iLen := Length(Buffer) - Index + 1;

    if Count < 0 then Inc(iLen, Count + 1) else
    if (Count < iLen) and (Count > 0) then iLen := Count;
  end else
    iLen := 0;

  if (Index > 0) and (iLen > 0) and (Index + iLen - 1 <= Length(Buffer)) then
  begin
    SetLength(Result, iLen);
    System.Move(Buffer[Index], Result[1], iLen);
  end else
    Result := '';
end;

class function Str.Extract(var Buffer: String; Index: Integer; const Count: Integer): String;
begin
  Result := Str.Copy(Buffer, Index, Count);
  Delete(Buffer, Index, Count); 
end;

class function Str.Between(const Buffer, Starter, Terminator: string; out Value: string; Init: Integer; Term, Start: PInteger): Boolean;
var
  StartPos, TermPos: Integer;
begin
  Result := (Length(Starter) > 0) and GetPos(Starter, Buffer, Init, StartPos);

  if Result then
  begin
    Inc(StartPos, Length(Starter));

    if Length(Terminator) > 0 then
      TermPos := Pos(Terminator, Buffer, StartPos) else
      TermPos := 0;

    if TermPos <= 0 then
      TermPos := Length(Buffer) + 1;

    Value := Copy(Buffer, StartPos, TermPos - StartPos);

    if System.Assigned(Term) then Term^ := TermPos + 1;
    if System.Assigned(Start) then Start^ := StartPos;
  end;
end;

class function Str.Split(const Buffer, Separator: string; var LeftValue, RightValue: string; ReturnLeft: Boolean): Boolean;
var
  SeparatorPos: Integer;
begin
  Result := (Length(Separator) > 0)
        and GetPos(Separator, Buffer, 1, SeparatorPos);

  if Result then
    Split(Buffer, SeparatorPos, LeftValue, RightValue, Length(Separator), ReturnLeft)
  else if ReturnLeft then
  begin
    LeftValue := Buffer;
    RightValue := '';
    Result := IsAssigned(Buffer);
  end else
  begin
    LeftValue := '';
    RightValue := Buffer;
    Result := IsAssigned(Buffer);
  end;
end;

class function Str.Split(const Buffer: string; Position: Integer; var LeftValue, RightValue: string; Length: Integer; ReturnLeft: Boolean): Boolean;
begin
  Result := (Position >= 1) and (Position <= System.Length(Buffer));
  if Result then
    begin
      LeftValue := Copy(Buffer, 1, Position - 1);
      Inc(Position, Length);
      RightValue := Copy(Buffer, Position);
    end
  else if ReturnLeft then
    begin
      LeftValue := Buffer;
      RightValue := '';
      Result := IsAssigned(Buffer);
    end
  else
    begin
      LeftValue := '';
      RightValue := Buffer;
      Result := IsAssigned(Buffer);
    end;
end;

class function Str.Count(const Buffer, Substr: String): Integer;
var
  i: Integer;
begin
  i := 1;
  Result := 0;
  repeat
    i := Pos(Substr, Buffer, i);
    if i > 0 then
    begin
      Inc(Result);
      Inc(i, Length(Substr));
    end;
  until i = 0;
end;

class procedure Str.Crypt(pcDest: PChar; const Size: Word; const Key: String);
var
  i, j: Integer;
begin
  j := 1;

  for i := 0 to Size - 1 do
  begin
    pcDest[i] := System.Chr(Ord(pcDest[i]) xor Ord(Key[j]) xor ((Size - i) mod 255 + 1));
    if j = Length(Key) then j := 1 else Inc(j);
  end;
end;

class procedure Str.DelControlChars(var Buffer: String);
var
  i: Integer;
begin
  i := 1;
  while i <= Length(Buffer) do
    if not (Buffer[i] in [#32..#128]) then
      Str.Delete(Buffer, i, 1) else
      Inc(i);
end;

class function Str.Delete(var Buffer: String; Index, Count: Integer): String;
begin
  if Count = -1 then Count := Length(Buffer) - Index + 1;
  if Index < 0 then Index := Length(Buffer) + Index + 1;
  System.Delete(Buffer, Index, Count);
  Result := Buffer;
end;

class function Str.DelimiterPos(const Delimiters, Buffer: String; BegPos: Integer): Integer;
begin
  Result := DelimiterPos(Delimiters, Buffer, BegPos, Length(Buffer));
end;

class function Str.DelimiterPos(const Delimiters, Buffer: String; BegPos, EndPos: Integer): Integer;
var
  i, iInc: Integer;
begin
  if BegPos < 0 then BegPos := Length(Buffer) + BegPos + 1;
  if EndPos < 0 then EndPos := Length(Buffer) + EndPos + 1;
  if BegPos < EndPos then iInc := 1 else iInc := -1;

  while BegPos <> EndPos do
  begin
    for i := 1 to Length(Delimiters) do
      if Buffer[BegPos] = Delimiters[i] then
      begin
        Result := BegPos;
        Exit;
      end;
    Inc(BegPos, iInc);
  end;
  Result := -1;
end;

class function Str.GetPos(const Substr, Buffer: String; const Init: Integer; var FoundAt: Integer): Boolean;
begin
  FoundAt := Str.Pos(Substr, Buffer, Init);
  Result := FoundAt > 0;
end;

class function Str.IfEmpty(const Value, Default: String): String;
begin
  if Length(Value) > 0 then
    Result := Value else
    Result := Default;
end;

class function Str.IIf(const Expr: Boolean; const RTrue: String; RFalse: String): String;
begin
  if Expr then Result := RTrue else Result := RFalse;
end;

class function Str.Insert(var Buffer: String; const Substr: String; Index: Integer): String;
begin
  System.Insert(Substr, Buffer, Index);
  Result := Substr;
end;

class function Str.IsWildCard(Pattern: PChar): Boolean;
begin
  Result := SysUtils.StrScan(pattern, '*') <> nil;
  if not Result then Result := SysUtils.StrScan(pattern, '?') <> nil;
end;

class function Str.IsWildCard(const Pattern: string): Boolean;
begin
  Result := IsWildcard(PChar(Pattern));
end;

class function Str.LastDelimiter(const Delimiters, Buffer: String): Integer;
var
  P: PChar;
begin
  Result := Length(Buffer);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (Buffer[Result] <> #0) and (StrScan(P, Buffer[Result]) <> nil) then
      if (ByteType(Buffer, Result) = mbTrailByte) then
        Dec(Result) else
        Exit;
    Dec(Result);
  end;
end;

class function Str.LastChar(const Buffer: String): Char;
begin
  if Length(Buffer) > 0 then
    Result := Buffer[Length(Buffer)] else
    Result := #0;
end;

class function Str.FirstChar(const Buffer: String): Char;
begin
  if Length(Buffer) > 0 then
    Result := Buffer[1] else
    Result := #0;
end;

class function Str.Char(const Buffer: String; Index: Integer): Char;
begin
  if Length(Buffer) >= Index then
    Result := Buffer[Index] else
    Result := #0;
end;

class function Str.LastPos(const Substr, Buffer: String; Init: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Init = 0 then Init := Length(Buffer) - Length(Substr) + 1;
  if (Init < 1) or (Init + Length(Substr) - 1 > Length(Buffer)) then Exit;

  for Result := Init downto 1 do
  begin
    i := Length(Substr);
    while i > 0 do
      if Buffer[Result + i - 1] <> Substr[i] then
        Break else
        Dec(i);
    if i = 0 then Exit;
  end;
  Result := 0;
end;

class function Str.Len(const Buffer: String): Cardinal;
begin
  Result := Length(Buffer);
end;

class function Str.Len(Buffer: PChar): Cardinal;
begin
  Result := SysUtils.StrLen(Buffer);
end;

class function Str.Move(const Source: String; Dest: PChar; const MaxLen: Integer; IncludeTerminator: Boolean): Integer;
begin
  Result := System.Length(Source);
  if Result > 0 then
  begin
    if IncludeTerminator then Inc(Result);
    if Result > MaxLen then Result := MaxLen;
    System.Move(Source[1], Dest^, Result * SizeOf(Source[1]));
  end;
end;

class function Str.NewLength(var Buffer: String; const Add: Integer): Integer;
begin
  Result := Length(Buffer) + Add;
  SetLength(Buffer, Result);
end;

class function Str.PadL(const Buffer: String; const Len: Integer; const Character: Char): String;
begin
  if Len < 1 then
  begin
    Result := '';
    Exit;
  end;

  if Length(Buffer) < Len then
  begin
    SetLength(Result, Len);
    System.Move(Buffer[1], Result[Len - Length(Buffer) + 1], Length(Buffer));
    FillChar(Result[1], Len - Length(Buffer), Character);
  end else
    Result := Str.Copy(Buffer, Succ(Length(Buffer)) - Len, Len);
end;

class function Str.PadR(const Buffer: String; const Len: Integer; const Character: Char): String;
begin
  if Len < 1 then
  begin
    Result := '';
    Exit;
  end;

  if Length(Buffer) < Len then
  begin
    SetLength(Result, Len);
    System.Move(Buffer[1], Result[1], Length(Buffer));
    FillChar(Result[Length(Buffer) + 1], Len - Length(Buffer), Character);
  end else
    Result := Str.Copy(Buffer, 1, Len);
end;

class function Str.Pos(const Substr, Buffer: String; const Init: Integer; IgnoreCase: boolean): Integer;
begin
  if (Length(Buffer) < Init) or (Init < 1) then
    Result := 0 else
  begin
    if IgnoreCase then
      Result := System.Pos(Str.ToLower(SubStr), PChar(Str.ToLower(Buffer)) + Init - 1) else
      Result := System.Pos(Substr, PChar(Buffer) + Init - 1);
    if Result > 0 then Inc(Result, Init - 1);
  end;
end;

class function Str.Remove(var Buffer: String; const Substr: String): String;
var
  i: Integer;
begin
  i := Str.Pos(Substr, Buffer, 1);
  if i > 0 then Str.Delete(Buffer, i, Length(Substr));
end;

class function Str.RemoveDelimiters(const Buffer: String; L, R: Char): String;
begin
  if Length(Buffer) = 0 then Exit;
  Result := Buffer;
  if Result[1] = L then Result := Str.Copy(Result, 2, -1 );
  if Result[Length(Result)] = R then Result := Str.Copy(Result, 1, Length(Result) - 1);
end;

class function Str.Replicate(const Substr: String; const Count: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Count do Result := Result + Substr;
end;

class function Str.ReplicateChar(const Character: Char; const Len: Integer): String;
begin
  if Len < 1 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Len);
  FillChar(Result[1], Len, Character);
end;

class function Str.Reverse(const Buffer: String): String;
begin
  Result := Buffer;
  ReverseBuf(Result);
end;

class procedure Str.ReverseBuf(var Buffer: String);
var
  i, n: Integer;
  c: System.Char;
begin
  n := Length(Buffer);
  for i := 1 to Length(Buffer) div 2 do
  begin
    c := Buffer[n];
    Buffer[n] := Buffer[i];
    Buffer[i] := c;
    Dec(n);
  end;
end;

class function Str.ToCurrency(const Buffer: string): Currency;
begin
  Result := SysUtils.StrToCurr(Buffer);
end;

{$HINTS OFF}

class function Str.IsByte(const Buffer: string): Boolean;
var
  E: Integer;
  F: Byte;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsShortint(const Buffer: string): Boolean;
var
  E: Integer;
  F: Shortint;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsSmallint(const Buffer: string): Boolean;
var
  E: Integer;
  F: Smallint;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsWord(const Buffer: string): Boolean;
var
  E: Integer;
  F: Word;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsInteger(const Buffer: string): Boolean;
var
  E: Integer;
  F: Integer;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsLarge(const Buffer: string): Boolean;
var
  E: Integer;
  F: LargeInt;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsExtended(const Buffer: string): Boolean;
var
  E: Integer;
  F: Extended;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsDouble(const Buffer: string): Boolean;
var
  E: Integer;
  F: Double;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

class function Str.IsSingle(const Buffer: string): Boolean;
var
  E: Integer;
  F: Single;
begin
  System.Val(Buffer, F, E);
  Result := E = 0;
end;

{$HINTS ON}

class function Str.IsFloat(const Buffer: string): Boolean;
begin
  Result := IsExtended(Buffer);
end;

class function Str.IsNumeric(const Buffer: string): Boolean;
begin
  Result := IsInteger(Buffer)
        or IsFloat(Buffer);
end;

class function Str.ToChr(const S: String; const Def: Char): Char;
begin
  if Length(S) > 0 then
    Result := S[1] else
    Result := Def;
end;

class function Str.ToByte(const Buffer: string): Byte;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToByte(const Buffer: string; Def: Byte): Byte;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToShort(const Buffer: string): Shortint;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToShort(const Buffer: string; Def: Shortint): Shortint;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToSmall(const Buffer: string): Smallint;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToSmall(const Buffer: string; Def: Smallint): Smallint;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToWord(const Buffer: string): Word;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToWord(const Buffer: string; Def: Word): Word;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToInt(const Buffer: string): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToInt(const Buffer: string; Def: Integer): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToInt(const Buffer: string; Trim: Boolean; Def: Integer): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if (E <> 0) and not Trim then Result := Def;
end;

class function Str.ToLongWord(const Buffer: string): LongWord;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToLongWord(const Buffer: string; Def: LongWord): LongWord;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToLarge(const Buffer: string): LargeInt;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function Str.ToLarge(const Buffer: string; const Def: LargeInt): LargeInt;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function Str.ToFloat(const Buffer: string): Double;
begin
  Result := SysUtils.StrToFloat(Buffer);
end;

class function Str.ToFloat(const Buffer, DecimalSeparators: string): Double;
var
  C: System.Char;
  I, N: Integer;
  Seps: string;
begin
  if NotEmpty(DecimalSeparators) then
    begin
      Result := 0;
      Seps := DecimalSeparators;
      if Str.Pos(SysUtils.DecimalSeparator, Seps) = 0 then
        Seps := SysUtils.DecimalSeparator + Seps;
      N := Length(Seps);
      for I := 1 to N do
        try
          C := SysUtils.DecimalSeparator;
          try
            SysUtils.DecimalSeparator := Seps[I];
            Result := ToFloat(Buffer)
          finally
            SysUtils.DecimalSeparator := C;
          end;
          Break;
        except
          if I = N then
            raise;
        end;
    end
  else
    Result := ToFloat(Buffer);
end;

class function Str.ToFloat(const Buffer: string; Def: Double): Double;
begin
  result := Float.FromStr( Buffer, Def );
end;

class function Str.ToCurrency(const Buffer: string; Def: Currency): Currency;
begin
  if not SysUtils.TextToFloat(PChar(Buffer), Result, fvCurrency) then
    Result := Def;
end;

class function Str.Token(const Buffer, Separator: String; var Position: Integer): String;
var
  Sub: String;
  intOld: Integer;

  function CheckOut: Boolean;
  begin
    Result := Position > Length(Buffer);
    if Result then Position := 0;
  end;

begin
  if CheckOut then
  begin
    Result := '';
    Exit;
  end;

  if Position < 1 then Position := 1;

  intOld := Position;
  Sub := PChar(Buffer) + (Position - 1);
  Position := System.Pos(Separator, Sub);

  if Position <> 0 then
  begin
    SetLength(Result, Position - 1);
    if Position > 1 then
      System.Move(Sub[1], Result[1], Position - 1);
    Inc(Position, intOld + Length(Separator) - 1);
  end else
    Result := Sub;
end;

class function Str.TokenToArray(const Buffer, Separator: String): TStringArray;
var
  sItem: String;
  i: Integer;
begin
  i := 0;
  SetLength(Result, 0);
  repeat
    sItem := Str.Token(Buffer, Separator, i);
    Str.ArrayAdd(Result, sItem);
  until i = 0;
end;

class function Str.TokenLast(const Buffer, Separator: String; EvenIfFirst: Boolean): String;
var
  i: Integer;
begin
  i := Str.LastPos(Separator, Buffer);

  if i > 0 then
    Result := Str.Copy(Buffer, i + 1)
  else
  if EvenIfFirst then
    Result := Buffer
  else
    Result := '';
end;

class function Str.Enumerate(const Buffer, Separator: String; var Item: string; var Position: Integer): Boolean;
var
  Pos, Sub: PChar;
  Prev, I: Integer;
begin
  Result := (Position <= Length(Buffer));

  if not Result then
  begin
    Position := 0;
    Item := '';
    Exit;
  end;

  Prev := Position;
  Sub := PChar(Buffer) + Position;

  Pos := StrPos(Sub, PChar(Separator));

  if System.Assigned(Pos) then
    I := Pos - Sub + 1 else
    I := 0;

  if I <> 0 then
    begin
      SetLength(Item, I - 1);
      if I > 1 then
        System.Move(Sub^, Item[1], I - 1);
      Inc(I, Prev + Length(Separator) - 1);
    end
  else
    begin
      Item := Sub;
      I := Length(Buffer) + 1;
    end;
  Position := I;
end;

class function Str.Replace(var Buffer: String; const SubStrList: array of String; const Replace: String; IgnoreCase: Boolean): String;
var
  i: Integer;
begin
  for i := 0 to Length(SubStrList) - 1 do
    Buffer := Str.Replace(Buffer, SubStrList[i], Replace, IgnoreCase);

  Result := Buffer;
end;

class function Str.Replace(var Buffer: string; const Text: string; Init, Term: Integer): string;
begin
  Result := Copy(Buffer, 1, Pred(Init) - 1) + Text + Copy(Buffer, Term);
  Buffer := Result;
end;

class function Str.Replace(var Buffer: String; const Character, Replace: Char): String;
var
  i: Integer;
begin
  for i := 1 to Length(Buffer) do
    if Buffer[i] = Character then Buffer[i] := Replace;

  Result := Buffer;
end;

class function Str.Replace(const Buffer, Substr, Replace: String; IgnoreCase: Boolean): String;
var
  Len, Pos, Ant: Integer;
begin
  Result := '';
  Ant := 1;

  repeat
    Pos := Str.Pos(Substr, Buffer, Ant, IgnoreCase);
    if Pos > 0 then
    begin
      if Pos > Ant then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Pos - Ant);
        System.Move(Buffer[Ant], Result[Len + 1], Pos - Ant);
      end;
      Result := Result + Replace;
      Ant := Pos + Length(Substr);
    end else
    begin
      if Length(Buffer) - Ant + 1 > 0 then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Length(Buffer) - Ant + 1);
        System.Move(Buffer[Ant], Result[Len + 1], Length(Buffer) - Ant + 1);
      end;
      Break;
    end;
  until false;
end;

class function Str.Replace(var Buffer: String; const FirstCharacter, LastCharacter, Replace: Char; InRange: Boolean): String;
var
  i: Integer;
begin
  for i := 1 to Length(Buffer) do
    if (((Buffer[i] < FirstCharacter) or (Buffer[i] > LastCharacter)) and not InRange) or
       (((Buffer[i] >= FirstCharacter) or (Buffer[i] <= LastCharacter)) and InRange) then
      Buffer[i] := Replace;

  Result := Buffer;
end;

class procedure Str.TranslateAccent(var Buffer: String);
var
  i: Integer;
begin
  for i := 1 to Length(Buffer) do
    case Buffer[i] of
      #192..#197: Buffer[i] := 'A';
      #200..#203: Buffer[i] := 'E';
      #204..#207: Buffer[i] := 'I';
      #210..#214: Buffer[i] := 'O';
      #217..#220: Buffer[i] := 'U';

      #224..#229: Buffer[i] := 'a';
      #232..#235: Buffer[i] := 'e';
      #236..#239: Buffer[i] := 'i';
      #242..#246: Buffer[i] := 'o';
      #249..#252: Buffer[i] := 'u';
    end;
end;

function DoLeftTrim(const Buffer: String; Len, Max, Ofs: Integer; Character: Char): Integer; overload;
begin
  Result := 1;
  while (Result <= Max) and (Buffer[Result + Ofs] = Character) do Inc(Result);
end;

function DoLeftTrim(const Buffer: String; Len, Max, Ofs: Integer; const Chars: TCharSet): Integer; overload; 
begin
  Result := 1;
  while (Result <= Max) and (Buffer[Result + Ofs] in Chars) do Inc(Result);
end;

function DoRightTrim(const Buffer: String; Len, Max, Ofs: Integer; Character: Char): Integer; overload; 
begin
  Result := 0;
  while (Result < Max) and (Buffer[Len - Result - Ofs] = Character) do Inc(Result);
  Result := Len - Result;
end;

function DoRightTrim(const Buffer: String; Len, Max, Ofs: Integer; const Chars: TCharSet): Integer; overload; 
begin
  Result := 0;
  while (Result < Max) and (Buffer[Len - Result - Ofs] in Chars) do Inc(Result);
  Result := Len - Result;
end;

class function Str.Trim(const Buffer: String; const Chars: TCharSet; MaxCount: Integer): String;
var
  Head, Tail, Len, Max, Ofs: Integer;
begin
  Len := Length(Buffer);
  Ofs := Int.IIf(MaxCount < 0, - MaxCount, 0);
  Max := Int.IIf(MaxCount > 0, MaxCount, Len);

  Head := DoLeftTrim(Buffer, Len, Max, Ofs, Chars);
  if Head <= Len then
  begin
    Tail := DoRightTrim(Buffer, Len, Max, Ofs, Chars);
    Result := Str.Copy(Buffer, Head, Tail - Head + 1);
  end else
    Result := '';
end;

class function Str.Trim(const Buffer: String; Character: Char; MaxCount: Integer): String;
var
  Head, Tail, Len, Max, Ofs: Integer;
begin
  Len := Length(Buffer);
  Ofs := Int.IIf(MaxCount < 0, - MaxCount, 0);
  Max := Int.IIf(MaxCount > 0, MaxCount, Len);

  Head := DoLeftTrim(Buffer, Len, Max, Ofs, Character);
  if Head <= Len then
  begin
    Tail := DoRightTrim(Buffer, Len, Max, Ofs, Character);
    Result := Str.Copy(Buffer, Head, Tail - Head + 1);
  end else
    Result := '';
end;

class function Str.TrimLeft(const Buffer: String; const Chars: TCharSet; MaxCount: Integer): String;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := Str.Copy(Buffer, DoLeftTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Chars), -1);
end;

class function Str.TrimLeft(const Buffer: String; Character: Char; MaxCount: Integer): String;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := Str.Copy(Buffer, DoLeftTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Character), -1);
end;

class function Str.TrimRight(const Buffer: String; Character: Char; MaxCount: Integer): String;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := Str.Copy(Buffer, 1, DoRightTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Character));
end;

class function Str.TrimRight(const Buffer: String; const Chars: TCharSet; MaxCount: Integer): String;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := Str.Copy(Buffer, 1, DoRightTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Chars));
end;

class function Str.WildCard(const Source, Pattern: String; IgnoreCase: Boolean): Boolean;

  function MatchPattern(Element, Pattern: PChar): Boolean;
  begin
    if SysUtils.StrComp(Pattern, '*') = 0 then
      Result := true else
    if (Element^ = #0) and (Pattern^ <> #0) then
      Result := false else
    if Element^ = #0 then
      Result := true else
    begin
      case Pattern^ of
        '*':
          if MatchPattern(Element, @Pattern[1]) then
             Result := true else
             Result := MatchPattern(@Element[1], Pattern);
        '?':
          Result := MatchPattern(@Element[1], @Pattern[1]);
      else
        if Element^ = Pattern^ then
          Result := MatchPattern(@Element[1], @Pattern[1]) else
          Result := False;
      end;
    end;
  end;

begin
  if IgnoreCase then
    Result := MatchPattern(PChar(Str.ToUpper(Source)), PChar(Str.ToUpper(Pattern))) else
    Result := MatchPattern(PChar(Source), PChar(Pattern));
end;

class function Str.WordWrap(const Buffer: String; const Size: Integer; var Position: Integer): String;

  procedure DoMove(Size: Integer);
  begin
    if Length(Buffer) - Position + 1 < Size then Size := Length(Buffer) - Position + 1;
    SetLength(Result, Size);
    System.Move(Buffer[Position], Result[1], Size);
  end;

var
  i: Integer;
begin
  if Position > Length(Buffer) then
  begin
    Result := '';
    Position := -1;
    Exit;
  end;

  if Position + Size >= Length(Buffer) then
  begin
    Result := Str.Copy(Buffer, Position, -1);
    Position := -1;
    Exit;
  end;

  for i := Position + Size - 1 downto Position + 1 do
    if Buffer[i] in ['.', ',', #32, #39] then
    begin
      DoMove(i - Position + 1);
      Position := i + 1;
      Exit;
    end;

  DoMove(Size);
  Inc(Position, Size);
end;

class function Str.Zero(const Number: Integer; const Len: Byte): String;
var
  Buffer: String;
  iLen: Integer;
begin
  if Len < 1 then
  begin
    Result := '';
    Exit;
  end;

  Result := Str.ReplicateChar('0', Len);
  System.Str(Number, Buffer);
  iLen := Int.Min(Length(Buffer), Len);
  System.Move(Buffer[1], Result[Len - iLen + 1], iLen);
end;

class function Str.ToHex(const Buffer: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(Buffer) do
    Result := Result + Int.ToHex(Byte(Buffer[i]), 2);
end;

class function Str.Format(const FormatText: string; const Args: array of const): string;
begin
  try
    Result := SysUtils.Format(FormatText, Args);
  except
    Result := FormatText;
  end;
end;

//
// Buffer: string formateado
// Format : formato del string
// Args contiene las variables donde poner el resultado
// se invoca  sscanf('10;hola;30','%d%s%d,[@a,@b,@c]);
// Formatos contemplados
  {
    's' : string
    'd' : LongInt
    'f' : Double
    'm' : Currency
    'w' : Word
    'b' : Byte
  }
class function Str.Scan(const Buffer: string; const Format: string; const Args: array of const) : Boolean;
var
  sSize, Separator, Valor: string;
  Tipo: System.Char;
  BufPtr, FmtPtr, EndPtr: PChar;
  ArgPtr: PVarRec;
  iSize, ArgCount: Integer;
begin
  ArgCount := Length(Args);
  ArgPtr := @Args[0];
  BufPtr := PChar(Buffer);
  FmtPtr := PChar(Format);
  try
    while (BufPtr^ <> #0) and (ArgCount > 0) do
    begin
      while not (FmtPtr^ in ['%', #0]) do
        Inc(FmtPtr);

      if FmtPtr^ = #0 then
      begin
        Result := True;
        Exit;
      end;

      Inc(FmtPtr);
      sSize := '';

      while FmtPtr^ in ['0'..'9'] do
      begin
        sSize := sSize + FmtPtr^;
        Inc(FmtPtr);
      end;

      iSize := Str.ToInt(sSize, -1);
      Tipo := Chr.Upper(FmtPtr^);

      Inc(FmtPtr);
      Separator := '';

      while not (FmtPtr^ in ['%', #0]) do
      begin
        Separator := Separator + FmtPtr^;
        Inc(FmtPtr);
      end;

      if iSize = -1 then
      begin
        Valor := '';
        EndPtr := StrPos(BufPtr, PChar(Separator));

        if EndPtr = nil then
          EndPtr := StrEnd(BufPtr);

        SetString(Valor, BufPtr, EndPtr - BufPtr);
        BufPtr := EndPtr + Length(Separator);
      end else
      begin
        SetString(Valor, BufPtr, iSize);
        Inc(BufPtr, iSize + Length(Separator));
      end;

      if (ArgPtr.VPointer <> nil) and (ArgPtr.VType = vtPointer) then
      begin
        case Chr.Upper(Tipo) Of
          'S': string(ArgPtr.VPointer^) := Valor;
          'D': Integer(ArgPtr.VPointer^) := Str.ToInt(Valor, 0);
          'F': Double(ArgPtr.VPointer^) := Str.ToFloat(Valor, 0);
          'M': Currency(ArgPtr.VPointer^) := Str.ToCurrency(Valor, 0);
          'W': Word(ArgPtr.VPointer^) := Str.ToInt(Valor, 0) and $ffff;
          'B': Byte(ArgPtr.VPointer^) := Str.ToInt(Valor, 0) and $ff;
        end;
      end;

      Inc(ArgPtr);
      Dec(ArgCount);
    end;
     
    Result := True;
  except
    Result := False;
  end;
end;
class procedure Str.Fill(var Buffer: String; Size: LongWord; Chr: Char);
begin
  if (Size > 0) and (Length(Buffer) <> Integer(Size)) then SetLength(Buffer, Size);
  FillChar(Buffer[1], Length(Buffer), Chr);
end;

class function Str.IsEmpty(const Buffer: string; Trim: Boolean): Boolean;
begin
  if Trim then
    Result := Length(Str.Trim(Buffer)) = 0 else
    Result := Length(Buffer) = 0;
end;

class function Str.NotEmpty(const Buffer: string; Trim: Boolean): Boolean;
begin
  if Trim then
    Result := Length(Str.Trim(Buffer)) > 0 else
    Result := Length(Buffer) > 0;
end;

class function Str.IsAssigned(const Buffer: string): Boolean;
begin
  Result := Length(Buffer) > 0;
end;

class function Str.IsEqual(const S1, S2: string): Boolean;
begin
  Result := S1 = S2;
end;

class function Str.Null: string;
begin
  Result := EmptyStr;
end;

class function Str.ToLower(const Buffer: String): String;
begin
  Result := LowerCase(Buffer);
end;

class function Str.ToUpper(const Buffer: String): String;
begin
  Result := UpperCase(Buffer);
end;

class function Str.Quoted(const Buffer: String; const Quotes: Char): string;
var
  I: Integer;
begin
  Result := Buffer;
  for I := Length(Result) downto 1 do
    if Result[I] = Quotes then System.Insert(Quotes, Result, I);
  Result := Quotes + Result + Quotes;
end;

class function Str.Quoted(const Buffer, Starter, Terminator: string): string;
begin
  Result := Starter + Buffer + Terminator;
end;

class function Str.IsQuoted(const Buffer, Starter, Terminator: string): Boolean;
begin
  Result := (Left(Buffer, Length(Starter)) = Starter) and (Right(Buffer, Length(Terminator)) = Terminator);
end;

class function Str.ArrayAdd(var A: TStringArray; const Value: String): Integer;
begin
  Result := Length(A) + 1;
  SetLength(A, Result);
  A[Result - 1] := Value;
end;

class function Str.ArrayDelete(var A: TStringArray; Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := (Index >= 0) and (Index <= High(A));

  if Result then
  begin
    for i := Index to High(A) - 1 do A[i] := A[i + 1];
    SetLength(A, Length(A) - 1);
  end;
end;

class function Str.ArrayFind(const A: TStringArray; const Value: String): Integer;
begin
  for Result := 0 to High(A) do if Str.IsEqual(A[Result], Value) then Exit;
  Result := -1;
end;

class function Str.Left(const Buffer: String; const Count: Integer): String;
begin
  Result := Str.Copy(Buffer, 1, Count);
end;

class function Str.Right(const Buffer: String; const Count: Integer): String;
begin
  Result := Str.Copy(Buffer, Length(Buffer) - Count + 1);
end;

class function Str.Make(Buffer: PChar; Size: LongWord): String;
begin
  SetString(Result, Buffer, Size);
end;

class procedure Str.Append(var Buffer: String; const Value; Size: LongWord);
var
  lwSize: LongWord;
begin
  lwSize := Length(Buffer);
  SetLength(Buffer, lwSize + Size);
  System.Move(Value, Buffer[lwSize + 1], Size);
end;

class function Str.ArrayLast(const A: TStringArray): String;
var
  i: Integer;
begin
  i := Length(A);

  if i > 0 then
    Result := A[i - 1]
  else
    Result := '';
end;

class procedure Str.ArraySet(var A: TStringArray; const Values: array of String);
var
  i: Integer;
begin
  i := Length(Values);
  SetLength(A, i);

  while i > 0 do
  begin
    A[i - 1] := Values[i - 1];
    Dec(i);
  end;
end;

class function Str.ReplaceControlChars(const Buffer: PChar; Size: LongWord): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Size - 1 do
    if Ord(Buffer[i]) >= High(AsciiChars) then
      Result := Result + Buffer[i] else
      Result := Result + '<' + AsciiChars[Ord(Buffer[i])] + '>';
end;

class procedure Str.Append(var Buffer: String; Value: LargeInt);
begin
  Append(Buffer, Value, SizeOf(Value));
end;

class procedure Str.Append(var Buffer: String; Value: Integer);
begin
  Append(Buffer, Value, SizeOf(Value));
end;

class procedure Str.Append(var Buffer: String; Value: Byte);
begin
  Append(Buffer, Value, SizeOf(Value));
end;

class procedure Str.Append(var Buffer: String; Value: Word);
begin
  Append(Buffer, Value, SizeOf(Value));
end;

class procedure Str.Append(var Buffer: String; const Value: string);
begin
  Append(Buffer, Value[1], Length(Value));
end;

class function Str.New(const Value: String; Size: Integer): PChar;
var
  Len: Integer;
begin
  Len := System.Length(Value);
  if Size = 0 then
    Size := Len
  else if Size < Len then
    Len := Size;
  Result := AllocMem(Size + 1);
  System.Move(Value[1], Result^, Len);
  Result[Len+1] := #0;
end;

class function Str.IsAlpha(const Buffer: string): Boolean;
var
  i, Len: Integer;
begin
  Len := Length(Buffer);
  i := DoLeftTrim(Buffer, Len, Len, 0, #32);

  if i <= Length(Buffer) then
    Result := Buffer[i] in ['A'..'Z', 'a'..'z'] else
    Result := false;
end;

class function Str.IsNumber(const Buffer: string): Boolean;
var
  i, Len: Integer;
begin
  Len := Length(Buffer);
  i := DoLeftTrim(Buffer, Len, Len, 0, #32);

  if i <= Length(Buffer) then
  begin
    Result := Buffer[i] in ['0'..'9'];
    if not Result and (Buffer[i] in ['+', '-']) then
      Result := (i + 1 <= Length(Buffer)) and (Buffer[i + 1] in ['0'..'9']);
  end else
    Result := false;
end;

{$IFDEF USE_DEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}

class function Str.Assigned(const Buffer: string): Boolean;
begin
  Result := IsAssigned(Buffer);
end;

class function Str.CompareCase(const Buffer, Compare: String; Sensitive: Boolean): Integer;
begin
  if Sensitive then
    Result := SysUtils.CompareStr(Buffer, Compare) else
    Result := Sysutils.CompareText(Buffer, Compare);
end;

class function Str.Translate(const Buffer, Substr, Replace: String; IgnoreCase: Boolean): String;
begin
  Result := Str.Replace(Buffer, Substr, Replace, IgnoreCase);
end;

class function Str.TrimCtrl(const Buffer: String): String;
begin
  Result := Sysutils.Trim( Buffer );
end;

class procedure Str.TranslateChar(var Buffer: String; const Character, Replace: Char);
begin
  Str.Replace(Buffer, Character, Replace);
end;

class function Str.CompareText(const Buffer, Compare: String; Complete: Boolean): Integer;
var
  cBuf, cCom: Cardinal;
begin
  if Complete then
  begin
    Result := SysUtils.CompareText(Buffer, Compare);
    Exit;
  end;

  cBuf := Length(Buffer);
  cCom := Length(Compare);

  if (cBuf = 0) or (cCom = 0) then
    Result := Ord(cBuf > 0) + Ord(cCom > 0) * -1 else
  if cBuf < cCom then
    Result := StrLIComp(PChar(Buffer), PChar(Compare), cBuf) else
    Result := StrLIComp(PChar(Buffer), PChar(Compare), cCom);
end;

class function Str.TextCompare(const Buffer, Compare: String): Integer;
begin
  Result := Sysutils.CompareText(Buffer, Compare);
end;

class function Str.TextPos(const Substr, Buffer: String; const Init: Integer): Integer;
begin
  Result := Str.Pos(Str.ToLower(SubStr), Str.ToLower(Buffer), Init);
end;

class function Str.CompareText(const Buffer, Compare: String; Count: Integer): Integer;
begin
  Result := StrLIComp(PChar(Buffer), PChar(Compare), Count);
end;

class function Str.IsEqualText(const S1, S2: string): Boolean;
begin
  Result := CompareText(S1, S2, true) = 0;
end;

class function Str.Coalesce(const A: Array of string; const Default: String): String;
var
  i: Integer;
begin
  result := Default;

  for i := Low(A) to High(A) do
    if not IsEmpty(A[i]) then
    begin
      result := A[i];
      break;
    end;
end;

{$IFDEF USE_DEPRECATED} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}

class function Str.StartsWith(const substr, buffer: string): boolean;
begin
  Result := Compare(substr, buffer, length(substr)) = 0;
end;

class function Str.EndsWith(const substr, buffer: string): boolean;
var
  P: PChar;
  N: Integer;
begin
  Result := False;

  if Length(substr) < Length(buffer) then
  begin
    N := Length(buffer)-Length(substr)+1;
    P := @buffer[N];
    Result := Compare(P, substr, N) = 0;
  end;
end;

end.
