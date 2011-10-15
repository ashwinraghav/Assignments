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

unit SilOjWStr;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  SilWStr = class(Tool)
    class function Enumerate(const Buffer, Separator: WideString; var Item: WideString; var Position: Integer): Boolean;
    class function Len(const Buffer: WideString): Integer; overload; virtual; abstract;
    class function Len(const Buffer: PWideChar; MaxLen: integer = -1): Integer; overload; virtual; abstract;
    class function Assigned(const Buffer: WideString): Boolean; overload; virtual;
    class function Assigned(const Buffer: PWideChar): Boolean; overload; virtual;
    class function IsAssigned(const Buffer: WideString): Boolean; overload;
    class function IsAssigned(const Buffer: PWideChar): Boolean; overload;
    class function IsEqual(const S1, S2: WideString; IgnoreCase: Boolean = True): Boolean;
    class function NotEmpty(const Buffer: WideString): Boolean; overload; virtual;
    class function NotEmpty(const Buffer: PWideChar): Boolean; overload; virtual;
    class function IsEmpty(const Buffer: WideString): Boolean; overload; virtual;
    class function IsEmpty(const Buffer: PWideChar): Boolean; overload; virtual;
    class function IfEmpty(const Value: WideString; const Default: WideString = ''): WideString;
    class function Null: WideString;
    class function IsNull(const Value: WideString; const Default: WideString = '-'): WideString;
    class function ToLower(const Buffer: WideString): WideString;
    class function ToUpper(const Buffer: WideString): WideString;
    class function Quoted(const Buffer: WideString; const LQuote: WideString = ''''; const RQuote: WideString = ''): WideString;
    class function Copy(const Buffer: WideString; const Index: integer; const Count: Integer {$IFDEF USE_DEFPARAMS} = -1 {$ENDIF}): WideString; overload;
    class function Copy(const Buffer: PWideChar; const BuffSize: integer; const Offset: integer = 0; const Count: Integer {$IFDEF USE_DEFPARAMS} = -1 {$ENDIF}): WideString; overload;
    class function Left(const Buffer: WideString; const Count: Integer): WideString;
    class function Right(const Buffer: WideString; const Count: Integer): WideString;
    class function Between(const Buffer: WideString; const Starter, Terminator: WideString; out Value: WideString; Init: Integer = 1; Term: PInteger = nil; Start: PInteger = nil): Boolean;
    class function Split(const Buffer: WideString; const Separator: WideString; var LeftValue, RightValue: WideString; ReturnLeft: Boolean = true): Boolean; overload;
    class function Split(const Buffer: WideString; Position: Integer; var LeftValue, RightValue: WideString; Length: Integer = 1; ReturnLeft: Boolean = true): Boolean; overload;
    class function Pos(const Substr, Buffer: WideString; Init: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}; IgnoreCase: boolean {$IFDEF USE_DEFPARAMS} = false {$ENDIF}): Integer; overload;
    class function GetPos(const Substr, Buffer: WideString; const Init: Integer; var FoundAt: Integer; IgnoreCase: boolean = True): Boolean;
    class function Compare(const Buffer, Compare: WideString; IgnoreCase: Boolean = True): Integer; overload; virtual; abstract;
    class function CompareText(const Buffer, Compare: WideString; Complete: Boolean {$IFDEF USE_DEFPARAMS} = false {$ENDIF}): Integer; overload; virtual; abstract;
    class function Compare(const Buffer, Compare: PWideChar; IgnoreCase: Boolean = True; Dummy: Integer = 0): Integer; overload; virtual; abstract;
    class function CompareText(const Buffer, Compare: PWideChar; Complete: Boolean {$IFDEF USE_DEFPARAMS} = false {$ENDIF}; Dummy: Integer = 0): Integer; overload; virtual; abstract;
    class function Move(const Source: WideString; Dest: PWideChar; const MaxLen: Integer; IncludeTerminator: Boolean = True): Integer;
    class function Add(var Buffer: WideString; const Substr: WideString; const Separator: WideString = ''; AllowDuplicateSeparator: Boolean = True): WideString; overload; 
    class function Add(var Buffer: WideString; const Substr: WideString; const Args: array of const; const Separator: WideString = ''; AllowDuplicateSeparator: Boolean = True): WideString; overload;
    class function Add(Buffer: PWideChar; BufferSize: Integer; const Substr: WideString; BufferRemaining: PInteger = nil): PWideChar; overload; 
    class function Delete(var Buffer: WideString; Index: Integer; Count: Integer = -1): WideString; overload; 
    class function Remove(var Buffer: WideString; const Substr: WideString): WideString;
    class function IIf(const Expr: Boolean; const RTrue: WideString; RFalse: WideString {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): WideString;
    class function Format(const FormatText: WideString; const Args: array of const): WideString;
    class function ToInt(const Buffer: WideString): Integer; overload;
    class function ToInt(const Buffer: WideString; Def: Integer): Integer; overload;
    class function ToInt(const Buffer: WideString; Trim: Boolean; Def: Integer = 0): Integer; overload;
    class function Fill(var Buffer: WideString; Size: Integer = 0; Chr: WideChar = #0): PWideChar; overload;
    class function Fill(const Buffer: PWideChar; BuffLen: integer; Chr: WideChar): PWideChar; overload;
    class function PadR(const Source: WideString; const Len: integer; const PadChar: WideChar = ' '): WideString; overload;
    class function PadR(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar = ' '): WideString; overload;
    class function PadL(const Buffer: WideString; const Len: Integer; const PadChar: WideChar = ' '): WideString;
    class function RepeatText(const Source: WideChar; const Count: integer): WideString; overload;
    class function RepeatText(const Source: WideString; const Count: integer): WideString; overload;
    class function RepeatText(const Buffer: PWideString; const BuffLen: integer; const Count: integer): WideString; overload;
    class function Replace(const Buffer, Substr, Replace: WideString; IgnoreCase: Boolean = false): WideString;
    class function Trim(const Buffer: WideString; const Character: WideChar = ' '; MaxCount: Integer = 0): WideString;
    class function TrimLeft(const Buffer: WideString; Character: WideChar = ' '; MaxCount: Integer = 0): WideString;
    class function TrimRight(const Buffer: WideString; Character: WideChar = ' '; MaxCount: Integer = 0): WideString;
  private
    class function DoCopy(const Buffer: PWideChar; BuffLen, Offset, Count: Integer): WideString;
    class function DoPadR(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar): WideString;
    class function DoPadL(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar): WideString;
    class function DoLeftTrim(const Buffer: WideString; Len, Max, Ofs: Integer; Character: WideChar): Integer;
    class function DoRightTrim(const Buffer: WideString; Len, Max, Ofs: Integer; Character: WideChar): Integer;
  end;

implementation

uses
{$IFDEF D40}
  SysConst,
{$ENDIF}
  SysUtils,
  SilBtInt;

{ SilWStr }

class function SilWStr.Enumerate(const Buffer, Separator: WideString; var Item: WideString; var Position: Integer): Boolean;
var
  Prev, Next, Count: Integer;
begin
  Result := (Position <= System.Length(Buffer));

  if not Result then
  begin
    Position := 0;
    Item := '';
    Exit;
  end
  else if Position = 0 then
    Position := 1;

  Prev := Position;
  Next := Pos(Separator, Buffer, Position, True);

  if Next <> 0 then
    Count := Next - Prev else
    Count := 0;

  if Count <> 0 then
  begin
    if Count > 0 then Item := Copy(Buffer, Prev, Count);
    Position := Next + System.Length(Separator);
  end else
  begin
    Item := Copy(Buffer, Prev);
    Position := System.Length(Buffer) + 1;
  end;
end;

class function SilWStr.Assigned(const Buffer: WideString): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.Assigned(const Buffer: PWideChar): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.IsAssigned(const Buffer: WideString): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.IsAssigned(const Buffer: PWideChar): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.IsEqual(const S1, S2: WideString; IgnoreCase: Boolean): Boolean;
begin
  Result := Compare(S1, S2, IgnoreCase) = 0;
end;

class function SilWStr.NotEmpty(const Buffer: WideString): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.NotEmpty(const Buffer: PWideChar): Boolean;
begin
  Result := Len(Buffer) > 0;
end;

class function SilWStr.IsEmpty(const Buffer: WideString): Boolean;
begin
  Result := Len(Buffer) = 0;
end;

class function SilWStr.IfEmpty(const Value: WideString; const Default: WideString): WideString;
begin
  if Len(Value) > 0 then
    Result := Value else
    Result := Default;
end;

class function SilWStr.IsEmpty(const Buffer: PWideChar): Boolean;
begin
  Result := Len(Buffer) = 0;
end;

class function SilWStr.Null: WideString;
begin
  Result := '';
end;

class function SilWStr.IsNull(const Value, Default: WideString): WideString;
begin
  if IsAssigned(Value) then
    Result := Value else
    Result := Default;
end;

class function SilWStr.ToLower(const Buffer: WideString): WideString;
begin
  Result := WideLowerCase(Buffer);
end;

class function SilWStr.ToUpper(const Buffer: WideString): WideString;
begin
  Result := WideUpperCase(Buffer);
end;

class function SilWStr.Quoted(const Buffer, LQuote, RQuote: WideString): WideString;
begin
  Result := LQuote + Buffer + IsNull(RQuote, LQuote);
end;

class function SilWStr.Copy(const Buffer: WideString; const Index, Count: Integer): WideString;
var
  Offset: integer;
begin
  if (Index > 0) then
    Offset := Index - 1
  else
    Offset := Len(Buffer) + Index;

  result := DoCopy(@Buffer[1], Len(Buffer), Offset, Count);
end;

class function SilWStr.Copy(const Buffer: PWideChar; const BuffSize, Offset, Count: Integer): WideString;
begin
  result := DoCopy(Buffer, Len(Buffer, BuffSize), Offset, Count);
end;

class function SilWStr.DoCopy(const Buffer: PWideChar; BuffLen, Offset, Count: Integer): WideString;
var
  copylen: integer;
begin
  if Count <> 0 then
  begin
    if Offset < 0 then Offset := BuffLen - Offset;
    copylen := BuffLen - Offset;

    if Count < 0 then
      Inc(copylen, Count + 1)
    else if (Count < copylen) then
      copylen := Count;
  end
  else
    copylen := 0;

  if (Offset >= 0) and (copylen > 0) then
  begin
    SetLength(Result, copylen);
    System.Move(Buffer[Offset], Result[1], copylen * sizeof(WideChar));
  end else
    Result := '';
end;

class function SilWStr.Left(const Buffer: WideString; const Count: Integer): WideString;
begin
  Result := Copy(Buffer, 1, Count);
end;

class function SilWStr.Right(const Buffer: WideString; const Count: Integer): WideString;
begin
  Result := Copy(Buffer, System.Length(Buffer) - Count + 1);
end;

class function SilWStr.Between(const Buffer, Starter, Terminator: WideString; out Value: WideString; Init: Integer; Term, Start: PInteger): Boolean;
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

class function SilWStr.Split(const Buffer, Separator: WideString; var LeftValue, RightValue: WideString; ReturnLeft: Boolean): Boolean;
var
  SeparatorPos: Integer;
begin
  Result := (Length(Separator) > 0) and GetPos(Separator, Buffer, 1, SeparatorPos);

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

class function SilWStr.Split(const Buffer: WideString; Position: Integer; var LeftValue, RightValue: WideString; Length: Integer; ReturnLeft: Boolean): Boolean;
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

class function SilWStr.Pos(const Substr, Buffer: WideString; Init: Integer; IgnoreCase: boolean): Integer;
begin
  if (Length(Buffer) < Init) or (Init < 1) then
    Result := 0 else
  begin
    if IgnoreCase then
      Result := System.Pos(ToLower(SubStr), PWideChar(ToLower(Buffer)) + Init - 1) else
      Result := System.Pos(Substr, PWideChar(Buffer) + Init - 1);
    if Result > 0 then Inc(Result, Init - 1);
  end;
end;

class function SilWStr.GetPos(const Substr, Buffer: WideString; const Init: Integer; var FoundAt: Integer; IgnoreCase: boolean): Boolean;
begin
  FoundAt := Pos(Substr, Buffer, Init, IgnoreCase);
  Result := FoundAt > 0;
end;

class function SilWStr.Move(const Source: WideString; Dest: PWideChar; const MaxLen: Integer; IncludeTerminator: Boolean): Integer;
begin
  Result := System.Length(Source);
  if IncludeTerminator then Inc(Result);
  if Result > MaxLen then Result := MaxLen;
  System.Move(Source[1], Dest^, Result * SizeOf(Source[1]));
end;

class function SilWStr.Add(var Buffer: WideString; const Substr: WideString; const Separator: WideString; AllowDuplicateSeparator: Boolean): WideString;
begin
  if (System.Length(Separator) <> 0) and (System.Length(Buffer) <> 0) then
  begin
    if AllowDuplicateSeparator or not IsEqual(Separator, Right(Buffer, System.Length(Separator))) then
      Buffer := Buffer + Separator;
  end;
  Buffer := Buffer + Substr;
  Result := Buffer;
end;

class function SilWStr.Add(var Buffer: WideString; const Substr: WideString; const Args: array of const; const Separator: WideString; AllowDuplicateSeparator: Boolean): WideString;
begin
  Result := Add(Buffer, SysUtils.WideFormat(Substr, Args), Separator, AllowDuplicateSeparator);
end;

class function SilWStr.Add(Buffer: PWideChar; BufferSize: Integer; const Substr: WideString; BufferRemaining: PInteger): PWideChar; 
var
  Count: Integer;
begin
  Count := Move(Substr, Buffer, BufferSize, False);
  if System.Assigned(BufferRemaining) then BufferRemaining^ := BufferSize - Count;
  Result := Buffer + Count;
end;

class function SilWStr.Delete(var Buffer: WideString; Index, Count: Integer): WideString;
begin
  if Count = -1 then Count := Length(Buffer) - Index + 1;
  if Index < 0 then Index := Length(Buffer) + Index + 1;
  System.Delete(Buffer, Index, Count);
  Result := Buffer;
end;

class function SilWStr.Remove(var Buffer: WideString; const Substr: WideString): WideString;
var
  I: Integer;
begin
  I := Pos(Substr, Buffer, 1);
  if I > 0 then Delete(Buffer, I, Length(Substr));
end;

class function SilWStr.IIf(const Expr: Boolean; const RTrue: WideString; RFalse: WideString): WideString;
begin
  if Expr then Result := RTrue else Result := RFalse;
end;

class function SilWStr.ToInt(const Buffer: WideString): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Buffer]);
end;

class function SilWStr.ToInt(const Buffer: WideString; Def: Integer): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if E <> 0 then Result := Def;
end;

class function SilWStr.ToInt(const Buffer: WideString; Trim: Boolean; Def: Integer): Integer;
var
  E: Integer;
begin
  System.Val(Buffer, Result, E);
  if (E <> 0) and not Trim then Result := Def;
end;

class function SilWStr.Format(const FormatText: WideString; const Args: array of const): WideString;
begin
  Result := SysUtils.WideFormat(FormatText, Args);
end;

class function SilWStr.Fill(var Buffer: WideString; Size: Integer; Chr: WideChar): PWideChar;
var
  Len: Integer;
begin
  Len := System.Length(Buffer);
  if (Size > 0) and (Len <> Size) then
    SetLength(Buffer, Size) else
    Size := Len;

  Result := Fill(@Buffer[1], Size, Chr);
end;

class function SilWStr.Fill(const Buffer: PWideChar; BuffLen: integer; Chr: WideChar): PWideChar;
var
  i1: integer;
begin
  for i1 := 0 to BuffLen - 1 do
    Buffer[i1] := Chr;
    
  result := Buffer;
end;

class function SilWStr.PadL(const Buffer: WideString; const Len: Integer; const PadChar: WideChar): WideString;
begin
  result := DoPadL(@Buffer[1], self.Len(Buffer), Len, PadChar);
end;

class function SilWStr.DoPadL(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar): WideString;
var
  i1: integer;
begin
  if Len < 1 then
  begin
    Result := '';
  end
  else if BuffSize < Len then
  begin
    SetLength(Result, Len);
    System.Move(Buffer^, Result[Len - Length(Buffer) + 1], BuffSize * sizeof(WideChar));
    for i1 := 1 to Len - Length(Buffer) do
      Result[i1] := PadChar;
  end
  else
    Result := Copy(Buffer, length(buffer), Length(Buffer) - Len, Len);
end;

class function SilWStr.PadR(const Source: WideString; const Len: integer; const PadChar: WideChar): WideString;
begin
  result := DoPadR(@Source[1], self.Len(Source), Len, PadChar);
end;

class function SilWStr.PadR(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar): WideString;
begin
  result := DoPadR(Buffer, self.Len(Buffer, BuffSize), Len, PadChar);
end;

class function SilWStr.DoPadR(const Buffer: PWideChar; const BuffSize, Len: integer; const PadChar: WideChar): WideString;
var
  i1: integer;
begin
  if Len < 1 then
  begin
    Result := '';
  end
  else if BuffSize < Len then
  begin
    SetLength(Result, Len);
    System.Move(Buffer^, Result[1], BuffSize * sizeof(WideChar));
    for i1 := BuffSize + 1 to Len do
      Result[i1] := PadChar;
  end
  else
    Result := Copy(Buffer, len, 0, len);
end;

class function SilWStr.RepeatText(const Source: WideChar; const Count: integer): WideString;
begin
  result := RepeatText(@Source, 1, Count);
end;

class function SilWStr.RepeatText(const Source: WideString; const Count: integer): WideString;
begin
  result := RepeatText(@Source[1], self.Len(Source), Count);
end;

class function SilWStr.RepeatText(const Buffer: PWideString; const BuffLen, Count: integer): WideString;
var
  i1: integer;
begin
  SetLength(Result, BuffLen * Count);
  for i1 := 0 to Count - 1 do
    System.Move(Buffer^, Result[1 + i1 * BuffLen], BuffLen);
end;

class function SilWStr.Replace(const Buffer, Substr, Replace: WideString; IgnoreCase: Boolean): WideString;
var
  Len, Pos, Ant: Integer;
begin
  Result := '';
  Ant := 1;

  repeat
    Pos := SilWStr.Pos(Substr, Buffer, Ant, IgnoreCase);
    if Pos > 0 then
    begin
      if Pos > Ant then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Pos - Ant);
        System.Move(Buffer[Ant], Result[Len + 1], (Pos - Ant) * 2);
      end;
      Result := Result + Replace;
      Ant := Pos + Length(Substr);
    end else
    begin
      if Length(Buffer) - Ant + 1 > 0 then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Length(Buffer) - Ant + 1);
        System.Move(Buffer[Ant], Result[Len + 1], (Length(Buffer) - Ant + 1) * 2);
      end;
      Break;
    end;
  until false;
end;

class function SilWStr.DoLeftTrim(const Buffer: WideString; Len, Max, Ofs: Integer; Character: WideChar): Integer;
begin
  Result := 1;
  while (Result <= Max) and (Buffer[Result + Ofs] = Character) do Inc(Result);
end;

class function SilWStr.DoRightTrim(const Buffer: WideString; Len, Max, Ofs: Integer; Character: WideChar): Integer;
begin
  Result := 0;
  while (Result < Max) and (Buffer[Len - Result - Ofs] = Character) do Inc(Result);
  Result := Len - Result;
end;

class function SilWStr.Trim(const Buffer: WideString; const Character: WideChar; MaxCount: Integer): WideString;
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
    Result := self.Copy(Buffer, Head, Tail - Head + 1);
  end else
    Result := '';
end;

class function SilWStr.TrimLeft(const Buffer: WideString; Character: WideChar; MaxCount: Integer): WideString;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := self.Copy(Buffer, DoLeftTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Character), -1);
end;

class function SilWStr.TrimRight(const Buffer: WideString; Character: WideChar; MaxCount: Integer): WideString;
var
  Len: Integer;
begin
  Len := Length(Buffer);
  Result := self.Copy(Buffer, 1, DoRightTrim(Buffer, Len, Int.IIf(MaxCount > 0, MaxCount, Len), Int.IIf(MaxCount < 0, -MaxCount, 0), Character));
end;

end.

