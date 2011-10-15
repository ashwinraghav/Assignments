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

unit SilBtDateTime;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  DateTime = class(Tool)
    class function Parts(const Date: TDateParts; const Time: TTimeParts): TDateTimeParts;
    class function Now: TDateTime;
    class function IsEmpty(const Buffer: String): Boolean;
    class procedure Inc(var Value: TDateTime; Position, Increment: Integer); overload;
    class procedure Inc(var Value: TDateTime; Element: TTimeElement; Increment: Integer); overload;
    class function Round(const Value: TDateTime; const Position: Integer): TDateTime;
    class function ToStr(const Value: TDateTime; const Format: string {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): string;
    class function FromStr(const Value: string): TDateTime; overload;
    class function FromStr(const Value: string; Default: TDateTime): TDateTime; overload;
    class function FromStr(const Value: string; const Format: String; Pivot: Word = 60): TDateTime; overload;
    class function FromStr(out Return: TDateTime; const Value: string): Boolean; overload;
    class function Decode(const Value: TDateTime): TDateTimeParts;
    class function Encode(const Parts: TDateTimeParts): TDateTime;
    class function ToMilisecs(const Value: TDateTime; const Offset: Integer = 0): LargeInt;
    class function ToSecs(const Value: TDateTime; const Offset: Integer = 0): LongWord;
    class function ToMinutes(const Value: TDateTime; const Offset: Integer = 0): LongWord;
    class function ToHours(const Value: TDateTime): Integer;
    class function ToPosix(const Value: TDateTime): LongWord;
    class function ToStamp(const Value: TDateTime): TDateTimeStamp;
    class function ToArray(const Value: TDateTime): TWordArray;
    class function FromSecs(Value: LongWord; const Offset: Integer = 0): TDateTime; overload;
    class function FromSecs(Value: LongWord; const Offset: TDateTime): TDateTime; overload;
    class function FromSecs(const Value: Double; const Offset: Integer = 0): TDateTime; overload;
    class function FromSecs(const Value: Double; const Offset: TDateTime): TDateTime; overload;
    class function FromHours(Value: LongWord): TDateTime;
    class function FromDays(Value: LongWord): TDateTime;
    class function FromMilisecs(Value: LargeInt; const Offset: Integer = 0): TDateTime;
    class function FromMinutes(Value: LargeInt; const Offset: Integer = 0): TDateTime;
    class function FromPosix(Value: LongWord): TDateTime;
    class function FromStamp(const Value: TDateTimeStamp): TDateTime;
    class function Compare(const V1, V2: TDateTime): Integer;
    class function Find(const Value: TDateTime; Position: TDayPosition; Day: TDayOfWeek): TDateTime; overload;
    class function Find(const Value: TDateTime; const Day: TDayOfWeek; Position: TDayPosition = 0): TDateTimeArray; overload;
    class function MinutesBetween(const Newest: TDateTime; const Oldest: TDateTime): LargeInt;
    class function MinutesToNow(const Value: TDateTime): LargeInt;
    class function HourOf(const Value: TDateTime): integer;
    class procedure Swap(var i1, i2: TDateTime);
    class function IIf(const Expr: Boolean; const RTrue: TDateTime; RFalse: TDateTime = 0): TDateTime;
  end;

implementation

uses
  SysUtils,
  SilBtTime,
  SilBtDate,
  SilBtStr,
  SilBtInt;

const
  MinutesPerDay    = 60 * 24;
  SecondsPerDay    = 60 * MinutesPerDay;
  PosixToDelphi    = 25569;

class function DateTime.Compare(const V1, V2: TDateTime): Integer;
var
  i1, i2: Integer;
begin
  Result := Trunc(V1) - Trunc(V2);

  if Result = 0 then
  begin
    i1 := Trunc(Frac(V1) * SecondsPerDay * 1000);
    i2 := Trunc(Frac(V2) * SecondsPerDay * 1000);

    Result := i1 - i2;
  end;
end;

class function DateTime.Decode(const Value: TDateTime): TDateTimeParts;
begin
  Result.Date := Date.Decode(Value);
  Result.Time := Time.Decode(Value);
end;

class function DateTime.Encode(const Parts: TDateTimeParts): TDateTime;
begin
  Result := Time.Encode(Parts.Time) + Date.Encode(Parts.Date);
end;

class procedure DateTime.Inc(var Value: TDateTime; Element: TTimeElement; Increment: Integer);
begin
  Inc(Value, Ord(Element), Increment);
end;

class procedure DateTime.Inc(var Value: TDateTime; Position, Increment: Integer);
begin
  case Position of
    0:  Value := IncMonth(Value, Increment * 12);             // years
    1:  Value := IncMonth(Value, Increment);                  // months
    2:  Value := Value + Increment;                           // days
    3:  Value := Value + Increment / 24;                      // hours
    4:  Value := Value + Increment / MinutesPerDay;           // mins
    5:  Value := Value + Increment / SecondsPerDay;           // secs
    6:  Value := Value + Increment / (SecondsPerDay * 1000);  // millis
  end;
end;

class function DateTime.Round(const Value: TDateTime; const Position: Integer): TDateTime;
var
  Vinc: Boolean;
  Parts: TDateTimeParts;
begin
  Result := Value;
  Parts := Decode(Value);

  case Position of
    2:    Vinc := Parts.Time.Hour >= 12;
    3:    Vinc := Parts.Time.Minutes >= 30;
    4:    Vinc := Parts.Time.Seconds >= 30;
    5:    Vinc := Parts.Time.mSeconds >= 500;
    else  Vinc := false;
  end;

  if Position < 3 then Parts.Time.Minutes := 0;
  if Position < 4 then Parts.Time.Minutes := 0;
  if Position < 5 then Parts.Time.Seconds := 0;
  if Position < 6 then Parts.Time.mSeconds := 0;

  Result := DateTime.Encode(Parts);

  if Vinc then Inc(Result, Position, 1);
end;

class function DateTime.ToStr(const Value: TDateTime; const Format: string): string;
begin
  if Length(Format) > 0 then
    Result := FormatDateTime(Format, Value) else
    Result := DateTimeToStr(Value);
end;

class function DateTime.FromStr(const Value: string): TDateTime;
begin
  Result := StrToDateTime(Value);
end;

class function DateTime.FromStr(const Value: string; Default: TDateTime): TDateTime;
begin
  if not FromStr(Result, Value) then
    Result := Default;
end;

class function DateTime.FromStr(const Value: string; const Format: String; Pivot: Word): TDateTime;
const
  Chars: String = 'ymdhnsz';
var
  Item: String;
  i1, Pos, Last: Integer;
  Parts: TDateTimeParts;
  DValue: TDateTime;
begin
  {if (Str.Pos('/', Format) = 0) and (Str.ToInt(Value, 0) = 0) then
  begin
    Result := 0;
    Exit;
  end;}

  FillChar(Parts, SizeOf(Parts), 0);

  for i1 := 1 to Length(Chars) do
  begin
    Pos := Str.Pos(Chars[i1], Format);

    if Pos > 0 then
    begin
      Last := Str.LastPos(Chars[i1], Format);
      Item := Str.Copy(Value, Pos, Last - Pos + 1);

      case Chars[i1] of
        'y':  Parts.Date.Year := Str.ToInt(Item);
        'm':  Parts.Date.Month := Str.ToInt(Item);
        'd':  Parts.Date.Day := Str.ToInt(Item);
        'h':  Parts.Time.Hour := Str.ToInt(Item);
        'n':  Parts.Time.Minutes := Str.ToInt(Item);
        's':  Parts.Time.Seconds := Str.ToInt(Item);
        'z':  Parts.Time.mSeconds := Str.ToInt(Item);
      end;
    end;
  end;

  DValue := 0;

  with Parts.Date do
    if Year + Month + Day > 0 then
    begin
      if Day = 0 then Day := 1;

      if Year < 1000 then
      begin
        if Year < Pivot then
          Year := 2000 + Year else
          Year := 1900 + Year;
      end;

      DValue := DValue + Date.Encode(Parts.Date);
    end;

  with Parts.Time do
    if Hour + Minutes + Seconds + mSeconds > 0 then
      DValue := DValue + Time.Encode(Parts.Time);

  Result := DValue;
end;

class function DateTime.FromStr(out Return: TDateTime; const Value: string): Boolean;
begin
  Result := TryStrToDateTime(Value, Return);
end;

class function DateTime.IsEmpty(const Buffer: String): Boolean;
var
  i: integer;
begin
  Result := true;

  for i := 1 to Length(Buffer) do
    if (Buffer[i] >= '0') and (Buffer[i] <= '9') then
    begin
      Result := false;
      Exit;
    end;
end;

class function DateTime.Now: TDateTime;
begin
  Result := SysUtils.Now;
end;

class function DateTime.Parts(const Date: TDateParts; const Time: TTimeParts): TDateTimeParts;
begin
  Result.Date := Date;
  Result.Time := Time;
end;

class function DateTime.ToMilisecs(const Value: TDateTime; const Offset: Integer): LargeInt;
begin
  Result := System.Trunc((Value - Offset) * SecondsPerDay * 1000);
end;

class function DateTime.ToSecs(const Value: TDateTime; const Offset: Integer): LongWord;
begin
  Result := System.Trunc((Value - Offset) * SecondsPerDay);
end;

class function DateTime.ToMinutes(const Value: TDateTime; const Offset: Integer = 0): LongWord;
begin
  Result := System.Trunc((Value - Offset) * MinutesPerDay);
end;

class function DateTime.ToHours(const Value: TDateTime): Integer;
begin
  Result := Trunc(Value * 24);
end;

class function DateTime.ToPosix(const Value: TDateTime): LongWord;
begin
  Result := ToSecs(Value, PosixToDelphi);
end;

class function DateTime.ToStamp(const Value: TDateTime): TDateTimeStamp;
var
  Stamp: SysUtils.TTimeStamp;
begin
  Stamp := SysUtils.DateTimeToTimeStamp(Value);
  Result.Time := Stamp.Time;
  Result.Date := Stamp.Date;
end;

class function DateTime.FromDays(Value: LongWord): TDateTime;
begin
  Result := Value;
end;

class function DateTime.FromHours(Value: LongWord): TDateTime;
begin
  Result := Value / 24;
end;

class function DateTime.FromSecs(Value: LongWord; const Offset: Integer): TDateTime;
begin
  Result := Value / SecondsPerDay + Offset;
end;

class function DateTime.FromSecs(Value: LongWord; const Offset: TDateTime): TDateTime;
begin
  Result := Value / SecondsPerDay + Offset;
end;

class function DateTime.FromSecs(const Value: Double; const Offset: Integer): TDateTime;
begin
  Result := Value / SecondsPerDay + Offset;
end;

class function DateTime.FromSecs(const Value: Double; const Offset: TDateTime): TDateTime;
begin
  Result := Value / SecondsPerDay + Offset;
end;

class function DateTime.FromMinutes(Value: LargeInt; const Offset: Integer): TDateTime;
begin
  Result := Value / MinutesPerDay + Offset;
end;

class function DateTime.FromMilisecs(Value: LargeInt; const Offset: Integer): TDateTime;
begin
  Result := Value / 1000.00 / SecondsPerDay + Offset;
end;

class function DateTime.FromPosix(Value: LongWord): TDateTime;
begin
  Result := FromSecs(Value, PosixToDelphi);
end;

class function DateTime.FromStamp(const Value: TDateTimeStamp): TDateTime;
var
  Stamp: SysUtils.TTimeStamp;
begin
  Stamp.Time := Value.Time;
  Stamp.Date := Value.Date;
  Result := SysUtils.TimeStampToDateTime(Stamp);
end;

class function DateTime.Find(const Value: TDateTime; const Day: TDayOfWeek; Position: TDayPosition): TDateTimeArray;
var
  i: Integer;
  DayTable: PDayTable;
  Parts: TDateTimeParts;
  Stamp: TDateTime;
  Pos: Integer;
begin
  SetLength(Result, 5);

  Parts := DateTime.Decode(Value);
  Parts.Date.Day := 1;
  Stamp := DateTime.Encode(Parts);
  DayTable := @MonthDays[Date.IsLeap(Stamp)];
  Pos := 0;

  for i := 1 to DayTable[Parts.Date.Month] do
  begin
    if Date.DayOfWeek(Stamp) = Day then
    begin
      Result[Pos] := Stamp;
      System.Inc(Pos);
    end;

    if (Position <> 0) and (Pos = Ord(Position)) then Break;
    Stamp := Stamp + 1;
  end;

  if Pos < 5 then SetLength(Result, Pos);
end;

class function DateTime.Find(const Value: TDateTime; Position: TDayPosition; Day: TDayOfWeek): TDateTime;
var
  Dates: TDateTimeArray;
begin
  Dates := DateTime.Find(Value, Day, Position);

  if (Position > 0) and (Position < 6) then
  begin
    if Position = 5 then
      Result := Dates[Length(Dates) - 1] else
      Result := Dates[Position - 1];
  end else
    Result := 0;
end;

class function DateTime.ToArray(const Value: TDateTime): SilBeTypes.TWordArray;
var
  Parts: TDateTimeParts;
begin
  Parts := Decode(Value);
  SetLength(Result, 7);
  Result[0] := Parts.Date.Year;
  Result[1] := Parts.Date.Month;
  Result[2] := Parts.Date.Day;
  Result[3] := Parts.Time.Hour;
  Result[4] := Parts.Time.Minutes;
  Result[5] := Parts.Time.Seconds;
  Result[6] := Parts.Time.mSeconds;
end;

class function DateTime.MinutesBetween(const Newest: TDateTime; const Oldest: TDateTime): LargeInt;
begin
  Result:= ToMinutes(Newest) - ToMinutes(Oldest);
end;

class function DateTime.MinutesToNow(const Value: TDateTime): LargeInt;
begin
  Result:= MinutesBetween(Now, Value);
end;

class function DateTime.HourOf(const Value: TDateTime): integer;
begin
  Result:= Time.Decode(Value).Hour;
end;

class procedure DateTime.Swap(var i1, i2: TDateTime);
var
  i: TDateTime;
begin
  i := i1;
  i1 := i2;
  i2 := i;
end;

class function DateTime.IIf(const Expr: Boolean; const RTrue: TDateTime; RFalse: TDateTime): TDateTime;
begin
  if Expr then Result := RTrue else Result := RFalse;
end;

end.


