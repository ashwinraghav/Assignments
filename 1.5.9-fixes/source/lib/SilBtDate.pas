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

unit SilBtDate;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  Date = class(Tool)
    class function Parts(Year, Month, Day: Word): TDateParts;
    class function Now: TDateTime;
    class function IsValid(const Buffer: String): Boolean;
    class function Extract(const Value: TDateTime): TDateTime;
    class function ToStr(const Value: TDateTime; const Format: string {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): string;
    class function FromStr(const Value: string): TDateTime;
    class function Decode(const Value: TDateTime): TDateParts;
    class function Encode(const Parts: TDateParts): TDateTime;
    class function EncodeParts(Year: Word; Month: Word = 0; Day: Word = 0): TDateTime;
    class procedure DecodeParts(const Value: TDateTime; var Year, Month, Day: Word);
    class function FromDateTime(const Value: TDateTime): TDateTime;
    class function DayOfWeek(const Value: TDateTime): TDayOfWeek;
    class function Compare(Const Value1, Value2: TDateTime): Integer;
    class function IsEqual(Const Value1, Value2: TDateTime): Boolean;
    class function IsLeap(Const Value: TDateTime): Boolean;
    class procedure Swap(var i1, i2: TDateTime);
  end;

implementation

uses
  SysUtils,
  SilBtInt;

class function Date.Decode(const Value: TDateTime): TDateParts;
begin
  DecodeDate(Value, Result.Year, Result.Month, Result.Day);
end;

class procedure Date.DecodeParts(const Value: TDateTime; var Year, Month, Day: Word);
var
  P: TDateParts;
begin
  P := Date.Decode(Value);
  Year := P.Year;
  Month := P.Month;
  Day := P.Day;
end;

class function Date.Encode(const Parts: TDateParts): TDateTime;
begin
  Result := EncodeDate(Parts.Year, Parts.Month, Parts.Day);
end;

class function Date.EncodeParts(Year, Month, Day: Word): TDateTime;
begin
  Result := Date.Encode(Date.Parts(Year, Month, Day));
end;

class function Date.Extract(const Value: TDateTime): TDateTime;
begin
  Result := Trunc(Value);
end;

class function Date.FromDateTime(const Value: TDateTime): TDateTime;
begin
  Result := Trunc(Value);
end;

class function Date.IsValid(const Buffer: String): Boolean;
begin
  try
    StrToDate(Buffer);
    Result := true;
  except
    Result := false;
  end;
end;

class function Date.Now: TDateTime;
begin
  Result := SysUtils.Date;
end;

class function Date.Parts(Year, Month, Day: Word): TDateParts;
begin
  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
end;

class function Date.ToStr(const Value: TDateTime; const Format: string): string;
begin
  if Length(Format) > 0 then
    Result := FormatDateTime(Format, Value) else
    Result := DateToStr(Value);
end;

class function Date.FromStr(const Value: string): TDateTime;
begin
  Result := StrToDate(Value);
end;

class function Date.DayOfWeek(const Value: TDateTime): TDayOfWeek;
begin
  Result := TDayOfWeek(SysUtils.DayOfWeek(Value));
end;

class function Date.Compare(const Value1, Value2: TDateTime): Integer;
begin
  Result := Trunc(Value1) - Trunc(Value2);
end;

class function Date.IsEqual(const Value1, Value2: TDateTime): Boolean;
begin
  Result := Compare(Value1, Value2) = 0;
end;

class function Date.IsLeap(const Value: TDateTime): Boolean;
begin
  Result := SysUtils.IsLeapYear(Date.Decode(Value).Year);
end;

class procedure Date.Swap(var i1, i2: TDateTime);
var
  i: TDateTime;
begin
  i := i1;
  i1 := i2;
  i2 := i;
end;

end.
 