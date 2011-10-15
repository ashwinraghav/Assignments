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

unit SilBtTime;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  Time = class(Tool)
    class function Parts(Hour, Minutes, Seconds, mSeconds: Word): TTimeParts;
    class function Now: TDateTime;
    class function IsValid(const Buffer: String): Boolean;
    class function Extract(const Value: TDateTime): TDateTime;
    class function ToStr(const Value: TDateTime; const Format: string {$IFDEF USE_DEFPARAMS} = ''{$ENDIF}): string;
    class function FromStr(const Value: string): TDateTime; overload;
    class function FromStr(const Value: string; const Default: TDateTime): TDateTime; overload;
    class function Decode(const Value: TDateTime): TTimeParts;
    class function Encode(const Parts: TTimeParts): TDateTime;
    class function EncodeParts(Hour: Word; Minutes: Word = 0; Seconds: Word = 0; mSeconds: Word = 0): TDateTime;
    class procedure DecodeParts(const Value: TDateTime; var Hour, Minutes, Seconds, mSeconds: Word);
    class function FromDateTime(const Value: TDateTime): TDateTime;
  end;

implementation

uses
  SysUtils;

class function Time.Decode(const Value: TDateTime): TTimeParts;
begin
  DecodeTime(Value, Result.Hour, Result.Minutes, Result.Seconds, Result.mSeconds);
end;

class procedure Time.DecodeParts(const Value: TDateTime; var Hour, Minutes, Seconds, mSeconds: Word);
var
  P: TTimeParts;
begin
  P := Time.Decode(Value);
  Hour := P.Hour;
  Minutes := P.Minutes;
  Seconds := P.Seconds;
  mSeconds := P.mSeconds;
end;

class function Time.Encode(const Parts: TTimeParts): TDateTime;
begin
  Result := EncodeTime(Parts.Hour, Parts.Minutes, Parts.Seconds, Parts.mSeconds);
end;

class function Time.EncodeParts(Hour, Minutes, Seconds, mSeconds: Word): TDateTime;
begin
  Result := Time.Encode(Time.Parts(Hour, Minutes, Seconds, mSeconds));
end;

class function Time.Extract(const Value: TDateTime): TDateTime;
begin
  Result := Value - Trunc(Value);
end;

class function Time.FromDateTime(const Value: TDateTime): TDateTime;
begin
  Result := Frac(Value);
end;

class function Time.IsValid(const Buffer: String): Boolean;
begin
  try
    StrToTime(Buffer);
    Result := true;
  except
    Result := false;
  end;
end;

class function Time.Now: TDateTime;
begin
  Result := SysUtils.Time;
end;

class function Time.Parts(Hour, Minutes, Seconds, mSeconds: Word): TTimeParts;
begin
  Result.Hour := Hour;
  Result.Minutes := Minutes;
  Result.Seconds := Seconds;
  Result.mSeconds := mSeconds;
end;

class function Time.ToStr(const Value: TDateTime; const Format: string): string;
begin
  if Length(Format) > 0 then
    Result := FormatDateTime(Format, Value) else
    Result := TimeToStr(Value);
end;

class function Time.FromStr(const Value: string): TDateTime;
begin
  Result := StrToTime(Value);
end;

class function Time.FromStr(const Value: string; const Default: TDateTime): TDateTime;
begin
  try
    Result := StrToTime(Value);
  except
    Result := Default;
  end;
end;

end.
