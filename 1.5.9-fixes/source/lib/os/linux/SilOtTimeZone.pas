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

unit SilOtTimeZone;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilOjTimeZone,
  SilBtDateTime;

type
  LinuxTimeZone = class (SilTimeZone)
    class function GlobalToLocal(const Value: TDateTime): TDateTime; override;
    class function LocalToGlobal(const Value: TDateTime): TDateTime; override;
    class function GetTimeZoneInfo(out Name: String; out Bias: Integer): Boolean; override;
  end;

implementation

uses
  SilOsError(*),
  Linux(*);

{ DateTime }

class function LinuxTimeZone.GetTimeZoneInfo(out Name: String; out Bias: Integer): Boolean;
(*)var
  lpZone: TTimeZoneInformation;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxTimeZone.GetTimeZoneInfo']);
(*)  Result := GetTimeZoneInformation(lpZone) <> $FFFFFFFF;
  Name := lpZone.StandardName;
  Bias := lpZone.Bias;(*)
end;

class function LinuxTimeZone.GlobalToLocal(const Value: TDateTime): TDateTime;
(*)var
  LT, UT: TSystemTime;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxTimeZone.GlobalToLocal']);
(*)  DateTimeToSystemTime(Value, UT);
  SystemTimeToTzSpecificLocalTime(nil, UT, LT);
  Result := SystemTimeToDateTime(LT);(*)
end;

class function LinuxTimeZone.LocalToGlobal(const Value: TDateTime): TDateTime;
(*)var
  LT, UT: TSystemTime;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxTimeZone.LocalToGlobal']);
(*)  DateTimeToSystemTime(Value, UT);
  SystemTimeToTzSpecificLocalTime(nil, UT, LT);
  Result := SystemTimeToDateTime(LT);(*)
end;

{ Api }

(*class function Api.DateTimeToSystemTime(D: TDateTime): TSystemTime;
var
  P: TDateTimeParts;
begin
  with Result do
	begin
    DateTime.Decode(D);
    wYear := P.Date.Year;
    wMonth := P.Date.Month;
    wDay := P.Date.Day;
		wDayOfWeek := Word(Abs(Trunc(D) - 1) mod 7);
    wHour := P.Time.Hour;
    wMinute := P.Time.Minutes;
    wSecond := P.Time.Seconds;
    wMilliseconds := P.Time.mSeconds;
	end;
end;

class function Api.SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  with SystemTime do
    Result :=
      Date.EncodeParts(wYear, wMonth, wDay) +
			Time.EncodeParts(wHour, wMinute, wSecond, wMilliSeconds);
end;*)

end.
