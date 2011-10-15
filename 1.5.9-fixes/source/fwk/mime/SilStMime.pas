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

unit SilStMime;

interface

uses
  SilBeTypes,
  SilBkTool,
  SilSiMailClient,
  SilSmMimeCoder;

type
  MimeType = class of MimeTool;
  MimeTool = class(Tool)
    class function Base64Coder: TMimeCoderClass;
    class function UUCoder: TMimeCoderClass;
    class function ISO88591Coder: TMimeCoderClass;
    class function QuotedPrintableCoder: TMimeCoderClass;
    class function Windows1252Coder: TMimeCoderClass;
    class function HexaCoder: TMimeCoderClass;
    class function DefaultCoder: TMimeCoderClass;
    class function DateTimeToStr(const Value: TDateTime): String;
    class function StrToDateTime(Value: String): TDateTime;
    class function XmlCoder: TMimeCoderClass;
  end;

implementation

uses
  SysUtils,

  SilBtText,
  SilBtStr,
  SilBtTime,
  SilBtDate,
  SilBtInt,
  SilOsTool;

const
  CMonthNames: string = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec';
  CDayNames: string   = 'Sun Mon Tue Wed Thu Fri Sat';

{ MimeTool }

//Date: Tue, 23 Jan 2001 09:29:37 -0300
class function MimeTool.DateTimeToStr(const Value: TDateTime): String;
var
  Parts: TDateParts;
  sDay, sMonth, sTime, sBias, sDummy: String;
  iBias: Integer;
begin
  Parts := Date.Decode(Value);
  sDay := Str.Copy(CDayNames, (DayOfWeek(Value) - 1) * 4 + 1, 3);
  sMonth := Str.Copy(CMonthNames, (Parts.Month - 1) * 4 + 1, 3);
  sTime := Time.ToStr(Value, 'hh:nn:ss');
  OsTimeZone.GetTimeZoneInfo(sDummy, iBias);
  iBias := Trunc(iBias / 60 * 100);
  sBias := Str.IIf(iBias > 0, '-', '+') + Str.PadL(Int.ToStr(iBias), 4, '0');
  Result := Str.Format('%s, %d %s %d %s %s', [sDay, Parts.Day, sMonth, Parts.Year, sTime, sBias]);
end;

class function MimeTool.StrToDateTime(Value: String): TDateTime;

  function p_FindMonth(const Month: String): Integer;
  var
    i: Integer;
  begin
    for i := 0 to 11 do
      if Text.Compare(Month, Str.Copy(CMonthNames, i * 4, 3)) = 0 then
      begin
        Result := i + 1;
        Exit;
      end;

    Result := -1;
  end;

  function p_TimeOffset(Offset: String): TDateTime;
  var
    i, iNeg: Integer;
  begin
    while true do
    begin
      i := Str.Pos(#34, Offset);
      if i = 0 then i := Str.Pos(#39, Offset);
      if i = 0 then Break;
      Str.Delete(Offset, i, 1);
    end;

    if Length(Offset) < 5 then
    begin
      Result := 0;
      Exit;
    end;

    if Offset[1] = '-' then
      iNeg := 1 else
      iNeg := -1;

    try
      Result := Time.EncodeParts(
        Str.ToInt(Str.Copy(Offset, 2, 2)),
        Str.ToInt(Str.Copy(Offset, 4, 2)), 0, 0) * iNeg;
    except
      Result := 0;
    end;
  end;


var
  i, iBias, iCount: Integer;
  List: array[0..5] of String;
  wYear: Word;
  sName: String;
begin
  Result := 0;
  Value := Str.Trim(Value);

  if (Length(Value) > 0) and (Value[1] in ['0'..'9']) then
  begin
    i := Pos('/', Value);
    if i > 0 then
      try
        Result := Date.EncodeParts(
          Str.ToInt(Str.Copy(Value, i + 4, 4), 0),
          Str.ToInt(Str.Copy(Value, i - 2, 2), 0),
          Str.ToInt(Str.Copy(Value, i + 1, 2), 0));
      except end;

    i := Str.Pos(':', Value);
    if i > 0 then
      try
        Result := Result + Time.EncodeParts(
          Str.ToInt(Str.Copy(Value, i - 2, 2), 0),
          Str.ToInt(Str.Copy(Value, i + 1, 2), 0),
          Str.ToInt(Str.Copy(Value, i + 4, 2), 0), 0);
      except end;

    Exit;
  end;

  i := Str.Pos(' ', Value);
  if i = 0 then Exit;

  Str.Delete(Value, 1, i);
  Value := Str.TrimLeft(Value);
  i := 1;
  iCount := 0;

  repeat
    if iCount > High(List) then Break;
    List[iCount] := Str.Token(Value, ' ', i);
    Inc(iCount);
  until i = 0;

  OsTimeZone.GetTimeZoneInfo(sName, iBias);

  try
    wYear := Str.ToInt(List[2]);
    Result :=
      Date.EncodeParts(wYear, p_FindMonth(List[1]), StrToInt(List[0])) +
      Time.FromStr(List[3]) + p_TimeOffset(List[4]) - (iBias / 24 / 60);
  except
    Result := 0;
  end;
end;

class function MimeTool.Base64Coder: TMimeCoderClass;
begin
  result := TBase64Coder;
end;

class function MimeTool.DefaultCoder: TMimeCoderClass;
begin
  Result := TDefaultCoder;
end;

class function MimeTool.ISO88591Coder: TMimeCoderClass;
begin
  Result := TISO8859_1Coder;
end;

class function MimeTool.QuotedPrintableCoder: TMimeCoderClass;
begin
  Result := TQuotedPrintableCoder;
end;

class function MimeTool.UUCoder: TMimeCoderClass;
begin
  Result := TUUCoder;
end;

class function MimeTool.Windows1252Coder: TMimeCoderClass;
begin
  Result := TWindows1252Coder;
end;

class function MimeTool.HexaCoder: TMimeCoderClass;
begin
  result := THexaCoder;
end;

class function MimeTool.XmlCoder: TMimeCoderClass;
begin
  Result := TXmlCoder;
end;

end.

