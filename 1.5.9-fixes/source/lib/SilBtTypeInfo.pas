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

unit SilBtTypeInfo;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypes,
  SilBdTypeInfo,
  SilBeTypeInfo,
  SilBtTypeTool,
  SilBtDataType;

type
  PropClass = class of PropTool;
  EnumClass = class of EnumTool;

  Typ = class(TypeTool)
    class function Prop: PropClass;
    class function Enum: EnumClass;
    class function Data: DataTypeClass;
  end;

  PropTool = class(Tool)
    class function Exists(Instance: TObject; const PropName: string): Boolean; overload;
    class function Exists(Classref: TClass; const PropName: string): Boolean; overload;
    class function Value(Instance: TObject; const PropName: string): Variant; overload;
    class procedure Value(Instance: TObject; const PropName: string; const Value: Variant); overload;
  end;

  EnumTool = class(Tool)
    class function Name(const Info: PTypeInfo; const Value: Integer; const Prefix: string = ''): string;
    class function Value(const Info: PTypeInfo; const Name: string; const Prefix: string = ''; const Separator: string = ','): Integer;
    class function ToStr(TypeInfo: PTypeInfo; Value: Integer; const Prefix: string = ''; const Separator: string = ','): string;
    class function SetToStr(TypeInfo: PTypeInfo; Value: Integer; const Prefix: string = ''; const Separator: string = ','): string;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  SilBtStr;

{ Typ }

class function Typ.Prop: PropClass;
begin
  Result := PropTool;
end;

class function Typ.Enum: EnumClass;
begin
  Result := EnumTool;
end;

class function Typ.Data: DataTypeClass;
begin
  Result := DataTypeTool;
end;

{ EnumTool }

class function EnumTool.Name(const Info: PTypeInfo; const Value: Integer; const Prefix: string): string;
var
  P: PTypeData;
begin
  P := Typ.GetData(Info);
  if P <> nil then
    begin
      if( Value < P.MinValue ) or ( Value > P.MaxValue ) then
        raise EEnumValueOutOfRange.CreateFmt( SEnumValueOutOfRange, [Value, Info.Name] );
      Result := Str.Copy(GetEnumName(Info, Value), Str.Len(Prefix) + 1);
    end
  else
     raise EEnumDataNotFound.Create(EmptyStr)
end;

class function EnumTool.SetToStr(TypeInfo: PTypeInfo; Value: Integer; const Prefix, Separator: string): string;
var
  S: TIntegerSet;
  I: Integer;
  P: PTypeInfo;
begin
  Result := '';
  Integer(S) := Value;

  if TypeInfo.Kind = tkSet then
    P := Typ.GetData(TypeInfo).CompType^
  else
    P := TypeInfo;

  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + Separator;
      Result := Result + Name(P, I, Prefix);
    end;
end;

class function EnumTool.ToStr(TypeInfo: PTypeInfo; Value: Integer; const Prefix, Separator: string): string;
begin
  if TypeInfo.Kind in [tkEnumeration, tkInteger, tkChar, tkWChar] then
    Result := Name(TypeInfo, Value, Prefix) else
    Result := SetToStr(TypeInfo, Value, Prefix, Separator);
end;

class function EnumTool.Value(const Info: PTypeInfo; const Name: string; const Prefix, Separator: string): Integer;
var
  P: PTypeData;
  S: string;
  Idx: Integer;
begin
  P := Typ.GetData(Info);

  if P <> nil then
  begin
    if Info.Kind = tkSet then
    begin
      Result := 0;
      Idx := 0;

      while Str.Enumerate(Name, Separator, S, Idx) do
        Include(TIntegerSet(Result), Value(P.CompType^, S, Prefix));
    end else
    begin
      S := Prefix + Name;
      Result := GetEnumValue(Info, S);

      if( Result < P.MinValue ) or ( Result > P.MaxValue ) then
        raise EEnumIdentNotFound.CreateFmt(SEnumIdentNotFound, [S, Info.Name]);
    end;
  end else
    raise EEnumDataNotFound.Create(EmptyStr)
end;

{ PropTool }

class function PropTool.Exists(Instance: TObject; const PropName: string): Boolean;
begin
  Result := TypInfo.IsPublishedProp(Instance, PropName);
end;

class function PropTool.Exists(Classref: TClass; const PropName: string): Boolean;
begin
  Result := TypInfo.IsPublishedProp(Classref, PropName);
end;

class function PropTool.Value(Instance: TObject; const PropName: string): Variant;
begin
  Result := GetPropValue(Instance, PropName, False);
end;

class procedure PropTool.Value(Instance: TObject; const PropName: string; const Value: Variant);
begin
  SetPropValue(Instance, PropName, Value);
end;

end.
