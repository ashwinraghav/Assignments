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

unit SilOtGuid;

{$I Defines.inc}

interface

uses
  SilOjGuid;

const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

type
  OsGuid = class(SilGuid)
    class function Create: TGUID; override;
    class function Null: TGUID; override;
    class function ToStr(const Value: TGUID): String; override;
    class function FromStr(const Value: String): TGUID; override;
    class function IsEqual(const Value1, Value2: TGUID): Boolean; override;
  end;

implementation

uses
//  SilOsError,
  SysUtils;

{ OsGuid }

class function OsGuid.Create: TGUID;
begin
  raise Exception.Create('OsGuid.Create: not implemented');
//  SysUtils.CreateGUID(Result);
end;

class function OsGuid.FromStr(const Value: String): TGUID;
begin
  Result := SysUtils.StringToGUID(Value);
end;

class function OsGuid.IsEqual(const Value1, Value2: TGUID): Boolean;
begin
  Result := SysUtils.IsEqualGUID(Value1, Value2);
end;

class function OsGuid.Null: TGUID;
begin
  Result := GUID_NULL;
end;

class function OsGuid.ToStr(const Value: TGUID): String;
begin
  Result := SysUtils.GUIDToString(Value);
end;

end.
