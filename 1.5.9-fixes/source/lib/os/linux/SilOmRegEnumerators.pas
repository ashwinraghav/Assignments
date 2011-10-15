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

unit SilOmRegEnumerators;

{$I Defines.inc}

interface

uses
  (*)Windows, (*)
  SysUtils,
  SilLkNamedItems,
  SilOeRegistry;

type
  TRegistryEnumerator = class(TSilNamedItemsEnumerator)
  protected
    (*)FKey: HKey;(*)
    (*)FInfo: TRegKeyInfo;(*)
  protected // TAbstractEnumerator
    procedure DoReset; override;
		(*)function DoGetKeyInfo(var Value: TRegKeyInfo): Boolean;(*)
  public
		(*)constructor Create(const Enum: IUnknown; RootKey: HKey); reintroduce;(*)
  end;

  TRegistryKeyEnumerator = class (
    // extends
    TRegistryEnumerator)
  protected
    function DoGetItem: String; override;
	protected // IEnumerator
		function DoHasMore: Boolean; override;
  end;

  TRegistryValueEnumerator = class (
    // extends
    TRegistryEnumerator)
  protected
    function DoGetItem: String; override;
	protected // IEnumerator
		function DoHasMore: Boolean; override;
  end;

implementation

uses
  SilBtStr;

{ TRegistryEnumerator }

(*)constructor TRegistryEnumerator.Create(const Enum: IUnknown; RootKey: HKey);
begin
  inherited Create(Enum, true);
  FKey := RootKey;
end;(*)

procedure TRegistryEnumerator.DoReset;
begin
(*)  DoGetKeyInfo(FInfo);(*)
  inherited;
end;

(*)function TRegistryEnumerator.DoGetKeyInfo(var Value: TRegKeyInfo): Boolean;
begin
  FillChar(Value, SizeOf(TRegKeyInfo), 0);
  Result := RegQueryInfoKey(FKey, nil, nil, nil, @Value.NumSubKeys,
    @Value.MaxSubKeyLen, nil, @Value.NumValues, @Value.MaxValueLen,
    @Value.MaxDataLen, nil, @Value.FileTime) = ERROR_SUCCESS;
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    with Value do
    begin
      Inc(MaxSubKeyLen, MaxSubKeyLen);
      Inc(MaxValueLen, MaxValueLen);
    end;
end;(*)

{ TRegistryKeyEnumerator }

function TRegistryKeyEnumerator.DoGetItem: String;
(*)var
  cLen: Cardinal;(*)
begin
(*)  cLen := FInfo.MaxSubKeyLen + 1;
  SetLength(Result, cLen);
  Str.Fill(Result, cLen);
  RegEnumKey(FKey, FIteration, PChar(Result), cLen);
  Result := Str.Trim(Result);(*)
end;

function TRegistryKeyEnumerator.DoHasMore: Boolean;
begin
(*)  Result := FIteration < FInfo.NumSubKeys;(*)
end;

{ TRegistryValueEnumerator }

function TRegistryValueEnumerator.DoGetItem: String;
(*)var
  cLen: Cardinal;(*)
begin
(*)  cLen := FInfo.MaxValueLen + 1;
  SetString(Result, nil, cLen);
  RegEnumValue(FKey, FIteration, PChar(Result), cLen, nil, nil, nil, nil);
  SetLength(Result, cLen);(*)
end;

function TRegistryValueEnumerator.DoHasMore: Boolean;
begin
(*)  Result := FIteration < FInfo.NumValues;(*)
end;

end.
