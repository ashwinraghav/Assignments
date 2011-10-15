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

unit SilOmRegValues;

{$I Defines.inc}

interface

uses
  (*)Windows, (*)SysUtils,
  SilLiKey,
  SilLiEnumerator,
  SilLkNamedValues,
  SilOmRegKey;

type
  TSilLinuxRegistryValues = class(TSilNamedValues)
  private
    FOwner: TSilLinuxRegistryKey;
    (*)FKey: HKey;(*)
  protected
    function DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
    function DoRemove(const Name: String): Boolean; override;
  protected
    function DoGetInfo(const Name: String; var DataType: TNamedKeyDataType; var Size: Integer): Boolean; override;
    function DoRead(const Name: String; var DataType: TNamedKeyDataType; var Buffer: String): Boolean; override;
    function DoWrite(const Name: String; Value: PChar; Size: Integer; DataType: TNamedKeyDataType): Boolean; override;
  public
    constructor Create(const Owner: TSilLinuxRegistryKey);
  end;

implementation

uses
  SilOsError,
  SilOgRegistry,
  SilOmRegEnumerators;

{ TSilLinuxRegistryValues }

constructor TSilLinuxRegistryValues.Create(const Owner: TSilLinuxRegistryKey);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxRegistryValues.Create']);

  inherited Create;
  FOwner := Owner;
(*)  FKey := FOwner.Key;(*)
end;

function TSilLinuxRegistryValues.DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
(*)var
  NumValues: LongWord;(*)
begin
(*)  Result := (RegQueryInfoKey(FKey, nil, nil, nil, nil, nil, nil, @NumValues, nil, nil, nil, nil) = ERROR_SUCCESS) and (NumValues> 0);
  if Result then Enum := TRegistryValueEnumerator.Create(Lockable, FKey);(*)
end;

function TSilLinuxRegistryValues.DoGetInfo(const Name: String; var DataType: TNamedKeyDataType; var Size: Integer): Boolean;
(*)var
  iDataType: Integer;(*)
begin
(*)  Result := RegQueryValueEx(FKey, PChar(Name), nil, @iDataType, nil, @Size) = ERROR_SUCCESS;
  if not Result then Exit;
  case iDataType of
    REG_SZ, REG_EXPAND_SZ:
        DataType := kdString;
    REG_DWORD:
        DataType := kdInteger;
    REG_BINARY:
        DataType := kdBinary;
    REG_MULTI_SZ:
        DataType := kdStrings;
  else
        DataType := kdUnknown;
  end;(*)
end;

function TSilLinuxRegistryValues.DoWrite(const Name: String; Value: PChar; Size: Integer; DataType: TNamedKeyDataType): Boolean;
(*)var
  iDataType: Integer;(*)
begin
(*)  case DataType of
    kdString:       iDataType := REG_SZ;
    kdInteger:      iDataType := REG_DWORD;
    kdBinary:       iDataType := REG_BINARY;
    kdStrings:      iDataType := REG_MULTI_SZ;
    else            iDataType := REG_NONE;
  end;

  Result := RegSetValueEx(FKey, PChar(Name), 0, iDataType, Value, Size) = ERROR_SUCCESS;(*)
end;

function TSilLinuxRegistryValues.DoRead(const Name: String; var DataType: TNamedKeyDataType; var Buffer: String): Boolean;
(*)var
  iSize: Integer;(*)
begin
(*)  Result := DoGetInfo(Name, DataType, iSize);
  if not Result then Exit;

  SetString(Buffer, nil, iSize);
  Result := RegQueryValueEx(FKey, PChar(Name), nil, nil, @Buffer[1], @iSize) = ERROR_SUCCESS;(*)
end;

function TSilLinuxRegistryValues.DoRemove(const Name: String): Boolean;
begin
(*)  Result := RegDeleteValue(FKey, PChar(Name)) = ERROR_SUCCESS;(*)
end;

end.
 