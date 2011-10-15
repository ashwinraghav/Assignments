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

unit SilLkNamedValues;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkNamedItems,
  SilLiKey,
  SilLiLock,
  SilLiEnumerator,
  SilLiStringList;

type
  TSilNamedValues = class (
    // extends
    TSilNamedItems,
    // implements
    INamedValues )
  protected // INamedValues
    function Exists(const Name: String): Boolean;
    function GetDataType(const Name: String): TNamedKeyDataType;
    function GetDataSize(const Name: String): LongWord;
    function ReadString(const Name: String; const Default: String = ''; CanCreate: Boolean = false): String;
    function ReadInteger(const Name: String; const Default: Integer = 0; CanCreate: Boolean = false): Integer;
    function ReadLargeInt(const Name: String; const Default: LargeInt = 0; CanCreate: Boolean = false): LargeInt;
    function ReadBoolean(const Name: String; const Default: Boolean = false; CanCreate: Boolean = false): Boolean;
    function ReadFloat(const Name: String; const Default: Double = 0; CanCreate: Boolean = false): Double;
    function ReadStrings(const Name: String; const Default: IStringList = nil; CanCreate: Boolean = false): IStringList;
    function WriteString(const Name: String; const Value: String): Boolean;
    function WriteInteger(const Name: String; Value: Integer): Boolean;
    function WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
    function WriteBoolean(const Name: String; Value: Boolean): Boolean;
    function WriteFloat(const Name: String; Value: Double): Boolean;
    function WriteStrings(const Name: String; const Value: IStringList): Boolean;
  protected //- primitivas
    function DoGetInfo(const Name: String; var DataType: TNamedKeyDataType; var Size: Integer): Boolean; virtual; abstract;
    function DoRead(const Name: String; var DataType: TNamedKeyDataType; var Buffer: String): Boolean; virtual; abstract;
    function DoWrite(const Name: String; Value: PChar; Size: Integer; DataType: TNamedKeyDataType): Boolean; virtual; abstract;
  end;

implementation

uses
  SysUtils,
  SilBtInt,
  SilBtFloat,
  SilBtStr,
  SilBtText,
  SilBtLarge,
  SilLtList;

{ TSilNamedValues }

function TSilNamedValues.GetDataSize(const Name: String): LongWord;
var
  iSize: Integer;
  DataType: TNamedKeyDataType;
begin
  if DoGetInfo(Name, DataType, iSize) then
    Result := iSize else
    Result := 0;
end;

function TSilNamedValues.GetDataType(const Name: String): TNamedKeyDataType;
var
  iSize: Integer;
begin
  if not DoGetInfo(Name, Result, iSize) then Result := kdUnknown;
end;

function TSilNamedValues.ReadLargeInt(const Name: String; const Default: LargeInt; CanCreate: Boolean): LargeInt;
var
  DataType: TNamedKeyDataType;
  Buffer: String;
begin
  if DoRead(Name, DataType, Buffer) then
  begin
    case DataType of
      kdString,
      kdBinary:     Result := Str.ToLarge(Buffer, Default);
      kdInteger:    Result := PInteger(@Buffer[1])^;
      kdLargeInt:   Result := PLargeInt(@Buffer[1])^;
      else          Result := Default;
    end;
  end else
  begin
    Result := Default;
    if CanCreate then WriteLargeInt(Name, Default);
  end;
end;

function TSilNamedValues.ReadInteger(const Name: String; const Default: Integer; CanCreate: Boolean): Integer;
var
  DataType: TNamedKeyDataType;
  Buffer: String;
begin
  if DoRead(Name, DataType, Buffer) then
  begin
    case DataType of
      kdString,
      kdBinary:     Result := Str.ToInt(Buffer, Default);
      kdInteger:    Result := PInteger(@Buffer[1])^;
      kdLargeInt:   Result := PLargeInt(@Buffer[1])^;
      else          Result := Default;
    end;
  end else
  begin
    Result := Default;
    if CanCreate then WriteInteger(Name, Default);
  end;
end;

function TSilNamedValues.ReadString(const Name, Default: String; CanCreate: Boolean): String;
var
  DataType: TNamedKeyDataType;
  Buffer: String;
begin
  if DoRead(Name, DataType, Buffer) then
  begin
    case DataType of
      kdString,
      kdBinary:     Result := Str.Copy(Buffer, 1, -2);
      kdInteger:    Result := Int.ToStr(PInteger(@Buffer[1])^);
      kdLargeInt:   Result := Large.ToStr(PLargeInt(@Buffer[1])^);
      else          Result := Default;
    end;
  end else
  begin
    Result := Default;
    if CanCreate then WriteString(Name, Default);
  end;
end;

function TSilNamedValues.WriteInteger(const Name: String; Value: Integer): Boolean;
begin
  Result := DoWrite(Name, @Value, SizeOf(Integer), kdInteger);
end;

function TSilNamedValues.WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
begin
  Result := DoWrite(Name, @Value, SizeOf(LargeInt), kdLargeInt);
end;

function TSilNamedValues.WriteString(const Name, Value: String): Boolean;
begin
  Result := DoWrite(Name, PChar(Value), Length(Value) + 1, kdString);
end;

function TSilNamedValues.ReadBoolean(const Name: String; const Default: Boolean; CanCreate: Boolean): Boolean;
begin
  Result := ReadInteger(Name, Ord(Default), CanCreate) = 1;
end;

function TSilNamedValues.ReadFloat(const Name: String; const Default: Double; CanCreate: Boolean): Double;
var
  sVal: String;
begin
  sVal := Float.ToStr(Default);
  sVal := ReadString(Name, Str.Replace(sVal, DecimalSeparator, '.'), CanCreate);
  Result := Str.ToFloat(Str.Replace(sVal, '.', DecimalSeparator), 0);
end;

function TSilNamedValues.WriteBoolean(const Name: String; Value: Boolean): Boolean;
begin
  Result := WriteInteger(Name, Ord(Value));
end;

function TSilNamedValues.WriteFloat(const Name: String; Value: Double): Boolean;
var
  sVal: String;
begin
  sVal := Float.ToStr(Value);
  Result := WriteString(Name, Str.Replace(sVal, DecimalSeparator, '.'));
end;

function TSilNamedValues.ReadStrings(const Name: String; const Default: IStringList; CanCreate: Boolean): IStringList;
var
  DataType: TNamedKeyDataType;
  Buffer: String;
  BufPtr: PChar;
begin
  if DoRead(Name, DataType, Buffer) and (DataType = kdStrings) then
  begin
    Result := ListTool.StringList;
    BufPtr := @Buffer[1];
    while BufPtr^ <> #0 do
    begin
      Result.Add(BufPtr);
      BufPtr := BufPtr + Str.Len(BufPtr) + 1;
    end;
  end else
  begin
    Result := Default;
    if CanCreate and Assigned(Default) then
      WriteStrings(Name, Default);
  end;
end;

function TSilNamedValues.WriteStrings(const Name: String; const Value: IStringList): Boolean;
var
  E: IEnumerator;
  S: string;
  Size: Integer;
  Data: string;
begin
  Result := True;
  if Value.Count = 0 then Exit;
  Data := '';
  Size := 0;
  while Value.Enumerate(E, S) do
  begin
     Data := Data + S + #0;
     Inc(Size, Length(S) + 1);
  end;
  Data := Data + #0;
  Inc(Size);
  Result := DoWrite(Name, @Data[1], Size, kdStrings);
end;

function TSilNamedValues.Exists(const Name: String): Boolean;
var
  e: IEnumerator;
  Item: String;
begin
  while Enumerate(e, Item) do
  begin
    Result := Text.IsEqual(Item, Name);
    if Result then Exit;
  end;
  Result := false;
end;

end.
