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

unit SilSmConfiguration;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiEnumerator,
  SilLiKey,
  SilLiStringList,
  SilLmStringList;

type
  TConfiguration = class;

  TSection = class (
    // extends
    TSilStringList,
    // implements
    INamedValues)
  private
    FOwner: TConfiguration;
  private
    function DoWrite(const Name, Value: String): Boolean;
  protected // INamedItems
    function Remove(const Name: String): Boolean; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; reintroduce;
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
  public
    constructor CreateOwned(Owner: TConfiguration);
  end;

  TConfiguration = class (
    // extends
    TSilStringList,
    // implements
    IValueKeys)
  private
    FSource: String;
    FCanCreate: Boolean;
    FChanged: Boolean;
    procedure DoInit;
  protected // INamedItems
    function Remove(const Name: String): Boolean;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; reintroduce;
  protected // IValueKeys
    function Add(const Name: String): INamedValues; reintroduce;
    function Exists(const Name: String): Boolean;
    function Find(const Name: String; out Values: INamedValues): Boolean;
    function Get(const Name: String; CanCreate: Boolean = false): INamedValues; reintroduce;
  public
    constructor Create(const Source: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilBtLarge,
  SilBtInterfacePtr,
  SilOtTool,
  SilOiTextFile,
  SilOiFile,
  SilLtTool,
  SilLtList,
  SilLtSerializer,
  SilBtStr,
  SilLtReference,
  SilBtStringPtr,
  SilBtFloat,
  SilBtInt;

{ TConfiguration }

constructor TConfiguration.Create(const Source: String; CanCreate: Boolean);
begin
  inherited Create(true, InterfaceHandler);

  FSource := OS.FileSystem.ExpandPath(Source);
  FCanCreate := CanCreate;

  DoInit;
end;

destructor TConfiguration.Destroy;
var
  FileDst: ITextFile;
  e1, e2: IEnumerator;
  Section, Line: String;
  Lines: IStringList;
begin
  if FChanged then
  begin
    FileDst := OS.FileSystem.CreateTextFile(FSource);

    while Enumerate(e1, Section) do
    begin
      FileDst.Stream.WriteLn('[' + Section + ']');
      Lines := IStringList(GetPtr(e1.Iteration));

      while Lines.Enumerate(e2, Line) do
        FileDst.Stream.WriteLn(Line + '=' + PString(Lines.Ptrs[e2.Iteration])^);

      FileDst.Stream.WriteLn;
    end;
  end;

  inherited;
end;

procedure TConfiguration.DoInit;
var
  FileSource: IFile;
  i: IEnumerator;
  sLine, sItem: String;
  iPos: Integer;
  Lines, Section: IStringList;
begin
  inherited;

  FileSource := OS.FileSystem.OpenFile(FSource, fmAccessReadWrite, fmShareRead, not FCanCreate);
  Lines := ListTool.StringList;
  Serializer.LoadFromStream(Lines, FileSource.Stream);

  while Lines.Enumerate(i, sLine) do
    if (Str.FirstChar(sLine) = '[') and Str.GetPos(']', sLine, 1, iPos) then
    begin
      Section := TSection.CreateOwned(Self);
      inherited Add(Str.Copy(sLine, 2, iPos - 2), @Section);
    end else
    if (Section <> nil) and Str.GetPos('=', sLine, 1, iPos) then
    begin
      sItem := Str.Copy(sLine, iPos + 1);
      Section.Add(Str.TrimLeft(Str.Copy(sLine, 1, iPos - 1)), @sItem);
    end;
end;

function TConfiguration.Add(const Name: String): INamedValues;
var
  Section: IStringList;
begin
  if IndexOf(Name) = -1 then
  begin
    Section := TSection.CreateOwned(Self);
    inherited Add(Name, @Section);
    Reference.GetInterface(Section, INamedValues, Result);
  end else
    Result := nil;
end;

function TConfiguration.Exists(const Name: String): Boolean;
begin
  Result := IndexOf(Name) >= 0;
end;

function TConfiguration.Find(const Name: String; out Values: INamedValues): Boolean;
begin
  Result := false;
end;

function TConfiguration.Get(const Name: String; CanCreate: Boolean): INamedValues;
var
  iIdx: Integer;
begin
  iIdx := IndexOf(Name);

  if iIdx < 0 then
  begin
    if CanCreate then
      Result := Add(Name) else
      Result := nil;
  end else
    Reference.GetInterface(IStringList(GetPtr(iIdx)), INamedValues, Result);
end;

function TConfiguration.Remove(const Name: String): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Name);
  Result := i >= 0;
  if Result then
  begin
    Delete(i);
    FChanged := true;
  end;
end;

function TConfiguration.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := inherited Enumerate(Enum, Name);
end;

{ TSection }

constructor TSection.CreateOwned(Owner: TConfiguration);
begin
  inherited Create(true, StringHandler);
  FOwner := Owner;
end;

function TSection.GetDataSize(const Name: String): LongWord;
begin
  Result := 0;
end;

function TSection.GetDataType(const Name: String): TNamedKeyDataType;
begin
  Result := kdUnknown;
end;

function TSection.ReadBoolean(const Name: String; const Default: Boolean; CanCreate: Boolean): Boolean;
begin
  Result := ReadString(Name, Str.IIf(Default, '1', '0'), CanCreate) = '1';
end;

function TSection.ReadFloat(const Name: String; const Default: Double; CanCreate: Boolean): Double;
var
  sDef: String;
begin
  sDef := Str.Translate(Float.ToStr(Default), DecimalSeparator, '.');
  Result := Str.ToFloat(Str.Translate(ReadString(Name, sDef, CanCreate), '.', DecimalSeparator));
end;

function TSection.ReadInteger(const Name: String; const Default: Integer; CanCreate: Boolean): Integer;
begin
  Result := Str.ToInt(ReadString(Name, Int.ToStr(Default), CanCreate));
end;

function TSection.ReadLargeInt(const Name: String; const Default: LargeInt; CanCreate: Boolean): LargeInt;
begin
  Result := Str.ToLarge(ReadString(Name, Large.ToStr(Default), CanCreate));
end;

function TSection.ReadString(const Name, Default: String; CanCreate: Boolean): String;
var
  i: Integer;
begin
  i := IndexOf(Name);

  if i < 0 then
  begin
    Result := Default;
    if CanCreate then DoWrite(Name, Result);
  end else
    Result := PString(GetPtr(i))^;
end;

function TSection.ReadStrings(const Name: String; const Default: IStringList; CanCreate: Boolean): IStringList;
begin
  Result := nil;
end;

function TSection.Remove(const Name: String): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Name);
  Result := i >= 0;
  if Result then
  begin
    Delete(i);
    FOwner.FChanged := true;
  end;
end;

function TSection.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := inherited Enumerate(Enum, Name);
end;

function TSection.DoWrite(const Name, Value: String): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Name);
  if i < 0 then
  begin
    Add(Name, @Value);
  end else
    PString(GetPtr(i))^ := Value;

  Result := true;
  FOwner.FChanged := Result;
end;

function TSection.WriteBoolean(const Name: String; Value: Boolean): Boolean;
begin
  Result := DoWrite(Name, Str.IIf(Value, '1', '0'));
end;

function TSection.WriteFloat(const Name: String; Value: Double): Boolean;
begin
  Result := DoWrite(Name, Str.Translate(Float.ToStr(Value), DecimalSeparator, '.'));
end;

function TSection.WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
begin
  Result := DoWrite(Name, Large.ToStr(Value));
end;

function TSection.WriteInteger(const Name: String; Value: Integer): Boolean;
begin
  Result := DoWrite(Name, Int.ToStr(Value));
end;

function TSection.WriteString(const Name, Value: String): Boolean;
begin
  Result := DoWrite(Name, Value);
end;

function TSection.WriteStrings(const Name: String; const Value: IStringList): Boolean;
begin
  Result := false;
end;

function TSection.Exists(const Name: String): Boolean;
begin
  Result := IndexOf(Name) >= 0;
end;

end.
