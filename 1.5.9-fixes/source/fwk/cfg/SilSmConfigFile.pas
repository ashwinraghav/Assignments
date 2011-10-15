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

unit SilSmConfigFile;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOiFile,
  SilOiTextFile,
  SilLiEnumerator,
  SilLiKey,
  SilLiStringList,
  SilLmStringList;

type
  IConfigLines = interface
    ['{99236B6E-DB65-445E-B935-0C9F83036A3C}']
    function GetIsChanged: Boolean;
    procedure SetIsChanged(Value: Boolean);
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
  end;

  TSilConfigFile = class (
    // extends
    TSilStringList,
    // implements
    IValueKeys,
    IConfigLines)
  private
    FFile: ITextFile;
    FIsChanged: Boolean;
  private
    procedure DoLoadFile(const ConfigFile: String; CanCreate: Boolean);
    procedure DoSaveFile;
  protected // INamedItems
    function Remove(const Name: String): Boolean;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; reintroduce;
  protected // IValueKeys
    function Add(const Name: String): INamedValues; reintroduce;
    function Exists(const Name: String): Boolean;
    function Find(const Name: String; out Values: INamedValues): Boolean;
    function Get(const Name: String; CanCreate: Boolean = false): INamedValues; reintroduce;
  protected // IConfigLines
    function GetIsChanged: Boolean;
    procedure SetIsChanged(Value: Boolean);
  public
    constructor Create(const ConfigFile: String; CanCreate: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilBtStr,
  SilBtLarge,
  SilLtList,
  SilBtInterfacePtr,
  SilBtStringPtr,
  SilBtInt,
  SilLtReference,
  SilBtFloat,
  SilLtSerializer,
  SilOtTool;

type
  TSection = class (
    // extends
    TSilStringList,
    // implements
    INamedValues)
  private
    FOwner: Pointer;
  private
    function DoGetOwner: IConfigLines;
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
    constructor Create(const Owner: IConfigLines);
    destructor Destroy; override;
  end;

{ TSilConfigFile }

constructor TSilConfigFile.Create(const ConfigFile: String; CanCreate: Boolean);
begin
  inherited Create(true, InterfaceHandler);

  SetIgnoreCase(true);
  DoLoadFile(ConfigFile, CanCreate);
end;

destructor TSilConfigFile.Destroy;
begin
  DoSaveFile;
  inherited;
end;

procedure TSilConfigFile.DoLoadFile(const ConfigFile: String; CanCreate: Boolean);
var
  Content, ActiveSection: IStringList;

  procedure DoCreateSection(const Line: String);
  var
    Item: String;
  begin
    ActiveSection := TSection.Create(Self);

    if not Str.Between(Line, '[', ']', Item) then Item := ';';
    inherited Add(Item, @ActiveSection);
  end;

  procedure DoAddComment(const Line: String);
  begin
    if Str.NotEmpty(Line) then
    begin
      if not Assigned(ActiveSection) then DoCreateSection(';');
      ActiveSection.Add(Line);
    end;
  end;

  procedure DoAddValue(const Line: String);
  var
    Name, Value: String;
  begin
    if Assigned(ActiveSection) and Str.NotEmpty(Line) then
    begin
      Str.Split(Line, '=', Name, Value);
      Value := Str.Trim(Value);

      if Str.NotEmpty(Name) then
        ActiveSection.Add(Str.Trim(Name), @Value);
    end;
  end;

var
  Enum: IEnumerator;
  Line: String;
begin
  FFile := OS.FileSystem.OpenTextFile(OS.FileSystem.ExpandPath(ConfigFile), fmAccessReadWrite, fmShareRead, not CanCreate);
  Content := ListTool.StringList;

  Serializer.LoadFromStream(Content, FFile.Stream);

  while Content.Enumerate(Enum, Line) do
  begin
    Line := Str.Trim(Line);

    case Str.ToChr(Line, ';') of
      '[':  DoCreateSection(Line);
      ';':  DoAddComment(Line);
      else  DoAddValue(Line);
    end;
  end;
end;

procedure TSilConfigFile.DoSaveFile;

  procedure DoSaveComments(const Section: IStringList);
  var
    Enum: IEnumerator;
    Item: String;
  begin
    while Section.Enumerate(Enum, Item) do
      FFile.Stream.WriteLn(Item);
  end;

  procedure DoSaveSection(const SectionName: String; const Section: IStringList);
  var
    Enum: IEnumerator;
    Item, Value: String;
  begin
    FFile.Stream.WriteLn(Str.Format('[%s]', [SectionName]));

    while Section.Enumerate(Enum, Item) do
      if Str.NotEmpty(Item) then
        if Str.ToChr(Item) <> ';' then
        begin
          Value := PString(Section.Ptrs[Enum.Iteration])^;
          FFile.Stream.WriteLn(Str.Format('%s=%s', [Item, Value]));
        end else
          FFile.Stream.WriteLn(Item);
  end;

var
  Enum: IEnumerator;
  SectionName: String;
  Section: IStringList;
begin
  if not FIsChanged then Exit;

  FFile.Stream.Position := 0;

  while inherited Enumerate(Enum, SectionName) do
    if Ref.GetInterface(IUnknown(GetPtr(Enum.Iteration)), IStringList, Section) then
    begin
      if SectionName = ';' then
        DoSaveComments(Section) else
        DoSaveSection(SectionName, Section);

      FFile.Stream.WriteLn;
    end;
end;

function TSilConfigFile.Add(const Name: String): INamedValues;
var
  Section: IStringList;
begin
  if IndexOf(Name) = -1 then
  begin
    Section := TSection.Create(Self);
    inherited Add(Name, @Section);
    Reference.GetInterface(Section, INamedValues, Result);
  end else
    Result := nil;
end;

function TSilConfigFile.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  repeat
    Result := inherited Enumerate(Enum, Name);
  until (not Result) or (Name <> ';');
end;

function TSilConfigFile.Exists(const Name: String): Boolean;
begin
  Result := IndexOf(Name) >= 0;
end;

function TSilConfigFile.Find(const Name: String; out Values: INamedValues): Boolean;
begin
  Values := Get(Name, false);
  Result := Assigned(Values);
end;

function TSilConfigFile.Get(const Name: String; CanCreate: Boolean): INamedValues;
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

function TSilConfigFile.Remove(const Name: String): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Name);
  Result := i >= 0;
  if Result then
  begin
    Delete(i);
    FIsChanged := true;
  end;
end;

function TSilConfigFile.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

procedure TSilConfigFile.SetIsChanged(Value: Boolean);
begin
  FIsChanged := Value;
end;

{ TSection }

constructor TSection.Create(const Owner: IConfigLines);
begin
  inherited Create(true, StringHandler);

  SetIgnoreCase(true);
  FOwner := Pointer(Owner);
end;

destructor TSection.Destroy;
begin
  FOwner := nil;
  inherited;
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
  sDef := Str.Replace(Float.ToStr(Default), DecimalSeparator, '.');
  Result := Str.ToFloat(Str.Replace(ReadString(Name, sDef, CanCreate), '.', DecimalSeparator));
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
    DoGetOwner.IsChanged := true;
  end;
end;

function TSection.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  repeat
    Result := inherited Enumerate(Enum, Name);
  until (not Result) or (Str.ToChr(Name) <> ';');
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
  DoGetOwner.IsChanged := Result;
end;

function TSection.WriteBoolean(const Name: String; Value: Boolean): Boolean;
begin
  Result := DoWrite(Name, Str.IIf(Value, '1', '0'));
end;

function TSection.WriteFloat(const Name: String; Value: Double): Boolean;
begin
  Result := DoWrite(Name, Str.Replace(Float.ToStr(Value), DecimalSeparator, '.'));
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

function TSection.DoGetOwner: IConfigLines;
begin
  Result := IConfigLines(FOwner);
end;

end.
 