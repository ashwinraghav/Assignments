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

unit SilOjRegistry;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiStringList,
  SilLiEnumerator,
  SilLiKey,
  SilOiVersion;

type
  SilRegistryTool = class(Tool)
    class function Open(
                     const Source: String;
                     CanCreate: Boolean = false): INamedKey; overload;

    class function Open(
                     const Source: String;
                     Permision: TNamedKeyPermision;
                     CanCreate: Boolean = false): INamedKey; overload; virtual; abstract;

    class function Open(
                     const Root: String;
                     const Version: IVersionNumber;
                           Permision: TNamedKeyPermision = kpReadWrite;
                           CanCreateRootKey: Boolean = true;
                           CanCreateSelector: Boolean = true;
                           CanCreateVersionKey: Boolean = true;
                           Selector: string = ''): INamedKey; overload;
    class function Select(
                     const Key: INamedKey;
                     const Selector: string;
                     const Default: string = '';
                           CanCreateSelector: Boolean = true;
                           CanCreateSubkey: Boolean = true): INamedKey;

    class function Choose(
                     const Key: INamedKey;
                     const Keys: array of string;
                     const Default: string;
                     CanCreateDefault: Boolean): INamedKey; overload;

    class function Choose(
                     const Key: INamedKey;
                     const Keys: array of string;
                     const Default: string): INamedKey; overload;

    class function Choose(
                     const Key: INamedKey;
                     const Keys: array of string): INamedKey; overload;

    class function Version(
                     const Key: INamedKey;
                     const Number: IVersionNumber = nil;
                           CanCreateSelector: Boolean = true;
                           CanCreateSubkey: Boolean = true;
                           Selector: string = ''): INamedKey;

    class function Read(const Key, Value: String): Variant;
    class procedure Delete(const Path: INamedKey; const Mask: String = ''; Recursive: Boolean = false); overload;
    class procedure Delete(const Path: INamedKey; Recursive: Boolean = false); overload;
    class function Delete(const KeyPath: String; Recursive: Boolean = false): Boolean; overload;

    class procedure Create(const KeyName, ValueName: string; const Value: string); overload;
    class procedure Create(const KeyName, ValueName: string; const Value: Integer); overload;
  end;

implementation

uses
  SilBtStr,
  SilBtError,
  SilLtList,
  SilOdRegistry,
  SilOtError,
  SilOtTool;

{ SilRegistryTool }

class function SilRegistryTool.Open(const Source: String; CanCreate: Boolean): INamedKey;
begin
  Result := Self.Open(Source, kpReadWrite, CanCreate);
end;

class function SilRegistryTool.Open(
  const Root: String;
  const Version: IVersionNumber;
        Permision: TNamedKeyPermision;
        CanCreateRootKey, CanCreateSelector, CanCreateVersionKey: Boolean;
        Selector: string): INamedKey;
begin
  Result := Self.Version(Open(Root, CanCreateRootKey), Version, CanCreateSelector, CanCreateVersionKey, Selector);
end;

class function SilRegistryTool.Select(const Key: INamedKey; const Selector, Default: string; CanCreateSelector: Boolean; CanCreateSubkey: Boolean): INamedKey;
begin
  Result := Key.Keys.Get(Key.Values.ReadString(Selector, Default, CanCreateSelector), CanCreateSubkey);
end;

class function SilRegistryTool.Choose(const Key: INamedKey; const Keys: array of string; const Default: string; CanCreateDefault: Boolean): INamedKey;
var
  I: Integer;
begin
  for I := Low(Keys) to High(Keys) do
    if Key.Keys.Exists(Keys[I]) then
    begin
      Result := Key.Keys.Get(Keys[I], False);
      Exit;
    end;
  if not Str.IsEmpty(Default) then
    Result := Key.Keys.Get(Default, CanCreateDefault) else
    Error.Throw(SCannotChooseSubkey);
end;

class function SilRegistryTool.Version(const Key: INamedKey; const Number: IVersionNumber; CanCreateSelector: Boolean; CanCreateSubkey: Boolean; Selector: string): INamedKey;

    function GetVersion(const Info: IVersionInfo): IVersionNumber;
    begin
      if (Number = nil) and (Info <> nil) then
        Result := Info.Number else
        Result := Number;
    end;
var                                        
  SubKey: string;
begin
  if Str.IsEmpty(Selector) then Selector := SCurrentVersion; 
  SubKey := OS.Version.ToStr(GetVersion(OS.Version.Info()), 'V%m.%0.2n');
  if not Key.Keys.Exists(SubKey) then
    SubKey := Key.Values.ReadString(Selector, SubKey, CanCreateSelector);
  Result := Key.Keys.Get(SubKey, CanCreateSubkey) 
end;

class function SilRegistryTool.Choose(const Key: INamedKey; const Keys: array of string; const Default: string): INamedKey;
begin
  Result := Self.Choose(Key, Keys, Default, false);
end;

class function SilRegistryTool.Choose(const Key: INamedKey; const Keys: array of string): INamedKey;
begin
  Result := Self.Choose(Key, Keys, '');
end;

class function SilRegistryTool.Read(const Key, Value: String): Variant;
var
  Root: INamedKey;
begin
  Root := Self.Open(Key);
  Result := Root.Values.ReadString(Value);
end;

class procedure SilRegistryTool.Delete(const Path: INamedKey; const Mask: String; Recursive: Boolean);
var
  List: IStringList;
  Enum: IEnumerator;
  Name: String;
begin
  List := ListTool.StringList;

  while Path.Keys.Enumerate(Enum, Name) do
  begin
    Delete(Path.Keys.Get(Name), Mask, Recursive);
    List.Add(Name);
  end;

  while List.Enumerate(Enum, Name) do
    Path.Keys.Remove(Name);

  List.Clear;

  while Path.Values.Enumerate(Enum, Name) do
    if Str.IsEmpty(Mask) or Str.WildCard(Name, Mask, true) then
      List.Add(Name);

  while List.Enumerate(Enum, Name) do
    Path.Values.Remove(Name);
end;

class procedure SilRegistryTool.Delete(const Path: INamedKey; Recursive: Boolean);
begin
  Delete(Path, '', Recursive);
end;

class function SilRegistryTool.Delete(const KeyPath: String; Recursive: Boolean): Boolean;
var
  ExPath, Mask: String;
  i: Integer;
begin
  try
    i := Str.LastPos('\', KeyPath);

    if Str.DelimiterPos('?*', KeyPath, i + 1, Length(KeyPath)) > 0 then
    begin
      Mask := Str.Copy(KeyPath, i + 1);
      ExPath := Str.Left(KeyPath, i - 1);
    end else
    begin
      Mask := '';
      ExPath := KeyPath;
    end;

    Delete(Open(ExPath), Mask, Recursive);

    Result := True;
  except
    Result := False;
  end;
end;

class procedure SilRegistryTool.Create(const KeyName, ValueName, Value: string);
var
  Key: INamedKey;
begin
  Key := Open(KeyName, True);
  if Str.NotEmpty(Value) then Key.Values.WriteString(ValueName, Value);
end;

class procedure SilRegistryTool.Create(const KeyName, ValueName: string; const Value: Integer);
var
  Key: INamedKey;
begin
  Key := Open(KeyName, True);
  Key.Values.WriteInteger(ValueName, Value);
end;

end.

