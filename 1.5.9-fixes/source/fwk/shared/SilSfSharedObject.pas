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

unit SilSfSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject;

function DoFindModule(
    const ClassID: TGuid;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoFindModule(
    const ProgID: string;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoGetContainer(
    const Path: String;
    const FileName: String;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoSearchModule(
    const FileName: String;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoSearchModule(
    const Path: String;
    const ClassID: TGuid;
      out Container: ISharedObjectReferences): Boolean; overload;
    
function DoSearchModule(
    const Path: String;
    const Mask: String;
    const ClassID: TGuid;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoLoadModule(
    const FileName: string;
    const ClassID: TGuid;
      out Container: ISharedObjectReferences): Boolean;

procedure DoRegisterModule(
    const Item: IFileInfo;
    const ClassID: TGuid;
    const Container: ISharedObjectReferences);

function DoFindContainer(
    const FileName: String;
      out Container: ISharedObjectReferences): Boolean; overload;
      
function DoFindContainer(
    const ClassID: TGuid;
      out Container: ISharedObjectReferences): Boolean; overload;

function DoLoadContainer(
    const FileName: string;
      out Container: ISharedObjectReferences): Boolean;
      
function DoFindLibrary(
    const Obj: IUnknown;
      out Lib: ISharedLibrary): Boolean;

function DoCreateTool(
    const Container: ISharedObjectReferences): TClass;

function DoCreateObject(
    const ClassID: TGuid;
    const Container: ISharedObjectReferences;
    const Owner, Controller: IUnknown): IUnknown; overload; 

function DoCreateObject(
    const ProgID: string;
    const Container: ISharedObjectReferences;
    const Owner, Controller: IUnknown): IUnknown; overload;

implementation

uses
  SilShSharedManager,
  SilSfSharedManager,
  SilStSharedObject,
  SilSmSharedReferences;

function DoFindModule(const ClassID: TGuid; out Container: ISharedObjectReferences): Boolean;
    (*)
var
  Pos: Integer;
  Item: string;
    (*)
begin
  Manager.Locked;
  
  Result := DoFindContainer(ClassID, Container);

  if not Result then
  begin
    (*)
    if not Result then
    begin
      while not Result and Sil.Str.Enumerate(MPath, ';', Item, Pos) do
      begin
        Item := Sil.Os.Filesystem.AddSlash(Sil.Os.Environment.Expand(Item));

        Result := DoSearchModule(Item, ClassID, Container);
      end;
    end;
    (*)

  end;
end;

function DoSearchModule(const Path: String; const ClassID: TGuid; out Container: ISharedObjectReferences): Boolean;
    (*)
var
  Pos: Integer;
  Item: string;
    (*)
begin
    (*)
  Result := False;
  Pos := 0;

  while not Result and Sil.Str.Enumerate(MMask, ';', Item, Pos) do
    Result := DoSearchModule(Path, Sil.Os.Environment.Expand(Item), ClassID, Container);
    (*)
  Result := False;    
end;

function DoSearchModule(const FileName: String; out Container: ISharedObjectReferences): Boolean;
    (*)
var
  Pos: Integer;
  Path, Item: string;
    (*)
begin
  Manager.Locked;
  
  Result := False;

  if Sil.Os.FileSystem.IsRelative(FileName) then
  begin

    (*)
    Pos := 0;
    Path := Sil.Os.Environment.Expand(MPath);

    while not Result and Sil.Str.Enumerate(Path, ';', Item, Pos) do
    begin
      Item := Sil.Os.Filesystem.AddSlash(Sil.Os.Environment.Expand(Item));

      Result := DoGetContainer(Path, FileName, Container);
    end;

    (*)
  end else
    Result := DoLoadContainer(FileName, Container);

end;

function DoFindModule(const ProgID: string; out Container: ISharedObjectReferences): Boolean;
begin
  Result := False;  //!
end;

function DoGetContainer(const Path: String; const FileName: String; out Container: ISharedObjectReferences): Boolean;
var
  FullName: String;
begin
  if Str.NotEmpty(Path) and Sil.Os.FileSystem.IsRelative(FileName) then
    FullName := Sil.Os.FileSystem.AddSlash(Path) + FileName else
    FullName := FileName;

  Result := DoFindContainer(FullName, Container);
  if not Result then
    Result := DoLoadContainer(FullName, Container);
end;

function DoLoadModule(const FileName: string; const ClassID: TGuid; out Container: ISharedObjectReferences): Boolean;
(*)var
  Module: ISharedObjectFunctions;(*)
begin
(*)  Result := TSilObjectModule.Create(FileName, Module) and (Guid.IsEmpty(ClassID) or Module.ClassQuery(ClassID));
  if Result then
    Container := TSilObjectReferences.Create(Module);(*)
  Result := False;
end;

procedure DoRegisterModule(const Item: IFileInfo; const ClassID: TGuid; const Container: ISharedObjectReferences);
    (*)
var
  Key: INamedKey;
    (*)
begin
    (*)
  if Assigned(MClasses) then
  begin
    Key := MClasses.Keys.Get(Sil.Guid.ToStr(ClassID), True);

    Key.Values.WriteString(CDefault, Item.Version.FullName);
    Key.Values.WriteString(CLibrary, Item.FullName);
    Key.Values.WriteString(CSignature, Container.Module.Signature);
  end;

  if Assigned(MLibraries) then
  begin
    Key := MLibraries.Keys.Get(Item.Name, True);

    Key.Values.WriteString(CClass, Guid.ToStr(ClassID));
    Key.Values.WriteString(CLibrary, Item.FullName);
  end;
    (*)
end;

function DoSearchModule(const Path: String; const Mask: String; const ClassID: TGuid; out Container: ISharedObjectReferences): Boolean;
var
  Enum: IEnumerator;
  Item: IFileInfo;
  Module: ISharedObjectFunctions;
begin
  Result := False;

  with OS.FileSystem.GetList(Path + Mask) do
    while not Result and Enumerate(Enum, Item) do
    try
      Result := DoLoadModule(Item.FullName, ClassID, Container);
      
      if Result then
        DoRegisterModule(Item, ClassID, Container);
      
    except
      Container := nil;
      Module := nil;
    end;
end;

function DoFindContainer(const FileName: String; out Container: ISharedObjectReferences): Boolean;
(*)var
  Enum: IEnumerator;(*)
begin
(*)  Result := false;
  with List do
    while not Result and Enumerate(Enum, Container) do
      Result := Str.CompareText(Container.Module.Dll.FileName, FileName) = 0;(*)
  Result := False;
end;

function DoFindContainer(const ClassID: TGuid; out Container: ISharedObjectReferences): Boolean;
(*)var
  Enum: IEnumerator;(*)
begin
(*)  Result := False;
  with List do
    while not Result and Enumerate(Enum, Container) do
      Result := Container.Module.ClassQuery(ClassID);(*)
  Result := False;
end;

function DoLoadContainer(const FileName: string; out Container: ISharedObjectReferences): Boolean;
(*)var
  Module: ISharedObjectFunctions;(*)
begin
(*)  if TSilObjectModule.Create(FileName, Module) then
  begin
    Container := TSilObjectReferences.Create(Module);
    Result := True;
  end else
    Result := False;(*)
  Result := False;
end;

function DoFindLibrary(const Obj: IUnknown; out Lib: ISharedLibrary): Boolean;
(*)var
  Enum: IEnumerator;
  Item: ISharedObjectReferences;
  Ptr: Pointer;(*)
begin
(*)  Ptr := Pointer(Obj as IUnknown);

  with List do
    while Enumerate(Enum, Item) do
      if Item.Contains(Ptr) then
      begin
        Lib := Item.Module.Dll;
        Result := true;
        Exit;
      end;

  Result := false;(*)
  Result := False;
end;

function DoCreateTool(const Container: ISharedObjectReferences): TClass;
begin
(*)  Result := Container.Module.GetTool();
  Container.AddRef(Pointer(Result));(*)
  Result := nil;
end;

function DoCreateObject(const ClassID: TGuid; const Container: ISharedObjectReferences; const Owner, Controller: IUnknown): IUnknown;
(*)var
  Instance: IUnknown;(*)
begin
(*)  Instance := Container.Module.CreateObject(ClassID, Owner, Controller);
  try
    Result := Instance as IUnknown;
    Container.AddRef(Pointer(Result));
  finally
    Instance := nil;
  end;(*)
end;

function DoCreateObject(const ProgID: string; const Container: ISharedObjectReferences; const Owner, Controller: IUnknown): IUnknown;
begin
//!
end;

end.
