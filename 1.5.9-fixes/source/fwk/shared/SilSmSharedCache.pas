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

unit SilSmSharedCache;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  TSilSharedCache = class(
    TSilObject,
    ISharedObjectCache )
  private
    FManager: Pointer;
    FClasses: INamedKey;
    FLibraries: INamedKey;
    FNames: INamedKey;
  private
    function DoGetRoot(const ID: ISharedID): INamedKey;
    function DoGetManager: ISharedObjectManager;
    function DoFindKey(const ID: ISharedID; out Key: INamedKey): Boolean;
    function DoAddFactory(const Provider: ISharedObjectProvider; const Factory: ISharedFactory): Boolean;
    function DoRemoveLibrary(const LibraryName: string): Boolean;
    function DoGetKeys(const Root: INamedKey; const ValueName, ValueData: string; out Keys: IStringList): Boolean;
    procedure DoRemoveKeys(const Root: INamedKey; const Keys: IStringList);
    procedure DoDeleteSubkeys(const Root: INamedKey; const KeyName: string);
  protected // ISharedObjectCache
    function Lookup(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean; overload;
    function Lookup(const LibraryName: string; out Provider: ISharedObjectProvider): Boolean; overload;
    function Add(const Provider: ISharedObjectProvider; const ID: ISharedID = nil): Boolean;
    function Remove(const Provider: ISharedObjectProvider): Boolean; overload;
    function Remove(const ID: ISharedID): Boolean; overload;
  public
    constructor Create(const Manager: ISharedObjectManager; const Key: INamedKey);
    destructor Destroy; override;
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

uses
  SilScSharedDefaults, SilSmSharedID;


function DateStamp(Value: TDateTime): string;
begin
  Result := Sil.DateTime.ToStr(Value, 'yyyy.dd.mm.hh.nn.ss');
end;

{ TSilSharedCache }

constructor TSilSharedCache.Create(const Manager: ISharedObjectManager; const Key: INamedKey);
begin
  inherited Create;
  FManager := Pointer(Manager);
  FClasses := Key.Keys.Get(CSharedClassKey, True);
  FLibraries := Key.Keys.Get(CSharedLibraryKey, True);
  FNames := Key.Keys.Get(CSharedNamesKey, True);
end;

destructor TSilSharedCache.Destroy;
begin
  FClasses := nil;
  FLibraries := nil;
  FNames := nil;
  FManager := nil;
  inherited;
end;

function TSilSharedCache.Lookup(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
var
  Key: INamedKey;
begin
  Result := DoFindKey(ID, Key);
  if Result then
  begin
    case ID.Kind of
      idName:
          Result := Key.Values.Exists(CSharedClassKey)
                and Lookup(TSilSharedID.Create(Sil.Guid.FromStr(Key.Values.ReadString(CSharedClassKey))), Provider);
      idClass:
          Result := Key.Values.Exists(CSharedLibraryKey)
                and Lookup(Key.Values.ReadString(CSharedLibraryKey), Provider);
      else
          Result := False;
    end;

    if not Result then
      Remove(ID);
  end;
end;

function TSilSharedCache.Lookup(const LibraryName: string; out Provider: ISharedObjectProvider): Boolean;
var
  KeyName: string;
  CurrTime, PrevTime: string;
begin
  KeyName := Sil.Os.Filesystem.GetFileName(LibraryName);
  Result := FLibraries.Keys.Exists(KeyName);
  if Result then
  try
    with FLibraries.Keys.Get(KeyName) do
    begin
      Provider := Manager.Loader.Get(Values.ReadString(CSharedLibraryKey));

      CurrTime := DateStamp(Provider.Module.Info.Time);
      PrevTime := Values.ReadString(CSharedLibraryTimestamp);
       
      if Str.IsEmpty(PrevTime) or (Sil.Text.Compare(CurrTime, PrevTime) <> 0) then
        Add(Provider);

    end;
  except
    Sil.Trace.Exception('LOADING ' + LibraryName, 'TSilSharedCache.Lookup', 'SharedObject');
    DoRemoveLibrary(KeyName);
    Result := False;
  end;
end;

function TSilSharedCache.Add(const Provider: ISharedObjectProvider; const ID: ISharedID): Boolean;
var
  Enum: IEnumerator;
  Item: ISharedFactory;
begin
  Result := False;
  try

    with Provider do
      while Enumerate(Enum, Item) do
        Result := DoAddFactory(Provider, Item);

    if not Result and Assigned(ID) and Provider.Find(ID, Item) then
      Result := DoAddFactory(Provider, Item);
      
  except
    Sil.Trace.Exception('REGISTERING ' + Provider.Module.Info.Name, 'TSilSharedCache.Add', 'SharedObject');
    Result := False;
  end;
end;

function TSilSharedCache.Remove(const Provider: ISharedObjectProvider): Boolean;
begin
  { TODO : !falta implementar! }
  Result := True;
end;

function TSilSharedCache.Remove(const ID: ISharedID): Boolean;
begin
  { TODO : !falta implementar! }
  Result := True;
end;

function TSilSharedCache.DoAddFactory(const Provider: ISharedObjectProvider; const Factory: ISharedFactory): Boolean;
var
  Key: INamedKey;
begin
  try
    Key := FLibraries.Keys.Get(Provider.Module.Info.Name, True);
    Key.Values.WriteString(CSharedLibraryKey, Provider.Module.Info.FullName);
    Key.Values.WriteString(CSharedLibraryVersion, Provider.Module.Info.Version.Number.ToStr(CLongVersion));
    Key.Values.WriteString(CSharedLibraryTimestamp, DateStamp(Provider.Module.Info.Time));

    Key := FClasses.Keys.Get(Sil.GUID.ToStr(Factory.ClassID), True);
    Key.Values.WriteString(CSharedLibraryKey, Provider.Module.Info.Name);
    if Sil.Str.IsAssigned(Factory.ProgID) then
    begin
      Key.Values.WriteString(CSharedNamesKey, Factory.ProgID);

      Key := FNames.Keys.Get(Factory.ProgID, True);
      Key.Values.WriteString(CSharedLibraryKey, Provider.Module.Info.Name);
      Key.Values.WriteString(CSharedClassKey, Sil.GUID.ToStr(Factory.ClassID));
    end;
    Result := True;
  except
    Sil.Trace.Exception(Provider.Module.Info.Name, 'DoAddFactory', 'SharedObject');
    Result := False;
  end;
end;

function TSilSharedCache.DoRemoveLibrary(const LibraryName: string): Boolean;
var
  Keys: IStringList;
begin
  try
    try
      if DoGetKeys(FNames, CSharedLibraryKey, LibraryName, Keys) then
        DoRemoveKeys(FNames, Keys);
      if DoGetKeys(FClasses, CSharedLibraryKey, LibraryName, Keys) then
        DoRemoveKeys(FClasses, Keys);
    finally
      DoDeleteSubkeys(FLibraries, LibraryName);
      FLibraries.Keys.Remove(LibraryName);
    end;
    Result := True;
  except
    Sil.Trace.Exception('REMOVING ' + LibraryName, 'DoRemoveLibrary', 'SharedObject');
    Result := False;
  end;
end;

function TSilSharedCache.DoGetKeys(const Root: INamedKey; const ValueName, ValueData: string; out Keys: IStringList): Boolean;
var
  Enum: IEnumerator;
  Item: string;
  Key: INamedKey;
begin
  with Root.Keys do
    while Enumerate(Enum, Item) do
    begin
      Key := Get(Item);
      try
        if Key.Values.Exists(ValueName) and (Sil.Text.Compare(Key.Values.ReadString(ValueName), ValueData) = 0) then
        begin
          if not Assigned(Keys) then Keys := Sil.List.StringList();
          Keys.Add(Item);
        end;
      finally
        Key := nil;
      end;
    end;
  Result := Assigned(Keys) and (Keys.Count > 0);
end;

procedure TSilSharedCache.DoRemoveKeys(const Root: INamedKey; const Keys: IStringList);
var
  Enum: IEnumerator;
  Item: string;
begin
  with Keys do
    while Enumerate(Enum, Item) do
    try
      DoDeleteSubkeys(Root, Item);
    finally
      Root.Keys.Remove(Item);
    end;
end;

procedure TSilSharedCache.DoDeleteSubkeys(const Root: INamedKey; const KeyName: string);
var
  Enum: IEnumerator;
  Item: string;
  Key: INamedKey;
  List: IStringList;
begin
  Key := Root.Keys.Get(KeyName);
  try
    with Key.Keys do
      while Enumerate(Enum, Item) do
      begin
        if not Assigned(List) then List := Sil.List.StringList();
        List.Add(Item);
      end;

    if Assigned(List) then
      DoRemoveKeys(Key, List);
          
  finally
    Key := nil;
  end;
end;

function TSilSharedCache.DoFindKey(const ID: ISharedID; out Key: INamedKey): Boolean;
var
  Root: INamedKey;
begin
  Root := DoGetRoot(ID);

  Result := Assigned(Root) and Root.Keys.Exists(ID.Value);

  if Result then
    Key := Root.Keys.Get(ID.Value);
end;

function TSilSharedCache.DoGetRoot(const ID: ISharedID): INamedKey;
begin
  case ID.Kind of
    idName:   Result := FNames;
    idClass:  Result := FClasses;
    else      Result := nil;
  end;
end;

function TSilSharedCache.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

end.
