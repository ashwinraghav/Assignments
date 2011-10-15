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

unit SilSmSharedLoader;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  ESilLoaderError = class(ESilSharedObjectError);

  TSilSharedLoader = class(
    TSilObject,
    ISharedObjectLoader )
  private
    FManager: Pointer;
  private
    function DoGetManager: ISharedObjectManager;
    function DoSearch(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
    function DoSearchPath(const Path: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
    function DoSearchMasks(const Path: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
    function DoSearchMask(const Path, Mask: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
    function DoCheck(const FileName: string; const ID: ISharedID; out Item: ISharedObjectProvider): Boolean;
    function DoLoad(const FileName: string; out Provider: ISharedObjectProvider): Boolean;
    function DoGet(const FileName: string): ISharedObjectProvider;
  protected // ISharedObjectLoader
    function Get(const FileName: string): ISharedObjectProvider; overload;
    function Get(const ID: ISharedID): ISharedObjectProvider; overload;
    function Search(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
  public
    constructor Create(const Manager: ISharedObjectManager);
    destructor Destroy; override;
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

uses
  SilSmSharedProvider,
  SilSmSharedID;

{ TSilSharedLoader }

constructor TSilSharedLoader.Create(const Manager: ISharedObjectManager);
begin
  inherited Create;
  FManager := Pointer(Manager);
end;

destructor TSilSharedLoader.Destroy;
begin
  FManager := nil;
  inherited;
end;

function TSilSharedLoader.Get(const FileName: string): ISharedObjectProvider;
begin
  if not Sil.Os.FileSystem.IsRelative(FileName) then
    Result := DoGet(FileName) else
    Result := Get(TSilSharedID.Create(idModule, FileName));
end;

function TSilSharedLoader.Get(const ID: ISharedID): ISharedObjectProvider;
begin
  if not Search(ID, Result) then
    raise Sil.Error.Create('NO ENCUENTRO UN MODULO QUE CONTENGA LA CLASE SOLICITADA', ESilLoaderError); { TODO : @!@ }
end;

function TSilSharedLoader.Search(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
begin
  if (ID.Kind = idModule) and not Sil.Os.Filesystem.IsRelative(ID.AsModule.ModuleID) then
    Result := DoLoad(ID.AsModule.ModuleID, Provider) else
    Result := DoSearch(ID, Provider);
end;

function TSilSharedLoader.DoSearch(const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
var
  Enum: IEnumerator;
  Path: string;
begin
  Result := False;
  with Manager.Config.Path do
    while not Result and Enumerate(Enum, Path) do
      if Sil.Os.Filesystem.DirectoryExists(Path) then
        Result := DoSearchPath(Path, ID, Provider);
end;

function TSilSharedLoader.DoSearchPath(const Path: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
var
  Mask: string;
begin
  Mask := Sil.Os.Filesystem.GetFileName(Path);
  if Sil.Str.IsEmpty(Mask) or not Sil.Str.IsWildCard(Mask) then
    Result := DoSearchMasks(Path, ID, Provider) else
    Result := DoSearchMask(Sil.Os.Filesystem.GetFilePath(Path), Mask, ID, Provider);
end;

function TSilSharedLoader.DoSearchMasks(const Path: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
var
  Enum: IEnumerator;
  Mask: string;
begin
  Result := False;
  with Manager.Config.Mask do
    while not Result and Enumerate(Enum, Mask) do
      Result := DoSearchMask(Path, Mask, ID, Provider);
end;

function TSilSharedLoader.DoSearchMask(const Path, Mask: string; const ID: ISharedID; out Provider: ISharedObjectProvider): Boolean;
var
  List: IFileInfoList;
  Enum: IEnumerator;
  Info: IFileInfo;
  Item: ISharedObjectProvider;
begin
  Result := False;
  List := Sil.Os.Filesystem.GetList(Sil.Os.Filesystem.AddSlash(Sil.Os.Environment.Expand(Path)) + Mask);
  with List do
    while not Result and Enumerate(Enum, Info) do
      if DoCheck(Info.FullName, ID, Item) then
      begin
        Manager.Cache.Add(Item, ID);
        Provider := Item;
        Result := True;
      end;
end;

function TSilSharedLoader.DoCheck(const FileName: string; const ID: ISharedID; out Item: ISharedObjectProvider): Boolean;
begin
  try
    Result := DoLoad(FileName, Item);
    if Result then
    begin
      Manager.Cache.Add(Item);
      Result := Item.Provides(ID);
    end;
  except
    Sil.Trace.Exception('LOADING ' + FileName, 'TSilSharedLoader.DoCheck', 'SharedObject');
    Item := nil;
    Result := False;
  end;
end;

function TSilSharedLoader.DoLoad(const FileName: string; out Provider: ISharedObjectProvider): Boolean;
begin
  Result := TSilObjectProvider.Load(FileName, Provider);
end;

function TSilSharedLoader.DoGet(const FileName: string): ISharedObjectProvider;
begin
  if not DoLoad(FileName, Result) then
    raise Sil.Error.Create('EL MODULO INDICADO NO ES COMPATIBLE CON ESTE RUNTIME', ESilLoaderError); { TODO : @!@ }
end;

function TSilSharedLoader.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

end.
