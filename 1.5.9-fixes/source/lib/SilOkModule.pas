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

unit SilOkModule;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilOiHandle,
  SilOiVersion,
  SilOiModule,
  SilOiFile,
  SilOsHandled,
  SilLmInterfaceList;

type
  TSilModule = class(
    TSilOsHandledObject,
    IModule)
  protected // IModule
    function GetName: string;
    function GetPath: string;
    function GetVersion: IVersionInfo;
    function GetFullName: string;
    function GetPathName: string;
  protected // IModule2
    function GetInfo: IFileInfo;
  protected
    function DoGetFullName: string; virtual; abstract;
  end;

  TSilModuleList = class(
    TSilInterfaceList,
    IModules,
    IModuleList )
  protected // IModules
    function Enumerate(var Enum: IEnumerator; out Item: IModule2): Boolean; reintroduce; virtual;  
  protected // IModuleList
    procedure Add(const Item: IModule2); reintroduce; virtual;   
  public
    constructor Create;
    destructor Destroy; override; 
  end;
  
implementation

uses
  SysUtils,
  SilOtTool;
  
{ TSilModule }

function TSilModule.GetFullName: string;
var
  Version: IVersionInfo;
begin
  Result := DoGetFullName();
  Version := GetVersion();
  if Version <> nil then
    Result := Result + ' ' + OS.Version.ToStr(Version.Number, CLongVersion);
end;

function TSilModule.GetInfo: IFileInfo;
begin
  Result := OS.FileSystem.GetInfo(DoGetFullName);
end;

function TSilModule.GetName: string;
begin
  Result := SysUtils.ExtractFileName(DoGetFullName());
end;

function TSilModule.GetPath: string;
begin
  Result := SysUtils.ExtractFilePath(DoGetFullName());
end;

function TSilModule.GetPathName: string;
begin
  Result := DoGetFullName();
end;

function TSilModule.GetVersion: IVersionInfo;
begin
  Result := OS.Version.Info(DoGetFullName());
end;

{ TSilModuleList }

constructor TSilModuleList.Create;
begin
  inherited Create;
end;

destructor TSilModuleList.Destroy;
begin
  inherited;
end;

procedure TSilModuleList.Add(const Item: IModule2);
begin
  inherited Add(Item);
end;

function TSilModuleList.Enumerate(var Enum: IEnumerator; out Item: IModule2): Boolean;
var
  Unk: IUnknown;
begin
  Result := inherited Enumerate(Enum, Unk);
  if Result and Assigned(Unk) then Item := Unk as IModule2;
end;

end.
