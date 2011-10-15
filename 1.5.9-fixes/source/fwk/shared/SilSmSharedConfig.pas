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

unit SilSmSharedConfig;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilShSharedManager;

type
  TSilSharedConfig = class(
    TSilObject,
    ISharedObjectConfig )
  private
    FManager: Pointer;
    FPath: IStringList;
    FMask: IStringList;
  private
    function DoGetManager: ISharedObjectManager;
  protected // ISharedObjectConfig
    procedure AddPath(const Value: string);
    procedure AddMask(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetMask(const Value: string);
    function GetPath: IStrings;
    function GetMask: IStrings;
  public
    constructor Create(const Manager: ISharedObjectManager; const Key: INamedKey);
    destructor Destroy; override;
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation              

uses
  SilScSharedDefaults;

{ TSilSharedConfig }

constructor TSilSharedConfig.Create(const Manager: ISharedObjectManager; const Key: INamedKey);
begin
  inherited Create;
  FManager := Pointer(Manager);
  FPath := Sil.List.StringList(CSharedDefaultPath, CPathListSeparator);
  FMask := Sil.List.StringList(CSharedDefaultMask, CPathListSeparator);
  SetPath(Key.Values.ReadString('OBJECT_PATH', FPath.Text, True));
  SetMask(Key.Values.ReadString('OBJECT_MASK', FMask.Text, True));
end;

destructor TSilSharedConfig.Destroy;
begin
  FMask := nil;
  FPath := nil;
  inherited;
end;

function TSilSharedConfig.GetMask: IStrings;
begin
  Result := FMask;
end;

function TSilSharedConfig.GetPath: IStrings;
begin
  Result := FPath;
end;

procedure TSilSharedConfig.AddMask(const Value: string);
begin
  if Sil.Str.NotEmpty(Value) and (FMask.IndexOf(Value) = -1) then
    FMask.AddText(Value);
end;

procedure TSilSharedConfig.AddPath(const Value: string);
begin
  if Sil.Str.NotEmpty(Value) and (FPath.IndexOf(Value) = -1) then
    FPath.AddText(Value);
end;

procedure TSilSharedConfig.SetMask(const Value: string);
var
  Pos: Integer;
  Item: String;
begin
  Pos := 0;
  FMask.Clear;
  while Sil.Str.Enumerate(Value, CPathListSeparator, Item, Pos) do
    AddMask(Item);
end;

procedure TSilSharedConfig.SetPath(const Value: string);
var
  Pos: Integer;
  Item: String;
begin
  FPath.Clear;
  Pos := 0;
  while Sil.Str.Enumerate(Value, CPathListSeparator, Item, Pos) do
    AddPath(Sil.Os.Environment.Expand(Item));
end;

function TSilSharedConfig.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

end.
