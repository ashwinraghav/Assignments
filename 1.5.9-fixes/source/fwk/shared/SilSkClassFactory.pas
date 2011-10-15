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

unit SilSkClassFactory;

{$I Defines.inc}

interface

uses
  SilSiSharedObject,
  SilSkSharedObject,
  SilSkSharedFactory;

type
  TSilClassFactory = class(
    TSilSharedFactory,
    ISharedClassFactory )
  private
    FTool: ClassFactoryType;
    FClassType: TClass;
  private
    function DoCheckProgID(const ProgID: string): string;
  protected 
    function CreateObject(const Owner: IUnknown; const IID: TGuid; out Instance; const Controller: IUnknown = nil; Param: Pointer = nil): Boolean; override;      
  protected // ISharedClassFactory
    function GetClassType: TClass;
  public
    constructor Create(Tool: ClassFactoryType; ClassType: TClass; const ClassID: TGUID; const ProgID: string);
    destructor Destroy; override;
  end;

implementation

uses
  Sil;

{ TSilClassFactory }

constructor TSilClassFactory.Create(Tool: ClassFactoryType; ClassType: TClass; const ClassID: TGUID; const ProgID: string);
begin
  inherited Create(ClassID, DoCheckProgID(ProgID));
  FTool := Tool;
  FClassType := ClassType;
end;

destructor TSilClassFactory.Destroy;
begin
  FTool := nil;
  FClassType := nil;
  inherited;
end;

function TSilClassFactory.CreateObject(const Owner: IInterface; const IID: TGuid; out Instance; const Controller: IInterface; Param: Pointer): Boolean;
var
  Unknown: IUnknown;
begin
  Unknown := FTool.Create(Self, FClassType, Owner, Controller, Param);
  Result := Unknown.QueryInterface(IID, Instance) = 0;
end;

function TSilClassFactory.GetClassType: TClass;
begin
  Result := FClassType;
end;

function TSilClassFactory.DoCheckProgID(const ProgID: string): string;
begin
  if Str.IsEmpty(ProgID) then
    Result := Sil.Os.Filesystem.ChangeFileExt(Module.Info.Name, '') + '.' + ClassType.ClassName else
    Result := ProgID;
end;

end.
 