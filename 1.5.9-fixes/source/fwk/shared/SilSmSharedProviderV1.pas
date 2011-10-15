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

unit SilSmSharedProviderV1;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilSmSharedProvider;

type
  TSilObjectProviderV1 = class(
    TSilObjectProvider )
  protected 
    function Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean; override; 
  public
    class function Check(const Functions: ISharedObjectFunctions): Boolean; override;
  end;

  TSilObjectFactoryV1 = class(
    TSilObject,
    ISharedObject,
    ISharedFactory,
    ISharedProgID,
    ISharedClassID )
  private
    FClassID: TGUID;
    FFunctions: ISharedObjectFunctions;
  protected // ISharedObject
    function GetClassID: TGUID;
    function GetModule: IModule2;
  protected // ISharedFactory
    function GetProgID: string;
    function CreateObject(const Owner: IUnknown; const IID: TGuid; out Instance; const Controller: IUnknown = nil; Param: Pointer = nil): Boolean;
  public
    constructor Create(const ClassID: TGUID; const Functions: ISharedObjectFunctions);
    destructor Destroy; override;   
  end;                                               

implementation

{ TSilObjectProviderV1 }

class function TSilObjectProviderV1.Check(const Functions: ISharedObjectFunctions): Boolean;
begin
  Result := (Functions.Rtl.Major <= 2)
        and ([epGetClassQuery, epCreateObject] * Functions.Support = [epGetClassQuery, epCreateObject]);
end;

function TSilObjectProviderV1.Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
begin
  Result := (ID.Kind = idClass) and Functions.ClassQuery(ID.AsClass.ClassID);
  if Result then Factory := TSilObjectFactoryV1.Create(ID.AsClass.ClassID, Functions); 
end;

{ TSilObjectFactoryV1 }

constructor TSilObjectFactoryV1.Create(const ClassID: TGUID; const Functions: ISharedObjectFunctions);
begin
  inherited Create;
  FClassID := ClassID;
  FFunctions := Functions;
end;

destructor TSilObjectFactoryV1.Destroy;
begin
  FFunctions := nil;
  inherited;
end;

function TSilObjectFactoryV1.GetClassID: TGUID;
begin
  Result := FClassID;
end;

function TSilObjectFactoryV1.GetModule: IModule2;
begin
  Result := FFunctions.Module;
end;

function TSilObjectFactoryV1.GetProgID: string;
begin
  Result := '[' + FFunctions.Module.Info.Name + '].' + '{' + Sil.GUID.ToStr(FClassID) + '}';
end;

function TSilObjectFactoryV1.CreateObject(const Owner: IInterface; const IID: TGuid; out Instance; const Controller: IInterface; Param: Pointer): Boolean;
var
  Unknown: IUnknown;
begin
  Unknown := FFunctions.CreateObject(FClassID, Owner, Controller);
  try
    Result := Sil.Ref.GetInterface(Unknown, IID, Instance);
  finally
    Unknown := nil;
  end;
end;

initialization
  TSilObjectProvider.Register(TSilObjectProviderV1);

end.
