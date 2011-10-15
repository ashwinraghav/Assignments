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

unit SilSmSharedProvider;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  TSilObjectProviderClass = class of TSilObjectProvider;
  
  TSilObjectProvider = class(
    TSilObject,
    ISharedObjectProvider )
  private
    FFunctions: ISharedObjectFunctions;
    FModule: IModule2;
  protected // ISharedObjectProvider
    function GetModule: IModule2;
    function Provides(const ID: ISharedID): Boolean; virtual;
    function Get(const ID: ISharedID): ISharedFactory; virtual;
    function Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean; virtual;
    function Enumerate(var Enum: IEnumerator; out Factory: ISharedFactory): Boolean; virtual;
  public
    constructor CreateNew(const Functions: ISharedObjectFunctions); reintroduce; overload; virtual;
    constructor CreateNew; reintroduce; overload; 
    destructor Destroy; override;
  public
    property Functions: ISharedObjectFunctions read FFunctions;
  public
    class function Create(const Functions: ISharedObjectFunctions): ISharedObjectProvider; overload;
    class function Check(const Functions: ISharedObjectFunctions): Boolean; virtual;
    class function Load(const FileName: string; out Provider: ISharedObjectProvider): Boolean; 
    class procedure Register(Item: TSilObjectProviderClass);
  end;

implementation

uses
  SilSfSharedProviders,
  SilSmSharedFunctions;

{ TSilObjectProvider }

class function TSilObjectProvider.Create(const Functions: ISharedObjectFunctions): ISharedObjectProvider;
var
  Enum: IEnumerator;
  Item: TSilObjectProviderClass;
begin
  with Providers do
    while not Assigned(Result) and Enumerate(Enum, Item) do
      if Item.Check(Functions) then
        Result := Item.CreateNew(Functions);
end;

class function TSilObjectProvider.Check(const Functions: ISharedObjectFunctions): Boolean;
begin
  Result := False;
end;

class function TSilObjectProvider.Load(const FileName: string; out Provider: ISharedObjectProvider): Boolean;
var
  Instance: ISharedObjectFunctions; 
begin
  Result := TSilObjectFunctions.Create(FileName, Instance);
  if Result then
  try
    Provider := Create(Instance);
    Result := Assigned(Provider);
  finally
    if not Result then Provider := nil;
    Instance := nil;
  end;
end;

class procedure TSilObjectProvider.Register(Item: TSilObjectProviderClass);
begin
  Providers.Add(Item);
end;

constructor TSilObjectProvider.CreateNew(const Functions: ISharedObjectFunctions);
begin
  inherited Create;
  FFunctions := Functions;
  FModule := FFunctions.Module;
end;

constructor TSilObjectProvider.CreateNew;
begin
  inherited Create;
  FModule := Sil.Os.Module.Get(ClassType) as IModule2;
end;

destructor TSilObjectProvider.Destroy;
begin
  FModule := nil;
  FFunctions := nil;
  inherited;
end;

function TSilObjectProvider.Enumerate(var Enum: IEnumerator; out Factory: ISharedFactory): Boolean;
begin
  Result := False;
end;

function TSilObjectProvider.Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
begin
  Result := False;
end;

function TSilObjectProvider.Get(const ID: ISharedID): ISharedFactory;
begin
  if not Find(ID, Result) then
    raise Sil.Error.Create('EL MODULO NO RECONOCE LA CLASE SOLICITADA'); { TODO : @!@ } 
end;

function TSilObjectProvider.Provides(const ID: ISharedID): Boolean;
var
  Dummy: ISharedFactory;
begin
  Result := Find(ID, Dummy);
end;

function TSilObjectProvider.GetModule: IModule2;
begin
  Result := FModule; 
end;

end.
