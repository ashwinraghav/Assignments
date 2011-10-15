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

unit SilSmSharedManager;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilShSharedManager;

type
  TSilSharedManager = class(
    TSilInterfacedObject,
    ISharedObjectManager )
  private
    FKey: INamedKey;
    FConfig: ISharedObjectConfig;
    FList: ISharedObjectList;
    FLoader: ISharedObjectLoader;
    FCache: ISharedObjectCache;
    FFactory: ISharedObjectFactory;
    FWatchdog: ISharedObjectWatchdog;
    FInitCount: Integer;
  private
    procedure DoInitialize(const Key: INamedKey);
    procedure DoFinalize;
    procedure DoCheckWatchdog;
  protected // ISharedObjectManager
    function GetConfig: ISharedObjectConfig;
    function GetList: ISharedObjectList;
    function GetLoader: ISharedObjectLoader;
    function GetCache: ISharedObjectCache;
    function GetFactory: ISharedObjectFactory;
    function GetWatchdog: ISharedObjectWatchdog;
  protected // ISharedInitialization
    procedure Initialize(const Key: INamedKey);
    procedure Finalize;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmSharedConfig,
  SilSmSharedCache,
  SilSmSharedFactory,
  SilSmSharedList,
  SilSmSharedLoader,
  SilSmSharedWatchdog;

{ TSilSharedManager }

constructor TSilSharedManager.Create;
begin
  inherited Create;
end;

destructor TSilSharedManager.Destroy;
begin
  Locked;
  
  if FInitCount <> 0 then
    DoFinalize;
  
  inherited;
end;

function TSilSharedManager.GetConfig: ISharedObjectConfig;
begin
  Result := FConfig;
end;

function TSilSharedManager.GetList: ISharedObjectList;
begin
  Result := FList;
end;

function TSilSharedManager.GetLoader: ISharedObjectLoader;
begin
  Result := FLoader;
end;

function TSilSharedManager.GetCache: ISharedObjectCache;
begin
  Result := FCache;
end;

function TSilSharedManager.GetFactory: ISharedObjectFactory;
begin
  Result := FFactory;
end;

procedure TSilSharedManager.DoCheckWatchdog;
begin
  if not Assigned(FWatchdog) then
  begin
    FWatchdog := TSilSharedWatchdog.Create(Self);
    FWatchdog.Initialize(nil {Key});
  end;
end;

function TSilSharedManager.GetWatchdog: ISharedObjectWatchdog;
begin
  DoCheckWatchdog;
  Result := FWatchdog;
end;

procedure TSilSharedManager.Initialize(const Key: INamedKey);
begin
  if Sil.Os.Locked.Increment(FInitCount) = 1 then
    DoInitialize(Key);
end;

procedure TSilSharedManager.Finalize;
begin
  if Sil.Os.Locked.Decrement(FInitCount) = 0 then
    DoFinalize;
end;

procedure TSilSharedManager.DoInitialize(const Key: INamedKey);
begin
  try
    FKey := Key;
    FConfig := TSilSharedConfig.Create(Self, Key);
    FLoader := TSilSharedLoader.Create(Self);
    FCache := TSilSharedCache.Create(Self, Key);
    FFactory := TSilSharedFactory.Create(Self);
    FList := TSilSharedList.Create(Self);
  except
    Sil.Trace.Exception('TSilSharedManager.DoInitialize');
  end;
end;

procedure TSilSharedManager.DoFinalize;
begin
  try
    if Assigned(FWatchdog) then
      FWatchdog.Finalize;

    FList := nil;
    FFactory := nil;
    FCache := nil;
    FLoader := nil;
    FConfig := nil;
    FWatchdog := nil;
    FKey := nil;
  except
    Sil.Trace.Exception('TSilSharedManager.DoFinalize');
  end;
end;

end.
