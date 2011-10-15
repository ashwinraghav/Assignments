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

unit SilLkContainerBase;

{$I Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiLock,
  SilLiParameters,
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLkObject;

type
  TSilBaseContainer = class(TSilBaseContainerType)
  private
    FHandler: ITypeHandler;
    FAllocator: ITypeAllocator;
    FScratch: HData;
    FSize: Integer;
    FIsLockable: Boolean;
    FLockable: ILockable;
  protected
    FCount: Integer;
  protected // ILockingSet
    function GetIsLockable: Boolean; override;
    procedure SetIsLockable(const Value: Boolean); override;
    function GetLockable: ILockable; override;
  protected //IBaseContainer
    function GetHandler: ITypeHandler;
    function GetAllocator: ITypeAllocator;
    function GetScratch: HData;
    function GetCount: Integer;
  protected
    property Handler: ITypeHandler read FHandler;
    property Allocator: ITypeAllocator read FAllocator;
    property Scratch: HData read FScratch;
    property Size: Integer read FSize;
    property Count: Integer read FCount;
  public
    constructor Create(const Handler: ITypeHandler; const Options: IParameters = nil; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLfParameters,
  SilLmContainerTypes,
  SilLmLockedSection,
  SilOtTool;

{ TSilBaseContainer }

constructor TSilBaseContainer.Create(const Handler: ITypeHandler; const Options: IParameters; const Controller: IUnknown);
begin
  inherited Create(Controller);

  IsLockable := ParamGet(Options, 'Locked', False);
    
  FHandler := Handler;
  FSize := FHandler.Size;
  FAllocator := TSilTypeAllocator.Create(FHandler);
  FScratch := FAllocator.Get();
end;

destructor TSilBaseContainer.Destroy;
begin
  Locked;
  
  FAllocator.Release(FScratch);
  FAllocator := nil;
  FHandler := nil;
  inherited;
end;

function TSilBaseContainer.GetHandler: ITypeHandler;
begin
  Result := FHandler;
end;

function TSilBaseContainer.GetAllocator: ITypeAllocator;
begin
  Result := FAllocator;
end;

function TSilBaseContainer.GetScratch: HData;
begin
  Result := FScratch;
end;

function TSilBaseContainer.GetCount: Integer;
begin
  Result := FCount;
end;

function TSilBaseContainer.GetIsLockable: Boolean;
begin
  Result := FIsLockable;
end;

procedure TSilBaseContainer.SetIsLockable(const Value: Boolean);
begin
  if not Value then FLockable := nil; 
  FIsLockable := Value;
  if Value then FLockable := OS.IPC.CriticalSection;
end;

function TSilBaseContainer.GetLockable: ILockable;
begin
  Result := FLockable;
end;

end.
