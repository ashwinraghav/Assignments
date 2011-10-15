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

unit SilOkIpcCriticalSection;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOsTypes,

  SilLiLock,
  SilOiHandle,
  SilOiIpc,

  SilOsIpc;

type
  TSilCriticalSection = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    ICriticalSection,
    ILocker )
  protected
    function DoCreateLock(IsLocked: Boolean): ILock;
  protected // ILockable
    procedure Lock; override;
    procedure Unlock; override;
  protected // ILocker
    function ILocker.Lock = DoLockerLock;
  protected // ICriticalSection
    function GetEntered: Boolean;
    function ICriticalSection.Locked = DoLocked;
    function ICriticalSection.TryLock = DoTryLock;
  protected
    function DoLocked: ILock; virtual;
    function DoLockerLock(const AContext: Pointer): ILock; virtual;
    function DoTryLock: ILock; virtual;
  protected
    procedure DoInitialize; virtual; abstract;
    procedure DoFinalize; virtual; abstract;
    procedure DoEnter; virtual; abstract;
    function DoTryEnter: Boolean; virtual; abstract;
    procedure DoLeave; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilLmLockedSection;

{ TSilCriticalSection }

constructor TSilCriticalSection.Create;
begin
  inherited Create;
  DoInitialize;
end;

destructor TSilCriticalSection.Destroy;
begin
  DoFinalize;
  inherited Destroy;
end;

function TSilCriticalSection.DoTryLock: ILock;
begin
  if DoTryEnter then
    Result := DoCreateLock(True) else
    Result := nil;
end;

procedure TSilCriticalSection.Lock;
begin
  DoEnter;
end;

procedure TSilCriticalSection.Unlock;
begin
  DoLeave;
end;

function TSilCriticalSection.DoLocked: ILock;
begin
  Result := DoCreateLock(False);
end;

function TSilCriticalSection.DoLockerLock(const AContext: Pointer): ILock;
begin
  Result := DoCreateLock(False);
end;

function TSilCriticalSection.DoCreateLock(IsLocked: Boolean): ILock;
begin
  Result := TSilLockedSection.Create(Self, IsLocked);
end;

function TSilCriticalSection.GetEntered: Boolean;
begin
  if DoTryEnter then
  begin
    DoLeave;
    Result := False;
  end else
    Result := True;
end;

end.
