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

unit SilLkAbstractLocker;

{$I Defines.inc}

interface

uses
{  SysUtils, Windows, Messages, Classes, }

  SilLiLock,
  SilLkInterfaced;

type                       
  TAbstractLocker = class(
    TSilInterfacedObject,
    ILocker )
  private
    procedure InternalLock(const ALock: ILock); virtual;
    procedure InternalUnlock(const ALock: ILock); virtual;
  protected //- ILocker
    function Lock(const AContext: Pointer): ILock; reintroduce;
  protected
    procedure DoLock(const ALock: ILock); virtual; abstract;
    procedure DoUnlock(const ALock: ILock); virtual; abstract;
  public
    constructor Create; 
  end;

  TExtendedLocker = class(TAbstractLocker,  IExtendedLocker)
  private
    FTarget: Pointer;
    FLocked: Boolean;
  protected //- IExtendedLocker
    function GetLocked: Boolean; virtual;
    function GetTarget: Pointer;
  protected
    procedure InternalLock(const ALock: ILock); override;
    procedure InternalUnlock(const ALock: ILock); override;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  protected
   property _Locked: Boolean read GetLocked;
    property _Target: Pointer read FTarget write FTarget;
  public
    constructor Create(ATarget: Pointer);
  end;

type
  TLockableLocker = class(TExtendedLocker)
  private
    FLockable: ILockable;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(const ALockable: IUnknown);
    destructor Destroy; override;
  end;

type
  TLock = class(TSilInterfacedObject, ILock)
  private
    FLocker: TAbstractLocker;
    FContext: Pointer;
    procedure Lock(ALocker: TAbstractLocker); reintroduce;
    procedure Unlock; reintroduce;
  protected
    function Context: Pointer;
    function Locker: ILocker;
    procedure ILock.Release = ReleaseLock;
    procedure ReleaseLock;
  protected
    property _Locker: TAbstractLocker read FLocker;
    property _Context: Pointer read FContext write FContext;
  public
    constructor Create(ALocker: TAbstractLocker; const AContext: Pointer);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtReference;

//----------------------------------------------------------------------------------------------------------------------------------

constructor TAbstractLocker.Create;
begin
  inherited Create;
end;

function TAbstractLocker.Lock(const AContext: Pointer): ILock;
begin
  Result := TLock.Create(Self, AContext);
end;

procedure TAbstractLocker.InternalLock(const ALock: ILock);
begin
  DoLock(ALock);
end;

procedure TAbstractLocker.InternalUnlock(const ALock: ILock);
begin
  DoUnlock(ALock);
end;

//----------------------------------------------------------------------------------------------------------------------------------

{ TExtendedLocker }

constructor TExtendedLocker.Create(ATarget: Pointer);
begin
  inherited Create;
  FTarget := ATarget;
end;

function TExtendedLocker.GetLocked: Boolean;
begin
  Result := FLocked; 
end;

function TExtendedLocker.GetTarget: Pointer;
begin
  Result := FTarget;
end;

procedure TExtendedLocker.InternalLock(const ALock: ILock);
begin
  if not _Locked then
  begin
    inherited InternalLock(ALock);
    FLocked := True;
  end;
end;

procedure TExtendedLocker.InternalUnlock(const ALock: ILock);
begin
  if _Locked then
  begin
    inherited InternalUnlock(ALock);
    FLocked := False;
  end;
end;

procedure TExtendedLocker.DoLock(const ALock: ILock);
begin
end;

procedure TExtendedLocker.DoUnlock(const ALock: ILock);
begin
end;

//----------------------------------------------------------------------------------------------------------------------------------

constructor TLock.Create(ALocker: TAbstractLocker; const AContext: Pointer);
begin
  Assert(ALocker <> nil, 'ALocker <> nil');
  inherited Create;
  FContext := AContext;
  Lock(ALocker);
end;

destructor TLock.Destroy;
begin
  Unlock;
  inherited;
end;

procedure TLock.ReleaseLock;
begin
  Unlock;
end;

function TLock.Context: Pointer;
begin
  Result := FContext;
end;

function TLock.Locker: ILocker;
begin
  Result := FLocker;
end;

procedure TLock.Lock(ALocker: TAbstractLocker);
begin
  if ALocker <> nil then
  begin
    FLocker := ALocker;
    FLocker._AddRef;
    FLocker.InternalLock(Self);
  end;
end;

procedure TLock.Unlock;
begin
  if FLocker <> nil then
  begin
    FLocker.InternalUnlock(Self);
    FLocker._Release;
    FLocker := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------------------

constructor TLockableLocker.Create(const ALockable: IUnknown);
begin
  inherited Create(Pointer(ALockable));
  FLockable := ALockable as ILockable;
end;

destructor TLockableLocker.Destroy;
begin
  FLockable := nil;
  inherited;
end;

procedure TLockableLocker.DoLock(const ALock: ILock);
begin
  FLockable.Lock;
end;

procedure TLockableLocker.DoUnlock(const ALock: ILock);
begin
  FLockable.Unlock;
end;

end.
