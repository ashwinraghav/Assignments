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

unit SilLkLockers;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLkAbstractLocker;

type
  TCountedLocker = class(TAbstractLocker, IExtendedLocker)
  private
    FLocker: IExtendedLocker;
    FLock: ILock;
  protected //- TAbstractLocker
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  protected
    property ExtendedLocker: IExtendedLocker read FLocker implements IExtendedLocker;
  public
    constructor Create(const ALocker: IExtendedLocker);
    destructor Destroy; override;
  end;

  TBooleanLocker = class(TExtendedLocker)
  private
    FVariable: ^Boolean;
    FLockedValue: Boolean;
  protected
    procedure DoLock(const ALock: ILock); override;
    procedure DoUnlock(const ALock: ILock); override;
  public
    constructor Create(var AVariable: Boolean; LockedValue: Boolean);
  end;


implementation

constructor TCountedLocker.Create(const ALocker: IExtendedLocker);
begin
  inherited Create;
  Assert(ALocker <> nil, 'ALocker <> nil');
  FLocker := ALocker;
end;

destructor TCountedLocker.Destroy;
begin
  FLocker := nil;
  inherited;
end;

procedure TCountedLocker.DoLock(const ALock: ILock);
begin
  if FLock = nil then
    FLock := FLocker.Lock(nil);
  FLock._AddRef;
end;

procedure TCountedLocker.DoUnlock(const ALock: ILock);
begin
  if FLock <> nil then
  begin
    if FLock._Release = 1 then
      FLock := nil;
  end;
end;

{ TBooleanLocker }

constructor TBooleanLocker.Create(var AVariable: Boolean; LockedValue: Boolean);
begin
  inherited Create(@AVariable);
  FVariable := @AVariable;
  FLockedValue := LockedValue;
end;

procedure TBooleanLocker.DoLock(const ALock: ILock);
begin
  FVariable^ := FLockedValue;
end;

procedure TBooleanLocker.DoUnlock(const ALock: ILock);
begin
  FVariable^ := not FLockedValue;
end;

end.
