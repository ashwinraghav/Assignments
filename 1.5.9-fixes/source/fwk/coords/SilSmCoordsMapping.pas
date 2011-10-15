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

unit SilSmCoordsMapping;

interface

uses
  Sil,
  SilMath;

type
  TCoordsMapping = class (
  // extends
    TSilInterfacedObject,
  // implements
    ICoordsMapping)
  private
    FClientMax: ClientType;
    FClientMin: ClientType;
    FWorldMax: WorldType;
    FWorldMin: WorldType;
    FLocks: Integer;
    FPendingChange: Boolean;
  private
    procedure DoFireChanged;
  protected // ILockable
		procedure Lock; override;
		procedure Unlock; override;
  protected // ICoordsMapping
    function GetClientMax: ClientType;
    function GetClientMin: ClientType;
    function GetClientSize: ClientType;
    function GetWorldMax: WorldType;
    function GetWorldMin: WorldType;
    function GetWorldSize: WorldType;
    procedure SetClientMax(const Value: ClientType);
    procedure SetClientMin(const Value: ClientType);
    procedure SetClientSize(const Value: ClientType);
    procedure SetWorldMax(const Value: WorldType);
    procedure SetWorldMin(const Value: WorldType);
    procedure SetWorldSize(const Value: WorldType);
    function Normalize: ICoordsMapping;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
  end;


implementation

uses SilLkInterfaced;

{ TCoordsMapping }

constructor TCoordsMapping.Create;
begin
  inherited Create;
end;

destructor TCoordsMapping.Destroy;
begin
  inherited;
end;

function TCoordsMapping.GetClientMax: ClientType;
begin
  Result := FClientMax;
end;

function TCoordsMapping.GetClientMin: ClientType;
begin
  Result := FClientMin;
end;

function TCoordsMapping.GetClientSize: ClientType;
begin
  Result := FClientMax - FClientMin;
end;

function TCoordsMapping.GetWorldMax: WorldType;
begin
  Result := FWorldMax;
end;

function TCoordsMapping.GetWorldMin: WorldType;
begin
  Result := FWorldMin;
end;

function TCoordsMapping.GetWorldSize: WorldType;
begin
  Result := FWorldMax - FWorldMin;
end;

procedure TCoordsMapping.SetClientMax(const Value: ClientType);
begin
  FClientMax := Value;
  Changed;
end;

procedure TCoordsMapping.SetClientMin(const Value: ClientType);
begin
  FClientMin := Value;
  Changed;
end;

procedure TCoordsMapping.SetClientSize(const Value: ClientType);
begin
  FClientMax := FClientMin + Value;
  Changed;
end;

procedure TCoordsMapping.SetWorldMax(const Value: WorldType);
begin
  FWorldMax := Value;
  Changed;
end;

procedure TCoordsMapping.SetWorldMin(const Value: WorldType);
begin
  FWorldMin := Value;
  Changed;
end;

procedure TCoordsMapping.SetWorldSize(const Value: WorldType);
begin
  FWorldMax := FWorldMin + Value;
  Changed;
end;

procedure TCoordsMapping.DoFireChanged;
var
  Enum: IEnumerator;
  Item: ICoordsMappingEvents;
  Event: TMappingChangedEvent;
begin
  if not HasConnections then Exit;

  Event.Sender := Self;
  while Events.Enumerate(Enum, Item, ICoordsMappingEvents) do
    Item.OnMappingChanged(Event);
  FPendingChange := False;
end;

function TCoordsMapping.Normalize: ICoordsMapping;
var
  Mapping: TCoordsMapping;
begin
  Mapping := TCoordsMapping.Create;
  Mapping.FClientMax := Int.Max(FClientMax, FClientMin);
  Mapping.FClientMin := Int.Max(FClientMax, FClientMin);
  Mapping.FWorldMax  := Float.Max(FWorldMax, FWorldMin);
  Mapping.FWorldMin  := Float.Min(FWorldMax, FWorldMin);
  Result := Mapping;
end;

procedure TCoordsMapping.Lock;
begin
  Sil.Os.Locked.Increment(FLocks);
end;

procedure TCoordsMapping.Unlock;
begin
  if (Sil.Os.Locked.Decrement(FLocks) = 0) and FPendingChange then
    DoFireChanged;
end;

procedure TCoordsMapping.Changed;
begin
  if FLocks = 0 then
    DoFireChanged;
  FPendingChange := True;
end;

end.
