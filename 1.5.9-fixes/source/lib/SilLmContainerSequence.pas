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

unit SilLmContainerSequence;

interface

{$INCLUDE Defines.inc}

uses
  Sil,
  SilLiContainerTypes,
  SilLiContainer;

type
  TSilSequenceLiteSimpleStatic = class(
    TSilObject,
    IContainerSequence,
    IContainerSequenceStatic  )
  private
    FOwner: IContainerStatic;
    FIteration: Integer;
    FKind: TGUID;
    FStart: HItem;
    FDelta: Integer;
    FIsDetached: Boolean;
  protected // IContainerSequence
    function GetIteration: Integer;
    function Get(var Obj): Boolean; overload;
    procedure Detach;
  protected // IContainerSequenceStatic
    function GetOwnerStatic: IContainerStatic;
  public
    constructor Create(const Owner: IContainerStatic; const Kind: TGUID; const Start: HItem = HNull; Delta: Integer = 1);
    destructor Destroy; override;
  end;

type
  TSilSequenceLiteSimpleDynamic = class(
    TSilSequenceLiteSimpleStatic,
    IContainerSequenceDynamic )
  protected // IContainerSequenceDynamic
    function GetOwnerDynamic: IContainerDynamic;
  public
    constructor Create(const Owner: IContainerDynamic; const Kind: TGUID; const Start: HItem = HNull; Delta: Integer = 1);
  end;

type
  TSilSequenceBase = class(
    TSilObject,
    IContainerSequence,
    IContainerSequenceStatic )
  private
    FOwner: IContainerStatic;
    FIteration: Integer;
    FKind: TGUID;
    FIsDetached: Boolean;
  protected
    function DoGetCursor(var Obj): Boolean; virtual; abstract;
    function DoGetNext(var Obj): Boolean; virtual;
  protected // IContainerSequence
    function GetIteration: Integer;
    function Get(var Obj): Boolean; overload;
    procedure Detach; virtual;
  protected // IContainerSequenceStatic
    function GetOwnerStatic: IContainerStatic;
  public
    constructor Create(const Owner: IContainerStatic; const Kind: TGUID);
    destructor Destroy; override;
  public
    property Kind: TGUID read FKind;
  end;

type
  TSilSequenceSimpleStatic = class(TSilSequenceBase)
  private
    FStart: HItem;
    FDelta: Integer;
  protected
    function DoGetCursor(var Obj): Boolean; override;
  protected
    procedure Detach; override;
  public
    constructor Create(const Owner: IContainerStatic; const Kind: TGUID; const Start: HItem = HNull; Delta: Integer = 1);
    destructor Destroy; override;
  end;

type
  TSilSequenceSimpleDynamic = class(
    TSilSequenceSimpleStatic,
    IContainerSequenceDynamic )
  protected // IContainerSequenceDynamic
    function GetOwnerDynamic: IContainerDynamic;
  public
    constructor Create(const Owner: IContainerDynamic; const Kind: TGUID; const Start: HItem = HNull; Delta: Integer = 1);
  end;
  
type
  TSilSequenceRangeStatic = class(TSilSequenceBase)
  private
    FFirst, FLast: IContainerCursor;
  protected
    function DoCheckCursor(const Cursor, Default: IContainerCursor): IContainerCursor;
    function DoGetCursor(var Obj): Boolean; override;
    function DoGetNext(var Obj): Boolean; override;
  protected // IContainerSequence
    procedure Detach; override;
  public
    constructor Create(const Owner: IContainerStatic; const First, Last: IContainerCursor; const Kind: TGUID);
    destructor Destroy; override;
  end;

  TSilSequenceRangeDynamic = class(
    TSilSequenceRangeStatic,
    IContainerSequenceDynamic )
  protected // IContainerSequenceDynamic
    function GetOwnerDynamic: IContainerDynamic;
  public
    constructor Create(const Owner: IContainerDynamic; const First, Last: IContainerCursor; const Kind: TGUID);
  end;

implementation

{ TSilSequenceLiteSimpleStatic }

constructor TSilSequenceLiteSimpleStatic.Create(const Owner: IContainerStatic; const Kind: TGUID; const Start: HItem; Delta: Integer);
begin
  inherited Create;
  FKind := Kind;
  FOwner := Owner;
  if Start = HNull then
    if Delta > 0 then
      FStart := FOwner.Items.First else
      FStart := FOwner.Items.Last;
  FDelta := Delta;
end;

destructor TSilSequenceLiteSimpleStatic.Destroy;
begin
  Detach;
  inherited;
end;

function TSilSequenceLiteSimpleStatic.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TSilSequenceLiteSimpleStatic.Get(var Obj): Boolean;
begin
  if FIsDetached then
    Result := False
  else if Assigned(Pointer(Obj)) then
  begin
    Inc(FIteration);
    Result := IContainerCursor(Obj).Next;
  end else
  begin
    FIteration := 0;
    with FOwner.Cursors do Result := Create(FStart, FKind, Obj, FDelta);
  end;
  if not Result then IContainerCursor(Obj) := nil;
end;

procedure TSilSequenceLiteSimpleStatic.Detach;
begin
  FIsDetached := True;
  FOwner := nil;
end;

function TSilSequenceLiteSimpleStatic.GetOwnerStatic: IContainerStatic;
begin
  Result := FOwner;
end;

{ TSilSequenceLiteSimpleDynamic }

constructor TSilSequenceLiteSimpleDynamic.Create(const Owner: IContainerDynamic; const Kind: TGUID; const Start: HItem; Delta: Integer);
begin
  inherited Create(Owner, Kind, Start, Delta);
end;

function TSilSequenceLiteSimpleDynamic.GetOwnerDynamic: IContainerDynamic;
begin
  Result := IContainerDynamic(FOwner);
end;

{ TSilSequenceBase }

constructor TSilSequenceBase.Create(const Owner: IContainerStatic; const Kind: TGUID);
begin
  inherited Create;
  FOwner := Owner;
  FKind := Kind;
end;

destructor TSilSequenceBase.Destroy;
begin
  Detach;
  FOwner := nil;
  inherited;
end;

function TSilSequenceBase.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TSilSequenceBase.Get(var Obj): Boolean;
begin
  if FIsDetached then
    Result := False
  else if Assigned(Pointer(Obj)) then
  begin
    Inc(FIteration);
    Result := DoGetNext(Obj);
  end else 
  begin
    FIteration := 0;
    Result := DoGetCursor(Obj);
  end;
  if not Result then IContainerCursor(Obj) := nil;
end;

procedure TSilSequenceBase.Detach;
begin
  FOwner := nil;
  FIsDetached := True;
end;

function TSilSequenceBase.GetOwnerStatic: IContainerStatic;
begin
  Result := FOwner;
end;

function TSilSequenceBase.DoGetNext(var Obj): Boolean;
begin
  Result := IContainerCursor(Obj).Next;
end;

{ TSilSequenceSimpleStatic }

constructor TSilSequenceSimpleStatic.Create(const Owner: IContainerStatic; const Kind: TGUID; const Start: HItem; Delta: Integer);
begin
  inherited Create(Owner, Kind);
  if Start = HNull then
    if Delta > 0 then
      FStart := FOwner.Items.First else
      FStart := FOwner.Items.Last;
  FDelta := Delta;
end;

destructor TSilSequenceSimpleStatic.Destroy;
begin
  inherited;
end;

procedure TSilSequenceSimpleStatic.Detach;
begin
  inherited;
end;

function TSilSequenceSimpleStatic.DoGetCursor(var Obj): Boolean;
begin
  with FOwner.Cursors do Result := Create(FStart, FKind, Obj, FDelta)
end;

{ TSilSequenceSimpleDynamic }

constructor TSilSequenceSimpleDynamic.Create(const Owner: IContainerDynamic; const Kind: TGUID; const Start: HItem; Delta: Integer);
begin
  inherited Create(Owner, Kind, Start, Delta);
end;

function TSilSequenceSimpleDynamic.GetOwnerDynamic: IContainerDynamic;
begin
  Result := IContainerDynamic(FOwner);
end;

{ TSilSequenceRangeStatic }

constructor TSilSequenceRangeStatic.Create(const Owner: IContainerStatic; const First, Last: IContainerCursor; const Kind: TGUID);
begin
  inherited Create(Owner, Kind);
  FFirst := DoCheckCursor(First, Owner.Cursors.First);
  FLast := DoCheckCursor(Last, Owner.Cursors.Last);
end;

destructor TSilSequenceRangeStatic.Destroy;
begin
  inherited;
end;

procedure TSilSequenceRangeStatic.Detach;
begin
  FFirst := nil;
  FLast := nil;
end;

function TSilSequenceRangeStatic.DoGetCursor(var Obj): Boolean;
begin
  Result := Assigned(FFirst) and (FFirst.QueryInterface(Kind, Obj) = 0);
end;

function TSilSequenceRangeStatic.DoGetNext(var Obj): Boolean;
begin
  Result := not IContainerCursor(Obj).IsEqual(FLast);
  if Result then IContainerCursor(Obj).Next;
end;

function TSilSequenceRangeStatic.DoCheckCursor(const Cursor, Default: IContainerCursor): IContainerCursor;
begin
  if Assigned(Cursor) and Cursor.IsValid then
    Result := Cursor else
    Result := Default;
end;

{ TSilSequenceRangeDynamic }

constructor TSilSequenceRangeDynamic.Create(const Owner: IContainerDynamic; const First, Last: IContainerCursor; const Kind: TGUID);
begin
  inherited Create(Owner, First, Last, Kind);
end;

function TSilSequenceRangeDynamic.GetOwnerDynamic: IContainerDynamic;
begin
  Result := IContainerDynamic(FOwner);
end;

end.
