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

unit SilLmContainerEnumerator;

{$INCLUDE Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLiContainer,
  SilLiContainerTypes,
  SilLiLock,
  SilLkObject,
  SilLkAbstractEnumerator;

type
  TSilContainerEnumerator = class(
    TSilObject,
    IEnumerator )
  private
    FContainer: IContainerStatic;
    FSequence: IContainerSequence;
    FCursor: IContainerCursor;
    FIteration: Integer;
    FLock: ILock;
    FDetached: Boolean;
  protected // IEnumerator
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function Next: Boolean;
    function GetIteration: Integer;
    procedure Detach;
    procedure Reset;
  public
    constructor Create(const Container: IContainerStatic; const Sequence: IContainerSequence; Locked: Boolean = False); reintroduce; overload; 
    constructor Create(const Sequence: IContainerSequenceStatic; Locked: Boolean = False); reintroduce; overload; 
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

{ TSilContainerEnumerator }

constructor TSilContainerEnumerator.Create(const Container: IContainerStatic; const Sequence: IContainerSequence; Locked: Boolean);
begin
  inherited Create;
  FContainer := Container;
  FSequence := Sequence;
  if Locked then FLock := Container.Synchronized.Lock;
end;

constructor TSilContainerEnumerator.Create(const Sequence: IContainerSequenceStatic; Locked: Boolean);
begin
  Create(Sequence.Owner, Sequence, Locked);
end;

destructor TSilContainerEnumerator.Destroy;
begin
  Detach;
  inherited;
end;

procedure TSilContainerEnumerator.AfterConstruction;
begin
  Reset;
  inherited;
end;

procedure TSilContainerEnumerator.Detach;
begin
  if not FDetached then
  begin
    FLock := nil;
    try
      FCursor := nil;
      if Assigned(FSequence) then
      begin
        FSequence.Detach;
        FSequence := nil;
      end;
    finally
      FContainer := nil;
      FDetached := True;
    end;
  end;
end;

procedure TSilContainerEnumerator.Reset;
begin
  FIteration := 0;
  FCursor := nil;
end;

function TSilContainerEnumerator.HasMore: Boolean;
begin
  if FDetached then
    Result := false
  else if Assigned(FCursor) then
    Result := FCursor.IsValid
  else
    Result := FSequence.Get(FCursor);
end;

function TSilContainerEnumerator.GetCurrent: Pointer;
begin
  if HasMore and Assigned(FCursor) then
    Result := FContainer.Cursors[FCursor] else
    Result := nil;
end;

function TSilContainerEnumerator.Get(out Item): Boolean;
begin
  HData(Item) := GetCurrent();
  Result := Assigned(HData(Item));
end;

function TSilContainerEnumerator.Next: Boolean;
begin
  Result := FSequence.Get(FCursor);
  if Result then Inc(FIteration);
end;

function TSilContainerEnumerator.GetIteration: Integer;
begin
  Result := FIteration;
end;

end.
