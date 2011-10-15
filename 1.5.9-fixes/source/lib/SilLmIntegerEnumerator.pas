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

unit SilLmIntegerEnumerator;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLkInterfaced;

type

{ TIntegerEnumerator }

  TIntegerEnumerator = class(
    TSilInterfacedObject,
    IEnumerator )
  protected
    FFirst: integer;
    FLast: integer;
    FStep: integer;
    FIteration: integer;
    FDetached: boolean;
  protected // IEnumerator
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function GetIteration: Integer;
    function Next: Boolean;
    procedure Detach;
    procedure Reset;
    property Current: Pointer read GetCurrent;
    property Iteration: Integer read GetIteration;
  public
    constructor Create(First, Last, Step: integer); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  SilBtError;

{ TAbstractEnumerator }

constructor TIntegerEnumerator.Create(First, Last, Step: integer);
begin
  inherited Create;

  if ( Step = 0 ) then
    Error.Throw( 'TIntegerEnumerator: Invalid step' );

  FFirst := First;
  FLast := Last;
  FStep := Step;
end;

destructor TIntegerEnumerator.Destroy;
begin
  Detach;
  inherited;
end;

procedure TIntegerEnumerator.AfterConstruction;
begin
  Reset;
  inherited;
end;

function TIntegerEnumerator.Get(out Item): Boolean;
begin
  Error.Throw( 'TIntegerEnumerator: Not implemented' );
  result := false;
end;

function TIntegerEnumerator.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TIntegerEnumerator.HasMore: Boolean;
begin
  if FDetached then
    Result := false
  else if ( FStep > 0 ) then
    Result := ( FIteration <= FLast )
  else
    Result := ( FIteration >= FLast );
end;

procedure TIntegerEnumerator.Detach;
begin
  FDetached := True;
end;

procedure TIntegerEnumerator.Reset;
begin
  FIteration := 0;
  FDetached := False;
end;

function TIntegerEnumerator.GetCurrent: Pointer;
begin
  Error.Throw( 'TIntegerEnumerator: Not implemented' );
  result := nil;
end;

function TIntegerEnumerator.Next: Boolean;
begin
  if not FDetached then
  begin
    Inc( FIteration, FStep );
    Result := HasMore;
  end
  else
    Result := False;
end;

end.
