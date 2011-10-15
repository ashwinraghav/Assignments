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

unit SilSmTicsEnumerator;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiCoords,
  SilSkCoordsMapper;

type
  TTicsEnumerator = class(
  // extends
    TSilInterfacedObject,
  // implements
    IEnumerator )
  private
    FMapper: TCoordsMapper;
    FIteration: Integer;
    FTic: ITicDataDef;
  protected // IEnumerator
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function GetIteration: Integer;
    function Next: Boolean;
    procedure Detach;
    procedure Reset;
  public
    constructor Create(const Mapper: TCoordsMapper);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;
  
{ TTicsEnumerator }

constructor TTicsEnumerator.Create(const Mapper: TCoordsMapper);
begin
  inherited Create;
  FMapper := Mapper;
  Next;
end;

destructor TTicsEnumerator.Destroy;
begin
  FMapper := nil;
  inherited;
end;

function TTicsEnumerator.Get(out Item): Boolean;
begin
  Result := FTic <> nil;
  if Result then
    ITicData(Item) := FTic;
end;

function TTicsEnumerator.GetCurrent: Pointer;
begin
  Result := Pointer(FTic);
end;

procedure TTicsEnumerator.Detach;
begin
  FTic := nil;
end;

function TTicsEnumerator.HasMore: Boolean;
begin
  Result := FTic <> nil;
end;

function TTicsEnumerator.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TTicsEnumerator.Next: Boolean;
begin
  Result := FMapper.CalcNextTic(FTic);
end;

procedure TTicsEnumerator.Reset;
begin
  FIteration := 0;
end;

end.
 