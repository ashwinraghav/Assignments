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

unit SilVectorSkEnumerator;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector;

type
  TSilVectorEnumerator = class (TSilObject, IVectorEnumerator, IEnumerator)
  protected
    fvector: IVector;
    fstep: integer;
    findex: integer;
    findexbegin: integer;
    findexend: integer;
    fpurge: TIntegerArray;
  protected // IVectorEnumerator
    procedure Reset;
    function GetIndex: integer;
    procedure Release; reintroduce;
  protected // IEnumerator
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function DoGet(out Item): Boolean; virtual;
    function DoGetCurrent: Pointer; virtual;
    function Next: Boolean;
    function GetIteration: integer;
    procedure Detach;
    procedure Purge;
    procedure DoPurgeClear; virtual;
  public
    constructor Create(const Vector: IVector; Step, IndexBegin, IndexEnd: integer);
    destructor Destroy; override;
  end;

implementation

{ TSilVectorEnumerator }

constructor TSilVectorEnumerator.Create(const Vector: IVector; Step, IndexBegin, IndexEnd: integer);
begin
  inherited Create;

  fvector := vector;
  fstep := step;
  reset;

  if (fstep < 0) and (indexbegin = 0) and (indexend = -1) then
  begin
    indexbegin := fvector.count - 1;
    indexend := 0;
  end;

  findex := indexbegin;
  findexbegin := indexbegin;

  if indexend = -1 then
    findexend := fvector.count - 1
  else
    findexend := indexend;
end;

destructor TSilVectorEnumerator.Destroy;
begin
  release;
  inherited;
end;

function TSilVectorEnumerator.GetIndex: integer;
begin
  result := findex - fstep;
end;

function TSilVectorEnumerator.GetIteration: integer;
begin
  result := findex;
end;

procedure TSilVectorEnumerator.Release;
begin
  DoPurgeClear;

  if fvector <> nil then
    fvector.Unlock;

  fvector := nil;
end;

procedure TSilVectorEnumerator.Reset;
begin
  fvector.Lock;
  findex := findexbegin;
end;

{ IEnumerator }

procedure TSilVectorEnumerator.Detach;
begin
  release;
  fvector := nil;
end;

function TSilVectorEnumerator.Get(out Item): Boolean;
begin
  result := DoGet(Item);

  if not result then
    release;
end;

function TSilVectorEnumerator.DoGet(out Item): Boolean;
begin
  result := false;
end;

function TSilVectorEnumerator.GetCurrent: Pointer;
begin
  result := DoGetCurrent;
end;

function TSilVectorEnumerator.DoGetCurrent: Pointer;
begin
  result := nil;
end;

function TSilVectorEnumerator.HasMore: Boolean;
begin
  if fstep > 0 then
    result := findex <= findexend
  else
    result := findex >= findexend;
end;

function TSilVectorEnumerator.Next: Boolean;
begin
  inc(findex, fstep);
  result := HasMore;
end;

procedure TSilVectorEnumerator.Purge;
begin
  Sil.Int.ArrayAdd(fpurge, findex - fstep);
end;

procedure TSilVectorEnumerator.DoPurgeClear;
var
  l, i: integer;
begin
  l := length(fpurge) - 1;

  if l > -1 then
  begin
    if fpurge[0] < fpurge[l] then
      for i := l downto 0 do
        fvector.Delete(fpurge[i])
    else
      for i := 0 to l do
        fvector.Delete(fpurge[i]);

    setlength(fpurge, 0);
  end;
end;

end.
