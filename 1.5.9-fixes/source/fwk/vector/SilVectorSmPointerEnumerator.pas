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

unit SilVectorSmPointerEnumerator;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector,
  SilVectorSkVector,
  SilVectorSkEnumerator;

type
  TSilVectorPointerEnumerator = class (TSilVectorEnumerator, IVectorPointerEnumerator)
  protected
    fvector: IVectorPointer;
  protected
    function DoGet(out Item): Boolean; override;
    function DoGetCurrent: Pointer; override;
  protected // IVectorPointerEnumerator
    function Enumerate(out Value: pointer): boolean;
  public
    constructor Create(const Vector: IVectorPointer; Step, IndexBegin, IndexEnd: integer);
  end;

implementation

{ TSilVectorPointerEnumerator }

constructor TSilVectorPointerEnumerator.Create(const Vector: IVectorPointer; Step, IndexBegin, IndexEnd: integer);
begin
  inherited create(vector, step, indexbegin, indexend);
  fvector := vector;
end;

function TSilVectorPointerEnumerator.Enumerate(out Value: pointer): boolean;
begin
  if fstep > 0 then
    Result := findex <= findexend
  else
    Result := findex >= findexend;

  if result then
  begin
    value := fvector.items[findex];
    inc(findex, fstep);
  end else
    release;
end;

function TSilVectorPointerEnumerator.DoGet(out Item): Boolean;
begin
  pointer(item) := fvector.items[findex];
  Result := true;
end;

function TSilVectorPointerEnumerator.DoGetCurrent: Pointer;
begin
  Result := fvector.items[findex];
end;

end.
