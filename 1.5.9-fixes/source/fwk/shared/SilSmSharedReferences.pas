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

unit SilSmSharedReferences;

{$INCLUDE Defines.inc}

interface

uses
  Sil,

  SilLmPointerList,
  SilShSharedObject;

type
  TSilObjectReferences = class (
    TSilPointerList,
    ISharedObjectReferences )
  private
    FReferences: IPointerList;
  protected // ISharedObjectReferences  
    procedure AddRef(Ref: PUnknown);
    function DropRef(Ref: PUnknown): Boolean; reintroduce;
    function Contains(Ref: PUnknown): Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TSilObjectReferences }

constructor TSilObjectReferences.Create;
begin
  inherited Create;
  FReferences := Sil.List.PointerList;
end;

procedure TSilObjectReferences.AddRef(Ref: PUnknown);
begin
  inherited Add(Ref);
end;

function TSilObjectReferences.DropRef(Ref: PUnknown): Boolean;
begin
  Result := inherited Remove(Ref) >= 0;
end;

destructor TSilObjectReferences.Destroy;
begin
  FReferences := nil;
  inherited;
end;

function TSilObjectReferences.Contains(Ref: PUnknown): Boolean;
begin
  Result := inherited IndexOf(Ref) >= 0;
end;

end.
 