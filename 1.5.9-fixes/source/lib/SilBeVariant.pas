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

unit SilBeVariant;

{$I Defines.inc}

interface

type
  TVariant = {packed} object
  private
    FData: TVarData;
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
    function GetKind: Word;
    function GetIsOK: Boolean;
    function GetIsEmpty: Boolean;
    function GetIsNull: Boolean;
  public
    property Value: Variant read GetValue write SetValue;
    property Kind: Word read GetKind;
    property IsOK: Boolean read GetIsOK;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsNull: Boolean read GetIsNull;
    procedure Clear;
    procedure Copy(V: TVariant);
    procedure CopyTo(var V: TVariant);
  end;

implementation

uses
  SilBtVart;

procedure TVariant.Clear;
begin
  VarClear(Variant(FData));
end;

procedure TVariant.Copy(V: TVariant);
begin
  VarCopy(Variant(FData), V.Value);
end;

procedure TVariant.CopyTo(var V: TVariant);
begin
  VarCopy(Variant(V.FData), Value);
end;

function TVariant.GetValue: Variant;
begin
  Result := Variant(FData);
end;

procedure TVariant.SetValue(const AValue: Variant);
begin
  Variant(FData) := AValue;
end;

function TVariant.GetKind: Word;
begin
  Result := FData.VType;
end;

function TVariant.GetIsOK: Boolean;
begin
  Result := Vart.IsOK(Variant(FData));
end;

function TVariant.GetIsEmpty: Boolean;
begin
  Result := Vart.IsEmpty(Variant(FData));
end;

function TVariant.GetIsNull: Boolean;
begin
  Result := Vart.IsNull(Variant(FData))
end;


end.
