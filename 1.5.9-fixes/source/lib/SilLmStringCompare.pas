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

unit SilLmStringCompare;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiCompare;

type 
  TStringCompare = class (
    // extends
    TSilInterfacedObject,
    // implements
    IComparable,
    IComparator)
  private
    FValue: String;
    FIgnoreCase: Boolean;
  protected // IComparable
    function CompareTo(const Item; Arg: Pointer = nil): Integer; virtual;
  protected // IComparator
    function Compare(const Item1, Item2; Arg: Pointer = nil): Integer; virtual;
  public
    constructor Create(IgnoreCase: Boolean = false; const Value: String = '');
  end;

implementation

uses
  SilBtText,
  SilBtStr;

{ TStringComparable }

constructor TStringCompare.Create(IgnoreCase: Boolean = false; const Value: String = '');
begin
  inherited Create;
  FValue := Value;
  FIgnoreCase := IgnoreCase;
end;

function TStringCompare.CompareTo(const Item; Arg: Pointer): Integer;
begin
  if FIgnoreCase then
    Result := Text.Compare(PChar(Item), FValue) else
    Result := Str.Compare(PChar(Item), FValue);
end;

function TStringCompare.Compare(const Item1; const Item2; Arg: Pointer): Integer;
begin
  if FIgnoreCase then
    Result := Text.Compare(PChar(Item1), PChar(Item2)) else
    Result := Str.Compare(PChar(Item1), PChar(Item2));
end;

end.
 