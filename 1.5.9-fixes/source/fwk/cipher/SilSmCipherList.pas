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

unit SilSmCipherList;

{$include Defines.inc}

interface

uses
  Sil,
  SilSiCipher,
  SilSiCipherList;

type
  TCipherList = class (
    // extends
    TSilObject,
    // implements
    ICipherList)
  private
    FList: IStringList;
  protected // ICipherList
    function Add(const Value: String; Ptr: Pointer = nil): Integer;
    function GetMethod(const Name: String): ICipher;
    function Enumerate(var Enum: IEnumerator; out Item: string): Boolean; overload;
  public
    constructor Create;
  end;

implementation

{ TCipherList }

function TCipherList.Add(const Value: String; Ptr: Pointer): Integer;
begin
  Result := FList.Add(Value, Ptr);
end;

constructor TCipherList.Create;
begin
  inherited Create;

  FList := Sil.List.StringList;
  FList.IgnoreCase := true;
end;

function TCipherList.Enumerate(var Enum: IEnumerator; out Item: string): Boolean;
begin
  Result := FList.Enumerate(Enum, Item);
end;

function TCipherList.GetMethod(const Name: String): ICipher;
var
  i: Integer;
begin
  i := FList.IndexOf(Name);

  if i >= 0 then
    TClass(FList.Ptrs[i]).Create.GetInterface(ICipher, Result) else
    Result := nil;
end;

end.
 