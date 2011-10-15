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

unit SilLmEnumeration;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiEnumerator;

type
  TEnumeration = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IEnumeration )
  private
    FEnum: IEnumerator;
    FInit: Boolean;
  protected //- IEnumeration
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  public
    constructor Create(const Enum: IEnumerator);
    destructor Destroy; override;
  end;

implementation

{ TEnumeration }

constructor TEnumeration.Create(const Enum: IEnumerator);
begin
  inherited Create;
  FEnum := Enum;
  FInit := True;
end;

destructor TEnumeration.Destroy;
begin
  FEnum := nil;
  inherited;
end;

function TEnumeration.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  if FEnum <> nil then
    begin
      if FInit then
        begin
          Result := FEnum.HasMore;
          FInit := False;
          Enum := FEnum;
        end
      else
          Result := FEnum.Next;
      if Result then
        FEnum.Get(Item);
    end
  else
      Result := False;
end;

end.
