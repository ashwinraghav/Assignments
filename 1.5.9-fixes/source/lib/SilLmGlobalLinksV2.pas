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

unit SilLmGlobalLinksV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  
  SilLiEnumerator,
  SilLiGlobalServicesV2,
  SilLmInterfaceList;

type
  TSilGlobalLinksV2 = class(
    TSilInterfaceList,
    IGlobalLinksV2 )
  private
    FOwner: Pointer;
  private
    function DoGetOwner: IGlobalServiceV2;
  protected // IGlobalLinksV2
    function Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean; reintroduce; 
    procedure Add(const Item: IGlobalServiceV2); reintroduce;
    procedure Remove(const Item: IGlobalServiceV2); reintroduce; 
  public
    constructor Create(const Owner: IGlobalServiceV2); reintroduce;
    destructor Destroy; override;
  public
    property Owner: IGlobalServiceV2 read DoGetOwner;
  end;

implementation

{ TSilGlobalLinksV2 }

constructor TSilGlobalLinksV2.Create(const Owner: IGlobalServiceV2);
begin
  inherited Create;
  MakeRef(Owner, @FOwner);
end;

destructor TSilGlobalLinksV2.Destroy;
begin
  DropRef(@FOwner);
  inherited;
end;

function TSilGlobalLinksV2.Enumerate(var Enum: IEnumerator; out Item: IGlobalServiceV2): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

procedure TSilGlobalLinksV2.Add(const Item: IGlobalServiceV2);
begin
  inherited Add(Item);
end;

procedure TSilGlobalLinksV2.Remove(const Item: IGlobalServiceV2);
begin
  inherited Remove(Item);
end;

function TSilGlobalLinksV2.DoGetOwner: IGlobalServiceV2;
begin
  Result := IGlobalServiceV2(FOwner);
end;

end.
