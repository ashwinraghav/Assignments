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

unit SilLkExtensible;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkAggregable,
  SilLhAggregation;

type
  TSilExtensibleObject = class(TSilAggregableObject)
  private
    procedure DoCheckExtensions;
  protected // IExtensible
    function GetExtension: IAggregation;
  protected
    property Extension: IAggregation read GetExtension;
  public
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilOsTypes,
  SilLkAggregation;

{ TSilExtensibleObject }

constructor TSilExtensibleObject.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
end;

destructor TSilExtensibleObject.Destroy;
begin
  if HasExtension then inherited Extension := nil;
  inherited;
end;

function TSilExtensibleObject.GetExtension: IAggregation;
begin
  DoCheckExtensions;
  Result := inherited Extension as IAggregation;
end;

procedure TSilExtensibleObject.DoCheckExtensions;
begin
  if not HasExtension then
    inherited Extension := TSilAggregationObject.Create(MainInterface, Self);
end;

end.
