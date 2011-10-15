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

unit SilSkSharedFactory;

{$INCLUDE Defines.inc}

interface

uses
  SilSkSharedObject,
  SilSiSharedObject;

type
  TSilSharedFactory = class(
    TSilSharedObject,
    ISharedFactory )
  private
    FProgID: string;
    FClassID: TGUID;
  protected // ISharedObject
    function GetClassID: TGUID; override;
  protected // ISharedFactory
    function GetProgID: string; override;
    function CreateObject(const Owner: IUnknown; const IID: TGuid; out Instance; const Controller: IUnknown = nil; Param: Pointer = nil): Boolean; virtual; abstract;     
  public
    constructor Create(const ClassID: TGUID; const ProgID: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TSilSharedFactory }

constructor TSilSharedFactory.Create(const ClassID: TGUID; const ProgID: string);
begin
  inherited Create;
  FClassID := ClassID;
  FProgID := ProgID;
end;

destructor TSilSharedFactory.Destroy;
begin
  inherited;
end;

function TSilSharedFactory.GetClassID: TGUID;
begin
  Result := FClassID;
end;

function TSilSharedFactory.GetProgID: string;
begin
  Result := FProgID;
end;

end.
 