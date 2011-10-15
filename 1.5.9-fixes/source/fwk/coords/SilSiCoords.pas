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

unit SilSiCoords;

interface

{$include Defines.inc}

uses
  Sil,
  SilSeCoords;

type
  ICoordsMapping = interface;

  TMappingChangedEvent = record
    Sender: ICoordsMapping;
  end;

  ICoordsMappingEvents = interface
    ['{53778824-C2FA-11D4-9DFD-00C0DFE46337}']
    procedure OnMappingChanged(const Event: TMappingChangedEvent);
  end;

  ICoordsMapping = interface
    ['{53778823-C2FA-11D4-9DFD-00C0DFE46337}']
    function GetClientMax: ClientType;
    function GetClientMin: ClientType;
    function GetClientSize: ClientType;
    function GetWorldMax: WorldType;
    function GetWorldMin: WorldType;
    function GetWorldSize: WorldType;
    procedure SetClientMax(const Value: ClientType);
    procedure SetClientMin(const Value: ClientType);
    procedure SetClientSize(const Value: ClientType);
    procedure SetWorldMax(const Value: WorldType);
    procedure SetWorldMin(const Value: WorldType);
    procedure SetWorldSize(const Value: WorldType);
    function Normalize: ICoordsMapping;
    property WorldMin: WorldType read GetWorldMin write SetWorldMin;
    property WorldMax: WorldType read GetWorldMax write SetWorldMax;
    property WorldSize: WorldType read GetWorldSize write SetWorldSize;
    property ClientMin: ClientType read GetClientMin write SetClientMin;
    property ClientMax: ClientType read GetClientMax write SetClientMax;
    property ClientSize: ClientType read GetClientSize write SetClientSize;
  end;

  TTicKind = (tkShort, tkLong);
  
  ITicData = interface
    ['{53778825-C2FA-11D4-9DFD-00C0DFE46337}']
    function GetPoint: WorldType;
    function GetKind: TTicKind;
    function GetCaption: string;
    property Point: WorldType read GetPoint;
    property Kind: TTicKind read GetKind;
    property Caption: string read GetCaption;
  end;

  ICoordsMapper = interface
    ['{53778821-C2FA-11D4-9DFD-00C0DFE46337}']
    function GetMapping: ICoordsMapping;
    function WorldToClient(const Point: WorldType): ClientType;
    function ClientToWorld(const Point: ClientType): WorldType;
    function IsPointValid(const Point: WorldType): Boolean;
    function Tics: IEnumeration;
    property Mapping: ICoordsMapping read GetMapping;
  end;

  ITicDataDef = interface(ITicData)
    procedure SetPoint(const Value: WorldType);
    procedure SetKind(const Value: TTicKind);
    procedure SetCaption(const Value: string);
    function GetNext: WorldType;
    procedure SetNext(const Value: WorldType);
    property Point: WorldType read GetPoint write SetPoint;
    property Kind: TTicKind read GetKind write SetKind;
    property Caption: string read GetCaption write SetCaption;
    property Next: WorldType read GetNext write SetNext;
  end;

implementation
end.
