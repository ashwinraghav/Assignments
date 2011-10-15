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

unit SilSkCoordsMapper;

interface

uses
  Sil,
  SilSeCoords,
  SilSiCoords;

type
  TCoordsMapperType = class of TCoordsMapper;
  
  TCoordsMapper = class(
  //extends
    TSilInterfacedObject,
  //implements
    ICoordsMapper,
    ICoordsMappingEvents,
    IEnumerable )
  private
    FMapping: ICoordsMapping;
  protected // ICoordsMapper
    function GetMapping: ICoordsMapping;
    function Tics: IEnumeration;
  protected // ICoordsMappingEvents
    procedure OnMappingChanged(const Event: TMappingChangedEvent);
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  protected
    procedure Update(const Mapping: ICoordsMapping); virtual; abstract;
    function SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping; virtual;
  public
    constructor Create(const Mapping: ICoordsMapping = nil); virtual;
    destructor Destroy; override;
    function WorldToClient(const Point: WorldType): ClientType; virtual; abstract;
    function ClientToWorld(const Point: ClientType): WorldType; virtual; abstract;
    function IsPointValid(const Point: WorldType): Boolean; virtual;
    function CalcNextTic(var Tic: ITicDataDef): Boolean; virtual; abstract;
    property Mapping: ICoordsMapping read FMapping;
  end;

implementation

uses
  SilSmCoordsMapping,
  SilSmTicsEnumerator;
  
{ TCoordsMapper }

constructor TCoordsMapper.Create(const Mapping: ICoordsMapping);
begin
  inherited Create;
  if Mapping = nil then
    FMapping := TCoordsMapping.Create else
    FMapping := Mapping;
  Sil.Sink.Connect(FMapping, Self);
  FMapping := SetupMapping(FMapping);
end;

destructor TCoordsMapper.Destroy;
begin
  Sil.Sink.Disconnect(FMapping, Self);
  inherited;
end;

function TCoordsMapper.GetMapping: ICoordsMapping;
begin
  Result := FMapping;
end;

function TCoordsMapper.IsPointValid(const Point: WorldType): Boolean;
begin
  Result := true;
end;

procedure TCoordsMapper.OnMappingChanged(const Event: TMappingChangedEvent);
begin
  Update(Event.Sender);  
end;

function TCoordsMapper.SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping; 
begin
  Result := Mapping;
end;

function TCoordsMapper.Tics: IEnumeration;
begin
  Result := Sil.Tk.Enumeration(Self);
end;

function TCoordsMapper.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  Result := False;
end;

function TCoordsMapper.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := true;
  Enum := TTicsEnumerator.Create(Self); 
end;

end.
