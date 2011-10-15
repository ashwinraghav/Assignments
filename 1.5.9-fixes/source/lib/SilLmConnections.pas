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

unit SilLmConnections;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilLkAggregated,
  SilLiEnumerator,
  SilLiLock,
  SilLiConnection,
  SilLiInterfaceList,
  SilLiEventList;

type
  TSilConnections = class (
    TSilAggregatedObject,
    IEvents,
    IEventList,
    IConnectable,
    IConnectionSink )
  private
    FSinks: IEventList;
  protected //- IEvents
    function GetCount: Integer;
    function IndexOf(const Item: IUnknown): Integer;
    function Locked: ILock;
    function First: IUnknown;
    function Last: IUnknown;
    function Enumerate(var Enum: IEnumerator; out Item; const IID: TGUID): Boolean;
  protected //- IEventList
    function Add(const Item: IUnknown): Integer;
    procedure AddList(const Source: IEvents);
    function Remove(const Item: IUnknown): Integer;
    procedure Clear;
  protected //- IConnectable
    procedure IConnectable.AddListener = DoAddListener;
    procedure IConnectable.RemoveListener = DoRemoveListener;
    procedure DoAddListener(const Listener: IUnknown);
    procedure DoRemoveListener(const Listener: IUnknown);
  protected //- IConnectionSink
    function GetListeners: IEventList;
  public
    constructor Create(const Controller: IUnknown);
    destructor Destroy; override;
  end;

implementation

uses
  SilLmEventList;

{ TSilConnections }

constructor TSilConnections.Create(const Controller: IInterface);
begin
  inherited Create(Controller);
  FSinks := TEventList.Create;
end;

destructor TSilConnections.Destroy;
begin
  FSinks := nil;
  inherited;
end;

function TSilConnections.GetListeners: IEventList;
begin
  Result := FSinks;
end;

procedure TSilConnections.DoAddListener(const Listener: IInterface);
begin
  FSinks.Add(Listener);
end;

procedure TSilConnections.DoRemoveListener(const Listener: IInterface);
begin
  FSinks.Remove(Listener);
end;

function TSilConnections.GetCount: Integer;
begin
  Result := FSinks.Count;
end;

function TSilConnections.Enumerate(var Enum: IEnumerator; out Item; const IID: TGUID): Boolean;
begin
  Result := FSinks.Enumerate(Enum, Item, IID);
end;

function TSilConnections.First: IUnknown;
begin
  Result := FSinks.First;
end;

function TSilConnections.Last: IUnknown;
begin
  Result := FSinks.Last;
end;

function TSilConnections.IndexOf(const Item: IInterface): Integer;
begin
  Result := FSinks.IndexOf(Item);
end;

function TSilConnections.Locked: ILock;
begin
  Result := FSinks.Locked;
end;

function TSilConnections.Add(const Item: IInterface): Integer;
begin
  Result := FSinks.Add(Item);
end;

procedure TSilConnections.AddList(const Source: IEvents);
begin
  FSinks.AddList(Source);
end;

function TSilConnections.Remove(const Item: IInterface): Integer;
begin
  Result := FSinks.Remove(Item);
end;

procedure TSilConnections.Clear;
begin
  FSinks.Clear;
end;

end.
