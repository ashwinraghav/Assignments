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

unit SilLmConnectableExtension;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiConnection,
  SilLiEventList,
  SilLkAggregable;

type
  TSilConnectableExtension = class(
    TSilAggregableObject,
    IConnectable,
    IConnections )
  private
    FList: IUnknown;
    FEventList: Pointer;
    FConnectable: Pointer;
  private
    procedure DoCheck;
    procedure DoCreate;
    procedure DoRelease;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean = true);
    procedure RemoveListener(const Listener: IUnknown);
  protected // IConnections
    function GetHasConnections: Boolean;
    function GetEventList: IEventList;
  protected
    property List: IEventList read GetEventList;
  public
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); override; 
    destructor Destroy; override;
  end;

implementation

uses
  SilLmEventList,
  SilLtReference; 
  
{ TSilConnectableExtension }

constructor TSilConnectableExtension.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
  if Assigned(Param) then
    Ref.Get(IUnknown(Param), IConnectable, @FConnectable);
end;

destructor TSilConnectableExtension.Destroy;
begin
  DoRelease;
  FConnectable := nil;
  inherited;
end;

procedure TSilConnectableExtension.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  DoCheck;
  List.AddListener(Listener, KeepRef);
  if Assigned(FConnectable) then IConnectable(FConnectable).AddListener(Listener, KeepRef);
end;

procedure TSilConnectableExtension.RemoveListener(const Listener: IInterface);
var
  Connectable: IConnectable;
  List: IEventList;
begin
  if Assigned(FConnectable) then
  begin
    Connectable := IConnectable(FConnectable);
    try
      Connectable.RemoveListener(Listener)
    finally
      Connectable := nil;
    end;
  end;

  if Assigned(FEventList) then
  begin
    List := IEventList(FEventList);
    try
      List.RemoveListener(Listener);
    finally
      if List.Count = 0 then
      try
        List := nil;
      finally
        DoRelease;
      end;
    end;
  end;
end;

function TSilConnectableExtension.GetHasConnections: Boolean;
begin
  Result := Assigned(FList) and (List.Count > 0);
end;

function TSilConnectableExtension.GetEventList: IEventList;
begin
  Result := IEventList(FEventList);
end;

procedure TSilConnectableExtension.DoCheck;
begin
  if not Assigned(FList) then
    DoCreate;
end;

procedure TSilConnectableExtension.DoCreate;
begin
  FList := TEventList.Create(Self);
  FEventList := Pointer(FList as IEventList);
end;

procedure TSilConnectableExtension.DoRelease;
begin
  FEventList := nil;
  FList := nil;
end;

end.
