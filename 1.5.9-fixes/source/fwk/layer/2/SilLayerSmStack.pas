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

unit SilLayerSmStack;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  SilLayerSiGlobal,
  SilLayerSkLayer;

type
  TSilLayerStack = class (TSilLayer, ILayerStack)
  private
    FList: IVectorInterface;
  private
    procedure DoDeactivateFinalize(Finalize: boolean; const Context: IUnknown = nil; Manual: Boolean = true);
  protected // ILayerStack
    procedure Add(const Value: ILayer);
    procedure Remove(const Value: ILayer);
    procedure Insert(Index: Integer; const Value: ILayer);
    procedure Clear;
    function GetToken: ILayerToken;
    function GetLowerLink(out Item: ILayer; const Ref: ILayer = nil): Boolean;
    function GetUpperLink(out Item: ILayer; const Ref: ILayer = nil): Boolean;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLayerSmToken;

{ TSilLayerStack }

constructor TSilLayerStack.Create;
begin
  inherited Create;
  FList := SilVector.InterfaceList(true);
end;

destructor TSilLayerStack.Destroy;
begin
  if FList <> nil then
  begin
    FList.Clear;
    FList := nil;
  end;

  inherited;
end;

procedure TSilLayerStack.Add(const Value: ILayer);
begin
  try
    FList.Lock;
    FList.Add(Value);
    Value.Stack := Self;
  finally
    FList.Unlock;
  end;

  if GetIsActive then
    Value.Status.Activate;
end;

procedure TSilLayerStack.Clear;
begin
  DoDeactivateFinalize(true, nil, false);
  FList.Clear;
end;

procedure TSilLayerStack.Insert(Index: Integer; const Value: ILayer);
begin
  try
    FList.Lock;
    FList.Insert(Index, Value);
  finally
    FList.Unlock;
  end;
end;

procedure TSilLayerStack.Remove(const Value: ILayer);
begin
  try
    FList.Lock;
    Value.Stack := nil;
    FList.Remove(Value);
  finally
    FList.Unlock;
  end;
end;

procedure TSilLayerStack.DoActivate(const Context: IInterface);
var
  list: IVectorInterface;
  item: ILayer;
begin
  list := SilVector.InterfaceList;

  with flist.enumerator do while enumerate(item) do
    list.add(item);

  with list.enumerator do while enumerate(item) do
    item.Status.Activate;
end;

procedure TSilLayerStack.DoDeactivateFinalize(Finalize: boolean; const Context: IInterface; Manual: Boolean);
var
  list: IVectorInterface;
  item: ILayer;
begin
  list := SilVector.InterfaceList;

  with flist.enumerator do while enumerate(item) do
    list.add(item);

  with list.enumerator do while enumerate(item) do
    item.Status.Deactivate;

  if Finalize then
    with list.enumerator do while enumerate(item) do
      item.Status.Finalize;
end;

procedure TSilLayerStack.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  DoDeactivateFinalize(false, context, manual);
end;

function TSilLayerStack.DoGetIsActive: Boolean;
var
  Enum: IVectorInterfaceEnumerator;
  Items: ILayer;
begin
  Result := true;
  Enum := FList.Enumerator;

  while Enum.Enumerate(Items) do
    if not Items.Status.IsActive then
    begin
      Result := false;
      break;
    end;
end;

function TSilLayerStack.GetLowerLink(out Item: ILayer; const Ref: ILayer): Boolean;
var
  i: integer;
begin
  try
    FList.Lock;

    if Ref <> nil then
    begin
      i := FList.IndexOf(Ref);
      if i > 0 then
        Item := ILayer(FList[i - 1])
      else
        Item := GetLower;
    end else
    if FList.Count > 0 then
      Item := ILayer(FList.First);
  finally
    FList.Unlock;
  end;

  Result := Item <> nil;
end;

function TSilLayerStack.GetUpperLink(out Item: ILayer; const Ref: ILayer): Boolean;
var
  i: integer;
begin
  try
    FList.Lock;

    if Ref <> nil then
    begin
      i := FList.IndexOf(Ref);
      if i < FList.Count - 1 then
        Item := ILayer(FList[i + 1])
      else
        Item := GetUpper;
    end else
    if FList.Count > 0 then
      Item := ILayer(FList.Last);
  finally
    FList.Unlock;
  end;

  Result := Item <> nil;
end;

function TSilLayerStack.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Result := GetLowerLink(Link, nil);
end;

function TSilLayerStack.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Result := GetUpperLink(Link, nil);
end;

function TSilLayerStack.GetToken: ILayerToken;
begin
  Result := TSilLayerToken.Create;
end;

end.
