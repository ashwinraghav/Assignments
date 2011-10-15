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

unit SilSmLayerSlot;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerLinkList;

type
  TSilLayerSlot = class (
    // extends
    TSilLayerLinkList,
    // implements
    ILayerSlot)
  private
    FIsActive: Boolean;
  protected // ILayerLinkList
    procedure Add(const Value: IUnknown); override;
    procedure Remove(const Value: IUnknown); override;
    procedure Insert(const Before, Value: IUnknown); override;
  protected // ILayerOperation
    procedure Write(const Command: ILayerCommand); override;
    procedure Read(const Command: ILayerCommand); override;
    procedure Receive(const Command: ILayerCommand); override;
  protected // ILayerControl
    function GetIsActive: Boolean; override;
    procedure Activate(const Context: IUnknown); override;
    procedure Deactivate(const Context: IUnknown; IsBroken: Boolean); override;
  protected // ILayerDuplicate
    function Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilStLayer,
  SilSmLayerLink,
  SilSmLayerCommand;

{ TSilLayerSlot }

destructor TSilLayerSlot.Destroy;
begin
  inherited;
end;

procedure TSilLayerSlot.Add(const Value: IInterface);
var
  ValueChk: IUnknown;
  Link: ILayerLink;
begin
  ValueChk := DoCheckValue(Value);

  if Ref.GetInterface(ValueChk, ILayerLink, Link) then
    Link.Lower := Self;

  DoAdd(ValueChk);
end;

procedure TSilLayerSlot.Insert(const Before, Value: IUnknown);
var
  Link: ILayerLink;
  ValueChk: IUnknown;
begin
  ValueChk := DoCheckValue(Value);

  if Ref.GetInterface(ValueChk, ILayerLink, Link) then
    Link.Lower := Self; 

  DoInsert(Before, ValueChk);
end;

procedure TSilLayerSlot.Remove(const Value: IInterface);
var
  Link: ILayerLink;
begin
  if List.IndexOf(Value) >= 0 then
  begin
    if Ref.GetInterface(Value, ILayerLink, Link) then
      Link.Lower := nil;

    inherited;
  end;
end;

function TSilLayerSlot.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TSilLayerSlot.Activate(const Context: IInterface);
var
  Enum: IEnumerator;
  Item: IUnknown;
  ItemLink: ILayerLink;
begin
  while Enumerate(Enum, Item) do
    if Ref.GetInterface(Item, ILayerLink, ItemLink) then
      ItemLink.Control.Activate(Context);

  FIsActive := true;
  inherited;
end;

procedure TSilLayerSlot.Deactivate(const Context: IInterface; IsBroken: Boolean);
var
  Enum: IEnumerator;
  Item: IUnknown;
  ItemLink: ILayerLink;
begin
  while Enumerate(Enum, Item) do
    if Ref.GetInterface(Item, ILayerLink, ItemLink) then
      ItemLink.Control.Deactivate(Context, IsBroken);

  FIsActive := false;
  inherited;
end;

function TSilLayerSlot.Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean;
var
  Enum: IEnumerator;
  Item: IUnknown;
  NewItem: IUnknown;
  NewList: ILayerLinkList;
begin
  NewList := TSilLayerSlot.Create(Parameters);
  
  while Enumerate(Enum, Item) do
    if DoDuplicate(Item, NewItem, Context) then
      NewList.Add(NewItem);

  Result := Assigned(NewList);
  if Result then Obj := NewList;
end;

procedure TSilLayerSlot.Read(const Command: ILayerCommand); 
begin
  Cmd.Read(Command);
end;

procedure TSilLayerSlot.Write(const Command: ILayerCommand);
begin
  Cmd.Write(Command);
end;

procedure TSilLayerSlot.Receive(const Command: ILayerCommand);
var
  Enum: IEnumerator;
  Item: IUnknown;
  Link: ILayerLink;
begin
  while Enumerate(Enum, Item) do
    if Ref.GetInterface(Item, ILayerLink, Link) then
    begin
      Command.Packet.Buffer.Position := 0;
      Cmd.Receive(Command, Link);
    end;
end;

end.
