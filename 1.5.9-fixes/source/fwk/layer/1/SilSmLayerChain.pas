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

unit SilSmLayerChain;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerLinkList;

type
  TSilLayerChain = class (
    // extends
    TSilLayerLinkList,
    // implements
    ILayerChain)
  private
    FIsActive: Boolean;
    procedure DoCheckLower;
  protected // ILayerLink
    procedure SetLower(const Value: ILayerLink); override;
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
  end;

implementation

uses
  SilStLayer,
  SilSmLayerLink,
  SilSmLayerCommand;

{ TSilLayerChain }

procedure TSilLayerChain.DoCheckLower;
var
  Link: ILayerLink;
begin
  if (Lower <> nil) and GetFirst(ILayerLink, Link) then
    Link.Lower := Lower;
end;

procedure TSilLayerChain.SetLower(const Value: ILayerLink);
begin
  inherited;
  DoCheckLower;
end;

procedure TSilLayerChain.Add(const Value: IUnknown);
var
  ValueChk: IUnknown;
  Link, Item: ILayerLink;
begin
  ValueChk := DoCheckValue(Value);

  if Ref.GetInterface(ValueChk, ILayerLink, Link) then
  begin
    if GetLast(ILayerLink, Item) then
    begin
      Item.Upper := Link;
      Link.Lower := Item;
    end;

    Link.Chain := Self;
  end;

  DoAdd(ValueChk);
  if List.Count = 1 then DoCheckLower;
end;

procedure TSilLayerChain.Insert(const Before, Value: IUnknown);
var
  i: Integer;
  ValueChk: IUnknown;
  Link, Upper, Lower: ILayerLink;
begin
  ValueChk := DoCheckValue(Value);
  i := -1;

  if Ref.GetInterface(ValueChk, ILayerLink, Link) then
  begin
    if Assigned(Before) then
      i := List.IndexOf(Before) else
      i := 0;

    if i >= 0 then
    begin
      if (i > 0) and GetLast(ILayerLink, Lower, i) then
      begin
        Link.Lower := Lower;
        Lower.Upper := Link;
      end;

      if (i <= List.Count - 1) and GetFirst(ILayerLink, Upper, i) then
      begin
        Upper.Lower := Link;
        Link.Upper := Upper;
      end;

      Link.Chain := Self;
    end else
      Exit;
  end;

  DoInsert(Before, ValueChk);
  if i = 0 then DoCheckLower;
end;

procedure TSilLayerChain.Remove(const Value: IUnknown);
var
  i: Integer;
  Link: ILayerLink;
begin
  i := List.IndexOf(Value);

  if i >= 0 then
  begin
    if Ref.GetInterface(Value, ILayerLink, Link) then
    begin
      if Assigned(Link.Lower) then
      begin
        Link.Lower.Upper := Link.Upper;
        Link.Lower := nil;
      end;

      if Assigned(Link.Upper) then
      begin
        Link.Upper.Lower := Link.Lower;
        Link.Upper := nil;
      end;

      Link.Chain := nil;

      if Assigned(Link.Operation) then
        Sil.Sink.Disconnect(Link.Operation, Self);
    end;

    inherited;
    if i = List.Count then DoCheckLower;
  end;
end;

function TSilLayerChain.Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean;
var
  Enum: IEnumerator;
  Item: IUnknown;
  NewItem: IUnknown;
  NewList: ILayerLinkList;
begin
  NewList := TSilLayerChain.Create(nil);

  while Enumerate(Enum, Item) do
    if DoDuplicate(Item, NewItem, Context) then
      NewList.Add(NewItem);

  Result := Assigned(NewList);
  if Result then Obj := NewList;
end;

function TSilLayerChain.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TSilLayerChain.Activate(const Context: IUnknown);
var
  Enum: IEnumerator;
  Item: IUnknown;
  ItemLink: ILayerLink;
  Terminal: ILayerTerminal;
  Links: IInterfaceList;
  Terminals: IInterfaceList;
begin
  Links := Sil.List.InterfaceList;
  Links.AddList(Self.List);

  while Links.Enumerate(Enum, Item) do
    if Ref.GetInterface(Item, ILayerLink, ItemLink) then
    begin
      ItemLink.Control.Activate(Context);

      if Ref.GetInterface(ItemLink.Operation, ILayerTerminal, Terminal) then
      begin
        if not Assigned(Terminals) then
          Terminals := Sil.List.InterfaceList;

        if Terminals.Count = 0 then
          Terminals.Add(Terminal) else
          Terminals.Insert(0, Terminal);
      end;
    end;

  if Assigned(Terminals) then
    while Terminals.Enumerate(Enum, Item) do
      ILayerTerminal(Item).StartLayer(Context);

  FIsActive := true;
  inherited;
end;

procedure TSilLayerChain.Deactivate(const Context: IUnknown; IsBroken: Boolean);
var
  Enum: IEnumerator;
  Item: IUnknown;
  ItemLink: ILayerLink;
  Links: IInterfaceList;
begin
  Links := Sil.List.InterfaceList;
  Links.AddList(Self.List);

  while Links.Enumerate(Enum, Item) do
    if Ref.GetInterface(Item, ILayerLink, ItemLink) then
      ItemLink.Control.Deactivate(Context, IsBroken);

  FIsActive := false;
  inherited;
end;

procedure TSilLayerChain.Write(const Command: ILayerCommand);
var
  Link: ILayerLink;
begin
  if GetLast(ILayerLink, Link) then Cmd.Write(Command, Link);
end;

procedure TSilLayerChain.Read(const Command: ILayerCommand);
var
  Link: ILayerLink;
begin
  if GetLast(ILayerLink, Link) then Cmd.Read(Command, Link);
end;

procedure TSilLayerChain.Receive(const Command: ILayerCommand);
var
  Link: ILayerLink;
begin
  if GetFirst(ILayerLink, Link) then Cmd.Receive(Command, Link);
end;

end.
