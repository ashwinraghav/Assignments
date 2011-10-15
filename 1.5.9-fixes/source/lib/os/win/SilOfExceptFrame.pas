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

unit SilOfExceptFrame;

{$I Defines.inc}

interface

uses
  SilOeExceptFrame;

procedure Insert(Item, Target: PExceptFrame);
procedure Remove(Item: PExceptFrame);
function GetHead: PExceptFrame;
function SetHead(const Item: PExceptFrame): PExceptFrame;
function Equals(const Item, Target: PExceptFrame): Boolean;
function IsTail(const Item: PExceptFrame): Boolean;
function Hook(Item: PExceptFrame; const Handler: TExceptHandler): TExceptHandler;
procedure Unhook(Item: PExceptFrame; const Handler: TExceptHandler); 

implementation

uses
  SilAfThreadInfoBlock;
  
(*
  antes:
    Head --> Frame1 --> Frame2 --> Target --> FrameN --> (end)

  despues (si target <> nil):
    Head --> Frame1 --> Frame2 --> Item --> Target --> FrameN --> (end)

  despues (si target = nil):
    Head --> Item --> Frame1 --> Frame2 --> FrameN --> (end)
 *)

procedure Insert(Item, Target: PExceptFrame);
var
  Head: PExceptFrame;
  Prev: PExceptFrame;
  Curr: PExceptFrame;
begin
  ASSERT(Item <> nil);
  Head := GetHead();
  Prev := nil;
  Curr := Head;
  if Target = nil then
    Target := Head;
  while not IsTail(Curr)
    and not Equals(Curr, Target) do
  begin
    Prev := Curr;
    Curr := Curr.Next;
  end;
  if not IsTail(Curr) then
  begin
    if Prev <> nil then
      Prev.Next := Item;
    Item.Next := Curr;
    if Head = Target then
      SetHead(Item);
  end;
end;

(*
  antes:
    Head --> Frame1 --> Frame2 --> Item --> Target --> FrameN --> (end)

  despues:
    Head --> Frame1 --> Frame2 --> Target --> FrameN --> (end)
 *)

procedure Remove(Item: PExceptFrame);
var
  Head,
  Curr,
  Prev: PExceptFrame;
begin
  ASSERT( Item <> nil );
  Head := GetHead();
  
  Curr := Head;
  Prev := nil;
  
  while not IsTail(Curr)
    and not Equals(Curr, Item) do
  begin
    Prev := Curr;
    Curr := Curr.Next;
  end;

  if not IsTail(Curr) then
  begin
    Curr := Item.Next;

    if Prev <> nil then
      Prev.Next := Curr;

    Item.Next := nil;
    
    if Item = Head then
      SetHead(Curr);
  end;
end;

function GetHead: PExceptFrame;
begin
  Result := SilAfThreadInfoBlock.Get(0);
end;

function SetHead(const Item: PExceptFrame): PExceptFrame;
begin
  Result := SilAfThreadInfoBlock.Put(0, Item);
end;

function Equals(const Item, Target: PExceptFrame): Boolean;
begin
  Result := Assigned(Item) and Assigned(Target) and (@Item.Handler = @Target.Handler);
end;

function IsTail(const Item: PExceptFrame): Boolean;
begin
  Result := not Assigned(Item) or (Cardinal(Item) = Cardinal($FFFFFFFF));
end;

function Hook(Item: PExceptFrame; const Handler: TExceptHandler): TExceptHandler;
begin
  Result := Item.Handler;
  Item.Handler := Handler;
end;

procedure Unhook(Item: PExceptFrame; const Handler: TExceptHandler);
begin
  Item.Handler := Handler;
end;

end.

 