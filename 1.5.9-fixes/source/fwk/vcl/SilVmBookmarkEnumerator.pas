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

unit SilVmBookmarkEnumerator;

{$I Defines.inc}

interface


uses
  Db, DBGrids, 
  
  SilClasses;

type
  TBookmarkEnumerator = class(TAbstractEnumerator)
  protected
    FLast: TBookmarkStr;
    FBookmarks: TBookmarkList;
    procedure GetItem;
  protected //- TAbstractEnumerator
    function DoHasMore: Boolean; override;
    function Get(out Item): Boolean; override;
    function GetCurrent: Pointer; override;
    function Next: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(ABookmarks: TBookmarkList; const Locked: Boolean = false); reintroduce;
  end;

implementation

uses
  Sil;

{ TBookmarkEnumerator }

constructor TBookmarkEnumerator.Create(ABookmarks: TBookmarkList; const Locked: Boolean = false);
begin
  Assert(Assigned(ABookmarks));
  inherited Create(nil, Locked);
  FBookmarks := ABookmarks;
end;

function TBookmarkEnumerator.Next: Boolean;
begin
  Inc(FIteration);
  Result := HasMore;
  if Result then
    GetItem else
    Detach;
end;

function TBookmarkEnumerator.Get(out Item): Boolean;
begin
  TBookmarkStr(Item) := FLast;
  Result := True;
end;

function TBookmarkEnumerator.GetCurrent: Pointer;
begin
  Get(Result);
end;

function TBookmarkEnumerator.DoHasMore: Boolean;
begin
  Result := FIteration < FBookmarks.Count;
end;

procedure TBookmarkEnumerator.DoReset;
begin
  inherited;
  GetItem;
end;

procedure TBookmarkEnumerator.GetItem;
begin
  FLast := FBookmarks[FIteration];
end;

end.
