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

unit SilLfGlobalServiceV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiGlobalServicesV2;

procedure DoInitialize(const Item: IGlobalServiceV2);
procedure DoFinalize(const Item: IGlobalServiceV2);
function DoCreate(const Item: IGlobalServiceV2): IUnknown;
procedure DoDestroy(const Item: IGlobalServiceV2);
procedure DoRefAdded(const Item: IGlobalServiceV2; const Ref: PUnknown);
procedure DoRefRemoved(const Item: IGlobalServiceV2; const Ref: PUnknown);
procedure DoLinkAdded(const Item: IGlobalServiceV2; const Link: IGlobalServiceV2);
procedure DoLinkRemoved(const Item: IGlobalServiceV2; const Link: IGlobalServiceV2);

implementation

procedure DoInitialize(const Item: IGlobalServiceV2);
begin
  with Item do
  begin
    if Assigned(Service.Initialize) then
      Service.Initialize(Service, List.Owner);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceInitialize(Item);
  end;
end;

procedure DoFinalize(const Item: IGlobalServiceV2);
begin
  with Item do
  begin
    if Assigned(Service.Finalize) then
      Service.Finalize(Service, List.Owner);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceFinalize(Item);
  end;
end;

function DoCreate(const Item: IGlobalServiceV2): IUnknown;
begin
  with Item do
  begin
    if not IsCreated and Assigned(Service.Create) then
      Result := Service.Create(Item.Service, List.Owner) else
      Result := nil; { TODO : !!! Shared Objectizar }
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceCreate(Item);
  end;
end;

procedure DoDestroy(const Item: IGlobalServiceV2);
begin
  with Item do
  begin
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceDestroy(Item);
    if IsCreated and Assigned(Service.Destroy) then
      Service.Destroy(Service, List.Owner);
  end;
end;

procedure DoRefAdded(const Item: IGlobalServiceV2; const Ref: PUnknown);
begin
  with Item do
  begin
    if Assigned(Service.RefAdded) then
      Service.RefAdded(Service, List.Owner, Ref);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceAddRef(Item, Ref);
  end;
end;
   
procedure DoRefRemoved(const Item: IGlobalServiceV2; const Ref: PUnknown);
begin
  with Item do
  begin
    if Assigned(Service.RefRemoved) then
      Service.RefRemoved(Service, List.Owner, Ref);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceDropRef(Item, Ref);
  end;
end;

procedure DoLinkAdded(const Item: IGlobalServiceV2; const Link: IGlobalServiceV2);
begin
  with Item do
  begin
    if Assigned(Service.LinkAdded) then
      Service.LinkAdded(Service, List.Owner, Link.Service);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceLink(Item, Link);
  end;
end;

procedure DoLinkRemoved(const Item: IGlobalServiceV2; const Link: IGlobalServiceV2);
begin
  with Item do
  begin
    if Assigned(Service.LinkRemoved) then
      Service.LinkRemoved(Service, List.Owner, Link.Service);
    with List do
      if Assigned(Hook) then
        IGlobalServicesHookV2(Hook).ServiceUnlink(Item, Link);
  end;
end;

end.
