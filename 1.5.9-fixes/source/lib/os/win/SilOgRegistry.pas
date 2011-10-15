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

unit SilOgRegistry;

{$I Defines.inc}

interface

uses
  Windows,
  SilLiKey,
  SilOeRegistry;

const
  GKeyPermisions: array [TNamedKeyPermision] of Integer =
    ( KEY_READ,
      KEY_WRITE,
      KEY_ALL_ACCESS );
      
const
  GNotificationFilters: array[TNamedKeyNotificationFilter] of Integer =
    ( REG_NOTIFY_CHANGE_NAME,
      REG_NOTIFY_CHANGE_ATTRIBUTES,
      REG_NOTIFY_CHANGE_LAST_SET,
      REG_NOTIFY_CHANGE_SECURITY );

function FiltersToInteger(const Filters: TNamedKeyNotificationFilters): Integer;

implementation

function FiltersToInteger(const Filters: TNamedKeyNotificationFilters): Integer;
var
  Filter: TNamedKeyNotificationFilter;
begin
  Result := 0;
  for Filter := Low(Filter) to High(Filter) do
    if Filter in Filters then
      Inc(Result, GNotificationFilters[Filter]);
end;

end.
