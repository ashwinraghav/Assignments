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

unit SilOfFilesystemNotifier;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOeFilesystemNotifier;

function FiltersToValue(const Value: TFilesystemChangeFilters): LongWord;

implementation

uses
  Windows;

const
	MFiltersMapping: array [TFilesystemChangeFilter] of Integer =
    (
      FILE_NOTIFY_CHANGE_FILE_NAME,
      FILE_NOTIFY_CHANGE_DIR_NAME,
      FILE_NOTIFY_CHANGE_ATTRIBUTES,
      FILE_NOTIFY_CHANGE_SIZE,
      FILE_NOTIFY_CHANGE_LAST_WRITE,
      FILE_NOTIFY_CHANGE_LAST_ACCESS,
      FILE_NOTIFY_CHANGE_SECURITY
    );

function FiltersToValue(const Value: TFilesystemChangeFilters): LongWord;
var
  Filter: TFilesystemChangeFilter;
begin
  Result := 0;
  for Filter := Low(Filter) to High(Filter) do
    if Filter in Value then
      Inc(Result, MFiltersMapping[Filter]);
end;



end.
