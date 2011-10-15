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

unit SilLtSearch;

{$I Defines.inc}

interface

uses
  SilBkTool;
  
type
  SearchClass = class of Search;

  Search = class(Tool)
    class function Custom(const This, Comparator: IUnknown; var Index: Integer): Boolean;
    class function Default(const This: IUnknown; const Value: Variant; var Index: Integer): Boolean; overload;
    class function Default(const This: IUnknown; const Value: Pointer; var Index: Integer): Boolean; overload;
  end;

implementation

uses
  SilLtReference,
  SilLiSearch,
  SilLiCompare,
  SilBtVart;

class function Search.Custom(const This, Comparator: IUnknown; var Index: Integer): Boolean;
var
  SearchObj: ISearchable;
  CompObj: IComparable;
begin
  Result :=
    Reference.GetInterface(This, ISearchable, SearchObj) and
    Reference.GetInterface(Comparator, IComparable, CompObj) and
    SearchObj.CustomSearch(CompObj, Index);
end;

class function Search.Default(const This: IUnknown; const Value: Variant; var Index: Integer): Boolean;
var
  SearchObj: ISearchable;
begin
  Result := Reference.GetInterface(This, ISearchable, SearchObj) and SearchObj.Search(Value, Index);
end;

class function Search.Default(const This: IUnknown; const Value: Pointer; var Index: Integer): Boolean;
var
  SearchObj: ISearchable;
begin
  Result := Reference.GetInterface(This, ISearchable, SearchObj) and SearchObj.Search(Vart.FromPtr(Value), Index);
end;

end.
