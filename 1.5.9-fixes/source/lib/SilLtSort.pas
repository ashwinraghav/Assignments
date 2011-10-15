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

unit SilLtSort;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiList,
  SilLiCompare;

type
  SortClass = class of SortTool;

  SortTool = class(Tool)
  private
    class procedure DoQuickSort(const List: IList; L, R: Integer; const Comparator: IComparator);
  public
    class function Custom(const This, Comparator: IUnknown): Boolean;
    class function Default(const This: IUnknown): Boolean;
    class function QuickSort(const List: IList; const Comparator: IComparator = nil; Locked: Boolean = false): Boolean;
  end;

implementation

uses
  SilLiSort,
  SilLtReference,
  SilLtLock;

class function SortTool.Custom(const This, Comparator: IUnknown): Boolean;
var
  SortObj: ISortable;
  CompObj: IComparator;
begin
  Result :=
    Reference.GetInterface(This, ISortable, SortObj) and
    Reference.GetInterface(Comparator, IComparator, CompObj) and
    SortObj.CustomSort(CompObj);
end;

class function SortTool.Default(const This: IUnknown): Boolean;
var
  SortObj: ISortable;
begin
  Result := Reference.GetInterface(This, ISortable, SortObj) and SortObj.Sort;
end;

class procedure SortTool.DoQuickSort(const List: IList; L, R: Integer; const Comparator: IComparator);

  function DoCompare(P1, P2: Pointer): Integer;
  begin
    Result := Comparator.Compare(P1, P2, Pointer(List));
  end;

var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while DoCompare(List.ItemPtr(I), List.ItemPtr(P)) < 0 do Inc(I);
      while DoCompare(List.ItemPtr(J), List.ItemPtr(P)) > 0 do Dec(J);
      if I <= J then
      begin
        List.Exchange(I, J);

        if P = I then P := J else
        if P = J then P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoQuickSort(List, L, J, Comparator);
    L := I;
  until I >= R;
end;

class function SortTool.QuickSort(const List: IList; const Comparator: IComparator; Locked: Boolean): Boolean;
var
  CompObj: IComparator;
begin
  if Locked then List.Locked;
  CompObj := Comparator;
  Result := (List.Count > 1) and ((CompObj <> nil) or (Reference.GetInterface(List, IComparator, CompObj)));
  if Result then DoQuickSort(List, 0, List.Count - 1, Comparator);
end;

end.
