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

unit SilVtEnumerator;

{$I Defines.inc}

interface

uses
  Classes, DB, DBGrids,

  Sil,

  SilViDatasetEnumerator;

type
  EnumeratorClass = class of EnumeratorTool;
  EnumeratorTool = class(Tool)
    class function Create(DataSet: TDataSet; const Container: IUnknown = nil; const Locked: Boolean = False; const MoveFirst: Boolean = True; const Rows: IEnumerator = nil): IDatasetEnumerator; overload;
    class function Create(Bookmarks: TBookmarkList; const Locked: Boolean = false): IEnumerator; overload;
    class function Create(List: TList; const Locked: Boolean = false; const TypeHandler: HandlerType = nil; FreeOnDestroy: Boolean = False; const TypeData: Pointer = nil): IEnumerator; overload;
  end;

implementation

uses
  SilVmBookmarkEnumerator,
  SilVmDatasetEnumerator,
  SilVmListWrapper;

{ EnumeratorTool }

class function EnumeratorTool.Create(DataSet: TDataSet; const Container: IUnknown; const Locked: Boolean; const MoveFirst: Boolean; const Rows: IEnumerator): IDatasetEnumerator;
begin
  Result := TDatasetEnumerator.Create(DataSet, Container, Locked, Rows, MoveFirst);
end;

class function EnumeratorTool.Create(Bookmarks: TBookmarkList; const Locked: Boolean): IEnumerator;
begin
  Result := TBookmarkEnumerator.Create(Bookmarks, Locked);
end;

class function EnumeratorTool.Create(
        List: TList;
  const Locked: Boolean;
  const TypeHandler: HandlerType;
        FreeOnDestroy: Boolean;
  const TypeData: Pointer): IEnumerator;
begin
  Result := SIL.List.ListEnumerator(TListWrapper.Create(List, FreeOnDestroy), Locked, TypeHandler, TypeData);
end;

end.
