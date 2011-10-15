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

unit SilVmDatasetEnumerator;

{$I Defines.inc}

interface

uses
  Db,

  Sil,
  SilClasses,

  SilViDatasetEnumerator;

type
  TDatasetEnumerator = class(
  //- extends
    TAbstractEnumerator,
  //- implements
    IDatasetEnumerator )
  private
    FRows: IEnumerator;
  protected
    FDataSet: TDataSet;
    procedure GetItem;
  protected //- TAbstractEnumerator
    function DoHasMore: Boolean; override;
    function Get(out Item): Boolean; override;
    function GetCurrent: Pointer; override;
    function Next: Boolean; override;
    procedure DoReset; override;
  protected //- IEnumerator
    procedure IDatasetEnumerator.Reset = Reset; 
    procedure Reset(const MoveFirst: Boolean = True); overload;
  public
    constructor Create(ADataSet: TDataSet; const Container: IUnknown = nil; const Locked: Boolean = False; const Rows: IEnumerator = nil; const MoveFirst: Boolean = True); reintroduce;
  end;

implementation

{ TDatasetEnumerator }

constructor TDatasetEnumerator.Create(ADataSet: TDataSet; const Container: IUnknown; const Locked: Boolean; const Rows: IEnumerator; const MoveFirst: Boolean);
begin
  Assert(ADataSet <> nil);
  inherited Create(Container, Locked);
  FRows := Rows;
  FDataSet := ADataSet;
  Reset(not Assigned(Rows) and MoveFirst);
end;

function TDatasetEnumerator.DoHasMore: Boolean;
begin
  if Assigned(FRows)
    then Result := FRows.HasMore
    else Result := not FDataSet.Eof;
end;

function TDatasetEnumerator.Get(out Item): Boolean;
begin
  GetItem;
  if Assigned(FContainer) then
    IUnknown(Item) := FContainer else
    Pointer(Item) := FDataSet;
  Result := True;
end;

function TDatasetEnumerator.GetCurrent: Pointer;             
begin
  Get(Result);
end;

function TDatasetEnumerator.Next: Boolean;
begin
  if HasMore then
    begin
      if Assigned(FRows)
        then FRows.Next
        else FDataSet.Next;
      Result := HasMore;
    end
  else
    Result := False;
end;

procedure TDatasetEnumerator.Reset(const MoveFirst: Boolean = True);
begin
  inherited Reset;
  if MoveFirst then FDataSet.First;
end;

procedure TDatasetEnumerator.GetItem;
  var BM: TBookmarkStr;
begin
  if Assigned(FRows) and FRows.Get(BM) then
    FDataSet.Bookmark := BM;
end;

procedure TDatasetEnumerator.DoReset;
begin
  inherited;
  GetItem;
end;

end.
