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

unit SilSmDataRowsetEnumerator;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiDataAccess,
  SilLkAbstractEnumerator;

type
  TSilDataRowsetEnumerator = class(TAbstractEnumerator)
  private
    FData: IDataRowset;
    FFields: IUnknown;
  protected 
    function DoHasMore: Boolean; override;
    function GetCurrent: Pointer; override;
    function Next: Boolean; override;
  protected
    procedure DoDetach; override;
    procedure DoReset; override;
  public
    constructor Create(const DataRowset: IDataRowset; const Fields: IUnknown = nil; Locked: Boolean = False); reintroduce; 
    destructor Destroy; override;
  end;

implementation

{ TSilDataRowsetEnumerator }

constructor TSilDataRowsetEnumerator.Create(const DataRowset: IDataRowset; const Fields: IUnknown; Locked: Boolean);
begin
  inherited Create(DataRowset, Locked, InterfaceHandler);
  FData := DataRowset;
  if Assigned(Fields) then
    FFields := Fields
  else if Assigned(FData) then
    FFields := FData.Fields; 
end;

destructor TSilDataRowsetEnumerator.Destroy;
begin
  FFields := nil;
  FData := nil;
  inherited;
end;

procedure TSilDataRowsetEnumerator.DoDetach;
begin
  FData := nil;
  inherited;
end;

function TSilDataRowsetEnumerator.DoHasMore: Boolean;
begin
  Result := Assigned(FData) and  not FData.IsEof;
end;

procedure TSilDataRowsetEnumerator.DoReset;
begin
  inherited;
  if Assigned(FData) then
    FData.First;
end;

function TSilDataRowsetEnumerator.GetCurrent: Pointer;
begin
  Result := Pointer(FFields);
end;

function TSilDataRowsetEnumerator.Next: Boolean;
begin
  if DoHasMore then FData.Next;
  Result := DoHasMore;
end;

end.
 