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

unit SilSkFirebirdSchemaFields;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdSchemaFields = class(TSilAggregatedObject)
  private
    FSession: IFbSessionInternal;
    FParent: Pointer;
    FCursor: IFbCursorInternal;
    FFields: array of IFbData;
  protected
    function DoCheck(Index: Integer; Name: PChar): IFbData;
    function DoGetCursor: IFbCursorInternal;
    function DoGetParent: IFbSchemaObjectInternal;
  public
    constructor Create(const Parent: IUnknown); reintroduce; 
    destructor Destroy; override;
  public
    property Session: IFbSessionInternal read FSession;
    property Parent: IFbSchemaObjectInternal read DoGetParent;
    property Cursor: IFbCursorInternal read DoGetCursor;
  end;

implementation

{ TSilFirebirdSchemaFields }

constructor TSilFirebirdSchemaFields.Create(const Parent: IUnknown);
begin
  inherited Create(Parent);
  FParent := Pointer(Parent as IFbSchemaObjectInternal);
  FCursor := IFbSchemaObjectInternal(FParent).Cursor;
  FSession := Cursor.Command.Session;
end;

destructor TSilFirebirdSchemaFields.Destroy;
begin
  FSession := nil;
  FFields := nil;
  FCursor := nil;
  FParent := nil;
  inherited;
end;

function TSilFirebirdSchemaFields.DoCheck(Index: Integer; Name: PChar): IFbData;
begin
  if Index >= Length(FFields) then
    SetLength(FFields, Index + 1);

  if not Assigned(FFields[Index]) then
    FFields[Index] := Cursor.Fields.ItemByName[Name];

  Result := FFields[Index];
end;

function TSilFirebirdSchemaFields.DoGetCursor: IFbCursorInternal;
begin
  Result := IFbCursorInternal(FCursor);
end;

function TSilFirebirdSchemaFields.DoGetParent: IFbSchemaObjectInternal;
begin
  Result := IFbSchemaObjectInternal(FParent);
end;

end.
