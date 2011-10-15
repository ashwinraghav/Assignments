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

unit SilSmFirebirdSchemaTable;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird,
  SilSmFirebirdSchemaRelation,
  SilSkFirebirdSchemaObject,
  SilSkFirebirdSchemaFields;
  
type
  TSilFirebirdSchemaTables = class(
    TSilFirebirdSchemaRelations,
    IFbSchemaTables )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaTables
    function GetTableFields: IFbSchemaTable;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaTable): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaTable = class(
    TSilFirebirdSchemaRelation,
    IFbSchemaTable )
  protected // IFbSchemaTable
    function GetExternalFile: IFbData;         // EXTERNAL_FILE
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries;

const
    CSysIndexTableExternalFile         =  13;

{ TSilFirebirdSchemaTables }

destructor TSilFirebirdSchemaTables.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaTables.GetTableFields: IFbSchemaTable;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaTable.Create(Self);
  Result := FFields as IFbSchemaTable;
end;

function TSilFirebirdSchemaTables.DoGetFields: IUnknown;
begin
  Result := GetTableFields();
end;

function TSilFirebirdSchemaTables.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaTable): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

{ TSilFirebirdSchemaTable }

destructor TSilFirebirdSchemaTable.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaTable.GetExternalFile: IFbData;
begin
  Result := DoCheck(CSysIndexTableExternalFile, CSysFieldRelationExternalFile);
end;

end.
 