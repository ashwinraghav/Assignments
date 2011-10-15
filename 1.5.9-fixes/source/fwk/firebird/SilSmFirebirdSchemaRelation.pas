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

unit SilSmFirebirdSchemaRelation;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird,
  SilSmFirebirdCursor,
  SilSkFirebirdSchemaObject,
  SilSkFirebirdSchemaFields;

type
  TSilFirebirdSchemaRelations = class(
    TSilFirebirdSchemaObject,
    IFbSchemaRelations )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaRelations
    function GetRelationFields: IFbSchemaRelation;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaRelation): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaRelation = class(
    TSilFirebirdSchemaFields,
    IFbSchemaRelation )
  protected // IFbSchemaRelation
    function GetDescription: IFbData;          // DESCRIPTION
    function GetId: IFbData;                   // RELATION_ID
    function GetSystemFlag: IFbData;           // SYSTEM_FLAG
    function GetDbkeyLength: IFbData;          // DBKEY_LENGTH
    function GetFormat: IFbData;               // FORMAT
    function GetFieldCount: IFbData;           // FIELD_ID
    function GetName: IFbData;                 // RELATION_NAME
    function GetSecurityClass: IFbData;        // SECURITY_CLASS
    function GetRuntime: IFbData;              // RUNTIME
    function GetExternalDescription: IFbData;  // EXTERNAL_DESCRIPTION
    function GetOwnerName: IFbData;            // OWNER_NAME
    function GetDefaultClass: IFbData;         // DEFAULT_CLASS
    function GetFlags: IFbData;                // FLAGS
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries;

const
    CSysIndexRelationDescription          =   0;
    CSysIndexRelationId                   =   1;
    CSysIndexRelationSystemFlag           =   2;
    CSysIndexRelationDbkeyLength          =   3;
    CSysIndexRelationFormat               =   4;
    CSysIndexRelationFieldCount           =   5;
    CSysIndexRelationName                 =   6;
    CSysIndexRelationSecurityClass        =   7;
    CSysIndexRelationRuntime              =   8;
    CSysIndexRelationExternalDescription  =   9;
    CSysIndexRelationOwnerName            =  10;
    CSysIndexRelationDefaultClass         =  11;
    CSysIndexRelationFlags                =  12;

{ TSilFirebirdSchemaRelations }

destructor TSilFirebirdSchemaRelations.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaRelations.GetRelationFields: IFbSchemaRelation;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaRelation.Create(Self);
  Result := FFields as IFbSchemaRelation;
end;

function TSilFirebirdSchemaRelations.DoGetFields: IUnknown;
begin
  Result := GetRelationFields();
end;

function TSilFirebirdSchemaRelations.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaRelation): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

{ TSilFirebirdSchemaRelation }

destructor TSilFirebirdSchemaRelation.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaRelation.GetDescription: IFbData;
begin
  Result := DoCheck(CSysIndexRelationDescription, CSysFieldRelationDescription);
end;

function TSilFirebirdSchemaRelation.GetId: IFbData;
begin
  Result := DoCheck(CSysIndexRelationId, CSysFieldRelationId);
end;

function TSilFirebirdSchemaRelation.GetSystemFlag: IFbData;
begin
  Result := DoCheck(CSysIndexRelationSystemFlag, CSysFieldRelationSystemFlag);
end;

function TSilFirebirdSchemaRelation.GetDbkeyLength: IFbData;
begin
  Result := DoCheck(CSysIndexRelationDbkeyLength, CSysFieldRelationDbkeyLength);
end;

function TSilFirebirdSchemaRelation.GetFormat: IFbData;
begin
  Result := DoCheck(CSysIndexRelationFormat, CSysFieldRelationFormat);
end;

function TSilFirebirdSchemaRelation.GetFieldCount: IFbData;
begin
  Result := DoCheck(CSysIndexRelationFieldCount, CSysFieldRelationFieldCount);
end;

function TSilFirebirdSchemaRelation.GetName: IFbData;
begin
  Result := DoCheck(CSysIndexRelationName, CSysFieldRelationName);
end;

function TSilFirebirdSchemaRelation.GetSecurityClass: IFbData;
begin
  Result := DoCheck(CSysIndexRelationSecurityClass, CSysFieldRelationSecurityClass);
end;

function TSilFirebirdSchemaRelation.GetRuntime: IFbData;
begin
  Result := DoCheck(CSysIndexRelationRuntime, CSysFieldRelationRuntime);
end;

function TSilFirebirdSchemaRelation.GetExternalDescription: IFbData;
begin
  Result := DoCheck(CSysIndexRelationDescription, CSysFieldRelationDescription);
end;

function TSilFirebirdSchemaRelation.GetOwnerName: IFbData;
begin
  Result := DoCheck(CSysIndexRelationOwnerName, CSysFieldRelationOwnerName);
end;

function TSilFirebirdSchemaRelation.GetDefaultClass: IFbData;
begin
  Result := DoCheck(CSysIndexRelationDefaultClass, CSysFieldRelationDefaultClass);
end;

function TSilFirebirdSchemaRelation.GetFlags: IFbData;
begin
  Result := DoCheck(CSysIndexRelationFlags, CSysFieldRelationFlags);
end;

end.
