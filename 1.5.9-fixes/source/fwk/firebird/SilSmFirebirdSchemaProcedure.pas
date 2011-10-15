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

unit SilSmFirebirdSchemaProcedure;

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
  TSilFirebirdSchemaProcedures = class(
    TSilFirebirdSchemaObject,
    IFbSchemaProcedures )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaProcedures
    function GetProcFields: IFbSchemaProcedure;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedure): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaProcedure = class(
    TSilFirebirdSchemaFields,
    IFbSchemaProcedure )
  protected // IFbSchemaProcedure
    function GetName: IFbData;         // RDB$PROCEDURE_NAME
    function GetId: IFbData;           // RDB$PROCEDURE_ID
    function GetInputs: IFbData;       // RDB$PROCEDURE_INPUTS
    function GetOutputs: IFbData;      // RDB$PROCEDURE_OUTPUTS
    function GetDescription: IFbData;  // RDB$DESCRIPTION
    function GetSource: IFbData;       // RDB$PROCEDURE_SOURCE
    function GetBlr: IFbData;          // RDB$PROCEDURE_BLR
    function GetSecurity: IFbData;     // RDB$SECURITY_CLASS
    function GetOwner: IFbData;        // RDB$OWNER_NAME
    function GetRuntime: IFbData;      // RDB$RUNTIME
    function GetSystem: IFbData;       // RDB$SYSTEM_FLAG
    function GetParameters: IFbSchemaProcedureParameters;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries,
  SilSmFirebirdSchemaProcedureParameters;

const
  CSysIndexProcedureName           =  0;
  CSysIndexProcedureId             =  1;
  CSysIndexProcedureInputs         =  2;
  CSysIndexProcedureOutputs        =  3;
  CSysIndexProcedureDescription    =  4;
  CSysIndexProcedureSource         =  5;
  CSysIndexProcedureBlr            =  6;
  CSysIndexProcedureSecurityClass  =  7;
  CSysIndexProcedureOwnerName      =  8;
  CSysIndexProcedureRuntime        =  9;
  CSysIndexProcedureSystemFlag     = 10;

{ TSilFirebirdSchemaProcedures }

destructor TSilFirebirdSchemaProcedures.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaProcedures.GetProcFields: IFbSchemaProcedure;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaProcedure.Create(Self);
  Result := FFields as IFbSchemaProcedure;
end;

function TSilFirebirdSchemaProcedures.DoGetFields: IUnknown;
begin
  Result := GetProcFields();
end;

function TSilFirebirdSchemaProcedures.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedure): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

{ TSilFirebirdSchemaProcedure }

destructor TSilFirebirdSchemaProcedure.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaProcedure.GetName: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureName, CSysFieldProcedureName);
end;

function TSilFirebirdSchemaProcedure.GetId: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureId, CSysFieldProcedureId);
end;

function TSilFirebirdSchemaProcedure.GetInputs: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureInputs, CSysFieldProcedureInputs);
end;

function TSilFirebirdSchemaProcedure.GetOutputs: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureOutputs, CSysFieldProcedureOutputs);
end;

function TSilFirebirdSchemaProcedure.GetDescription: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureDescription, CSysFieldProcedureDescription);
end;

function TSilFirebirdSchemaProcedure.GetSource: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureSource, CSysFieldProcedureSource);
end;

function TSilFirebirdSchemaProcedure.GetBlr: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureBlr, CSysFieldProcedureBlr);
end;

function TSilFirebirdSchemaProcedure.GetSecurity: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureSecurityClass, CSysFieldProcedureSecurityClass);
end;

function TSilFirebirdSchemaProcedure.GetOwner: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureOwnerName, CSysFieldProcedureOwnerName);
end;

function TSilFirebirdSchemaProcedure.GetRuntime: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureRuntime, CSysFieldProcedureRuntime);
end;

function TSilFirebirdSchemaProcedure.GetSystem: IFbData;
begin
  Result := DoCheck(CSysIndexProcedureSystemFlag, CSysFieldProcedureSystemFlag);
end;

function TSilFirebirdSchemaProcedure.GetParameters: IFbSchemaProcedureParameters;
begin
  Result := Session.Database.Schema.Parameters(GetName.AnsiString.Value);
end;

end.
