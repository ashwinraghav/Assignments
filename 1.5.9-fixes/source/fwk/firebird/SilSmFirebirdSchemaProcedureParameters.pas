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

unit SilSmFirebirdSchemaProcedureParameters;

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
  TSilFirebirdSchemaProcedureParameters = class(
    TSilFirebirdSchemaObject,
    IFbSchemaProcedureParameters )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaProcedureParameters
    function GetProcParamFields: IFbSchemaProcedureParameter;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedureParameter): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaProcedureParameter = class(
    TSilFirebirdSchemaFields,
    IFbSchemaProcedureParameter )
  protected // IFbSchemaProcedureParameter
    function GetName: IFbData;             
    function GetOwner: IFbData;            
    function GetNumber: IFbData;           
    function GetKind: IFbData;             
    function GetField: IFbData;
    function GetDescription: IFbData;      
    function GetSystemFlag: IFbData;       
  public
    destructor Destroy; override;
  end;
  
implementation

uses
  SilSgFirebirdSchemaQueries;

const
  CSysIndexProcParamName           =  0;
  CSysIndexProcParamProcedure      =  1;
  CSysIndexProcParamNumber         =  2;
  CSysIndexProcParamKind           =  3;
  CSysIndexProcParamFieldSource    =  4;
  CSysIndexProcParamDescription    =  5;
  CSysIndexProcParamSystemFlag     =  6; 

{ TSilFirebirdSchemaProcedureParameters }

destructor TSilFirebirdSchemaProcedureParameters.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaProcedureParameters.GetProcParamFields: IFbSchemaProcedureParameter;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaProcedureParameter.Create(Self);
  Result := FFields as IFbSchemaProcedureParameter;
end;

function TSilFirebirdSchemaProcedureParameters.DoGetFields: IUnknown;
begin
  Result := GetProcParamFields();
end;

function TSilFirebirdSchemaProcedureParameters.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedureParameter): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

{ TSilFirebirdSchemaProcedureParameter }

destructor TSilFirebirdSchemaProcedureParameter.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaProcedureParameter.GetName: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamName, CSysFieldProcParamName);
end;

function TSilFirebirdSchemaProcedureParameter.GetOwner: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamProcedure, CSysFieldProcParamProcedure);
end;

function TSilFirebirdSchemaProcedureParameter.GetNumber: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamNumber, CSysFieldProcParamNumber);
end;

function TSilFirebirdSchemaProcedureParameter.GetKind: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamKind, CSysFieldProcParamKind);
end;

function TSilFirebirdSchemaProcedureParameter.GetField: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamFieldSource, CSysFieldProcParamFieldSource);
end;

function TSilFirebirdSchemaProcedureParameter.GetDescription: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamDescription, CSysFieldProcParamDescription);
end;

function TSilFirebirdSchemaProcedureParameter.GetSystemFlag: IFbData;
begin
  Result := DoCheck(CSysIndexProcParamSystemFlag, CSysFieldProcParamSystemFlag);
end;

end.
