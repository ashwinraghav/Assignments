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

unit SilSmFirebirdSchema;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdSchema = class(
    TSilObject,
    IFbSchemaInternal )
  private
    FSession: Pointer;
    FQueries: IFbSchemaQueries;
  private
    procedure DoCheckQueries;
    function DoGetSession: IFbSessionInternal;
  protected // IFbSchema
    function Domains(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaDomains;
    function Tables(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaTables;
    function Views(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaViews;
    function Procedures(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaProcedures;
    function ProcedureByName(const Name: String; const Transaction: IFbTransaction = nil): IFbSchemaProcedure;
    function Parameters(const ProcedureName: string; const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaProcedureParameters;
    function ParameterByName(const ProcedureName, ParameterName: string; const Transaction: IFbTransaction = nil): IFbSchemaProcedureParameter;
    function Generators(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaGenerators;
    function Collations(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaCollations;
    function Charsets(const Transaction: IFbTransaction = nil; const Filter: String = ''; const Args: IParameterList = nil; const Order: string = ''): IFbSchemaCharsets;
  protected // IFbSchemaInternal
    function GetQueries: IFbSchemaQueries;
  public
    constructor Create(const Session: IFbSessionInternal);
    destructor Destroy; override;
  public
    property Session: IFbSessionInternal read DoGetSession;
  end;

implementation

uses
  SilScFirebirdSQL,
  SilSfFirebird,
  SilSgFirebirdSchemaQueries,
  SilSmFirebirdSchemaQueries,
  SilSmFirebirdSchemaDomain,
  SilSmFirebirdSchemaTable,
  SilSmFirebirdSchemaView,
  SilSmFirebirdSchemaProcedure,
  SilSmFirebirdSchemaProcedureParameters, SilSfFirebirdSQL;

{ TSilFirebirdSchema }

constructor TSilFirebirdSchema.Create(const Session: IFbSessionInternal);
begin
  inherited Create;
  FSession := Pointer(Session);
end;

destructor TSilFirebirdSchema.Destroy;
begin
  FSession := nil;
  inherited;
end;

function TSilFirebirdSchema.Domains(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaDomains;
begin
  DoCheckQueries;
  Result := TSilFirebirdSchemaDomains.Create(FQueries.QueryDomains(Filter, Order, Transaction).Open(Args));
end;

function TSilFirebirdSchema.Tables(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaTables;
begin
  DoCheckQueries;
  Result := TSilFirebirdSchemaTables.Create(
        FQueries.QueryRelations(
          SQLFilterJoin(
            Filter,
            CSysFieldRelationViewBlr  + ccSPC + CSqlPredIsNull
            ),
          Order,
          Transaction).Open(Args));
end;

function TSilFirebirdSchema.Views(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaViews;
begin
  DoCheckQueries;
  Result := TSilFirebirdSchemaViews.Create(
      FQueries.QueryRelations(
          SQLFilterJoin(
            Filter,
            CSysFieldRelationViewBlr  + ccSPC + CSqlPredIsNotNull
            ),
          Order,
          Transaction).Open(Args));
end;

function TSilFirebirdSchema.Procedures(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaProcedures;
begin
  DoCheckQueries;
  Result := TSilFirebirdSchemaProcedures.Create(
      FQueries.QueryProcedures(
          Filter,
          Order,
          Transaction).Open(Args));
end;

function TSilFirebirdSchema.ProcedureByName(const Name: String; const Transaction: IFbTransaction): IFbSchemaProcedure;
begin
  with Procedures(Transaction, CSysFieldProcedureName + CSqlOpEqual + CSqlParamMark, Values([Name])) do
    Result := Fields;
end;

function TSilFirebirdSchema.Parameters(const ProcedureName: string; const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaProcedureParameters;
begin
  DoCheckQueries;
  Result := TSilFirebirdSchemaProcedureParameters.Create(
      FQueries.QueryProcedureParameters(
          ProcedureName,
          Filter,
          Order,
          Transaction).Open(
      SQLParamAdd(
          Args,
          CSysFieldProcedureName,
          ProcedureName)
      ));
end;

function TSilFirebirdSchema.ParameterByName(const ProcedureName, ParameterName: string; const Transaction: IFbTransaction): IFbSchemaProcedureParameter;
begin
  with Parameters(
          ProcedureName,
          Transaction,
          CSysFieldProcParamName
            + CSqlOpEqual
            + CSqlParamStart
            + CSysFieldProcParamName,
          Params(
            [ Param(
                CSysFieldProcParamName,
                ParameterName )
            ])
        ) do Result := Fields;
end;

function TSilFirebirdSchema.Generators(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaGenerators;
begin
  DoCheckQueries;

end;

function TSilFirebirdSchema.Collations(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaCollations;
begin
  DoCheckQueries;

end;

function TSilFirebirdSchema.Charsets(const Transaction: IFbTransaction; const Filter: String; const Args: IParameterList; const Order: string): IFbSchemaCharsets;
begin
  DoCheckQueries;

end;

function TSilFirebirdSchema.GetQueries: IFbSchemaQueries;
begin
  Result := FQueries;
end;

function TSilFirebirdSchema.DoGetSession: IFbSessionInternal;
begin
  Result := IFbSessionInternal(FSession);
end;

procedure TSilFirebirdSchema.DoCheckQueries;
begin
  if not Assigned(FQueries) then
    FQueries := TSilFirebirdSchemaQueries.Create(Session);
end;

end.
