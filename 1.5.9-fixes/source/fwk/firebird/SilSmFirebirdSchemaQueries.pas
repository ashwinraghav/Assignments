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

unit SilSmFirebirdSchemaQueries;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdSchemaQueries = class(
    TSilObject,
    IFbSchemaQueries )
  private
    FSession: Pointer;
  private
    function DoGetSession: IFbSessionInternal;
  protected // IFbSchemaQueries
    function QueryDomains(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryRelations(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryProcedures(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryProcedureParameters(const ProcedureName: string; const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
  public
    constructor Create(const Session: IFbSessionInternal);
    destructor Destroy; override;
  public
    property Session: IFbSessionInternal read DoGetSession;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries,
  SilSfFirebirdSQL,
  SilScFirebirdSQL;

{ TSilFirebirdSchemaQueries }

constructor TSilFirebirdSchemaQueries.Create(const Session: IFbSessionInternal);
begin
  inherited Create;
  FSession := Pointer(Session);
end;

destructor TSilFirebirdSchemaQueries.Destroy;
begin
  FSession := nil;
  inherited;
end;

function TSilFirebirdSchemaQueries.QueryDomains(const Filter, Order: string; const Transaction: IFbTransaction): IFbCommandInternal;
begin
  Result := Session.Statement(
              SQLBuild(
                  GQueryDomains,
                  Filter,
                  Order)
            ).Prepare(Transaction) as IFbCommandInternal;
end;

function TSilFirebirdSchemaQueries.QueryRelations(const Filter, Order: string; const Transaction: IFbTransaction): IFbCommandInternal;
begin
  Result := Session.Statement(
              SQLBuild(
                  GQueryRelations,
                  Filter,
                  Order)
            ).Prepare(Transaction) as IFbCommandInternal;
end;

function TSilFirebirdSchemaQueries.QueryProcedures(const Filter, Order: string; const Transaction: IFbTransaction): IFbCommandInternal;
begin
  Result := Session.Statement(
              SQLBuild(
                  GQueryProcedures,
                  Filter,
                  Order)
            ).Prepare(Transaction) as IFbCommandInternal;
end;

function TSilFirebirdSchemaQueries.QueryProcedureParameters(const ProcedureName, Filter, Order: string; const Transaction: IFbTransaction): IFbCommandInternal;
begin
  Result := Session.Statement(
              SQLBuild(
                  GQueryProcParams,
                  SQLFilterJoin(
                      Filter,
                      CSysFieldProcParamProcedure
                        + CSqlOpEqual
                        + CSqlParamStart
                        + CSysFieldProcedureName),
                  Order)
            ).Prepare(Transaction) as IFbCommandInternal;
end;

function TSilFirebirdSchemaQueries.DoGetSession: IFbSessionInternal;
begin
  Result := IFbSessionInternal(FSession);
end;

end.
