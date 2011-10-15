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

unit SilSmFirebirdVariable;

{$INCLUDE Defines.inc}

interface

uses
  Sil,  
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdVariable = class(
    TSilFirebirdVariableType,
    IFbVariableInternal )
  private
    FList: Pointer;
    FSession: Pointer;
    FStatement: Pointer;
    FData: RFbFieldData;
    FDomain: IFbDomain;
    FIndex: Integer;
  private
    function DoGetList: IFbVariablesInternal;
    function DoGetData(const Source: PXSQLVAR_V1): RFbVariableData;
  protected // IFbVariable
    function GetSession: IFbSession;
    function GetList: IFbVariables;
    function GetIndex: Integer;
    function GetPosition: Integer;
    function GetName: string;
    function GetKind: TFbVariableKind; virtual; abstract;
    function GetDomain: IFbDomain;
  protected // IFbVariableInternal
    function DoGetStatement: IFbStatementInternal;
    function DoGetSession: IFbSessionInternal;
    procedure Detach;
  protected
    function DoGetName: string; virtual; 
  protected
    property List: IFbVariablesInternal read DoGetList;
    property Statement: IFbStatementInternal read DoGetStatement;
    property Session: IFbSessionInternal read DoGetSession;
    property Kind: TFbVariableKind read GetKind;
  public
    constructor Create(const List: IFbVariablesInternal; const Statement: IFbStatementInternal; const Source: PXSQLVAR_V1); override;
    destructor Destroy; override;
  end;

  TSilFirebirdParam = class(TSilFirebirdVariable)
  protected // IFbVariable
    function DoGetName: string; override;
    function GetKind: TFbVariableKind; override;      
  end;

  TSilFirebirdField = class(
    TSilFirebirdVariable,
    IFbField )
  protected // IFbField
    function GetFieldName: string;
    function GetOwnerName: string;
    function GetTableName: string;
    function GetAliasName: string;
  protected
    function DoGetName: string; override;
    function GetKind: TFbVariableKind; override;      
  protected
    property FieldName: string read GetFieldName;
    property OwnerName: string read GetOwnerName;
    property TableName: string read GetTableName;
    property AliasName: string read GetAliasName;
  end;

implementation

uses
  SilSfFirebirdDomain;

{ TSilFirebirdVariable }

constructor TSilFirebirdVariable.Create(const List: IFbVariablesInternal; const Statement: IFbStatementInternal; const Source: PXSQLVAR_V1);
var
  Data: RFbVariableData;
begin
  inherited Create;
  FList := Pointer(List);
  FSession := Pointer(Statement.Session);
  FStatement := Pointer(Statement);
  Data := DoGetData(Source);
  FData := Data.Field;
  DoGetClass(Data.Domain.BaseType).Create(Data.Domain).GetInterface(IFbDomain, FDomain);
  FIndex := List.Add(Self);
end;

destructor TSilFirebirdVariable.Destroy;
begin
  Detach;
  FDomain := nil;
  FSession := nil;
  FStatement := nil;
  FList := nil;
  inherited;
end;

function TSilFirebirdVariable.GetSession: IFbSession;
begin
  Result := List.Session;
end;

function TSilFirebirdVariable.GetList: IFbVariables;
begin
  Result := Self.List;
end;

function TSilFirebirdVariable.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TSilFirebirdVariable.GetPosition: Integer;
begin
  Result := Succ(FIndex);
end;

function TSilFirebirdVariable.GetName: string;
const
  SKind: array[TFbVariableKind] of PChar = ('Param ', 'Field ');
begin
  Result := DoGetName();
  if Sil.Str.IsEmpty(Result) then Result := SKind[Kind] + Sil.Int.ToStr(FIndex + 1);
end;

function TSilFirebirdVariable.GetDomain: IFbDomain;
begin
  Result := FDomain;
end;

function TSilFirebirdVariable.DoGetSession: IFbSessionInternal;
begin
  Result := IFbSessionInternal(FSession);
end;

function TSilFirebirdVariable.DoGetStatement: IFbStatementInternal;
begin
  Result := IFbStatementInternal(FStatement);
end;

procedure TSilFirebirdVariable.Detach;
begin
  if Assigned(FList) then
  begin
    List.Remove(Self);
    FList := nil;
  end;
end;

function TSilFirebirdVariable.DoGetName: string;
begin
  Result := '';
end;

function TSilFirebirdVariable.DoGetList: IFbVariablesInternal;
begin
  Result := IFbVariablesInternal(FList);
end;

function TSilFirebirdVariable.DoGetData(const Source: PXSQLVAR_V1): RFbVariableData;
begin
  with Result do
  begin
    Field.FieldName := Source.sqlname;
    Field.AliasName := Source.aliasname;
    Field.OwnerName := Source.ownname;
    Field.TableName := Source.relname;
    Domain.BaseType := TFbType(Source.sqltype div 2);
    Domain.AllowNulls := (Source.sqltype mod 2) = 1;
    Domain.BlobType := TFbBlobType(Source.sqlsubtype);
    Domain.Scale := Source.sqlscale;
    Domain.Length := Source.sqllen;
  end;
end;

{ TSilFirebirdParam }

function TSilFirebirdParam.DoGetName: string;
begin
  if Statement.Names.ValidIndex(FIndex) then
    Result := Statement.Names[FIndex] else
    Result := '';
end;

function TSilFirebirdParam.GetKind: TFbVariableKind;
begin
  Result := fbvkParameters;
end;

{ TSilFirebirdField }

function TSilFirebirdField.GetAliasName: string;
begin
  Result := FData.AliasName;
end;

function TSilFirebirdField.GetFieldName: string;
begin
  Result := FData.FieldName;
end;

function TSilFirebirdField.GetOwnerName: string;
begin
  Result := FData.OwnerName;
end;

function TSilFirebirdField.GetTableName: string;
begin
  Result := FData.TableName;
end;

function TSilFirebirdField.DoGetName: string;
begin
  Result := AliasName;
  if Sil.Str.IsEmpty(Result) then Result := FieldName;
end;

function TSilFirebirdField.GetKind: TFbVariableKind;
begin
  Result := fbvkFields;
end;

end.
