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

unit SilVmFirebirdStoredProc;

{$I Defines.inc}

interface

uses
  Classes, Db,

  Sil,
  SilSiFirebird,
  SilStFirebird,
  SilVmFirebirdDatabase;

type
  TSilFbStoredProc = class (TComponent)
  private
    FDatabase: TSilFbDatabase;
    FProcedureName: String;
    FStmt: IFbStatement;
    FCommand: IFbCommand;
    FParams: TParams;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
  private
    function GetParams: TParams;
    function GetDatabase: TSilFbDatabase;
    function GetProcedureName: String;
    procedure SetDatabase(const Value: TSilFbDatabase);
    procedure SetProcedureName(const Value: String);
    procedure DoGetSchema(Refresh: Boolean);
    procedure DoSetInputParams;
    procedure DoSetOutputParams(const Results: IFbBuffer);
    procedure SetParamsList(Value: TParams);
  published
    property Database: TSilFbDatabase read GetDatabase write SetDatabase;
    property ProcedureName: String read GetProcedureName write SetProcedureName;
    property Parameters: TParams read GetParams write SetParamsList;
  public
    function StartTransaction: IFbTransaction;
    function ParamByName(const Name: String): TParam;
    procedure Execute;
    procedure RefreshParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  AFbFieldType: array [TFbDataType] of Db.TFieldType = (
    Db.ftUnknown,
    Db.ftString,
    Db.ftFixedChar,
    Db.ftFloat,
    Db.ftFloat,
    Db.ftInteger,
    Db.ftSmallInt,
    Db.ftDateTime,
    Db.ftBlob,
    Db.ftFloat,
    Db.ftArray,
    Db.ftLargeInt,
    Db.ftTime,
    Db.ftDate,
    Db.ftLargeInt);

{ TSilFbStoredProc }

constructor TSilFbStoredProc.Create(AOwner: TComponent); 
begin
  inherited;
  FParams := TParams.Create(Self);
end;

destructor TSilFbStoredProc.Destroy;
begin
  FStmt := nil;
  FParams.Free;
  SetDatabase(nil);

  inherited;
end;

procedure TSilFbStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Parameters);
end;

procedure TSilFbStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Parameters);
end;

procedure TSilFbStoredProc.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TSilFbStoredProc(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

function TSilFbStoredProc.GetDatabase: TSilFbDatabase;
begin
  Result := FDatabase;
end;

function TSilFbStoredProc.GetProcedureName: String;
begin
  Result := FProcedureName;
end;

procedure TSilFbStoredProc.SetDatabase(const Value: TSilFbDatabase);
begin
  if FDatabase <> Value then
  begin
    if Assigned(FDatabase) then FDatabase.RemoveObject(Self);
    FDatabase := Value;
    if Assigned(FDatabase) then FDatabase.AddObject(Self);
    DoGetSchema(csDesigning in ComponentState);
  end;
end;

procedure TSilFbStoredProc.SetProcedureName(const Value: String);
begin
  FProcedureName := Value;
  DoGetSchema(csDesigning in ComponentState);
end;

function TSilFbStoredProc.GetParams: TParams;
begin
  DoGetSchema(csDesigning in ComponentState);
  Result := FParams;
end;

procedure TSilFbStoredProc.DoGetSchema(Refresh: Boolean);
var
  Fields: IFbSchemaProcedureParameters;
  Field: IFbSchemaProcedureParameter;
  Enum: IEnumerator;
  Stmt, InParams: String;
begin
  if (Refresh or (FParams.Count = 0)) and Assigned(FDatabase) and Str.NotEmpty(FProcedureName) then
  begin
    if not FDatabase.Connected then
      FDatabase.Open;

    FParams.Clear;
    Stmt := 'execute procedure';
    InParams := '';
    Fields := FDatabase.Session.Database.Schema.Parameters(FProcedureName);

    while Fields.Enumerate(Enum, Field) do
      with Field do
        case Kind.Integer.Value of
          0:  // in
          begin
            Str.Add(InParams, ':' + Str.Trim(Str.Trim(Name.AnsiString.Value, #0)), ',');
            FParams.CreateParam(AFbFieldType[Field.Binding.Domain.DataType], Name.AnsiString.Value, ptInput);
          end;
          1:  // out
            FParams.CreateParam(AFbFieldType[Field.Binding.Domain.DataType], Name.AnsiString.Value, ptOutput);
        end;

    if not (csDesigning in ComponentState) then
    begin
      if Str.NotEmpty(InParams) then
        Stmt := Str.Format('%s %s(%s)', [Stmt, FProcedureName, InParams]) else
        Stmt := Str.Format('%s %s', [Stmt, FProcedureName]);

      FStmt := FDatabase.Session.Statement(Stmt);
    end;
  end;
end;

function TSilFbStoredProc.StartTransaction: IFbTransaction;
begin
  Result := FDatabase.Session.StartTransaction;

  if Assigned(FStmt) then
    FCommand := FStmt.Prepare(Result);
end;

function TSilFbStoredProc.ParamByName(const Name: String): TParam;
begin
  DoGetSchema(false);
  Result := FParams.ParamByName(Name);
end;

procedure TSilFbStoredProc.DoSetInputParams;
var
  i: Integer;
begin
  for i := 0 to FParams.Count - 1 do
    with FParams[i] do
      if ParamType = ptInput then
        FCommand.Parameters.Items[i].Value := Value;
end;

procedure TSilFbStoredProc.DoSetOutputParams(const Results: IFbBuffer);
var
  Enum: IEnumerator;
  Item: IFbData;
begin
  if Assigned(Results) then
    while Results.Enumerate(Enum, Item) do
      FParams.ParamByName(Item.Binding.Name).Value := Item.Value;
end;

procedure TSilFbStoredProc.Execute;
var
  Results, Dummy: IFbBuffer;
begin
  if not Assigned(FCommand) or FCommand.Transaction.IsTerminated then
    raise Sil.Error.Create('Transaction not active.');

  DoSetInputParams;
  FCommand.Execute(Dummy, @Results);
  DoSetOutputParams(Results);
end;

procedure TSilFbStoredProc.RefreshParams;
begin
  DoGetSchema(true);
end;

procedure TSilFbStoredProc.SetParamsList(Value: TParams);
begin
  FParams.Assign(Value);
end;

end.

