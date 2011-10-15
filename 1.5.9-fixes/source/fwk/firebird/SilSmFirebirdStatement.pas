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

unit SilSmFirebirdStatement;

{$INCLUDE Defines.inc}

interface

uses
  Sil,  
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdStatement = class(
    TSilFirebirdHandled,
    IFbStatementInternal )
  private
    FSession: IFbSessionInternal;
    FTransaction: IFbTransactionInternal;
    FNames: IStringList;
    FKind: TFbStatementKind;
    FVariables: array[TFbVariableKind] of IFbVariablesInternal;
  private
    procedure DoAllocHandle;
    procedure DoFreeHandle;
    procedure DoPrepare(const Text: string);
    function DoExtractParams(const Text: string; var ParamList: IStringList): string;
    function DoGetInfo(Info: Byte): string;
    function DoGetKind: TFbStatementKind;
    function DoCreate(Kind: TFbVariableKind): IFbVariablesInternal;
    procedure DoCheck(Kind: TFbVariableKind);
    function DoGet(Kind: TFbVariableKind): IFbVariablesInternal;
  protected
    procedure DoCloseHandle(const Sender: IFbHandle); override;
  protected // IFbStatement
    function GetSession: IFbSession;
    function GetKind: TFbStatementKind;
    function GetParameters: IFbVariables;
    function GetFields: IFbVariables;
    function Prepare(const Transaction: IFbTransaction = nil; const CursorName: string = ''): IFbCommand;
  protected // IFbStatementInternal
    function DoGetSession: IFbSessionInternal;
    function DoGetHandle: PISC_STMT_HANDLE;
    function DoGetNames: IStringList;
  protected
    property Session: IFbSessionInternal read DoGetSession;
    property Kind: TFbStatementKind read GetKind;
  public
    constructor Create(const Session: IFbSessionInternal; const Text: String);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilScFirebirdSQL,
  SilSfFirebird,
  SilSmFirebirdVariables,
  SilSmFirebirdCommand;

const
  MVariablesKind: array[TFbVariableKind] of TSilFirebirdVariablesClass = (
      TSilFirebirdParameters,
      TSilFirebirdFields
    );

{ TSilFirebirdStatement }

constructor TSilFirebirdStatement.Create(const Session: IFbSessionInternal; const Text: String);
begin
  inherited Create;
  FSession := Session;
  DoAllocHandle;
  FTransaction := Session.StartTransaction('', Session.Transactions.Defaults.AutoCommit) as IFbTransactionInternal;
  DoPrepare(DoExtractParams(Text, FNames));
  FKind := DoGetKind();
end;

destructor TSilFirebirdStatement.Destroy;
begin
  Close;
  FNames := nil;
  FTransaction := nil;
  FSession := nil;
  inherited;
end;

function TSilFirebirdStatement.GetSession: IFbSession;
begin
  Result := Session;
end;

function TSilFirebirdStatement.GetKind: TFbStatementKind;
begin
  Result := FKind;
end;

function TSilFirebirdStatement.GetParameters: IFbVariables;
begin
  Result := DoGet(fbvkParameters);
end;

function TSilFirebirdStatement.GetFields: IFbVariables;
begin
  Result := DoGet(fbvkFields);
end;

function TSilFirebirdStatement.Prepare(const Transaction: IFbTransaction; const CursorName: string): IFbCommand;
begin
  Result := IFbCommandInternal(TSilFirebirdCommand.Create(Self, CheckTransaction(Transaction, FTransaction), CursorName));  
end;

function TSilFirebirdStatement.DoGetSession: IFbSessionInternal;
begin
  Result := FSession;
end;

function TSilFirebirdStatement.DoGetHandle: PISC_STMT_HANDLE;
begin
  Result := inherited Handle.Value;
end;

function TSilFirebirdStatement.DoGetNames: IStringList;
begin
  Result := FNames
end;

procedure TSilFirebirdStatement.DoAllocHandle;
begin
  Check(fb.api.sql.alloc_statement2(fb.status, Session.Database.Handle, Handle.Value));
end;

procedure TSilFirebirdStatement.DoFreeHandle;
begin
  Check(fb.api.sql.free_statement(fb.status, Handle.Value, DSQL_drop))
end;

procedure TSilFirebirdStatement.DoPrepare(const Text: string);
begin
  Check(fb.api.sql.prepare(fb.status, FTransaction.Handle, Handle.Value, 0, PChar(Text), SQL_DIALECT_CURRENT, nil));
end;

function TSilFirebirdStatement.DoExtractParams(const Text: string; var ParamList: IStringList): string;
var
  P: PChar;
  C: Char;
  Ident: string;
begin
  if (Sil.Str.Pos(CSqlParamMark, Text) <> 0) or (Sil.Str.Pos(CSqlParamStart, Text) <> 0) then
  begin
    if not Assigned(ParamList) then
      ParamList := Sil.List.StringList() else
      ParamList.Clear; 

    P := PChar(Text);

    while Assigned(P) and (P^ <> #0) do
    begin
      C := P^;
      case C of
        CSqlParamMark:
          begin
            ParamList.Add('$' + Sil.Int.ToStr(ParamList.Count + 1));
            Inc(P);
          end;
        CSqlParamStart:
          begin
            Inc(P);
            if P^ in CAlphaNum then
            begin
              Ident := '';
              while P^ in CIdentifier do
              begin
                Sil.Str.Add(Ident, P^);
                Inc(P);
              end;
              ParamList.Add(Ident);
            end;
            C := CSqlParamMark;
          end;
        else
            Inc(P);
      end;
      Sil.Str.Add(Result, C);
    end;

  end else
    Result := Text;
end;

function TSilFirebirdStatement.DoGetInfo(Info: Byte): string;
var
  Buffer: array[0..1023] of Byte;
  Len: PSmallint;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  Check(fb.api.sql.info(fb.status, Handle.Value, SizeOf(Info), @Info, SizeOf(Buffer), @Buffer));
  if Info = Buffer[0] then
  begin
    Len := @Buffer[1]; 
    SetString(Result, PChar(@Buffer[3]), Len^);
  end;
end;

procedure TSilFirebirdStatement.DoCloseHandle(const Sender: IFbHandle);
begin
  if Sender.IsAssigned then DoFreeHandle;
end;

function TSilFirebirdStatement.DoGetKind: TFbStatementKind;
var
  Buffer: string;
begin
  Buffer := DoGetInfo(isc_info_sql_stmt_type);
  Result := TFbStatementKind(fb.api.utils.vax_integer(PChar(Buffer), Length(Buffer)));
end;

function TSilFirebirdStatement.DoCreate(Kind: TFbVariableKind): IFbVariablesInternal;
begin
  MVariablesKind[Kind].Create(Self).GetInterface(IFbVariablesInternal, Result);
end;

procedure TSilFirebirdStatement.DoCheck(Kind: TFbVariableKind);
begin
  if FVariables[Kind] = nil then
    FVariables[Kind] := DoCreate(Kind);
end;

function TSilFirebirdStatement.DoGet(Kind: TFbVariableKind): IFbVariablesInternal;
begin
  DoCheck(Kind);
  Result := FVariables[Kind];
end;

end.
