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

unit SilSmFirebirdCommand;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdCommand = class(
    TSilObject,
    IFbHandled,
    IFbCommandInternal )
  private
    FStatement: IFbStatementInternal;
    FTransaction: IFbTransactionInternal;
    FParameters: IFbBufferInternal;
    FCursorName: string;
  private
    procedure DoCheckParameters;
  protected // IFbHandled
    function GetHandle: IFbHandle;
  protected // IFbCommand
    function GetSession: IFbSession;
    function GetStatement: IFbStatement;
    function GetTransaction: IFbTransaction;
    function GetParameters: IFbBuffer;
    function Open(const Parameters: IFbBuffer; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Open(const Parameters: IParameterList; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Open(const Parameters: array of RParameter; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Execute(const Parameters: IFbBuffer = nil; Results: PFbBuffer = nil): Boolean; overload;
    function Execute(const Parameters: IParameterList; Results: PFbBuffer = nil): Boolean; overload;
    function Execute(const Parameters: array of RParameter; Results: PFbBuffer = nil): Boolean; overload;
  protected // IFbCommandInternal
    function DoGetSession: IFbSessionInternal;
    function DoGetStatement: IFbStatementInternal;
    function DoGetTransaction: IFbTransactionInternal;
    function DoGetParameters: IFbBufferInternal;
    function DoGetHandle: PISC_STMT_HANDLE;
    procedure Execute(const Parameters: IFbValuesInternal; const Results: IFbBufferInternal = nil); overload;
    function Execute(const Parameters: IFbValuesInternal; Results: PFbBufferInternal): Boolean; overload;
  protected 
    property Session: IFbSessionInternal read DoGetSession;
    property Statement: IFbStatementInternal read DoGetStatement;
    property Transaction: IFbTransactionInternal read DoGetTransaction;
    property Parameters: IFbBufferInternal read DoGetParameters;
    property Handle: PISC_STMT_HANDLE read DoGetHandle;
  public
    constructor Create(const Statement: IFbStatementInternal; const Transaction: IFbTransactionInternal; const CursorName: string = '');
    destructor Destroy; override;
  end;

implementation

uses
  SilSfFirebirdClient,
  SilSfFirebird,
  SilStFirebird,
  SilSmFirebirdValues,
  SilSmFirebirdCursor;

{ TSilFirebirdCommand }

constructor TSilFirebirdCommand.Create(const Statement: IFbStatementInternal; const Transaction: IFbTransactionInternal; const CursorName: string);
begin
  inherited Create;
  FStatement := Statement;
  FTransaction := Transaction;
  FCursorName := CursorName;
end;

destructor TSilFirebirdCommand.Destroy;
begin
  FParameters := nil; 
  FTransaction := nil;
  FStatement := nil;
  inherited;
end;

function TSilFirebirdCommand.GetHandle: IFbHandle;
begin
  Result := IFbStatement(FStatement).Handle;
end;

function TSilFirebirdCommand.GetSession: IFbSession;
begin
  Result := FStatement.Session;
end;

function TSilFirebirdCommand.GetStatement: IFbStatement;
begin
  Result := FStatement;
end;

function TSilFirebirdCommand.GetTransaction: IFbTransaction;
begin
  Result := FTransaction;
end;

function TSilFirebirdCommand.GetParameters: IFbBuffer;
begin
  Result := Self.Parameters;
end;

function TSilFirebirdCommand.Open(const Parameters: IFbBuffer; const Fields: IFbBindings): IFbCursor;
begin
  Execute(Parameters);
  Result := IFbCursorInternal(TSilFirebirdCursor.Create(Self, IFbBindingsInternal(Fields)));
end;

function TSilFirebirdCommand.Open(const Parameters: IParameterList; const Fields: IFbBindings): IFbCursor;
begin
  Execute(Parameters);
  Result := IFbCursorInternal(TSilFirebirdCursor.Create(Self, IFbBindingsInternal(Fields)));
end;

function TSilFirebirdCommand.Open(const Parameters: array of RParameter; const Fields: IFbBindings): IFbCursor;
begin
  Execute(Parameters);
  Result := IFbCursorInternal(TSilFirebirdCursor.Create(Self, IFbBindingsInternal(Fields)));
end;

function TSilFirebirdCommand.Execute(const Parameters: IFbBuffer; Results: PFbBuffer): Boolean;  
var
  Params: IFbBufferInternal;
  Input: IFbValuesInternal;
begin
  Params := IFbBufferInternal(Parameters);
  if not Assigned(Params) then Params := Self.Parameters;
  if Assigned(Params) then Input := Params.Values;
  Result := Execute(Input, PFbBufferInternal(Results));
end;

function TSilFirebirdCommand.Execute(const Parameters: IParameterList; Results: PFbBuffer): Boolean;
begin
  Result := Execute(SetValues(Self.Parameters, Parameters), Results);
end;

function TSilFirebirdCommand.Execute(const Parameters: array of RParameter; Results: PFbBuffer): Boolean;
begin
  Result := Execute(Params(Parameters), Results);
end;

function TSilFirebirdCommand.DoGetSession: IFbSessionInternal;
begin
  Result := FStatement.Session;
end;

function TSilFirebirdCommand.DoGetTransaction: IFbTransactionInternal;
begin
  Result := FTransaction;
end;

function TSilFirebirdCommand.DoGetParameters: IFbBufferInternal;
begin
  DoCheckParameters;
  Result := FParameters;
end;

function TSilFirebirdCommand.DoGetStatement: IFbStatementInternal;
begin
  Result := FStatement;
end;

function TSilFirebirdCommand.DoGetHandle: PISC_STMT_HANDLE;
begin
  Result := FStatement.Handle;
end;

procedure TSilFirebirdCommand.Execute(const Parameters: IFbValuesInternal; const Results: IFbBufferInternal);
var
  hParameters: PXSQLDA_V1;
  hResults: PXSQLDA_V1;
begin
  if Assigned(Parameters) then
    hParameters := Parameters.Handle else
    hParameters := nil;
  if Assigned(Results) then
    hResults := Results.Values.Handle else
    hResults := nil;
  Check(fb.api.sql.execute2(fb.status, Transaction.Handle, Handle, SQL_DIALECT_CURRENT, hParameters, hResults));
end;

function TSilFirebirdCommand.Execute(const Parameters: IFbValuesInternal; Results: PFbBufferInternal): Boolean;
var
  Output: IFbBufferInternal;
begin                                                                                                
  if Assigned(Results) then
  begin
    if Statement.Fields.Count > 0 then
      Output := IFbBufferInternal(Firebird.Buffer(IFbCommandInternal(Self), Firebird.Bindings(Statement.Fields))) else
      Output := nil;
  end;

  Execute(Parameters, Output);

  Result := Assigned(Results) and Assigned(Output);
  if Result then
    Results^ := Output;
end;

procedure TSilFirebirdCommand.DoCheckParameters;
begin
  if not Assigned(FParameters) then
    FParameters := Buffer(Self, Bindings(IFbVariablesInternal(FStatement.Parameters)), False);
end;

end.
