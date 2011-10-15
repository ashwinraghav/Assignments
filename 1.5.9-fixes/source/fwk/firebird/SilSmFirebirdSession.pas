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

unit SilSmFirebirdSession;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,  
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdSession = class(
    TSilFirebirdHandled,
    IFbSessionInternal,
    IFbSessionOptions,
    IFbDatabaseInternal )
  private
    FApplication: IFbApplicationInternal;
    FTransactions: IFbTransactionsInternal;
    FSchema: IFbSchemaInternal;
  private
    procedure DoConnect(const Database: string; const Values: IParameterList);
    function DoBuildParams(const Values: IParameterList): String;
  protected
    procedure DoCloseHandle(const Sender: IFbHandle); override;
  protected // IFbSession
    function GetApplication: IFbApplication;
    function GetDatabase: IFbDatabase;
    function GetTransactions: IFbTransactions;
    function StartTransaction(const Name: string = ''; AutoCommit: Boolean = True): IFbTransaction;
    function Statement(const Text: string): IFbStatement;
  protected // IFbSessionInternal
    function DoGetApplication: IFbApplicationInternal;
    function DoGetDatabase: IFbDatabaseInternal;
    function DoGetTransactions: IFbTransactionsInternal;
  protected // IFbSessionOptions
  protected // IFbDatabase
    function GetSchema: IFbSchema;
  protected // IFbDatabaseInternal
    function DoGetHandle: PISC_DB_HANDLE;
    function DoGetSchema: IFbSchemaInternal;
  public
    constructor Create(const Application: IFbApplicationInternal; const Database: string; const Parameters: IParameterList = nil);
    destructor Destroy; override;
  public
    property Application: IFbApplicationInternal read DoGetApplication;
  end;

implementation

uses
  SilScFirebirdClient,
  SilSfFirebirdClient,
  SilSmFirebirdHandle,
  SilSmFirebirdTransactions,
  SilSmFirebirdStatement,
  SilSmFirebirdParameters,
  SilSfFirebird,
  SilSmFirebirdSchema;

{ TSilFirebirdSession }

constructor TSilFirebirdSession.Create(const Application: IFbApplicationInternal; const Database: string; const Parameters: IParameterList);
begin
  inherited Create;
  FApplication := Application;
  FTransactions := TSilFirebirdTransactionList.Create(Self);
  DoConnect(Database, Parameters);
end;

destructor TSilFirebirdSession.Destroy;
begin
  FTransactions := nil;
  Close;
  FApplication := nil;
  inherited;
end;

function TSilFirebirdSession.GetDatabase: IFbDatabase;
begin
  Result := IFbDatabaseInternal(Self);
end;

function TSilFirebirdSession.GetTransactions: IFbTransactions;
begin
  Result := FTransactions;
end;

function TSilFirebirdSession.GetApplication: IFbApplication;
begin
  Result := DoGetApplication();
end;

function TSilFirebirdSession.StartTransaction(const Name: string; AutoCommit: Boolean): IFbTransaction;
begin
  Result := FTransactions.Start(Name, AutoCommit);
end;

function TSilFirebirdSession.Statement(const Text: string): IFbStatement;
begin
  Result := IFbStatementInternal(TSilFirebirdStatement.Create(Self, Text));
end;

function TSilFirebirdSession.DoGetApplication: IFbApplicationInternal;
begin
  Result := FApplication;
end;

function TSilFirebirdSession.DoGetDatabase: IFbDatabaseInternal;
begin
  Result := Self;
end;

function TSilFirebirdSession.DoGetTransactions: IFbTransactionsInternal;
begin
  Result := FTransactions;
end;

procedure TSilFirebirdSession.DoConnect(const Database: string; const Values: IParameterList);
var
  DPB: String;
begin
  DPB := DoBuildParams(Values);
  Check(fb.api.session.attach(fb.status, Length(Database), PChar(Database), Self.Handle.Value, Length(DPB), PChar(DPB)));
end;

function TSilFirebirdSession.DoBuildParams(const Values: IParameterList): String;
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  if Assigned(Values) then
  begin
    Result := Char(isc_dpb_version1);
    with Values do
      while Enumerate(Enum, Item) do
        Application.Properties.Add(Result, Item.Name, Item.Value);
  end;
end;

procedure TSilFirebirdSession.DoCloseHandle(const Sender: IFbHandle);
begin
  if Sender.IsAssigned then fb.api.session.detach(fb.status, Sender.Value);
end;

function TSilFirebirdSession.GetSchema: IFbSchema;
begin
  Result := DoGetSchema;
end;

function TSilFirebirdSession.DoGetHandle: PISC_DB_HANDLE;
begin
  Result := Self.Handle.Value;
end;

function TSilFirebirdSession.DoGetSchema: IFbSchemaInternal;
begin
  if not Assigned(FSchema) then FSchema := TSilFirebirdSchema.Create(Self);
  Result := FSchema;  
end;

end.
