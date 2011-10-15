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

unit SilVmFirebirdDatabaseParams;

{$I Defines.inc}

interface

uses
  Classes,
  
  Sil,
  SilSiFirebird;

type
  TSilFbDatabaseParams = class (TPersistent)
  private
    FParams: IParameterList;
  private
    procedure SetUserName(const Value: String);
    function GetUserName: String;
    function GetPassword: String;
    procedure SetPassword(const Value: String);
    function GetConnectTimeout: Integer;
    function GetSqlDialect: Integer;
    function GetSqlRoleName: String;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetSqlDialect(const Value: Integer);
    procedure SetSqlRoleName(const Value: String);
  published
    property UserName: String read GetUserName write SetUserName;
    property Password: String read GetPassword write SetPassword;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property SqlRoleName: String read GetSqlRoleName write SetSqlRoleName;
    property SqlDialect: Integer read GetSqlDialect write SetSqlDialect;
  public
    property Parameters: IParameterList read FParams;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSilFbDatabaseParams }

constructor TSilFbDatabaseParams.Create;
begin
  inherited;
  FParams := Sil.List.Parameters(true);
end;

destructor TSilFbDatabaseParams.Destroy;
begin
  FParams := nil;
  inherited;
end;

function TSilFbDatabaseParams.GetConnectTimeout: Integer;
begin
  Result := FParams[CDpbConnectTimeout];
end;

function TSilFbDatabaseParams.GetPassword: String;
begin
  Result := FParams[CDpbPassword];
end;

function TSilFbDatabaseParams.GetSqlDialect: Integer;
begin
  Result := FParams[CDpbSqlDialect];
end;

function TSilFbDatabaseParams.GetSqlRoleName: String;
begin
  Result := FParams[CDpbSqlRole];
end;

function TSilFbDatabaseParams.GetUserName: String;
begin
  Result := FParams[CDpbUserName];
end;

procedure TSilFbDatabaseParams.SetConnectTimeout(const Value: Integer);
begin
  FParams[CDpbConnectTimeout] := Value;
end;

procedure TSilFbDatabaseParams.SetPassword(const Value: String);
begin
  FParams[CDpbPassword] := Value;
end;

procedure TSilFbDatabaseParams.SetSqlDialect(const Value: Integer);
begin
  FParams[CDpbSqlDialect] := Value;
end;

procedure TSilFbDatabaseParams.SetSqlRoleName(const Value: String);
begin
  FParams[CDpbSqlRole] := Value;
end;

procedure TSilFbDatabaseParams.SetUserName(const Value: String);
begin
  FParams[CDpbUserName] := Value;
end;

end.
