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

unit SilVmDataConnection;

interface

uses
  Windows, SysUtils, Forms, Classes,

  Sil,
  SilData,
  SilProtocol,

  SilVmSocketProt,
  SilViConnection;

type
  TSilSqlConnection = class (
    // extends
    TComponent,
    // implements
    IUnknown,
    IConnectionClient)
  private
    FMachine: String;
    FConnection: TSilSocketProtocol;
    FActive: Boolean;
    FProtSql: IProtSqlClient;
    FConnectionStr: String;
    FClients: IInterfaceList;
    procedure SetActive(Value: Boolean);
    function GetProtSql: IProtSqlClient;
    procedure DoDisconnect;
    procedure SetConnection(const Value: TSilSocketProtocol);
    procedure DoConnect;
    procedure DoDeactivateChildren;
  protected // IConnectionClient
    function GetActive: Boolean;
    procedure Cleanup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClient(const Obj: IUnknown);
    procedure RemoveClient(const Obj: IUnknown);
    property ProtSql: IProtSqlClient read GetProtSql;
    procedure Open;
    procedure Close;
  published
    property ConnectionStr: String read FConnectionStr write FConnectionStr;
    property Active: Boolean read FActive write SetActive;
    property Connection: TSilSocketProtocol read FConnection write SetConnection;
  end;

procedure Register;

implementation

uses SilLtList;

procedure Register;
begin
	RegisterComponents('SIL', [TSilSqlConnection]);
end;

{ TSilSqlConnection }

constructor TSilSqlConnection.Create(AOwner: TComponent);
begin
  inherited;
  FClients := Sil.List.InterfaceList(true);
end;

destructor TSilSqlConnection.Destroy;
begin
  while FClients.Count > 0 do
    RemoveClient(FClients.Last);

  SetActive(false);
  SetConnection(nil);
  DoDisconnect;
  FClients := nil;

  inherited;
end;

function TSilSqlConnection.GetProtSql: IProtSqlClient;
begin
  Result := FProtSql;
end;

procedure TSilSqlConnection.DoConnect;
var
  lwSize, lwID: LongWord;
begin
  if FConnection.Blind.Request(IProtSqlClient, lwID) then
  begin
    FProtSql := SilProtocol.Tk.SqlClient(lwID, FConnection.Connection);

    lwSize := MAX_COMPUTERNAME_LENGTH + 1;
    SetLength(FMachine, lwSize);
    GetComputerName(PChar(FMachine), lwSize);
    SetLength(FMachine, lwSize);

    FProtSql.OpenDatabase(FMachine, FConnectionStr);
  end;
end;

procedure TSilSqlConnection.DoDeactivateChildren;
var
  i: Integer;
  Item: IConnectionClient;
begin
  if not (csDesigning in ComponentState) then Exit;

  for i := FClients.Count - 1 downto 0 do
    if Ref.GetInterface(FClients[i], IConnectionClient, Item) then
      Item.Active := false;
end;

procedure TSilSqlConnection.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      if (FConnection <> nil) and not FConnection.Active then FConnection.Active := true;
      if (FConnection = nil) or not FConnection.Active then Exit;
    end;

    if Value then
      DoConnect else
    begin
      DoDeactivateChildren;
      DoDisconnect;
    end;

    FActive := Value;
  end;
end;

procedure TSilSqlConnection.SetConnection(const Value: TSilSocketProtocol);
var
  OldValue: TSilSocketProtocol;
begin
  if FConnection <> Value then
  begin
    OldValue := FConnection;
    FConnection := Value;

    if OldValue <> nil then
      OldValue.RemoveClient(Self);
      
    if FConnection <> nil then
      FConnection.AddClient(Self) else
      SetActive(false);
  end;
end;

procedure TSilSqlConnection.DoDisconnect;
begin
  if FProtSql <> nil then
  begin
    if (FConnection <> nil) and (FConnection.Connection <> nil) then
    begin
      if FConnection.Connection.IsConnected then
        FProtSql.CloseDatabase;

      Sil.Sink.Disconnect(FConnection.Connection, FProtSql);
    end;

    Sil.Sink.Disconnect(FProtSql, Self);
    FProtSql := nil;
  end;
end;

procedure TSilSqlConnection.AddClient(const Obj: IUnknown);
begin
  FClients.Add(Obj);
end;

procedure TSilSqlConnection.RemoveClient(const Obj: IUnknown);
var
  Item: IConnectionClient;
begin
  if FClients.Remove(Obj) >= 0 then
    if Ref.GetInterface(Obj, IConnectionClient, Item) then
      Item.Cleanup;
end;

procedure TSilSqlConnection.Close;
begin
  Active := false;
end;

procedure TSilSqlConnection.Open;
begin
  Active := true;
end;

procedure TSilSqlConnection.Cleanup;
begin
  FConnection := nil;
end;

function TSilSqlConnection.GetActive: Boolean;
begin
  Result := FActive;
end;

end.
