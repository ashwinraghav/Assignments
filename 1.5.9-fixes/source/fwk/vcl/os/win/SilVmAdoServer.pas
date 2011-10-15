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

unit SilVmAdoServer;

interface

uses
  SysUtils, Forms, Classes, Activex,

  Sil, SilTool,
  SilProtocol,

  SilSiAbstractConnection,
  SilSiSocketConnection,
  SilSiProtocolBase,
  
  SilViAdoSession,
  SilVmAdoSession;

type
  IAdoServer = interface
    ['{859294B8-0949-492B-9087-C9906BC755D4}']
    function GetPort: Word;
    procedure SetPort(Value: Word);
    procedure Start;
    procedure Stop;
    property Port: Word read GetPort write SetPort;
  end;

  TAdoServer = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAdoServer,
    IConnectingEvents,
    IConnectedEvents,
    ISocketServerEvents)
  private
    FSessions: ISqlSessionList;
    FSockSrv: IServerSocketConnection;
    FClients: IInterfaceList;
    FPort: Word;
  protected // IConnectingEvents
    procedure OnConnected(const Event: TConnectionEvent);
    procedure OnFailed(const Event: TConnectionFailedEvent);
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent);
    procedure OnDataSent(const Event: TConnectionDataEvent);
    procedure OnDisconnected(const Event: TConnectionBreakEvent);
  protected // ISocketServerEvents
    procedure OnListen(const Event: TSocketConnectionEvent);
    procedure OnListenFailed(const Event: TSocketConnectionEvent);
  protected // IAdoServer
    function GetPort: Word;
    procedure SetPort(Value: Word);
    procedure Start;
    procedure Stop;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  IAdoClient = interface
    ['{F32A209C-F58E-4ED7-95A6-3E8CB6C7ADB3}']
    function GetConnection: IAbstractConnection;
    procedure Unbind;
    property Connection: IAbstractConnection read GetConnection;
  end;

  TAdoClient = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAdoClient,
    IBlindProtocolEvents,
    IProtSqlServerEvents)
  private
    FConnection: IAbstractConnection;
    FProtBlind: IBlindProtocol;
    FProtSql: IProtSqlServer;
    FSessions: ISqlSessionList;
    FAppID: String;
  protected // IBlindProtocolEvents
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindProtocolIDEvent);
  protected // IProtSqlServerEvents
    procedure OnOpenDatabase(var Event: TProtSqlServerEvent);
    procedure OnCloseDatabase(var Event: TProtSqlCloseEvent);
    procedure OnQuery(var Event: TProtSqlQueryEvent);
    procedure OnExecute(var Event: TProtSqlExecuteEvent);
    procedure OnStoredProc(var Event: TProtSqlStoredProcEvent);
    procedure OnQueryFields(var Event: TProtSqlQueryFieldsEvent);
    procedure OnQueryStoredProcParams(var Event: TProtSqlQueryFieldsEvent);
  protected // IAdoClient
    function GetConnection: IAbstractConnection;
    procedure Unbind;
  public
    constructor Create(const Sessions: ISqlSessionList; const Connection: IAbstractConnection);
    destructor Destroy; override;
  end;

type
  TSilAdoSocketServer = class (TComponent)
  private
    FServer: IAdoServer;
    function GetPort: Word;
    procedure SetPort(const Value: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Port: Word read GetPort write SetPort;
  end;

implementation

uses
  SilStSocketConnection;

{ TAdoClient }

constructor TAdoClient.Create(const Sessions: ISqlSessionList; const Connection: IAbstractConnection);
begin
  inherited Create;

  FConnection := Connection;
  FSessions := Sessions;
  FProtBlind := SilProtocol.Tk.Blind(FConnection);
  Sil.Sink.Connect(FProtBlind, Self);
end;

destructor TAdoClient.Destroy;
begin
  Unbind;
  inherited;
end;

function TAdoClient.GetConnection: IAbstractConnection;
begin
  if FProtSql <> nil then
    Result := (FProtSql as IProtocol).Connection else
    Result := nil;
end;

procedure TAdoClient.Unbind;

  function DoUnbind(const Obj: IUnknown): Boolean;
  begin
    Result := Obj <> nil;
    if Result then Sil.Sink.Disconnect(Obj, Self);
  end;

begin
  if DoUnbind(FProtSql) then FProtSql := nil;
  if DoUnbind(FProtBlind) then FProtBlind := nil;

  if FSessions <> nil then
  begin
    FSessions.Disconnect(FAppID);
    FSessions := nil;
  end;
end;

procedure TAdoClient.OnRequest(var Event: RBlindProtocolIDEvent);
begin
  if Guid.Compare(Event.Obj, IProtSqlClient) = 0 then
  begin
    if FProtSql = nil then
    begin
      FProtBlind.AddFilter(Event.Id);
      FProtSql := SilProtocol.Tk.SqlServer(Event.Id, FConnection);
      Sil.Sink.Connect(FProtSql, Self);
    end else
      Event.Id := (FProtSql as IFormatedProtocol).ProtocolID;
  end;
end;

procedure TAdoClient.OnText(var Event: RBlindTextEvent);
begin             
end;

procedure TAdoClient.OnOpenDatabase(var Event: TProtSqlServerEvent);
begin
  if FSessions <> nil then
  begin
    Event.Info.User := Event.AppID;
    FAppID := FSessions.CreateItem(Event.Sender.RemoteBuild, Event.Info);
  end else
    FAppID := '';
end;

procedure TAdoClient.OnCloseDatabase(var Event: TProtSqlCloseEvent);
begin
end;

procedure TAdoClient.OnExecute(var Event: TProtSqlExecuteEvent);
begin
  if FSessions <> nil then
    Event.Records := FSessions.Execute(FAppID, Event.QueryStr) else
    Event.Records := -1;
end;

procedure TAdoClient.OnQuery(var Event: TProtSqlQueryEvent);
begin
  if FSessions <> nil then
    Event.Rowset := FSessions.Query(FAppID, Event.QueryStr) else
    Event.Rowset := nil;
end;

procedure TAdoClient.OnStoredProc(var Event: TProtSqlStoredProcEvent);
begin
  if FSessions <> nil then
    Event.Rowset := FSessions.StoredProc(FAppID, Event.ProcName, Event.Params, Event.WithRecords) else
    Event.Rowset := nil;
end;

procedure TAdoClient.OnQueryFields(var Event: TProtSqlQueryFieldsEvent);
begin
  if FSessions <> nil then
    FSessions.QueryFields(FAppID, Event.Command, Event.Fields);
end;

procedure TAdoClient.OnQueryStoredProcParams(var Event: TProtSqlQueryFieldsEvent);
begin
  if FSessions <> nil then
    FSessions.QueryStoredProcParams(FAppID, Event.Command, Event.Fields);
end;

{ TAdoServer }

constructor TAdoServer.Create;
begin
  inherited Create;
  FSessions := TSqlSessionList.Create;
  FClients := Sil.List.InterfaceList(true);
end;

destructor TAdoServer.Destroy;
begin
  FSessions := nil;
  FClients := nil;

  inherited;
end;

procedure TAdoServer.Start;
begin
  FSockSrv := SilStSocketConnection.SocketConnection.CreateServer(0, FPort);
  Sil.Sink.Connect(FSockSrv, Self);
  FSockSrv.Listen;
end;

procedure TAdoServer.Stop;
begin
  if FSockSrv <> nil then
  begin
    FSockSrv.Cancel;
    FSockSrv.DisconnectClients;
    Sil.Sink.Disconnect(FSockSrv, Self);
  end;
end;

procedure TAdoServer.OnListen(const Event: TSocketConnectionEvent);
var
  Client: IClientSocketConnection;
begin
  while Event.Sender.Accept(Client) do
  begin
    Sil.Sink.Connect(Client, Self);
    Client.Parameters.WriteTimeout := 60000;
    Client.SpawnThread;
  end;
end;

procedure TAdoServer.OnListenFailed(const Event: TSocketConnectionEvent);
begin
end;

procedure TAdoServer.OnConnected(const Event: TConnectionEvent);
var
  Client: IAdoClient;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  Client := TAdoClient.Create(FSessions, Event.Sender);
  FClients.Add(Client);
end;

procedure TAdoServer.OnDataReceived(const Event: TConnectionDataEvent);
begin
end;

procedure TAdoServer.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

procedure TAdoServer.OnDisconnected(const Event: TConnectionBreakEvent);
var
  e: IEnumerator;
  Item: IUnknown;
  Client: IAdoClient;
begin
  while FClients.Enumerate(e, Item) do
  begin
    Client := Item as IAdoClient;

    if Sil.Ref.SameObject(Client.Connection, Event.Sender) then
    begin
      FClients.Delete(e.Iteration);
      Client.Unbind;
    end;
  end;

  Sil.Sink.Disconnect(Event.Sender, Self);
  CoUninitialize;
end;

procedure TAdoServer.OnFailed(const Event: TConnectionFailedEvent);
begin               
end;

function TAdoServer.GetPort: Word;
begin
  Result := FPort;
end;

procedure TAdoServer.SetPort(Value: Word);
begin
  FPort := Value;
end;

{ TSilAdoSocketServer }

constructor TSilAdoSocketServer.Create(AOwner: TComponent);
begin
  inherited;     
  FServer := TAdoServer.Create;
end;

destructor TSilAdoSocketServer.Destroy;
begin
  FServer := nil;
  inherited;
end;

function TSilAdoSocketServer.GetPort: Word;
begin
  Result := FServer.Port;
end;

procedure TSilAdoSocketServer.SetPort(const Value: Word);
begin
  FServer.Port := Value;
end;

procedure TSilAdoSocketServer.Start;
begin
  FServer.Start;
end;

procedure TSilAdoSocketServer.Stop;
begin
  FServer.Stop;
end;

end.
