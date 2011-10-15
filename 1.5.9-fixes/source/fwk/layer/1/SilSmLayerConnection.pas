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

unit SilSmLayerConnection;

{$I Defines.inc}

interface

uses
  Sil,
  SilLayer,
  SilCoder,
  SilLkInterfaced,
  SilLmInterfaceList,

  SilSiKeepAlive,
  SilSmKeepAlive,
  SilSiLayerConnection;

type
  TSilLayerConnection = class;
  TSilLayerConnectionServer = class;
  TSilLayerConnectionList = class;

  TSilLayerConnection = class (
    TSilInterfacedObject,
    ILayerConnection,
    ILayerActivationEvents)
  private
    FList: Pointer;
    FChain: ILayerChain;
    FManager: ILayerConnectionManager;
    FContext: IUnknown;
    FMode: TLayerConnectionMode;
  protected
    function DoGetList: ILayerConnectionList;
    procedure DoInitLayers;
  protected // ILayerConnection
    function GetChain: ILayerChain;
    function GetContext: IUnknown;
    function GetManager: ILayerConnectionManager;
    function GetMode: TLayerConnectionMode;
    procedure SetContext(const Value: IUnknown);
    procedure Configure(const Manager: ILayerConnectionManager; const Params: IParameters);
    procedure Start(const Params: IParameters); virtual; abstract;
    procedure Stop; virtual;
  protected // ILayerActivationEvents
    procedure OnLayerActivated(const Event: RLayerActivated);
    procedure OnLayerDeactivated(const Event: RLayerDeactivated);
  public
    constructor Create(const List: ILayerConnectionList = nil);
    destructor Destroy; override;
  end;

  TSilLayerConnectionPeer = class (
    TSilLayerConnection,
    IKeepAliveEvents)
  private
    FKeepAlive: IKeepAlive;
  private
    procedure DoFireConnectionFailed;
    procedure DoReleaseKeepAlive;
  protected // ILayerConnection
    procedure Start(const Params: IParameters); override;
    procedure Stop; override;
  protected // IKeepAliveEvents
    procedure OnKeepAlive(const Sender: IKeepAlive; out Success: Boolean);
  public
    constructor CreatePeer(const List: ILayerConnectionList; const Chain: ILayerChain);
  end;

  TSilLayerConnectionServer = class (
    TSilLayerConnection,
    ILayerConnectionServer,
    ILayerChainSource)
  protected // ILayerConnection
    procedure Start(const Params: IParameters); override;
  protected // ILayerConnectionServer
    function GetClients: ILayerConnections;
    function GetClientList: ILayerConnectionList;
    property Clients: ILayerConnections read GetClients;
  protected // ILayerChainSource
    procedure CreateChain(const Chain: ILayerChain; const Context: IUnknown);
    procedure DestroyChain(const Chain: ILayerChain; const Context: IUnknown);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSilLayerConnectionList = class (
    // extends
    TSilInterfaceList,
    // implements
    ILayerConnections,
    ILayerConnectionList)
  private
    procedure DoStopAll;
  protected // ILayerConnections
    function GetItem(Index: Integer): ILayerConnection;
    function Enumerate(var Enum: IEnumerator; out Item: ILayerConnection): Boolean; reintroduce;
    property Items[Index: Integer]: ILayerConnection read GetItem; default;
  protected // ILayerConnectionList
    function Add(const Value: ILayerConnection): Integer; reintroduce;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilSiLayerProtocolBlind;

{ TSilLayerConnectionList }

constructor TSilLayerConnectionList.Create;
begin
  inherited Create(true);
end;

destructor TSilLayerConnectionList.Destroy;
begin
  DoStopAll;
  inherited;
end;

function TSilLayerConnectionList.Add(const Value: ILayerConnection): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilLayerConnectionList.Enumerate(var Enum: IEnumerator; out Item: ILayerConnection): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilLayerConnectionList.GetItem(Index: Integer): ILayerConnection;
begin
  Result := ILayerConnection(inherited GetItem(Index));
end;

procedure TSilLayerConnectionList.DoStopAll;
var
  Enum: IEnumerator;
  Connection: ILayerConnection;
begin
  while Enumerate(Enum, Connection) do
    Connection.Stop;
end;

{ TSilLayerConnection }

constructor TSilLayerConnection.Create(const List: ILayerConnectionList);
begin
  inherited Create;
  FList := Pointer(List);
  FMode := cmClientPeer;
end;

destructor TSilLayerConnection.Destroy;
begin
  Stop;

  FList := nil;
  FContext := nil;
  FManager := nil;

  inherited;
end;

function TSilLayerConnection.DoGetList: ILayerConnectionList;
begin
  Result := ILayerConnectionList(FList);
end;

procedure TSilLayerConnection.Stop;
var
  List: ILayerConnectionList;
begin
  if Assigned(FChain) then
  begin
    FChain.Clear(true);

    Sil.Sink.Disconnect(FChain, Self);
    FChain := nil;

    if Assigned(FManager) and (FMode <> cmServer) then
      FManager.Disconnected(Self);
  end;

  List := DoGetList;

  if Assigned(List) then
    List.Remove(Self);
end;

function TSilLayerConnection.GetChain: ILayerChain;
begin
  Result := FChain;
end;

procedure TSilLayerConnection.DoInitLayers;
begin
  if not Assigned(FChain) then
    FChain := SilLayer.Layer.Chain;

  if Assigned(FManager) {and (FMode in [cmClientPeer, cmServer])} then
    FManager.Initialize(Self, FChain);

  Sil.Sink.Connect(FChain, Self);

  if Assigned(FManager) and (FMode <> cmServer) then
    FManager.Connected(Self);
end;

function TSilLayerConnection.GetContext: IUnknown;
begin
  Result := FContext;
end;

procedure TSilLayerConnection.SetContext(const Value: IUnknown);
begin
  FContext := Value;
end;

procedure TSilLayerConnection.Configure(const Manager: ILayerConnectionManager; const Params: IParameters);
begin
  FManager := Manager;
end;

function TSilLayerConnection.GetManager: ILayerConnectionManager;
begin
  Result := FManager;
end;

procedure TSilLayerConnection.OnLayerActivated(const Event: RLayerActivated);
begin
end;

procedure TSilLayerConnection.OnLayerDeactivated(const Event: RLayerDeactivated);
begin
  if (FMode = cmClientPeer) and Event.IsBroken then
  begin
    Stop;
    Start(nil);
  end;
end;

function TSilLayerConnection.GetMode: TLayerConnectionMode;
begin
  Result := FMode;
end;

{ TSilLayerConnectionServer }

constructor TSilLayerConnectionServer.Create;
begin
  inherited Create(nil);

  FMode := cmServer;
  ILayerConnectionList(FList) := TSilLayerConnectionList.Create;
end;

destructor TSilLayerConnectionServer.Destroy;
begin
  ILayerConnectionList(FList) := nil;
  inherited;
end;

function TSilLayerConnectionServer.GetClients: ILayerConnections;
begin
  Result := ILayerConnections(FList);
end;

function TSilLayerConnectionServer.GetClientList: ILayerConnectionList;
begin
  Result := ILayerConnectionList(FList);
end;

procedure TSilLayerConnectionServer.Start(const Params: IParameters);
begin
  DoInitLayers;
end;

procedure TSilLayerConnectionServer.CreateChain(const Chain: ILayerChain; const Context: IInterface);
var
  Connection: ILayerConnection;
  Connections: ILayerConnectionList;
  Manager: ILayerConnectionManager;
begin
  Connections := GetClientList;
  Connection := TSilLayerConnectionPeer.CreatePeer(Connections, Chain);
  Connections.Add(Connection);

  if Assigned(FManager) then
    FManager.Connected(Connection);

  if not Assigned(Connection.Manager) then
    if Ref.GetInterface(Connection.Context, ILayerConnectionManager, Manager) then
      Connection.Configure(Manager);

  Connection.Start;
end;

procedure TSilLayerConnectionServer.DestroyChain(const Chain: ILayerChain; const Context: IInterface);
var
  Enum: IEnumerator;
  Connection: ILayerConnection;
  Connections: ILayerConnectionList;
begin
  Connections := GetClientList;

  while Connections.Enumerate(Enum, Connection) do
    if Connection.Chain.Id = Chain.Id then
    begin
      Connection.Stop;

      if Assigned(FManager) then
        FManager.Disconnected(Connection);
      Break;
    end;
end;

{ TSilLayerConnectionPeer }

constructor TSilLayerConnectionPeer.CreatePeer(const List: ILayerConnectionList; const Chain: ILayerChain);
begin
  Create(List);
  FChain := Chain;
  FMode := cmServerPeer;
end;

procedure TSilLayerConnectionPeer.Start(const Params: IParameters);
begin
  if FMode = cmClientPeer then
  begin
    FKeepAlive := TSilKeepAlive.Create;
    Sil.Sink.Connect(FKeepAlive, Self);
    FKeepAlive.Start(Sil.List.Params.Get(Params, 'Interval', 5000));
  end else
    DoInitLayers;
end;

procedure TSilLayerConnectionPeer.Stop;
begin
  inherited;
  DoReleaseKeepAlive;
end;

procedure TSilLayerConnectionPeer.DoFireConnectionFailed;
var
  Client: ILayerClientConnectionManager;
begin
  if (FMode = cmClientPeer) and Ref.GetInterface(FManager, ILayerClientConnectionManager, Client) then
    Client.ConnectFailed(Self);
end;

procedure TSilLayerConnectionPeer.DoReleaseKeepAlive;
begin
  if Assigned(FKeepAlive) then
  begin
    FKeepAlive.Stop;
    Sil.Sink.Disconnect(FKeepAlive, Self);
    //FKeepAlive := nil;
  end;
end;

procedure TSilLayerConnectionPeer.OnKeepAlive(const Sender: IKeepAlive; out Success: Boolean);
begin
  try
    DoInitLayers;
    Success := true;
    DoReleaseKeepAlive;
  except
    inherited Stop;
    DoFireConnectionFailed;
    Success := false;
  end;
end;

end.
