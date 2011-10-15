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

unit SilSmSocketConnection;

{$I Defines.inc}

interface

uses
  SilSiAbstractConnection,
  SilLiEnumerator,
  SilOiIpc,
  SilLiEventList,
  SilLiInterfaceList,
  SilLiStream,
  SilOiHandle,
  SilOiSocket,
  SilSiSocketConnection,
  SilSmAbstractConnection,
  SilLiConnection,
  SilLmInterfaceList;

type
  TSocketConnection = class (
    // extends
    TAbstractConnection,
    // implements
    IAbstractConnection)
  private
    FSocket: ISocket;
  protected // IAbstractConnection
    function GetBufferSize: LongWord; override;
    function GetConnected: Boolean; override;
  protected
    function GetHandle: IHandle;
    function GetParameters: ISocketParameters;
  end;

  TClientSocketConnection = class (
    // extends
    TSocketConnection,
    // implements
    IAbstractConnection,
    IClientSocketConnection)
  private
    FClient: ISocketClient;
    FServer: IServerSocketConnection;
    FHostAddress: ISocketAddress;
  protected
    function Initialize: Boolean; override;
    procedure DoDisconnect; override;
  protected
    function GetRequested: Boolean;
    function GetServer: IServerSocketConnection;
    function GetHostAddress: ISocketAddress;
    procedure SetHostAddress(const Value: ISocketAddress);
    function GetStream: IStream;
    function GetInfo: ISocketPeerInfo;
  public
    constructor Create(const Socket: ISocketClient = nil; NewThread: Boolean = true); overload;
    constructor Create(const Socket: ISocketClient; const Server: IServerSocketConnection); overload;
    destructor Destroy; override;
  end;

  TServerSocketConnection = class (
    // extends
    TSocketConnection,
    // implements
    IAbstractConnection,
    IServerSocketConnection)
  private
    FServer: ISocketServer;
    FClients: IInterfaceList;
    FSockets: IClientSockets;
    FAlone: IEvent;
  protected
    procedure FireConnected; override;
    procedure FireFailed; override;
    function Initialize: Boolean; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure Cleanup; override;
  protected
    function Listen(const ThreadName: string = ''): Boolean;
    procedure DisconnectClients;
    procedure ClientConnected(const Client: IClientSocketConnection);
    procedure ClientDisconnected(const Client: IClientSocketConnection);
    procedure Cancel(Wait: Boolean);
    procedure Bind(const Address: ISocketAddress = nil);
    function Accept(out Client: IClientSocketConnection): Boolean;
    function GetListening: Boolean;
    function GetStream: IStream;
    function GetClients: IClientSockets;
    function GetInfo: ISocketInfo;
  public
    constructor Create(const Socket: ISocketServer = nil; NewThread: Boolean = true);
    destructor Destroy; override;
  end;

  TSilClientSocketList = class (
    // extends
    TSilInterfaceList,
    // implements
    IClientSockets)
  protected // ISockets
    function Get(Index: Integer): IClientSocketConnection;
    function Enumerate(var Enum: IEnumerator; out Item: IClientSocketConnection): Boolean; reintroduce;
  public
    constructor Create;
  end;

implementation

uses
  SilBtInt,
  SilLtTool,
  SilLtList,
  SilOsTool,
  SilOsClasses,
  SilOtTool,
  SilOsTypes;

{ TSocketConnection }

function TSocketConnection.GetConnected: Boolean;
begin
  Result := FConnected and FSocket.Handle.IsValid;
end;

function TSocketConnection.GetBufferSize: LongWord;
begin
  with FSocket.Parameters do
    Result := Int.Min(ReceiveBufferSize, SendBufferSize);
end;

function TSocketConnection.GetHandle: IHandle;
begin
  Result := FSocket.Handle;
end;

function TSocketConnection.GetParameters: ISocketParameters;
begin
  Result := FSocket.Parameters;
end;

{ TServerSocketConnection }

function TServerSocketConnection.Accept(out Client: IClientSocketConnection): Boolean;
var
  NewSocket: ISocketClient;
begin
  Result := FServer.Accept(NewSocket);
  if Result then Client := TClientSocketConnection.Create(NewSocket, Self);
end;

function TServerSocketConnection.Listen(const ThreadName: string): Boolean;
begin
  Result := inherited Connect(ThreadName);
end;

procedure TServerSocketConnection.Cancel(Wait: Boolean);
begin
  inherited Disconnect(Wait);
end;

procedure TServerSocketConnection.FireConnected;
var
  n: IEnumerator;
  l: ISocketServerEvents;
  Event: TSocketConnectionEvent;
begin
  if Self.Events = nil then Exit;
  Event.Sender := Self;

  Events.Enumerate(n, l, ISocketServerEvents);
  n := nil;
  if l <> nil then l.OnListen(Event);
end;

procedure TServerSocketConnection.FireFailed;
var
  n: IEnumerator;
  l: ISocketServerEvents;
  Event: TSocketConnectionEvent;
begin
  if Self.Events = nil then Exit;
  Event.Sender := Self;

  with Events do
    while Enumerate(n, l, ISocketServerEvents) do
      l.OnListenFailed(Event);
end;

function TServerSocketConnection.GetListening: Boolean;
begin
  Result := (FServer <> nil) and FServer.IsListening;
end;

procedure TServerSocketConnection.DoDisconnect;
begin
  if FServer <> nil then FServer.Cancel;
end;

function TServerSocketConnection.Initialize: Boolean;
begin
  if (FServer <> nil) and FServer.Handle.IsValid then
    try
      FServer.Listen;
    except
      //
    end;

  Result := FServer.IsListening;
end;

constructor TServerSocketConnection.Create(const Socket: ISocketServer; NewThread: Boolean);
begin
  inherited Create;

  FAlone := OS.IPC.Event(true, true);
  FClients := TSilClientSocketList.Create;
  FSockets := FClients as IClientSockets;

  if Socket = nil then
    //FServer := OS.Socket.StreamServer else
    FServer := TSilOsSocket.Create else
    FServer := Socket;

  FSocket := FServer;

  if FServer.IsListening then
  begin
    FConnected := true;
    if NewThread then SpawnThread;
  end;
end;

function TServerSocketConnection.GetConnected: Boolean;
begin
  Result := (FSocket.Handle <> nil) and FSocket.Handle.IsValid;
end;

function TServerSocketConnection.GetClients: IClientSockets;
begin
  Result := FSockets;
end;

procedure TServerSocketConnection.DisconnectClients;
var
  e: IEnumerator;
  Item: IClientSocketConnection;
begin
  while FSockets.Enumerate(e, Item) do
    Item.Disconnect(true);

  FAlone.WaitFor(INFINITE, true);
end;

procedure TServerSocketConnection.ClientConnected(const Client: IClientSocketConnection);
begin
  FClients.Locked;
  FClients.Add(Client);
  if FClients.Count > 0 then FAlone.Reset;
end;

procedure TServerSocketConnection.ClientDisconnected(const Client: IClientSocketConnection);
begin
  FClients.Locked;
  FClients.Remove(Client);
  if FClients.Count = 0 then FAlone.Signal;
end;

procedure TServerSocketConnection.Cleanup;
begin
  inherited;
  FClients.Clear;
end;

function TServerSocketConnection.GetStream: IStream;
begin
  Result := nil;
end;

procedure TServerSocketConnection.Bind(const Address: ISocketAddress);
begin
  if FServer <> nil then
    FServer.Bind(Address);
end;

destructor TServerSocketConnection.Destroy;
begin
  FServer := nil;
  FClients := nil;
  FSockets := nil;
  FAlone := nil;

  inherited;
end;

function TServerSocketConnection.GetInfo: ISocketInfo;
begin
  Result := FServer.Info;
end;

{ TClientSocketConnection }

constructor TClientSocketConnection.Create(const Socket: ISocketClient; const Server: IServerSocketConnection);
begin
  Create(Socket, false);
  FServer := Server;
end;

constructor TClientSocketConnection.Create(const Socket: ISocketClient; NewThread: Boolean);
begin
  inherited Create;

  if Socket = nil then
    //FClient := OS.Socket.StreamClient else
    FClient := TSilOsSocket.Create else
    FClient := Socket;

  FSocket := FClient;
  SetStream(FClient.Stream);

  if FClient.IsConnected then
  begin
    FConnected := true;
    if NewThread then SpawnThread('client');
  end;
end;

destructor TClientSocketConnection.Destroy;
begin
  inherited;
end;

procedure TClientSocketConnection.DoDisconnect;
begin
  FClient.Disconnect;

  if FServer <> nil then
  begin
    FServer.ClientDisconnected(Self);
    FServer := nil;
  end;
end;

function TClientSocketConnection.GetRequested: Boolean;
begin
  Result := FClient.IsLocal;
end;

function TClientSocketConnection.GetServer: IServerSocketConnection;
begin
  Result := FServer;
end;

function TClientSocketConnection.GetStream: IStream;
begin
  Result := FClient.Stream;
end;

function TClientSocketConnection.GetHostAddress: ISocketAddress;
begin
  Result := FHostAddress;
end;

procedure TClientSocketConnection.SetHostAddress(const Value: ISocketAddress);
begin
  FHostAddress := Value;
end;

function TClientSocketConnection.Initialize: Boolean;
begin
  if not FClient.IsConnected {FClient.Handle.IsValid} then
    try
      FClient.Connect(FHostAddress);
    except
      //
    end;

  Result := FClient.IsConnected;
  if Result and (FServer <> nil) then FServer.ClientConnected(Self);
end;

function TClientSocketConnection.GetInfo: ISocketPeerInfo;
begin
  Result := FClient.Info;
end;

{ TSilClientSocketList }

constructor TSilClientSocketList.Create;
begin
  inherited Create(true);
end;

function TSilClientSocketList.Enumerate(var Enum: IEnumerator; out Item: IClientSocketConnection): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilClientSocketList.Get(Index: Integer): IClientSocketConnection;
begin
  Result := IClientSocketConnection(inherited GetItem(Index));
end;

end.
