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

unit SilSiSocketConnection;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiSocket,
  SilOiHandle,
  SilLiEnumerator,
  SilSiAbstractConnection;

type
  IServerSocketConnection = interface;

  TSocketConnectionEvent = record
    Sender: IServerSocketConnection;
  end;

  ISocketServerEvents = interface
    ['{E5A7E582-CE99-11D3-9869-00104B0FA1EF}']
    procedure OnListen(const Event: TSocketConnectionEvent);
    procedure OnListenFailed(const Event: TSocketConnectionEvent);
  end;

  ISocketConnection = interface(IAbstractConnection)
    ['{C43CC572-D8D8-4382-8BDA-45E150E01574}']
    function GetHandle: IHandle;
    function GetParameters: ISocketParameters;
    function GetStream: IStream;
    property Handle: IHandle read GetHandle;
    property Stream: IStream read GetStream;
    property Parameters: ISocketParameters read GetParameters;
  end;

  IClientSocketConnection = interface(ISocketConnection)
    ['{FE04C8A1-C9E4-11D3-9868-00104B0FA1EF}']
    function GetRequested: Boolean;
    function GetServer: IServerSocketConnection;
    function GetHostAddress: ISocketAddress;
    procedure SetHostAddress(const Value: ISocketAddress);
    function GetInfo: ISocketPeerInfo;
    function Connect(const ThreadName: string = ''): Boolean;
    procedure Disconnect(Wait: Boolean = false);
    property IsRequested: Boolean read GetRequested;
    property Server: IServerSocketConnection read GetServer;
    property HostAddress: ISocketAddress read GetHostAddress write SetHostAddress;
    property Info: ISocketPeerInfo read GetInfo;
  end;

  IClientSockets = interface
    ['{BCB1AD4A-E066-400C-85E0-332A649C97D8}']
    function GetCount: Integer;
    function Get(Index: Integer): IClientSocketConnection;
    function Enumerate(var Enum: IEnumerator; out Item: IClientSocketConnection): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    // properties
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IClientSocketConnection read Get; default;
  end;

  IServerSocketConnection = interface(ISocketConnection)
    ['{67F41BF2-954B-11D4-989E-00104B0FA1EF}']
    function GetListening: Boolean;
    function GetClients: IClientSockets;
    function GetInfo: ISocketInfo;
    function Listen(const ThreadName: string = ''): Boolean;
    procedure DisconnectClients;
    procedure ClientConnected(const Client: IClientSocketConnection);
    procedure ClientDisconnected(const Client: IClientSocketConnection);
    procedure Cancel(Wait: Boolean = false);
    procedure Bind(const Address: ISocketAddress = nil);
    function Accept(out Client: IClientSocketConnection): Boolean;
    property Info: ISocketInfo read GetInfo;
    property IsListening: Boolean read GetListening;
    property Clients: IClientSockets read GetClients;
  end;

implementation

end.
