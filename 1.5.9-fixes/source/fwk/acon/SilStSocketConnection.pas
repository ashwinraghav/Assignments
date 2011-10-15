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

unit SilStSocketConnection;

{$I Defines.inc}

interface

uses
  SilOiSocket,
  SilBkTool,
  SilSiSocketConnection;

type
  SvcSocketConnectionClass = class of SocketConnection;

  SocketConnection = class(Tool)
    class function CreateClient(const Socket: ISocketClient; NewThread: Boolean = true): IClientSocketConnection; overload;
    class function CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol = spTcp; Address: LongWord = 0; Port: Word = 0): IClientSocketConnection; overload;
    class function CreateClient(Address: LongWord = 0; Port: Word = 0): IClientSocketConnection; overload;
    class function CreateClient(const Host: String; Port: Word = 0): IClientSocketConnection; overload;
    class function CreateServer(const Socket: ISocketServer; NewThread: Boolean = true): IServerSocketConnection; overload;
    class function CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol = spTcp; Address: LongWord = 0; Port: Word = 0): IServerSocketConnection; overload;
    class function CreateServer(Address: LongWord = 0; Port: Word = 0): IServerSocketConnection; overload;
    class function CreateServer(const Host: String; Port: Word = 0): IServerSocketConnection; overload;
  end;

implementation

uses
  SilOsSocket,
  SilSmSocketConnection;

{ Connection }

class function SocketConnection.CreateClient(const Socket: ISocketClient; NewThread: Boolean): IClientSocketConnection;
begin
  Result := TClientSocketConnection.Create(Socket, NewThread);
end;

class function SocketConnection.CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: LongWord; Port: Word): IClientSocketConnection;
begin
  Result := TClientSocketConnection.Create;
  Result.HostAddress := OsIP.Create(TypeSpec, Protocol, Address, Port);
end;

class function SocketConnection.CreateServer(const Socket: ISocketServer; NewThread: Boolean): IServerSocketConnection;
begin
  Result := TServerSocketConnection.Create(Socket, NewThread);
end;

class function SocketConnection.CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: LongWord; Port: Word): IServerSocketConnection;
var
  Addr: ISocketAddress;
begin
  Result := TServerSocketConnection.Create;
  Addr := OsIP.Create(TypeSpec, Protocol, Address, Port);
  Result.Bind(Addr);
end;

class function SocketConnection.CreateClient(Address: LongWord; Port: Word): IClientSocketConnection;
begin
  Result := CreateClient(stStream, spTCP, Address, Port);
end;

class function SocketConnection.CreateServer(Address: LongWord; Port: Word): IServerSocketConnection;
begin
  Result := CreateServer(stStream, spTCP, Address, Port);
end;

class function SocketConnection.CreateClient(const Host: String; Port: Word): IClientSocketConnection;
begin
  Result := CreateClient(OsIP.FromStr(Host), Port);
end;

class function SocketConnection.CreateServer(const Host: String; Port: Word): IServerSocketConnection;
begin
  Result := CreateServer(OsIP.FromStr(Host), Port);
end;

end.
 