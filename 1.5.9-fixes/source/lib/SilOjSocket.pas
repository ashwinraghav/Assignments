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

unit SilOjSocket;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiStringList,
  SilOcTypes,
  SilOiSocket;

type
  IP = class (Tool)
    class function FromHostName(const Host: String): LongWord; virtual; abstract;
    class function FromHostAddress(const Address: String): LongWord; virtual; abstract;
    class function HostToNetwork(Host: LongWord): LongWord; virtual; abstract;
    class function PortToNetwork(Port: Word): Word; virtual; abstract;
    class function NetworkToHost(NetworkAddress: LongWord): LongWord; virtual; abstract;
    class function NetworkToPort(NetworkPort: Word): Word; virtual; abstract;
    class function ToStr(HostAddress: LongWord): String; virtual; abstract;
    class function ToHostName(HostAddress: LongWord): String; virtual; abstract;
    class function FromStr(const Address: String): LongWord; virtual;
    class function IsDottedFormat(const Address: String): Boolean;
    class function Create(TypeSpec: TSocketType = stUnknown; Protocol: TSocketProtocol = spUnknown; Address: LongWord = 0; Port: Word = 0): ISocketAddressDef; overload;
    class function Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word = 0): ISocketAddressDef; overload;
    class function Create(const Host: String; Port: Word): ISocketAddressDef; overload;
    class function Create(Address: LongWord; Port: Word): ISocketAddressDef; overload;
    class function CreateList(Locked: Boolean = false; TypeData: Pointer = nil): ISocketAddressList;
  end;

  Host = class (Tool)
    class function GetByAddress(Address: LongWord): ISocketAddress; overload; virtual;
    class function GetByAddress(const Address: String): ISocketAddress; overload; virtual;
    class function GetByName(const Host: String): ISocketAddress; virtual; abstract;
    class function LocalName: String; virtual; abstract;
  end;

  Info = class (Tool)
    class function Service(Port: Word; const Protocol: String = ''): ISocketService; overload; virtual; abstract;
    class function Service(const Name: String; const Protocol: String = ''): ISocketService; overload; virtual; abstract;
    class function Protocol(Number: Integer): ISocketProtocol; overload; virtual; abstract;
    class function Protocol(const Name: String): ISocketProtocol; overload; virtual; abstract;
    class function AdapterList: ISocketAddresses; virtual; abstract;
  end;

  Socket = class (Tool)
    class function CreateClient: ISocketClient; overload;
    class function CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol = spUnknown; Address: Cardinal = 0; Port: Word = 0): ISocketClient; overload;
    class function CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word = 0): ISocketClient; overload;
    class function CreateServer: ISocketServer; overload;
    class function CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol = spUnknown; Address: Cardinal = 0; Port: Word = 0): ISocketServer; overload;
    class function CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word = 0): ISocketServer; overload;
    class function CreateList(Locked: Boolean = false): ISocketList;
    class function WaitFor(const States: TSocketStates; const ListIn: ISockets; var ListOut: ISocketList; Timeout: LongWord = INFINITE): Boolean; virtual; abstract;
    class function LastError: Integer; virtual; abstract;
    class function ErrorMessage(const ErrCode: Integer): String; virtual; abstract;
  end;

implementation

uses
  SilBtText,
  SilBtStr,
  SilOkSocket,
  SilOsClasses,
  SilOsTool,
  SilOsSocket;

{ IPAddr }

class function IP.Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: LongWord; Port: Word): ISocketAddressDef;
begin
  Result := TSilSocketAddress.Create(TypeSpec, Protocol, Address, Port);
end;

class function IP.Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word): ISocketAddressDef;
begin
  Result := TSilSocketAddress.Create(TypeSpec, Protocol, FromStr(Host), Port);
end;

class function IP.Create(const Host: String; Port: Word): ISocketAddressDef;
begin
  Result := TSilSocketAddress.Create(stUnknown, spUnknown, FromStr(Host), Port);
end;

class function IP.Create(Address: LongWord; Port: Word): ISocketAddressDef;
begin
  Result := TSilSocketAddress.Create(stUnknown, spUnknown, Address, Port);
end;

class function IP.CreateList(Locked: Boolean; TypeData: Pointer): ISocketAddressList;
begin
  Result := TSilSocketAddressList.Create(Locked, TypeData);
end;

class function IP.IsDottedFormat(const Address: String): Boolean;
var
  i, iDots: Integer;
begin
  Result := false;
  iDots := 0;

  if Length(Address) = 0 then
    Exit;

  for i := 1 to Length(Address) do
    if Address[i] = '.' then
      Inc(iDots) else
    if not (Address[i] in ['0'..'9']) then
    begin
      iDots := -1;
      Break;
    end;

  Result := iDots = 3
end;

class function IP.FromStr(const Address: String): LongWord;
begin
  if Str.NotEmpty(Address) then
  begin
    if not IsDottedFormat(Address) then
    begin
      Result := FromHostName(Address);
      if (Result = 0) and (Text.Compare('localhost', Address) = 0) then
        Result := FromHostAddress('127.0.0.1');
    end else
      Result := FromHostAddress(Address);
  end else
    Result := 0;
end;

{ Host }

class function Host.GetByAddress(const Address: String): ISocketAddress;
begin
  Result := GetByAddress(OsIP.FromStr(Address));
end;

class function Host.GetByAddress(Address: LongWord): ISocketAddress;
begin
  Result := TSilSocketAddress.Create(stStream, spTCP, Address, 0);
end;

{ Socket }

class function Socket.CreateClient: ISocketClient;
begin
  Result := TSilOsSocket.Create;
end;

class function Socket.CreateServer: ISocketServer;
begin
  Result := TSilOsSocket.Create;
end;

class function Socket.CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: Cardinal; Port: Word): ISocketClient;
var
  Addr: ISocketAddress;
begin
  Addr := TSilSocketAddress.Create(TypeSpec, Protocol, Address, Port);
  Result := TSilOsSocket.Create(Addr);
end;

class function Socket.CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: Cardinal; Port: Word): ISocketServer;
var
  Addr: ISocketAddress;
begin
  Addr := TSilSocketAddress.Create(TypeSpec, Protocol, Address, Port);
  Result := TSilOsSocket.Create;
  Result.Bind(Addr);
end;

class function Socket.CreateClient(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word): ISocketClient;
begin
  Result := CreateClient(TypeSpec, Protocol, OsSocket.IP.FromStr(Host), Port);
end;

class function Socket.CreateServer(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word): ISocketServer;
begin
  Result := CreateServer(TypeSpec, Protocol, OsSocket.IP.FromStr(Host), Port);
end;

class function Socket.CreateList(Locked: Boolean): ISocketList;
begin
  Result := TSilSocketList.Create(Locked);
end;

end.
