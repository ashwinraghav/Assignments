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

unit SilOtSocket;

interface

{$INCLUDE Defines.inc}

uses
  SilBeTypes,
  SilBkTool,
  SilLiStringList,
  SilOcTypes,
  SilOiSocket,
  SilOjSocket;

type
  WindowsIP = class (IP)
    class function FromHostName(const Host: String): LongWord; override;
    class function FromHostAddress(const Address: String): LongWord; override;
    class function HostToNetwork(Host: LongWord): LongWord; override;
    class function PortToNetwork(Port: Word): Word; override;
    class function NetworkToHost(NetworkAddress: LongWord): LongWord; override;
    class function NetworkToPort(NetworkPort: Word): Word; override;
    class function ToStr(HostAddress: LongWord): String; override;
    class function ToHostName(HostAddress: LongWord): String; override;
  end;

  WindowsHost = class (Host)
    class function GetByAddress(Address: LongWord): ISocketAddress; overload; override;
    class function GetByAddress(const Address: String): ISocketAddress; overload; override;
    class function GetByName(const Host: String): ISocketAddress; override;
    class function LocalName: String; override;
  end;

  WindowsInfo = class (Info)
    class function Service(Port: Word; const Protocol: String = ''): ISocketService; overload; override;
    class function Service(const Name: String; const Protocol: String = ''): ISocketService; overload; override;
    class function Protocol(Number: Integer): ISocketProtocol; overload; override;
    class function Protocol(const Name: String): ISocketProtocol; overload; override;
    class function AdapterList: ISocketAddresses; override;
  end;

  WindowsSocket = class (Socket)
    class function WaitFor(const States: TSocketStates; const ListIn: ISockets; var ListOut: ISocketList; Timeout: LongWord = INFINITE): Boolean; override;
    class function LastError: Integer; override;
    class function ErrorMessage(const ErrCode: Integer): String; override;
  end;

implementation

uses
  WinSock,

  SilBtStr,
  SilBtMem,
  SilBtError,
  SilLiEnumerator,
  SilLtList,
  SilOdSocket,
  SilOmSocket,
  SilOkSocket,
  SilOsClasses,
  SilOsWntIpHlpApi;

var
  MHelper: IIPHelper;

procedure DoCheckIPHelper;
begin
  if not Assigned(MHelper) then
    MHelper := IPHelper.Create();
end;

{ IPAddr }

class function WindowsIP.HostToNetwork(Host: LongWord): LongWord;
begin
  Result := WinSock.htonl(Host);
end;

class function WindowsIP.PortToNetwork(Port: Word): Word;
begin
  Result := WinSock.htons(Port);
end;

class function WindowsIP.NetworkToHost(NetworkAddress: LongWord): LongWord;
begin
  Result := WinSock.ntohl(NetworkAddress);
end;

class function WindowsIP.NetworkToPort(NetworkPort: Word): Word;
begin
  Result := WinSock.ntohs(NetworkPort);
end;

class function WindowsIP.ToStr(HostAddress: LongWord): String;
var
  InAddr: in_addr;
begin
  InAddr.S_addr := HostToNetwork(HostAddress);
  Result := WinSock.inet_ntoa(InAddr);
end;

class function WindowsIP.ToHostName(HostAddress: LongWord): String;
var
  HostEnt: PHostEnt;
  lwNetworkAddress: LongWord;
begin
  Initialize;

  lwNetworkAddress := HostToNetwork(HostAddress);
  HostEnt := WinSock.gethostbyaddr(@lwNetworkAddress, SizeOf(lwNetworkAddress), PF_INET);

  if HostEnt <> nil then
    Result := HostEnt.h_name else
    Result := '';
end;

class function WindowsIP.FromHostName(const Host: String): LongWord;
var
  HostEnt: PHostEnt;
begin
  Initialize;
  HostEnt := WinSock.gethostbyname(PChar(Host));

  if HostEnt <> nil then
    Result := NetworkToHost(PLongWord(HostEnt.h_addr^)^) else
    Result := 0;
end;

class function WindowsIP.FromHostAddress(const Address: String): LongWord;
begin
  Result := NetworkToHost(WinSock.inet_addr(PChar(Address)));
end;

{ Host }

class function WindowsHost.GetByAddress(const Address: String): ISocketAddress;
begin
  Result := GetByAddress(WindowsIP.FromStr(Address));
end;

class function WindowsHost.GetByAddress(Address: LongWord): ISocketAddress;
begin
  Result := TSilSocketAddress.Create(stStream, spIP, Address, 0);
end;

class function WindowsHost.GetByName(const Host: String): ISocketAddress;
var
  HostEnt: PHostEnt;
  lwAddress: LongWord;
  Address: PChar;
  i: Integer;
  Addr: ISocketAddressDef;
begin
  Initialize;
  Address := @lwAddress;
  HostEnt := WinSock.gethostbyname(PChar(Host));

  if Assigned(HostEnt) then
  begin
    for i := 0 to 3 do Address[i] := HostEnt.h_addr^[i];
    Addr := TSilSocketAddress.Create(stStream, spIP, WindowsIP.NetworkToHost(lwAddress), 0);
    Addr.Host := HostEnt.h_name;
  end else
    raise Error.Create(SSocketHostNotFound, [Host]);

  Result := Addr;
end;

class function WindowsHost.LocalName: String;
var
  LocalName: array[0..255] of Char;
begin
  Initialize;

  if WinSock.gethostname(LocalName, SizeOf(LocalName)) = 0 then
    Result := LocalName else
    Result := '';
end;

class function WindowsInfo.Protocol(Number: Integer): ISocketProtocol;
begin
  Initialize;
  Result := TSilOsSocketProtocol.Create(WinSock.getprotobynumber(Number));
end;

class function WindowsInfo.AdapterList: ISocketAddresses; 
var
  Info: PMIB_IPADDRTABLE;
  dwSize: LongWord;
  i: Integer;
  List: ISocketAddressList;
  Addr: ISocketAddressDef;
begin
  dwSize := 2048;
  Info := Mem.Get(dwSize);

  try
    DoCheckIPHelper;

    List := TSilSocketAddressList.Create;
    i := MHelper.GetIpAddrTable(Info, @dwSize, false);

    if i = 0 then
      for i := 0 to Info.dwNumEntries - 1 do
      begin
        Addr := TSilSocketAddress.Create;
        Addr.Address := WindowsIP.NetworkToHost(Info.table[i].dwAddr);
        Addr.SubnetMask := WindowsIP.NetworkToHost(Info.table[i].dwMask);
        List.Add(Addr);
      end;
  finally
    Mem.Free(Info);
    Result := List;
  end;
end;

class function WindowsInfo.Protocol(const Name: String): ISocketProtocol;
begin
  Initialize;
  Result := TSilOsSocketProtocol.Create(WinSock.getprotobyname(PChar(Name)));
end;

class function WindowsInfo.Service(Port: Word; const Protocol: String): ISocketService;
var
  wNetworkPort: Word;
  PProt: PChar;
begin
  Initialize;

  if Str.IsEmpty(Protocol) then
    PProt := nil else
    PProt := PChar(Protocol);

  wNetworkPort := WindowsIP.PortToNetwork(Port);
  Result := TSilOsSocketService.Create(WinSock.getservbyport(wNetworkPort, PProt));
end;

class function WindowsInfo.Service(const Name, Protocol: String): ISocketService;
var
  PProt: PChar;
begin
  Initialize;
  
  if Str.IsEmpty(Protocol) then
    PProt := nil else
    PProt := PChar(Protocol);

  Result := TSilOsSocketService.Create(WinSock.getservbyname(PChar(Name), PProt));
end;

{ WindowsSocket }

class function WindowsSocket.ErrorMessage(const ErrCode: Integer): String;
begin
  Result := 'function (WindowsSocket.ErrorMessage) not implemented';
end;

class function WindowsSocket.LastError: Integer;
begin
  Result := WinSock.WSAGetLastError;
end;

class function WindowsSocket.WaitFor(const States: TSocketStates; const ListIn: ISockets; var ListOut: ISocketList; Timeout: LongWord): Boolean;
var
  PTime: PTimeVal;
  Timeval: TTimeVal;
  Readfds, Writefds, Errorfds: TFdSet;
  PReadfds, PWritefds, PErrorfds: PFdSet;
  i, iHandle, iCount: Integer;
  Enum: IEnumerator;
  Socket: ISocket;

  procedure DoSet(var fds: TFdSet; var pfds: PFdSet);
  begin
    fds.fd_count := 1;
    fds.fd_array[0] := iHandle;
    pfds := @fds;
  end;

begin
  PReadfds := nil;
  PWritefds := nil;
  PErrorfds := nil;

  if Timeout <> INFINITE then
  begin
    Timeval.tv_sec := Timeout div 1000;
    Timeval.tv_usec := Timeout mod 1000 * 1000;
    PTime := @Timeval;
  end else
    PTime := nil;

  while ListIn.Enumerate(Enum, Socket) do
  begin
    iHandle := Socket.Handle.Value;

    if ssRead in States then
      DoSet(Readfds, PReadfds) else
      PReadfds := nil;

    if ssWrite in States then
      DoSet(Writefds, PWritefds) else
      PWritefds := nil;

    if ssError in States then
      DoSet(Errorfds, PErrorfds) else
      PErrorfds := nil;
  end;

  iCount := WinSock.select(0, PReadfds, PWritefds, PErrorfds, PTime);
  Result := iCount > 0;

  if Result then
  begin
    if ListOut = nil then
      ListOut := TSilSocketList.Create(false) else
      ListOut.Clear;

      for i := 0 to ListIn.Count - 1 do
      begin
        iHandle := ListIn[i].Handle.Value;
        if (ssRead in States) and FD_ISSET(iHandle, Readfds) then
          ListOut.Add(ListIn[i]) else
        if (ssWrite in States) and FD_ISSET(iHandle, Writefds) then
          ListOut.Add(ListIn[i]) else
        if (ssError in States) and FD_ISSET(iHandle, Errorfds) then
          ListOut.Add(ListIn[i]);
      end;

    Result := ListOut.Count > 0;
  end;
end;

end.
