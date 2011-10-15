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
  Libc,
  SilBkTool,
  SilLiStringList,
  SilOcTypes,
  SilOiSocket,
  SilOjSocket;

type
  LinuxIP = class (IP)
    class function FromHostName(const Host: String): LongWord; override;
    class function FromHostAddress(const Address: String): LongWord; override;
    class function HostToNetwork(Host: LongWord): LongWord; override;
    class function PortToNetwork(Port: Word): Word; override;
    class function NetworkToHost(NetworkAddress: LongWord): LongWord; override;
    class function NetworkToPort(NetworkPort: Word): Word; override;
    class function ToStr(HostAddress: LongWord): String; override;
    class function ToHostName(HostAddress: LongWord): String; override;
  end;

  LinuxHost = class (Host)
    class function GetByAddress(Address: LongWord): ISocketAddress; overload; override;
    class function GetByAddress(const Address: String): ISocketAddress; overload; override;
    class function GetByName(const Host: String): ISocketAddress; override;
    class function LocalName: String; override;
  end;

  LinuxInfo = class (Info)
    class function Service(Port: Word; const Protocol: String = ''): ISocketService; overload; override;
    class function Service(const Name: String; const Protocol: String = ''): ISocketService; overload; override;
    class function Protocol(Number: Integer): ISocketProtocol; overload; override;
    class function Protocol(const Name: String): ISocketProtocol; overload; override;
    class function AdapterList: ISocketAddresses; override;
  end;

  LinuxSocket = class (Socket)
    class function WaitFor(const States: TSocketStates; const ListIn: ISockets; var ListOut: ISocketList; Timeout: LongWord = INFINITE): Boolean; override;
    class function LastError: Integer; override;
    class function ErrorMessage(const ErrCode: Integer): String; override;
  end;

implementation

uses
  SysUtils,

  SilOtError,
  SilOdSocket,
  SilOkSocket,
  SilOmSocket;

{ IPAddr }

class function LinuxIP.HostToNetwork(Host: LongWord): LongWord;
begin
  Result := libc.htonl(Host);
end;

class function LinuxIP.PortToNetwork(Port: Word): Word;
begin
  Result := libc.htons(Port);
end;

class function LinuxIP.NetworkToHost(NetworkAddress: LongWord): LongWord;
begin
  Result := libc.ntohl(NetworkAddress);
end;

class function LinuxIP.NetworkToPort(NetworkPort: Word): Word;
begin
  Result := libc.ntohs(NetworkPort);
end;

class function LinuxIP.ToStr(HostAddress: LongWord): String;
var
  InAddr: in_addr;
begin
  InAddr.S_addr := HostToNetwork(HostAddress);
  Result := libc.inet_ntoa(InAddr);
end;

class function LinuxIP.ToHostName(HostAddress: LongWord): String;
var
  HostEnt: PHostEnt;
  lwNetworkAddress: LongWord;
begin
  lwNetworkAddress := HostToNetwork(HostAddress);
  HostEnt := libc.gethostbyaddr(@lwNetworkAddress, SizeOf(lwNetworkAddress), PF_INET);

  if HostEnt <> nil then
    Result := HostEnt.h_name else
    Result := '';
end;

class function LinuxIP.FromHostName(const Host: String): LongWord;
var
  HostEnt: PHostEnt;
begin
  HostEnt := libc.gethostbyname(PChar(Host));

  if HostEnt <> nil then
    Result := NetworkToHost(PLongWord(HostEnt.h_addr^)^) else
    Result := 0;
end;

class function LinuxIP.FromHostAddress(const Address: String): LongWord;
begin
  Result := NetworkToHost(libc.inet_addr(PChar(Address)));
end;

{ Host }

class function LinuxHost.GetByAddress(const Address: String): ISocketAddress;
begin
  Result := GetByAddress(LinuxIP.FromStr(Address));
end;

class function LinuxHost.GetByAddress(Address: LongWord): ISocketAddress;
begin
  Result := TSilSocketAddress.Create(stStream, spIP, Address, 0);
end;

class function LinuxHost.GetByName(const Host: String): ISocketAddress;
var
  HostEnt: PHostEnt;
  lwAddress: LongWord;
  Address: PChar;
  i: Integer;
  Addr: ISocketAddressDef;
begin
  Address := @lwAddress;
  HostEnt := libc.gethostbyname(PChar(Host));

  if Assigned(HostEnt) then
  begin
    for i := 0 to 3 do Address[i] := HostEnt.h_addr^[i];
    Addr := TSilSocketAddress.Create(stStream, spIP, LinuxIP.NetworkToHost(lwAddress), 0);
    Addr.Host := HostEnt.h_name;
  end else
    raise LinuxErrorTool.Create(SSocketHostNotFound, [Host]);

  Result := Addr;
end;

class function LinuxHost.LocalName: String;
var
  LocalName: array[0..255] of Char;
begin
  if libc.gethostname(LocalName, SizeOf(LocalName)) = 0 then
    Result := LocalName else
    Result := '';
end;

class function LinuxInfo.Protocol(Number: Integer): ISocketProtocol;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxInfo.Protocol']);
(*)
  Initialize;
  Result := TSilOsSocketProtocol.Create(WinSock.getprotobynumber(Number));
(*)end;

class function LinuxInfo.AdapterList: ISocketAddresses;
(*)var
  Info: PMIB_IPADDRTABLE;
  dwSize: LongWord;
  i: Integer;
  List: ISocketAddressList;
  Addr: ISocketAddressDef;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxInfo.AdapterList']);
(*)
  dwSize := 2048;
  Info := Mem.Get(dwSize);

  try
    DoCheckIPHelper;

    List := TSilSocketAddressList.Create;
    i := FIPHelper.GetIpAddrTable(Info, @dwSize, false);

    if i = 0 then
      for i := 0 to Info.dwNumEntries - 1 do
      begin
        Addr := TSilSocketAddress.Create;
        Addr.Address := LinuxIP.NetworkToHost(Info.table[i].dwAddr);
        Addr.SubnetMask := LinuxIP.NetworkToHost(Info.table[i].dwMask);
        List.Add(Addr);
      end;
  finally
    Mem.Free(Info);
    Result := List;
  end;
(*)end;

class function LinuxInfo.Protocol(const Name: String): ISocketProtocol;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxInfo.Protocol']);
(*)
  Initialize;
  Result := TSilOsSocketProtocol.Create(WinSock.getprotobyname(PChar(Name)));
(*)end;

class function LinuxInfo.Service(Port: Word; const Protocol: String): ISocketService;
(*)var
  wNetworkPort: Word;
  PProt: PChar;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxInfo.Service']);
(*)
  Initialize;

  if Str.IsEmpty(Protocol) then
    PProt := nil else
    PProt := PChar(Protocol);

  wNetworkPort := LinuxIP.PortToNetwork(Port);
  Result := TSilOsSocketService.Create(WinSock.getservbyport(wNetworkPort, PProt));
(*)end;

class function LinuxInfo.Service(const Name, Protocol: String): ISocketService;
(*)var
  PProt: PChar;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxInfo.Service']);
(*)
  Initialize;

  if Str.IsEmpty(Protocol) then
    PProt := nil else
    PProt := PChar(Protocol);

  Result := TSilOsSocketService.Create(WinSock.getservbyname(PChar(Name), PProt));
(*)end;

{ LinuxSocket }

class function LinuxSocket.ErrorMessage(const ErrCode: Integer): String;
begin
  Result := 'function (LinuxSocket.ErrorMessage) not implemented';
end;

class function LinuxSocket.LastError: Integer;
begin
  Result := libc.errno;
end;

class function LinuxSocket.WaitFor(const States: TSocketStates; const ListIn: ISockets; var ListOut: ISocketList; Timeout: LongWord): Boolean;
(*)var
  PTime: PTimeVal;
  Timeval: TTimeVal;
  Readfds, Writefds, Errorfds: TFdSet;
  PReadfds, PWritefds, PErrorfds: PFdSet;
  i, iHandle, iCount: Integer;
  Enum: IEnumerator;
  Socket: ISocket;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxSocket.WaitFor']);
(*)
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
    if ssRead in States then WinSock.FD_SET(iHandle, Readfds);
    if ssWrite in States then WinSock.FD_SET(iHandle, Writefds);
    if ssError in States then WinSock.FD_SET(iHandle, Errorfds);
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
        iHandle := Socket.Handle.Value;
        if (ssRead in States) and FD_ISSET(iHandle, Readfds) then
          ListOut.Add(ListIn[i]) else
        if (ssWrite in States) and FD_ISSET(iHandle, Writefds) then
          ListOut.Add(ListIn[i]) else
        if (ssError in States) and FD_ISSET(iHandle, Errorfds) then
          ListOut.Add(ListIn[i]);
      end;

    Result := ListOut.Count > 0;
  end;
(*)end;

end.
