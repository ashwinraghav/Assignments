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

unit SilOmSocket;

{$I Defines.inc}

interface

uses
  WinSock,
  Windows,

  SilLiStream,
  SilLiEnumerator,
  SilLiStringList,
  SilLkInterfaced,
  SilLmInterfaceList,
  SilOsTypes,
  SilOiSocket,
  SilOiHandle,
  SilOmHandle,
  SilOkSocket;

type
  TSilWindowsSocketHandle = class (
    // extends
    TSilHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  public
    destructor Destroy; override;
  end;

  TSilWindowsSocketPeerAddress = class (
    // extends
    TSilSocketAddress)
  private
    FHandle: IHandle;
  protected
    function GetHandle: IHandle;
  protected
    procedure DoGetPeerHost; override;
    procedure DoGetPeerAddress; override;
    procedure DoGetSockAddrIn(out SockAddrIn: TSockAddrIn); virtual; abstract;
  public
    constructor Create(const Handle: IHandle; const Address: ISocketAddress); overload;
    destructor Destroy; override;
  end;

  TSilWindowsSocketLocalAddress = class (
    // extends
    TSilWindowsSocketPeerAddress)
  protected
    procedure DoGetSockAddrIn(out SockAddrIn: TSockAddrIn); override;
  public
    destructor Destroy; override;
  end;

  TSilWindowsSocketRemoteAddress = class (
    // extends
    TSilWindowsSocketPeerAddress)
  protected
    procedure DoGetSockAddrIn(out SockAddrIn: TSockAddrIn); override;
  public
    destructor Destroy; override;
  end;

  TSilWindowsSocket = class (
    // extends
    TSilSocket)
  protected
    function DoRead(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer; override;
    function DoWrite(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer; override;
    function DoReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): Integer; override;
    function DoWriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): Integer; override;
    procedure DoBind(const Address: ISocketAddress); override;
    function DoConnect(const Address: ISocketAddress): Boolean; override;
    procedure DoListen; override;
    function DoAccept(out Client: RSocketClient): Boolean; override;
    function DoSocket(const Address: ISocketAddress): THandle; override;
  protected
    procedure DoShutdown(const Mode: TSocketShutdown); override;
    function DoGetSockOpt(Level, Cmd: Integer): Integer; override;
    function DoSetSockOpt(Level, Cmd: Integer; Value: Integer): Boolean; override;
    procedure DoGetIoCtl(Cmd: Integer; var Arg: Integer); override;
  protected // ISocket
    function WaitFor(const States: TSocketStates; Timeout: LongWord): Boolean; override;
  public
    constructor Create(const Address: ISocketAddress = nil); override;
  end;

  TSilWindowsSocketProtocol = class (
    // extends
    TSilSocketProtocol)
  protected
    procedure DoRecoverInfo(ProtoEnt: PProtoEnt);
  public
    constructor Create(ProtoEnt: PProtoEnt);
  end;

  TSilWindowsSocketService = class (
    // extends
    TSilSocketService)
  protected
    procedure DoRecoverInfo(ServEnt: PServEnt);
  public
    constructor Create(ServEnt: PServEnt);
  end;

procedure Initialize;

implementation

uses
  SilBtText,
  SilLtList,
  SilOsClasses,
  SilOtTool,
  SilOsTool,
  SilOsSocket,
  SilOdSocket,
  SilBtStr,
  SilBtInt,
  SilOtSocket,
  SilOvSocketInitializer;

const
  AProt: array [TSocketProtocol] of Byte =
    (0, IPPROTO_IP, IPPROTO_ICMP, IPPROTO_TCP, IPPROTO_UDP, IPPROTO_RAW);

type
  PFdSetEx = ^TFdSetEx;
  TFdSetEx = record
    fd_count: Integer;
    fd_array: array [0..MaxInt div SizeOf(Integer) - 2] of Integer;
  end;

var
  MSocketInitializer: IUnknown = nil;

procedure Initialize;
begin
  SilOvSocketInitializer.Get(@MSocketInitializer, IUnknown, SysInit.HInstance);
end;

procedure Finalize;
begin
  SilOvSocketInitializer.Release(@MSocketInitializer, SysInit.HInstance);
end;

function DoLookupName(const Host: String): TInAddr;
var
  HostEnt: PHostEnt;
begin
  HostEnt := WinSock.gethostbyname(PChar(Host));

  if HostEnt <> nil then
    PLongWord(@Result.S_un_b)^ := PLongWord(HostEnt.h_addr^)^ else
    FillChar(Result, SizeOf(Result), 0);
end;

function DoGetINetAddr(const Address: ISocketAddress): TSockAddrIn;
begin
  Result.sin_family := PF_INET;
  Result.sin_port := WinSock.htons(Address.Port);

  if afAddress in Address.Flags then
    Result.sin_addr.S_addr := OsIP.HostToNetwork(Address.Address) else
  if Str.IsEmpty(Address.Host) and (Address.Address = 0) then
    Result.sin_addr.s_addr := 0 else
  if OsIP.IsDottedFormat(Address.Host) then
    Result.sin_addr.s_addr := WinSock.inet_addr(PChar(Address.Host)) else
    Result.sin_addr := DoLookupName(Address.Host);
end;

{ TSilWindowsSocketHandle }

destructor TSilWindowsSocketHandle.Destroy;
begin
  inherited;
end;

procedure TSilWindowsSocketHandle.HandleClose;
var
  hSocket: THandle;
begin
  hSocket := Handle;
  Handle := 0; // no sacar
  WinSock.closesocket(hSocket);
end;

procedure TSilWindowsSocketHandle.HandleIsValid(var Result: Boolean);
begin
  Result := Result and (Handle > 0);
end;

{ TSilWindowsSocketProtocol }

constructor TSilWindowsSocketProtocol.Create(ProtoEnt: PProtoEnt);
begin
  Initialize;

  inherited Create;
  if ProtoEnt <> nil then DoRecoverInfo(ProtoEnt);
end;

procedure TSilWindowsSocketProtocol.DoRecoverInfo(ProtoEnt: PProtoEnt);
var
  PList, PStart: PChar;
  sItem: String;
begin
  FName := ProtoEnt.p_name;
  FId := ProtoEnt.p_proto;
  FAliases := ListTool.StringList;

  PList := ProtoEnt.p_aliases^;
  sItem := '';

  if PList <> nil then
  begin
    while true do
    begin
      PStart := PList;
      while not (PList^ in [#0, #10, #13, #32]) do Inc(PList);

      if PList - PStart = 0 then Break;
      SetString(sItem, PStart, PList - PStart);
      FAliases.Add(sItem, nil);
      Inc(PList);
    end;

    Inc(PList);
    while PList^ in [#0, #32] do Inc(PList);

    PStart := PList;
    while not (PList^ in [#0, #10, #13]) do Inc(PList);

    if PList - PStart > 0 then
      SetString(FComment, PStart, PList - PStart);
  end;

  FValid := Str.NotEmpty(FName);
end;

{ TSilWindowsSocketService }

constructor TSilWindowsSocketService.Create(ServEnt: PServEnt);
begin
  Initialize;

  inherited Create;
  if ServEnt <> nil then DoRecoverInfo(ServEnt);
end;

procedure TSilWindowsSocketService.DoRecoverInfo(ServEnt: PServEnt);
var
  PList, PStart: PChar;
  sItem: String;
begin
  FName := ServEnt.s_name;
  FPort := OsIP.NetworkToPort(ServEnt.s_port);
  FProtocolName := ServEnt.s_proto;
  FAliases := ListTool.StringList;

  PList := ServEnt.s_aliases^;
  sItem := '';

  if PList <> nil then
    while true do
    begin
      PStart := PList;
      while not (PList^ in [#0, #10, #13, #32]) do Inc(PList);

      if PList - PStart = 0 then Break;
      SetString(sItem, PStart, PList - PStart);
      if Text.Compare(sItem, FName) = 0 then Break;
      FAliases.Add(sItem, nil);
      Inc(PList);
    end;

  FValid := Str.NotEmpty(FName);
end;

{ TSilWindowsSocketPeerAddress }

constructor TSilWindowsSocketPeerAddress.Create(const Handle: IHandle; const Address: ISocketAddress);
begin
  inherited Create;

  if Address <> nil then
  begin
    FTypeSpec := Address.TypeSpec;
    FProtocol := Address.Protocol;
    FPort := Address.Port;
  end;

  //MakeRef(Handle, @FHandle);
  FHandle := Handle;
end;

destructor TSilWindowsSocketPeerAddress.Destroy;
begin
  FHandle := nil;
  inherited;
end;

procedure TSilWindowsSocketPeerAddress.DoGetPeerHost;
var
  SockAddrIn: TSockAddrIn;
  HostEnt: PHostEnt;
begin
  if (FHandle = nil) or not GetHandle.IsValid then Exit;

  if FTypeSpec = stStream then
  begin
    DoGetSockAddrIn(SockAddrIn);
    HostEnt := WinSock.gethostbyaddr(@SockAddrIn.sin_addr.s_addr, SizeOf(LongWord), PF_INET);

    if HostEnt <> nil then
      FHost := HostEnt.h_name else
      FHost := '';
  end else
    FHost := WindowsIP.ToHostName(FAddress);
end;

procedure TSilWindowsSocketPeerAddress.DoGetPeerAddress;
var
  SockAddrIn: TSockAddrIn;
begin
  if (FHandle = nil) or not GetHandle.IsValid then Exit;

  DoGetSockAddrIn(SockAddrIn);

  FPort := OsIP.NetworkToPort(SockAddrIn.sin_port);
  FAddress := OsIP.NetworkToHost(SockAddrIn.sin_addr.S_addr);
end;

function TSilWindowsSocketPeerAddress.GetHandle: IHandle;
begin
  Result := FHandle;
end;

{ TSilWindowsSocketLocalAddress }

procedure TSilWindowsSocketLocalAddress.DoGetSockAddrIn(out SockAddrIn: TSockAddrIn);
var
  iSize: Integer;
begin
  iSize := SizeOf(SockAddrIn);
  WinSock.getsockname(GetHandle.Value, SockAddrIn, iSize);
end;

destructor TSilWindowsSocketLocalAddress.Destroy;
begin
  inherited;
end;

{ TSilWindowsSocketRemoteAddress }

procedure TSilWindowsSocketRemoteAddress.DoGetSockAddrIn(out SockAddrIn: TSockAddrIn);
var
  iSize: Integer;
begin
  iSize := SizeOf(SockAddrIn);
  WinSock.getpeername(GetHandle.Value, SockAddrIn, iSize);
end;

destructor TSilWindowsSocketRemoteAddress.Destroy;
begin
  inherited;
end;

{ TSilWindowsSocket }

constructor TSilWindowsSocket.Create(const Address: ISocketAddress);
begin
  Initialize;
  inherited Create(Address);
end;

function TSilWindowsSocket.DoAccept(out Client: RSocketClient): Boolean;
var
  SockAddrIn: TSockAddrIn;
  iLen: Integer;
begin
  iLen := SizeOf(SockAddrIn);
  Client.Socket := WinSock.accept(Handle.Value, @SockAddrIn, @iLen);
  Result := (Client.Socket > 0) and (Client.Socket < INVALID_HANDLE_VALUE);

  if Result then
    Client.Address := TSilSocketAddress.Create(
      FLocal.TypeSpec,
      FLocal.Protocol,
      OsIP.NetworkToHost(SockAddrIn.sin_addr.S_addr),
      OsIP.NetworkToPort(SockAddrIn.sin_port));
end;

procedure TSilWindowsSocket.DoBind(const Address: ISocketAddress);
var
  SockAddrIn: TSockAddrIn;
begin
  SockAddrIn := DoGetINetAddr(FLocal);

  if WinSock.bind(Handle.Value, SockAddrIn, SizeOf(SockAddrIn)) <> 0 then
    raise OS.Error.Create(Windows.GetLastError, SSocketBindError);
end;

function TSilWindowsSocket.DoConnect(const Address: ISocketAddress): Boolean;
var
  SockAddrIn: TSockAddrIn;
begin
  SockAddrIn := DoGetINetAddr(Address);
  Result := WinSock.connect(Handle.Value, SockAddrIn, SizeOf(SockAddrIn)) = 0;

  if not Result then
    raise OS.Error.Create(Windows.GetLastError, SSocketConnectError);
end;

procedure TSilWindowsSocket.DoGetIoCtl(Cmd: Integer; var Arg: Integer);
begin
  WinSock.ioctlsocket(Handle.Value, Cmd, Arg);
end;

function TSilWindowsSocket.DoGetSockOpt(Level, Cmd: Integer): Integer;
var
  iLen: Integer;
begin
  if Handle.IsValid then
  begin
    iLen := SizeOf(Result);
    WinSock.getsockopt(Handle.Value, Level, Cmd, PChar(@Result), iLen);
  end else
    Result := -1;
end;

procedure TSilWindowsSocket.DoListen;
begin
  if WinSock.listen(Handle.Value, SOMAXCONN) <> 0 then
    raise OS.Error.Create(Windows.GetLastError, SSocketListenError);
end;

function TSilWindowsSocket.DoRead(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer;
begin
  Result := WinSock.recv(Handle.Value, Buffer, Count, Byte(Flags));
  if (Result = 0) and (Windows.GetLastError <> 0) then Result := -1;
end;

function TSilWindowsSocket.DoReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): Integer;
var
  iLen: Integer;
  SockAddrIn: TSockAddrIn;
  AddressDef: ISocketAddressDef;
begin
  iLen := SizeOf(SockAddrIn);
  FillChar(SockAddrIn, iLen, 0);
  Result := WinSock.recvfrom(Handle.Value, Buffer, Count, Byte(Flags), SockAddrIn, iLen);

  AddressDef := TSilWindowsSocketRemoteAddress.Create(Handle, FLocal);
  AddressDef.Address := OsIP.NetworkToHost(SockAddrIn.sin_addr.S_addr);
  AddressDef.Port := OsIP.NetworkToPort(SockAddrIn.sin_port);

  Address := AddressDef;
  if (Result = 0) and (Windows.GetLastError <> 0) then Result := -1;
end;

function TSilWindowsSocket.DoSetSockOpt(Level, Cmd, Value: Integer): Boolean;
var
  iLen: Integer;
begin
  iLen := SizeOf(Value);
  Result := WinSock.setsockopt(Handle.Value, Level, Cmd, PChar(@Value), iLen) = 0;
end;

procedure TSilWindowsSocket.DoShutdown(const Mode: TSocketShutdown);
begin
  if WinSock.shutdown(Handle.Value, Ord(Mode)) <> 0 then
    raise OS.Error.Create(Windows.GetLastError, SSocketShutdownError);
end;

function TSilWindowsSocket.DoSocket(const Address: ISocketAddress): THandle;
begin
  Result := WinSock.socket(PF_INET, Ord(Address.TypeSpec), AProt[Address.Protocol]);
end;

function TSilWindowsSocket.DoWrite(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer;
var
  PBuf: Pointer;
begin
  PBuf := Pointer(@Buffer);
  Result := WinSock.send(Handle.Value, PBuf^, Count, Byte(Flags));
  if (Result = 0) and (Windows.GetLastError <> 0) then Result := -1;
end;

function TSilWindowsSocket.DoWriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): Integer; 
var
  PBuf: Pointer;
  SockAddrIn: TSockAddrIn;
begin
  PBuf := @Buffer;
  SockAddrIn := DoGetINetAddr(Address);
  Result := Winsock.sendto(Handle.Value, PBuf^, Count, Byte(Flags), SockAddrIn, SizeOf(SockAddrIn));
  if (Result = 0) and (Windows.GetLastError <> 0) then Result := -1;
end;

function TSilWindowsSocket.WaitFor(const States: TSocketStates; Timeout: LongWord): Boolean;
var
  PTime: PTimeVal;
  Timeval: TTimeVal;
  Readfds, Writefds, Errorfds: TFdSet;
  PReadfds, PWritefds, PErrorfds: PFdSet;
  iHandle: Integer;

  procedure DoSet(var fds: TFdSet; var pfds: PFdSet);
  begin
    fds.fd_count := 1;
    fds.fd_array[0] := iHandle;
    pfds := @fds;
  end;

begin
  if not Handle.IsValid then
  begin
    Result := false;
    Exit;
  end;

  iHandle := Handle.Value;

  if Timeout <> INFINITE then
  begin
    Timeval.tv_sec := Timeout div 1000;
    Timeval.tv_usec := Timeout mod 1000 * 1000;
    PTime := @Timeval;
  end else
    PTime := nil;

  if ssRead in States then
    DoSet(Readfds, PReadfds) else
    PReadfds := nil;

  if ssWrite in States then
    DoSet(Writefds, PWritefds) else
    PWritefds := nil;

  if ssError in States then
    DoSet(Errorfds, PErrorfds) else
    PErrorfds := nil;

  Result := WinSock.select(0, PReadfds, PWritefds, PErrorfds, PTime) > 0;
end;

initialization

finalization
  Finalize;

end.


