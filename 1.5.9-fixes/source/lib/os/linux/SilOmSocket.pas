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
  libc,

  SilOsTypes,

  SilLiStream,
  SilLiEnumerator,
  SilLiStringList,
  SilLkInterfaced,
  SilLmInterfaceList,
  SilOiSocket,
  SilOiHandle,
  SilOmHandle,
  SilOkSocket;

type
  TSockAddrIn = libc.sockaddr;

  TSilLinuxSocketHandle = class (
    // extends
    TSilHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  public
    destructor Destroy; override;
  end;

  TSilLinuxSocketPeerAddress = class (
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

  TSilLinuxSocketLocalAddress = class (
    // extends
    TSilLinuxSocketPeerAddress)
  protected
    procedure DoGetSockAddrIn(out SockAddrIn: TSockAddrIn); override;
  public
    destructor Destroy; override;
  end;

  TSilLinuxSocketRemoteAddress = class (
    // extends
    TSilLinuxSocketPeerAddress)
  protected
    procedure DoGetSockAddrIn(out SockAddrIn: TSockAddrIn); override;
  public
    destructor Destroy; override;
  end;

  TSilLinuxSocket = class (
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
    (*)constructor Create(const Address: ISocketAddress = nil); override;(*)
  end;

  TSilLinuxSocketProtocol = class (
    // extends
    TSilSocketProtocol)
  protected
    procedure DoRecoverInfo(ProtoEnt: PProtoEnt);
  public
    constructor Create(ProtoEnt: PProtoEnt);
  end;

  TSilLinuxSocketService = class (
    // extends
    TSilSocketService)
  protected
    procedure DoRecoverInfo(ServEnt: PServEnt);
  public
    constructor Create(ServEnt: PServEnt);
  end;

implementation

uses
  SysUtils,
  SilLtList,
  SilOsClasses,
  SilOtTool,
  SilOsTool,
  SilOsSocket,
  SilOdSocket,
  SilBtStr,
  SilBtInt,
  SilOtSocket;

const
  AProt: array [TSocketProtocol] of Byte =
    (0, IPPROTO_IP, IPPROTO_ICMP, IPPROTO_TCP, IPPROTO_UDP, IPPROTO_RAW);

function DoGetINetAddr(HostAddress: LongWord; Port: Word): TSockAddrIn;
begin
  Result.sin_family := PF_INET;
  Result.sin_port := OsIP.PortToNetwork(Port);
  Result.sin_addr.S_addr := OsIp.HostToNetwork(HostAddress);
end;

{ TSilLinuxSocketHandle }

destructor TSilLinuxSocketHandle.Destroy;
begin
  inherited;
end;

procedure TSilLinuxSocketHandle.HandleClose;
var
  hSocket: THandle;
begin
  hSocket := Handle;
  Handle := NULL_HANDLE_VALUE;
  libc.__close(hSocket);
end;

procedure TSilLinuxSocketHandle.HandleIsValid(var Result: Boolean);
begin
  Result := Result and (Handle > 0);
end;

{ TSilLinuxSocketProtocol }

constructor TSilLinuxSocketProtocol.Create(ProtoEnt: PProtoEnt);
begin
  inherited Create;
  if ProtoEnt <> nil then DoRecoverInfo(ProtoEnt);
end;

procedure TSilLinuxSocketProtocol.DoRecoverInfo(ProtoEnt: PProtoEnt);
begin
  FValid := Str.NotEmpty(FName);
end;

{ TSilLinuxSocketService }

constructor TSilLinuxSocketService.Create(ServEnt: PServEnt);
begin
  inherited Create;
  if ServEnt <> nil then DoRecoverInfo(ServEnt);
end;

procedure TSilLinuxSocketService.DoRecoverInfo(ServEnt: PServEnt);
begin
  FValid := Str.NotEmpty(FName);
end;

{ TSilLinuxSocketPeerAddress }

constructor TSilLinuxSocketPeerAddress.Create(const Handle: IHandle; const Address: ISocketAddress);
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

destructor TSilLinuxSocketPeerAddress.Destroy;
begin
  FHandle := nil;
  inherited;
end;

procedure TSilLinuxSocketPeerAddress.DoGetPeerHost;
var
  SockAddrIn: TSockAddrIn;
  HostEnt: PHostEnt;
begin
  if (FHandle = nil) or not GetHandle.IsValid then Exit;

  DoGetSockAddrIn(SockAddrIn);
  HostEnt := libc.gethostbyaddr(@SockAddrIn.sin_addr.s_addr, SizeOf(LongWord), PF_INET);

  if HostEnt <> nil then
    FHost := HostEnt.h_name else
    FHost := '';
end;

procedure TSilLinuxSocketPeerAddress.DoGetPeerAddress;
var
  SockAddrIn: TSockAddrIn;
begin
  if (FHandle = nil) or not GetHandle.IsValid then Exit;

  DoGetSockAddrIn(SockAddrIn);

  FPort := OsIp.NetworkToPort(SockAddrIn.sin_port);
  FAddress := OsIp.NetworkToHost(SockAddrIn.sin_addr.S_addr);
end;

function TSilLinuxSocketPeerAddress.GetHandle: IHandle;
begin
  Result := {IHandle(}FHandle{)};
end;

{ TSilLinuxSocketLocalAddress }

procedure TSilLinuxSocketLocalAddress.DoGetSockAddrIn(out SockAddrIn: TSockAddrIn);
var
  Size: LongWord;
begin
  Size := SizeOf(SockAddrIn);
  libc.getsockname(GetHandle.Value, SockAddrIn, Size);
end;

destructor TSilLinuxSocketLocalAddress.Destroy;
begin
  inherited;
end;

{ TSilLinuxSocketRemoteAddress }

procedure TSilLinuxSocketRemoteAddress.DoGetSockAddrIn(out SockAddrIn: TSockAddrIn);
var
  Size: LongWord;
begin
  Size := SizeOf(SockAddrIn);
  libc.getpeername(IHandle(FHandle).Value, SockAddrIn, Size);
end;

destructor TSilLinuxSocketRemoteAddress.Destroy;
begin
  inherited;
end;

{ TSilLinuxSocket }

function TSilLinuxSocket.DoAccept(out Client: RSocketClient): Boolean;
var
  SockAddrIn: TSockAddrIn;
  iLen: Integer;

  procedure _accept;
  begin
    iLen := SizeOf(SockAddrIn);
    Client.Socket := libc.accept(Handle.Value, @SockAddrIn, @iLen);
  end;

begin
  if not OS.Thread.IsMain then
  begin
    Client.Socket := 0;

    while Handle.IsValid do
      if WaitFor([ssRead], 1000) then
      begin
        _accept;
        Break;
      end;
  end else
    _accept;

  Result := Client.Socket > 0;

  if Result then
    Client.Address := TSilSocketAddress.Create(
      FLocal.TypeSpec,
      FLocal.Protocol,
      OsIP.NetworkToHost(SockAddrIn.sin_addr.S_addr),
      OsIP.NetworkToPort(SockAddrIn.sin_port));
end;

procedure TSilLinuxSocket.DoBind(const Address: ISocketAddress);
var
  SockAddrIn: TSockAddrIn;
begin
  SockAddrIn := DoGetINetAddr(FLocal.Address, FLocal.Port);

  if libc.bind(Handle.Value, SockAddrIn, SizeOf(SockAddrIn)) <> 0 then
    raise OS.Error.Create(libc.errno, SSocketBindError);
end;

function TSilLinuxSocket.DoConnect(const Address: ISocketAddress): Boolean;
var
  SockAddrIn: TSockAddrIn;
begin
  SockAddrIn := DoGetINetAddr(Address.Address, Address.Port);

  Result := libc.connect(Handle.Value, @SockAddrIn, SizeOf(SockAddrIn)) = 0;

  if not Result then
    raise OS.Error.Create(libc.errno, SSocketConnectError);
end;

procedure TSilLinuxSocket.DoGetIoCtl(Cmd: Integer; var Arg: Integer);
begin
  libc.fcntl(Handle.Value, Cmd, Arg);
end;

function TSilLinuxSocket.DoGetSockOpt(Level, Cmd: Integer): Integer;
var
  Len: LongWord;
begin
  if Handle.IsValid then
  begin
    Len := SizeOf(Result);
    libc.getsockopt(Handle.Value, Level, Cmd, PChar(@Result), Len);
  end else
    Result := -1;
end;

procedure TSilLinuxSocket.DoListen;
begin
  if libc.listen(Handle.Value, SOMAXCONN) <> 0 then
    raise OS.Error.Create(libc.errno, SSocketListenError);
end;

function TSilLinuxSocket.DoRead(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer;
var
  PBuf: Pointer;
  Flagv: Byte;
begin
  PBuf := @Buffer;
  Flagv := PByte(@Flags)^;
  Result := libc.recv(Handle.Value, PBuf, Count, Flagv);

  if (Result = 0) and (libc.errno <> 0) then Result := -1;
end;

function TSilLinuxSocket.DoReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): Integer;
var
  PBuf: Pointer;
  Flagv: Byte;
  Len: socklen_t;
  SockAddrIn: TSockAddrIn;
  AddressDef: ISocketAddressDef;
begin
  PBuf := @Buffer;
  Flagv := PByte(@Flags)^;
  Len := SizeOf(SockAddrIn);
  FillChar(SockAddrIn, Len, 0);
  Result := libc.recvfrom(Handle.Value, PBuf, Count, Flagv, SockAddrIn, @Len);

  AddressDef := TSilOsSocketLocalAddress.Create(Handle, FLocal);
  AddressDef.Address := OsIp.NetworkToHost(SockAddrIn.sin_addr.S_addr);
  Address := AddressDef;

  if (Result = 0) and (libc.errno <> 0) then Result := -1;
end;

function TSilLinuxSocket.DoSetSockOpt(Level, Cmd, Value: Integer): Boolean;
var
  iLen: Integer;
begin
  iLen := SizeOf(Value);
  Result := libc.setsockopt(Handle.Value, Level, Cmd, PChar(@Value), iLen) = 0;
end;

procedure TSilLinuxSocket.DoShutdown(const Mode: TSocketShutdown);
begin
  if libc.shutdown(Handle.Value, Ord(Mode)) <> 0 then
    raise OS.Error.Create(libc.errno, SSocketShutdownError);
end;

function TSilLinuxSocket.DoSocket(const Address: ISocketAddress): THandle;
begin
  Result := libc.socket(PF_INET, Ord(Address.TypeSpec), AProt[Address.Protocol]);
end;

function TSilLinuxSocket.DoWrite(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer;
var
  PBuf: Pointer;
  Flagv: Byte;
begin
  PBuf := @Buffer;
  Flagv := PByte(@Flags)^;
  Result := libc.send(Handle.Value, PBuf, Count, Flagv);

  if (Result = 0) and (libc.errno <> 0) then Result := -1;
end;

function TSilLinuxSocket.DoWriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): Integer;
var
  PBuf: Pointer;
  Flagv: Byte;
  SockAddrIn: TSockAddrIn;
begin
  PBuf := @Buffer;
  Flagv := PByte(@Flags)^;
  SockAddrIn := DoGetINetAddr(Address.Address, Address.Port);
  Result := libc.sendto(Handle.Value, PBuf, Count, Flagv, SockAddrIn, SizeOf(SockAddrIn));

  if (Result = 0) and (libc.errno <> 0) then Result := -1;
end;

function TSilLinuxSocket.WaitFor(const States: TSocketStates; Timeout: LongWord): Boolean;
var
  PTime: PTimeVal;
  Timeval: TTimeVal;
  Readfds, Writefds, Errorfds: TFdset;
  PReadfds, PWritefds, PErrorfds: PFdSet;
  iHandle: Integer;

  procedure DoSet(var fds: TFdSet; var pfds: PFdset);
  begin
    FD_ZERO(fds);
    FD_SET(iHandle, fds);
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

  // esto es rarisimo: 1er parametro = mayor handle + 1
  Result := libc.select(iHandle + 1, PReadfds, PWritefds, PErrorfds, PTime) > 0;
end;

end.
