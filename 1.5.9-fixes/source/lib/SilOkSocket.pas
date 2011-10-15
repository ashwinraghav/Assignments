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

unit SilOkSocket;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLiStream,
  SilLiEnumerator,
  SilLiStringList,
  SilLmInterfaceList,
  SilOsTypes,
  SilOsHandled,
  SilOiSocket,
  SilOiHandle;

type
  TSilSocketAddress = class (
    // extends
    TSilObject,
    // implements
    ISocketAddress,
    ISocketAddressDef)
  protected
    FHost: String;
    FAddress: LongWord;
    FPort: Word;
    FTypeSpec: TSocketType;
    FProtocol: TSocketProtocol;
    FSubnetMask: LongWord;
  protected
    procedure DoGetPeerHost; virtual;
    procedure DoGetPeerAddress; virtual;
    procedure DoGetSubnetMask; virtual;
  protected // ISocketAddress
    function GetHost: String;
    function GetAddress: LongWord;
    function GetPort: Word;
    function GetTypeSpec: TSocketType;
    function GetProtocol: TSocketProtocol;
    function GetSubnetMask: LongWord;
    function GetNetworkClass: TSocketNetworkClass;
    function GetBroadcast: LongWord;
    function GetNetwork: LongWord;
    function GetFlags: TSocketAddressFlags;
    function Format(const FmtStr: String = CAddressShort): String;
  protected // ISocketAddressDef
    procedure SetHost(const Value: String);
    procedure SetAddress(const Value: LongWord);
    procedure SetPort(const Value: Word);
    procedure SetTypeSpec(const Value: TSocketType);
    procedure SetProtocol(const Value: TSocketProtocol);
    procedure SetSubnetMask(const Value: LongWord);
  public
    constructor Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: LongWord; Port: Word); overload;
    constructor Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word); overload;
    destructor Destroy; override;
  end;

  TSilSocketAddressList = class (
    // extends
    TSilInterfaceList,
    // implements
    ISocketAddresses,
    ISocketAddressList)
  protected // ISocketAddresses
    function Get(Index: Integer): ISocketAddress;
    function Enumerate(var Enum: IEnumerator; out Item: ISocketAddress): Boolean; reintroduce;
  protected // ISocketAddressAddressList
    procedure Put(Index: Integer; const Value: ISocketAddress);
    function Add(const Value: ISocketAddress): Integer; reintroduce;
    procedure Insert(Index: Integer; const Value: ISocketAddress); reintroduce;
  end;

  RSocketClient = record
    Socket: THandle;
    Address: ISocketAddress;
  end;

  TSilSocket = class (
    // extends
    TSilOsHandledObject,
    // implements
    ISocketStream,
    IStream,
    ISocketParameters,
    ISocketInfo,
    ISocketPeerInfo,
    ISocketClient,
    ISocketServer,
    ISocketPeer,
    ISocket)
  protected
    FLocal: ISocketAddress;
    FRemote: ISocketAddress;
    FWriteTimeout: LongWord;
    FReadTimeout: LongWord;
    FIsConnected: Boolean;
    FIsLocal: Boolean;
    FFlags: TSocketFlags;
    FAutoDisconnect: Boolean;
  private
    function DoCheckResult(Value: Integer; CloseIfZero: Boolean): LongWord;
    procedure DoSetupHandle(const Address: ISocketAddress);
    procedure DoSetFlags;
  protected
    function DoRead(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer; virtual; abstract;
    function DoWrite(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): Integer; virtual; abstract;
    function DoReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): Integer; virtual; abstract;
    function DoWriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): Integer; virtual; abstract;
    procedure DoBind(const Address: ISocketAddress); virtual; abstract;
    function DoConnect(const Address: ISocketAddress): Boolean; virtual; abstract;
    procedure DoListen; virtual; abstract;
    function DoAccept(out Client: RSocketClient): Boolean; virtual; abstract;
    function DoSocket(const Address: ISocketAddress): THandle; virtual; abstract;
  protected
    procedure DoShutdown(const Mode: TSocketShutdown); virtual; abstract;
    function DoGetSockOpt(Level, Cmd: Integer): Integer; virtual; abstract;
    function DoSetSockOpt(Level, Cmd: Integer; Value: Integer): Boolean; virtual; abstract;
    procedure DoGetIoCtl(Cmd: Integer; var Arg: Integer); virtual; abstract;
  protected // IStream
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord; overload;
    function Write(const Buffer; Count: LongWord): LongWord; overload;
    function Read(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): LongWord; overload;
    function Write(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): LongWord; overload;
  protected // ISocketStream
    function ReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): LongWord;
    function WriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): LongWord;
  protected // ISocketParameters
    function GetWriteTimeout: LongWord;
    procedure SetWriteTimeout(const Value: LongWord);
    function GetReadTimeout: LongWord;
    procedure SetReadTimeout(const Value: LongWord);
    function GetFlag(Index: TSocketFlag): Boolean;
    procedure SetFlag(Index: TSocketFlag; const Value: Boolean);
    function GetReceiveBufferSize: LongWord;
    procedure SetReceiveBufferSize(const Value: LongWord);
    function GetSendBufferSize: LongWord;
    procedure SetSendBufferSize(const Value: LongWord);
    function GetAutoDisconnect: Boolean;
    procedure SetAutoDisconnect(Value: Boolean);
  protected // ISocket
    function GetParameters: ISocketParameters;
    procedure Bind(const Address: ISocketAddress); overload;
    procedure Bind(const Host: String; Port: Word = 0); overload;
    procedure Bind(const Address: Cardinal; Port: Word = 0); overload;
    procedure Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word = 0); overload;
    procedure Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: Cardinal; Port: Word = 0); overload;
    function WaitFor(const States: TSocketStates; Timeout: LongWord): Boolean; virtual; abstract;
    procedure Shutdown(const Mode: TSocketShutdown);
    procedure Disconnect;
  protected // ISocketInfo
    function GetLocal: ISocketAddress;
    function GetErrorStatus: Integer;
    function GetPendingDataSize: LongWord;
  protected // ISocketPeerInfo
    function GetRemote: ISocketAddress;
  protected // ISocketPeer
    function GetStream: ISocketStream;
    function GetInfo: ISocketPeerInfo;
  protected // ISocketClient
    function GetIsLocal: Boolean;
    function GetIsConnected: Boolean;
    // methods
    procedure Connect(const Address: ISocketAddress = nil); overload;
    procedure Connect(const Host: String; Port: Word); overload;
    procedure Connect(const Address: Cardinal; Port: Word); overload;
  protected // ISocketServer
    function GetIsListening: Boolean;
    function ISocketServer.GetInfo = DoSocketServerGetInfo;
    function DoSocketServerGetInfo: ISocketInfo;
    // methods
    procedure Listen;
    procedure Cancel;
    function Accept(out Client: ISocketClient): Boolean;
  public
    constructor Create(const Address: ISocketAddress = nil); reintroduce; virtual;
    constructor CreateAccepted(Handle: THandle; const Local: ISocketAddress; const Remote: ISocketAddress = nil);
    destructor Destroy; override;
  end;

  TSilSocketList = class (
    // extends
    TSilInterfaceList,
    // implements
    ISockets,
    ISocketList)
  protected // ISockets
    function Get(Index: Integer): ISocket;
    function Enumerate(var Enum: IEnumerator; out Item: ISocket): Boolean; reintroduce;
  protected // ISocketList
    procedure Put(Index: Integer; const Value: ISocket);
    function Add(const Value: ISocket): Integer; reintroduce;
    procedure Insert(Index: Integer; const Value: ISocket); reintroduce;
  public
    constructor Create(Locked: Boolean);
  end;

  TSilSocketEtcInfo = class (
    // extends
    TSilObject)
  protected
    FValid: Boolean;
    FName: String;
    FAliases: IStringList;
    FComment: String;
  protected // ISocketProtocol
    function GetIsValid: Boolean;
    function GetName: String;
    function GetAliases: IStringList;
    function GetComment: String;
  end;

  TSilSocketProtocol = class (
    // extends
    TSilSocketEtcInfo,
    // implements
    ISocketProtocol)
  protected
    FId: Smallint;
  protected // ISocketProtocol
    function GetId: Smallint;
  end;

  TSilSocketService = class (
    // extends
    TSilSocketEtcInfo,
    // implements
    ISocketService)
  protected
    FPort: Word;
    FProtocol: ISocketProtocol;
    FProtocolName: String;
  protected // ISocketService
    function GetPort: Word;
    function GetProtocol: ISocketProtocol;
  end;

implementation

uses
  SilBtStr,
  SilBtInt,
  SilOtTool,
  SilOsClasses,
  SilOsSocket,
  SilOdSocket,
  SilOcSocket,
  SilOtSocket,
  SilOjSocket,
  SilOmHandle;

const
  ASocketFlags: array [TSocketFlag, 0..1] of Integer = (
    (SOL_SOCKET, SO_BROADCAST),
    (SOL_SOCKET, SO_DEBUG),
    (SOL_SOCKET, SO_DONTLINGER),
    (SOL_SOCKET, SO_DONTROUTE),
    (SOL_SOCKET, SO_KEEPALIVE),
    (SOL_SOCKET, SO_OOBINLINE),
    (SOL_SOCKET, SO_REUSEADDR),
    (SOL_SOCKET, SO_SNDTIMEO),
    (SOL_SOCKET, SO_RCVTIMEO),
    (IPPROTO_TCP, TCP_NODELAY));

{ TSilSocket }

constructor TSilSocket.Create(const Address: ISocketAddress);
begin
  inherited Create(INVALID_HANDLE_VALUE, false);

  FWriteTimeout := INFINITE;
  FReadTimeout := INFINITE;
  FAutoDisconnect := true;

  FIsConnected := false;
  FIsLocal := true;

  if Address <> nil then
  begin
    FRemote := Address;
    DoSetupHandle(Address);
  end;
end;

constructor TSilSocket.CreateAccepted(Handle: THandle; const Local: ISocketAddress; const Remote: ISocketAddress);
begin
  Create;
  Self.Handle := TSilOsSocketHandle.Create(Handle, true);

  FIsConnected := true;
  FIsLocal := false;

  FLocal := TSilOsSocketLocalAddress.Create(Self.Handle, Local);
  FRemote := TSilOsSocketRemoteAddress.Create(Self.Handle, Remote);
end;

destructor TSilSocket.Destroy;
begin
  Disconnect;

  FLocal := nil;
  FRemote := nil;

  inherited;
end;

procedure TSilSocket.Disconnect;
begin
  if GetIsConnected then DoShutdown(shBoth);
  if handle <> nil then Handle.Close;
  FIsConnected := false;
end;

function TSilSocket.GetParameters: ISocketParameters;
begin
  Result := Self;
end;

function TSilSocket.GetReadTimeout: LongWord;
begin
  Result := FReadTimeout;
end;

function TSilSocket.GetWriteTimeout: LongWord;
begin
  Result := FWriteTimeout;
end;

procedure TSilSocket.SetReadTimeout(const Value: LongWord);
begin
  FReadTimeout := Value;
end;

procedure TSilSocket.SetWriteTimeout(const Value: LongWord);
begin
  FWriteTimeout := Value;
end;

function TSilSocket.GetFlag(Index: TSocketFlag): Boolean;
begin
  Result := Boolean(DoGetSockOpt(ASocketFlags[Index, 0], ASocketFlags[Index, 1]));
end;

procedure TSilSocket.SetFlag(Index: TSocketFlag; const Value: Boolean);
begin
  if Value then
    Include(FFlags, Index) else
    Exclude(FFlags, Index);

  if Handle.IsValid then
    DoSetSockOpt(ASocketFlags[Index, 0], ASocketFlags[Index, 1], Ord(Value));
end;

procedure TSilSocket.Shutdown(const Mode: TSocketShutdown);
begin
  if Handle.IsValid then DoShutdown(Mode);
end;

function TSilSocket.DoCheckResult(Value: Integer; CloseIfZero: Boolean): LongWord;
begin
  if (Value < 0) or ((Value = 0) and FAutoDisconnect and CloseIfZero) then
  begin
    Result := 0;
    Handle.Close;
  end else
    Result := Value;
end;

function TSilSocket.GetErrorStatus: Integer;
begin
  Result := DoGetSockOpt(SOL_SOCKET, SO_ERROR);
end;

function TSilSocket.GetInfo: ISocketPeerInfo;
begin
  Result := Self;
end;

function TSilSocket.GetLocal: ISocketAddress;
begin
  Result := FLocal;
end;

function TSilSocket.GetRemote: ISocketAddress;
begin
  Result := FRemote;
end;

function TSilSocket.GetSize: LongWord;
begin
  Result := GetPendingDataSize;
end;

function TSilSocket.GetPendingDataSize: LongWord;
var
  iVal: Integer;
begin
  if Handle.IsValid then
  begin
    DoGetIoCtl(FIONREAD, iVal);
    Result := LongWord(iVal);
  end else
    Result := 0;
end;

function TSilSocket.GetReceiveBufferSize: LongWord;
var
  TypeSpec: TSocketType;
begin
  if Assigned(FRemote) then
    TypeSpec := FRemote.TypeSpec else
  if Assigned(FLocal) then
    TypeSpec := FLocal.TypeSpec else
    TypeSpec := stUnknown;

  if TypeSpec <> stDatagram then
    Result := DoGetSockOpt(SOL_SOCKET, SO_RCVBUF) else
    Result := GetSendBufferSize;

  if Result = 0 then Result := 8192;
end;

procedure TSilSocket.SetReceiveBufferSize(const Value: LongWord);
begin
  if (FRemote <> nil) and (FRemote.TypeSpec <> stDatagram) then
    DoSetSockOpt(SOL_SOCKET, SO_RCVBUF, Value);
end;

function TSilSocket.GetSendBufferSize: LongWord;
begin
  if FLocal = nil then
  begin
    Result := 0;
    Exit;
  end;

  if FLocal.TypeSpec = stDatagram then
  begin
    Result := DoGetSockOpt(SOL_SOCKET, SO_MAX_MSG_SIZE);
    if Result = 0 then Result := 65527;
  end else
  begin
    Result := DoGetSockOpt(SOL_SOCKET, SO_SNDBUF);
    if Result = 0 then Result := 8192;
  end;
end;

procedure TSilSocket.SetSendBufferSize(const Value: LongWord);
begin
  if (FRemote <> nil) and (FRemote.TypeSpec <> stDatagram) then
    DoSetSockOpt(SOL_SOCKET, SO_SNDBUF, Value);
end;

function TSilSocket.GetStream: ISocketStream;
begin
  Result := Self;
end;

function TSilSocket.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := Read(Buffer, Count, []);
end;

function TSilSocket.Read(var Buffer; Count: LongWord; Flags: TSocketStreamFlags): LongWord;
var
  Affected: Integer;
begin
  if FIsConnected and WaitFor([ssRead], FReadTimeOut) then
    Affected := DoRead(Buffer, Count, Flags) else
    Affected := 0;

  Result := DoCheckResult(Affected, not (rfPeek in Flags));
end;

function TSilSocket.ReadFrom(var Buffer; Count: LongWord; out Address: ISocketAddress; Flags: TSocketStreamFlags): LongWord;
var
  Affected: Integer;
begin
  if WaitFor([ssRead], FReadTimeOut) and Handle.IsValid then
    Affected := DoReadFrom(Buffer, Count, Address, Flags) else
    Affected := 0;

  if Affected < 0 then
    Result := 0 else
    Result := Affected;
end;

function TSilSocket.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := Write(Buffer, Count, []);
end;

function TSilSocket.Write(const Buffer; Count: LongWord; Flags: TSocketStreamFlags): LongWord;
var
  Affected: Integer;
begin
  if FIsConnected and WaitFor([ssWrite], FWriteTimeOut) then
    Affected := DoWrite(Buffer, Count, Flags) else
    Affected := 0;

  Result := DoCheckResult(Affected, false);
end;

function TSilSocket.WriteTo(const Buffer; Count: LongWord; const Address: ISocketAddress; Flags: TSocketStreamFlags): LongWord;
var
  Affected: Integer;
begin
  if WaitFor([ssWrite], FWriteTimeOut) and Handle.IsValid then
    Affected := DoWriteTo(Buffer, Count, Address, Flags) else
    Affected := 0;

  if Affected < 0 then
    Result := 0 else
    Result := Affected;
end;

function TSilSocket.GetIsLocal: Boolean;
begin
  Result := FIsLocal;
end;

function TSilSocket.GetIsConnected: Boolean;
begin
  Result := Handle.IsValid and FIsConnected;
end;

function TSilSocket.GetIsListening: Boolean;
begin
  Result := DoGetSockOpt(SOL_SOCKET, SO_ACCEPTCONN) > 0;
end;

procedure TSilSocket.DoSetupHandle(const Address: ISocketAddress);
begin
  Handle := TSilOsSocketHandle.Create(DoSocket(Address), true);
  if Handle.IsValid then DoSetFlags;
end;

procedure TSilSocket.DoSetFlags;
var
  i: TSocketFlag;
begin
  if FFlags <> [] then
    for i := Low(i) to High(i) do
      if i in FFlags then
        SetFlag(i, true);
end;

procedure TSilSocket.Bind(const Address: ISocketAddress);
begin
  if Address <> nil then
  begin
    FLocal := Address;
    DoSetupHandle(Address);
  end else
  if FLocal = nil then
    raise OS.Error.Create(SSocketBindNoAddressError);

  DoBind(FLocal);
  FLocal := TSilOsSocketLocalAddress.Create(Handle, FLocal);
end;

procedure TSilSocket.Bind(const Host: String; Port: Word);
begin
  if Assigned(FRemote) then
    Bind(FRemote.TypeSpec, FRemote.Protocol, Host, Port) else
  if Assigned(FLocal) then
    Bind(FLocal.TypeSpec, FLocal.Protocol, Host, Port) else
end;

procedure TSilSocket.Bind(const Address: Cardinal; Port: Word);
begin
  if Assigned(FRemote) then
    Bind(FRemote.TypeSpec, FRemote.Protocol, Address, Port) else
  if Assigned(FLocal) then
    Bind(FLocal.TypeSpec, FLocal.Protocol, Address, Port) else
end;

procedure TSilSocket.Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word);
begin
  Bind(TypeSpec, Protocol, OsIP.FromStr(Host), Port);
end;

procedure TSilSocket.Bind(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: Cardinal; Port: Word);
var
  Addr: ISocketAddress;
begin
  Addr := TSilSocketAddress.Create(TypeSpec, Protocol, Address, Port);
  Bind(Addr);
end;

procedure TSilSocket.Connect(const Host: String; Port: Word);
var
  Addr: ISocketAddress;
begin
  Addr := TSilSocketAddress.Create(stStream, spTCP, Host, Port);
  Connect(Addr);
end;

procedure TSilSocket.Connect(const Address: Cardinal; Port: Word);
var
  Addr: ISocketAddress;
begin
  Addr := TSilSocketAddress.Create(stStream, spTCP, Address, Port);
  Connect(Addr);
end;

procedure TSilSocket.Connect(const Address: ISocketAddress);
begin
  if not FIsLocal then
    raise OS.Error.Create(SSocketConnectAcceptedError);

  if Address <> nil then
    FRemote := Address else
  if FRemote = nil then
    raise OS.Error.Create(SSocketConnectNoAddressError);

  try
    DoSetupHandle(FRemote);
    FIsConnected := DoConnect(FRemote);
  except
    Handle.Close;
    FIsConnected := false;
    raise;
  end;

  if FIsConnected then
  begin
    FLocal := TSilOsSocketLocalAddress.Create(Handle, FRemote);
    FRemote := TSilOsSocketRemoteAddress.Create(Handle, FRemote);
  end;
end;

procedure TSilSocket.Listen;
begin
  DoListen;
end;

procedure TSilSocket.Cancel;
begin
  Disconnect;
end;

function TSilSocket.DoSocketServerGetInfo: ISocketInfo;
begin
  Result := Self;
end;

function TSilSocket.Accept(out Client: ISocketClient): Boolean;
var
  SocketClient: RSocketClient;
begin
  Result := false;
  if not Handle.IsValid then Exit;

  Result := DoAccept(SocketClient);

  if Result then
  begin
    Client := TSilOsSocket.CreateAccepted(SocketClient.Socket, FLocal, SocketClient.Address);

    with Client.Parameters do
    begin
      WriteTimeout := FWriteTimeout;
      ReadTimeout := FReadTimeout;
    end;
  end;
end;

function TSilSocket.GetAutoDisconnect: Boolean;
begin
  Result := FAutoDisconnect;
end;

procedure TSilSocket.SetAutoDisconnect(Value: Boolean);
begin
  FAutoDisconnect := Value;
end;

{ TSilSocketEtcInfo }

function TSilSocketEtcInfo.GetAliases: IStringList;
begin
  Result := FAliases;
end;

function TSilSocketEtcInfo.GetComment: String;
begin
  Result := FComment;
end;

function TSilSocketEtcInfo.GetName: String;
begin
  Result := FName;
end;

function TSilSocketEtcInfo.GetIsValid: Boolean;
begin
  Result := FValid;
end;

{ TSilSocketProtocol }

function TSilSocketProtocol.GetId: Smallint;
begin
  Result := FId;
end;

{ TSilSocketService }

function TSilSocketService.GetPort: Word;
begin
  Result := FPort;
end;

function TSilSocketService.GetProtocol: ISocketProtocol;
begin
  if FProtocol = nil then FProtocol := OsInfo.Protocol(FProtocolName);
  Result := FProtocol;
end;

{ TSilSocketAddress }

constructor TSilSocketAddress.Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; Address: LongWord; Port: Word);
begin
  inherited Create;

  FTypeSpec := TypeSpec;
  FProtocol := Protocol;
  FAddress := Address;
  FPort := Port;
end;

constructor TSilSocketAddress.Create(TypeSpec: TSocketType; Protocol: TSocketProtocol; const Host: String; Port: Word);
begin
  inherited Create;

  FTypeSpec := TypeSpec;
  FProtocol := Protocol;
  FHost := Host;
  FPort := Port;
end;

destructor TSilSocketAddress.Destroy;
begin
  inherited;
end;

procedure TSilSocketAddress.DoGetPeerHost;
begin
  if Str.IsEmpty(FHost) and (FAddress <> 0) then FHost := OsIP.ToHostName(FAddress);
end;

procedure TSilSocketAddress.DoGetPeerAddress;
begin
  if (FAddress = 0) and Str.NotEmpty(FHost) then FAddress := OsIP.FromStr(FHost);
end;

function TSilSocketAddress.GetAddress: LongWord;
begin
  if FAddress = 0 then DoGetPeerAddress;
  Result := FAddress;
end;

function TSilSocketAddress.GetBroadcast: LongWord;
begin
  Result := GetNetwork or not FSubnetMask;
end;

function TSilSocketAddress.GetHost: String;
begin
  if Str.IsEmpty(FHost) then DoGetPeerHost;
  Result := FHost;
end;

function TSilSocketAddress.GetNetwork: LongWord;
begin
  if FSubnetMask = 0 then DoGetSubnetMask;
  Result := FSubnetMask and FAddress;
end;

function TSilSocketAddress.GetNetworkClass: TSocketNetworkClass;
begin
  if FAddress shr 24 = 127 then
    Result := snInternal else
  if FAddress shr 31 = 0 then
    Result := snClassA else
  if FAddress shr 30 = 2 then
    Result := snClassB else
  if FAddress shr 29 = 6 then
    Result := snClassC else
    Result := snUnknown;
end;

function TSilSocketAddress.GetPort: Word;
begin
  if FPort = 0 then DoGetPeerAddress;
  Result := FPort;
end;

function TSilSocketAddress.GetProtocol: TSocketProtocol;
begin
  Result := FProtocol;
end;

procedure TSilSocketAddress.DoGetSubnetMask;
var
  List: ISocketAddresses;
  Enum: IEnumerator;
  Item: ISocketAddress;
begin
  List := OsInfo.AdapterList;

  while List.Enumerate(Enum, Item) do
    if Item.Address = FAddress then
    begin
      FSubnetMask := Item.SubnetMask;
      Exit;
    end;
end;

function TSilSocketAddress.GetSubnetMask: LongWord;
begin
  if FSubnetMask = 0 then DoGetSubnetMask;
  Result := FSubnetMask;
end;

function TSilSocketAddress.GetTypeSpec: TSocketType;
begin
  Result := FTypeSpec;
end;

procedure TSilSocketAddress.SetAddress(const Value: LongWord);
begin
  FAddress := Value;
end;

procedure TSilSocketAddress.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TSilSocketAddress.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TSilSocketAddress.SetProtocol(const Value: TSocketProtocol);
begin
  FProtocol := Value;
end;

procedure TSilSocketAddress.SetSubnetMask(const Value: LongWord);
begin
  FSubnetMask := Value;
end;

procedure TSilSocketAddress.SetTypeSpec(const Value: TSocketType);
begin
  FTypeSpec := Value;
end;

{
  CAddress = '%a';
  CAddressPort = '%p';
  CAddressMask = '%m';
  CAddressBroadcast = '%b';
  CAddressNetwork = '%n';
  CAddressNetBits = '%t';
}
function TSilSocketAddress.Format(const FmtStr: String): String;

  function DoGetNetBits: String;
  var
    Mask, Bits: LongWord;
  begin
    Mask := GetSubnetMask;

    if Mask > 0 then
      Bits := SizeOf(LongWord) * 8 else
      Bits := 0;

    while Mask > 0 do
    begin
      if Mask and 1 = 1 then Break;
      Dec(Bits);
      Mask := Mask shr 1;
    end;

    Result := Int.ToStr(Bits);
  end;

const
  List: String = 'abmnpt';
var
  i, iPos: Integer;
  Find, Item: String;
begin
  Result := FmtStr;

  for i := 1 to Length(List) do
  begin
    Find := '%' + List[i];
    iPos := Str.Pos(Find, FmtStr);

    if iPos > 0 then
    begin
      case List[i] of
        'a':  Item := OsIP.ToStr(GetAddress);
        'b':  Item := OsIP.ToStr(GetBroadcast);
        'm':  Item := OsIP.ToStr(GetSubnetMask);
        'n':  Item := OsIP.ToStr(GetNetwork);
        'p':  Item := Int.ToStr(GetPort);
        't':  Item := DoGetNetBits;
      end;

      Result := Str.Replace(Result, Find, Item);
    end;
  end;
end;

function TSilSocketAddress.GetFlags: TSocketAddressFlags;
begin
  Result := [];
  if Str.NotEmpty(FHost) then Include(Result, afHost);
  if FAddress > 0 then Include(Result, afAddress);
  if FPort > 0 then Include(Result, afPort);
  if FTypeSpec <> stUnknown then Include(Result, afTypeSpec);
  if FProtocol <> spUnknown then Include(Result, afProtocol);
  if FSubnetMask > 0 then Include(Result, afSubnetMask);
end;

{ TSilSocketList }

constructor TSilSocketList.Create(Locked: Boolean);
begin
  inherited Create(Locked);
end;

function TSilSocketList.Add(const Value: ISocket): Integer;
begin
  Result := inherited Add(Value);
end;

procedure TSilSocketList.Insert(Index: Integer; const Value: ISocket);
begin
  inherited Insert(Index, Value);
end;

function TSilSocketList.Enumerate(var Enum: IEnumerator; out Item: ISocket): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilSocketList.Get(Index: Integer): ISocket;
begin
  Result := ISocket(GetItem(Index));
end;

procedure TSilSocketList.Put(Index: Integer; const Value: ISocket);
begin
  inherited SetItem(Index, Value);
end;

{ TSilSocketAddressList }

function TSilSocketAddressList.Add(const Value: ISocketAddress): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilSocketAddressList.Enumerate(var Enum: IEnumerator; out Item: ISocketAddress): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilSocketAddressList.Get(Index: Integer): ISocketAddress;
begin
  Result := ISocketAddress(GetItem(Index));
end;

procedure TSilSocketAddressList.Insert(Index: Integer; const Value: ISocketAddress);
begin
  inherited Insert(Index, Value);
end;

procedure TSilSocketAddressList.Put(Index: Integer; const Value: ISocketAddress);
begin
  inherited SetItem(Index, Value);
end;

end.
