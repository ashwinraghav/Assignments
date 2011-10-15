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

unit SilSmSocks5Protocol;

{$I Defines.inc}

interface

uses
  Sil,

  SilSiSocks5Protocol,
  SilSiAbstractConnection;

const
  MI_BASE = $FA0;
  MI_AUTHENTICATION_METHODS = MI_BASE + 1;
  MI_AUTHENTICATION = MI_AUTHENTICATION_METHODS + 1;
  MI_CONNECT = MI_AUTHENTICATION + 1;
  MI_BIND = MI_CONNECT + 1;
  MI_UDP_ASSOCIATE = MI_BIND + 1;

const
  Socks5Version = #$05;
  Socks5AuthVersion = #$01;

type
  TSocks5State = (ssNone, ssConnect, ssLogin, ssRequest, ssUdpPacket);

  TSocks5Message = record
    Id: LongWord;
    Buffer: PChar;
    Size: LongWord
  end;
  
  TSocks5Protocol = class (
    // extends
    TSilInterfacedObject,
    // implements
    IConnectedEvents,
    ISocks5Client,
    ISocks5Server)
  private
    FServerState: TSocks5State;
    FConnection: IAbstractConnection;
    FServers: IEventList;
  private
    procedure DoReceive(Buffer: PChar; Size: LongWord);
  private // from ISocks5ServerEvents
    procedure FireAuthenticationMethods(var Msg: TSocks5Message); message MI_AUTHENTICATION_METHODS;
    procedure FireAuthentication(var Msg: TSocks5Message); message MI_AUTHENTICATION;
    procedure FireConnect(var Msg: TSocks5Message); message MI_CONNECT;
    procedure FireBind(var Msg: TSocks5Message); message MI_BIND;
    procedure FireUdpAssociate(var Msg: TSocks5Message); message MI_UDP_ASSOCIATE;
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent);
    procedure OnDataSent(const Event: TConnectionDataEvent);
    procedure OnDisconnected(const Event: TConnectionBreakEvent);
  protected // IProtocol
    function GetConnection: IAbstractConnection;
    procedure SetConnection(const Value: IAbstractConnection);
  protected // ISocks5Client
    function Login(const UserName: String = ''; const Password: String = ''): Boolean;
    function Connect(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function Bind(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function Accept(out Data: TSocks5Resp): Boolean;
    function UdpAssociate(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function SendUdpPacket(const Host: String; Port: LongWord; const Data): Boolean;
    function WaitUdpPacket(out Data: TSocks5UdpResp): Boolean;
  public
    constructor Create(const Connection: IAbstractConnection);
  end;

implementation

const
  NO_AUTHENTICATION = $00;
  GSSAPI = $01;
  USERNAME_PASSWORD = $02;
  IANA_ASSIGNED = $03;
  RESERVED = $80;
  NO_ACCEPTABLE_METHODS = $FF;

const
  REQUEST_CONNECT = $01;
  REQUEST_BIND = $02;
  REQUEST_UDP_ASSOCIATE = $03;

  IPV4_ADDRESS = $01;
  DOMAINNAME = $03;
  IPV6_ADDRESS = $04;

const
  SUCCEEDED = $00;
  SERVER_FAILURE = $01;
  CONNECTION_NOT_ALLOWED = $02;
  NETWORK_UNREACHABLE = $03;
  HOST_UNREACHABLE = $04;
  CONNECTION_REFUSED = $05;
  TTL_EXPIRED = $06;
  COMMAND_NOT_SUPPORTED = $07;
  ADDRESS_TYPE_NOT_SUPPORTED = $08;

type
  TSocks5Connection = record
    SocksVersion: Byte;
    MethodCount: Byte;
    Methods: TByteArray;
  end;

  TSocks5SelectedMethod = record
    SocksVersion: Byte;
    Method: Byte;
  end;

  TSocks5Authentication = record
    AuthVersion: Byte; {01}
    UserName: String;
    Password: String;
  end;

  TSocks5AuthenticationResponse = record
    AuthVersion: Byte; {01}
    Status: Byte;
  end;

  TSocks5Request = record
    SocksVersion: Byte;
    Command: Byte;
    AddressType: Byte;
    Address: String;
    Port: Word;
  end;

  TSocks5Reply = record
    SocksVersion: Byte;
    Reply: Byte;
    AddressType: Byte;
    BoundAddress: String;
    BoundPort: Word;
  end;

  TSocks5UdpDatagram = record
    FragmentNumber: Byte;
    AddressType: Byte;
    Address: String;
    Port: Word;
    Buffer: String;
  end;

function IsIPAddress(const Address: String): Boolean;
var
  i: Word;
begin
  for i := 1 to Length(Address) do
    if not (Address[i] in ['0'..'9', '.']) then
    begin
      Result := false;
      Exit;
    end;
  Result := true;
end;

function IPAddressToLongWord(const Address: String): LongWord;
var
  i, idx: Integer;
  sItem: String;
begin
  try
    idx := 4;
    i := 1;
    repeat
      sItem := Str.Token(Address, '.', i);
      Dec(idx);
      if (Length(sItem) > 0) and (idx >= 0) then
        PChar(@Result)[idx] := Char(Str.ToInt(sItem));
    until i = 0;
  except
    Result := 0;
  end;
end;

function LongWordToIPAddress(Address: LongWord): String;
begin
  Result :=
    Int.ToStr(Byte(PChar(@Address)[0])) + '.' +
    Int.ToStr(Byte(PChar(@Address)[1])) + '.' +
    Int.ToStr(Byte(PChar(@Address)[2])) + '.' +
    Int.ToStr(Byte(PChar(@Address)[3]));
end;

function Socks5ConnectSend: String;
begin
  Result := Char(Socks5Version) + Char(2) + Char(NO_AUTHENTICATION) + Char(USERNAME_PASSWORD);
end;

function Socks5ConnectRecv(const Msg: TSocks5Message; out Data: TSocks5Connection): Boolean;
begin
  Result := Msg.Size >= 3;
  if Result then
  begin
    Data.SocksVersion := Byte(Msg.Buffer[0]);
    Data.MethodCount := Byte(Msg.Buffer[1]);

    SetLength(Data.Methods, Data.MethodCount);
    Move(Msg.Buffer[2], Data.Methods[0], Data.MethodCount);
  end;
end;

function Socks5SelectMethodSend(Method: Byte): String;
begin
  Result := Char(Socks5Version) + Char(Method);
end;

function Socks5SelectMethodRecv(const Buffer: String; out Data: TSocks5SelectedMethod): Boolean;
begin
  Result := Length(Buffer) >= 2;
  if Result then
  begin
    Data.SocksVersion := Byte(Buffer[1]);
    Data.Method := Byte(Buffer[2]);
  end;
end;

function Socks5AuthenticationSend(const UserName, Password: String): String;
begin
  Result :=
    Char(Socks5AuthVersion) +
    Char(Length(UserName)) + UserName +
    Char(Length(Password)) + Password;
end;

function Socks5AuthenticationRecv(const Msg: TSocks5Message; out Data: TSocks5Authentication): Boolean;
var
  wPos: Word;
begin
  Result := Msg.Size >= 5;
  if Result then
  begin
    Data.AuthVersion := Byte(Msg.Buffer[0]);
    SetString(Data.UserName, PChar(@Msg.Buffer[2]), Byte(Msg.Buffer[1]));
    wPos := 2 + Byte(Msg.Buffer[1]);
    SetString(Data.Password, PChar(@Msg.Buffer[wPos + 1]), Byte(Msg.Buffer[wPos]));
  end;
end;

function Socks5AuthenticationResponseSend(Status: Byte): String;
begin
  Result := Char(Socks5AuthVersion) + Char(Status);
end;

function Socks5AuthenticationResponseRecv(const Buffer: String; out Data: TSocks5AuthenticationResponse): Boolean;
begin
  Result := Length(Buffer) >= 2;
  if Result then
  begin
    Data.AuthVersion := Byte(Buffer[1]);
    Data.Status := Byte(Buffer[2]);
  end;
end;

function Socks5RequestSend(Command: Byte; Address: String; Port: Word): String;
var
  APort: array [0..1] of Char;
  bAddressType: Byte;
  lwAddress: LongWord;
begin
  Move(Port, APort, 2);
  if IsIPAddress(Address) then
  begin
    bAddressType := IPV4_ADDRESS;
    lwAddress := IPAddressToLongWord(Address);
    SetString(Address, PChar(@lwAddress), SizeOf(LongWord));
  end else
  begin
    bAddressType := DOMAINNAME;
    Address := Char(Length(Address)) + Address;
  end;
  Result :=
    Char(Socks5Version) + Char(Command) + #0 +
    Char(bAddressType) + Address + APort[1] + APort[0];
end;

function Socks5RequestRecv(const Msg: TSocks5Message; out Data: TSocks5Request): Boolean;
var
  wPos: Word;
  APort: array [0..1] of Char;
begin
  Result := Msg.Size >= 8;
  if Result then
  begin
    Data.SocksVersion := Byte(Msg.Buffer[0]);
    Data.Command := Byte(Msg.Buffer[1]);
    Data.AddressType := Byte(Msg.Buffer[3]);

    if Data.AddressType = IPV4_ADDRESS then
    begin
      Data.Address := LongWordToIPAddress(PLongWord(@Msg.Buffer[4])^);
      wPos := 9;
    end else
    begin
      SetString(Data.Address, PChar(@Msg.Buffer[5]), Byte(Msg.Buffer[4]));
      wPos := 5 + Byte(Msg.Buffer[4]);
    end;

    APort[1] := Msg.Buffer[wPos];
    APort[0] := Msg.Buffer[wPos + 1];
    Move(APort, Data.Port, 2);
  end;
end;

function Socks5ReplySend(Reply: Byte; Address: String; Port: Word): String;
var
  APort: array [0..1] of Char;
  bAddressType: Byte;
  lwAddress: LongWord;
begin
  Move(Port, APort, 2);
  if IsIPAddress(Address) then
  begin
    bAddressType := IPV4_ADDRESS;
    lwAddress := IPAddressToLongWord(Address);
    SetString(Address, PChar(@lwAddress), SizeOf(LongWord));
  end else
  begin
    bAddressType := DOMAINNAME;
    Address := Char(Length(Address)) + Address;
  end;
  Result :=
    Char(Socks5Version) + Char(Reply) + #0 +
    Char(bAddressType) + Address + APort[1] + APort[0];
end;

function Socks5ReplyRecv(const Buffer: String; out Data: TSocks5Reply): Boolean;
var
  wPos: Word;
  APort: array [0..1] of Char;
begin
  Result := Length(Buffer) >= 8;
  if Result then
  begin
    Data.SocksVersion := Byte(Buffer[1]);
    Data.Reply := Byte(Buffer[2]);
    Data.AddressType := Byte(Buffer[4]);

    if Data.AddressType = IPV4_ADDRESS then
    begin
      Data.BoundAddress := LongWordToIPAddress(PLongWord(@Buffer[5])^);
      wPos := 9;
    end else
    begin
      SetString(Data.BoundAddress, PChar(@Buffer[6]), Byte(Buffer[5]));
      wPos := 6 + Byte(Buffer[5]);
    end;

    APort[1] := Buffer[wPos];
    APort[0] := Buffer[wPos + 1];
    Move(APort, Data.BoundPort, 2);
  end;
end;

function Socks5DatagramSend(Address: String; Port: Word; const Data): String;
var
  APort: array [0..1] of Char;
  bAddressType: Byte;
  lwAddress: LongWord;
begin
  Move(Port, APort, 2);
  if IsIPAddress(Address) then
  begin
    bAddressType := IPV4_ADDRESS;
    lwAddress := IPAddressToLongWord(Address);
    SetString(Address, PChar(@lwAddress), SizeOf(LongWord));
  end else
  begin
    bAddressType := DOMAINNAME;
    Address := Char(Length(Address)) + Address;
  end;
  Result := #0#0#0 + Char(bAddressType) + Address + APort[1] + APort[0] + PChar(@Data);
end;

function Socks5DatagramRecv(const Buffer: String; out Data: TSocks5UdpDatagram): Boolean;
var
  wPos: Word;
  APort: array [0..1] of Char;
begin
  Result := Length(Buffer) >= 8;
  if Result then
  begin
    Data.FragmentNumber := Byte(Buffer[3]);
    Data.AddressType := Byte(Buffer[4]);

    if Data.AddressType = IPV4_ADDRESS then
    begin
      Data.Address := LongWordToIPAddress(PLongWord(@Buffer[5])^);
      wPos := 9;
    end else
    begin
      SetString(Data.Address, PChar(@Buffer[6]), Byte(Buffer[5]));
      wPos := 6 + Byte(Buffer[5]);
    end;

    APort[1] := Buffer[wPos];
    APort[0] := Buffer[wPos + 1];
    Move(APort, Data.Port, 2);

    SetString(Data.Buffer, PChar(@Buffer[wPos + 2]), Length(Buffer) - wPos + 3);
  end;
end;

{ TSocks5Protocol }

constructor TSocks5Protocol.Create(const Connection: IAbstractConnection);
begin
  inherited Create;
  FServerState := ssConnect;
  FConnection := Connection;
end;

function TSocks5Protocol.GetConnection: IAbstractConnection;
begin
  Result := FConnection;
end;

procedure TSocks5Protocol.SetConnection(const Value: IAbstractConnection);
begin
  FConnection := Value;
end;

procedure TSocks5Protocol.DoReceive(Buffer: PChar; Size: LongWord);
var
  Msg: TSocks5Message;
begin
  Msg.Id := MI_BASE + Ord(FServerState);
  Msg.Buffer := Buffer;
  Msg.Size := Size;
  Dispatch(Msg);
end;

procedure TSocks5Protocol.OnDataReceived(const Event: TConnectionDataEvent);
begin
  DoReceive(Event.Buffer, Event.Size);
end;

procedure TSocks5Protocol.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  if FConnection <> nil then
  begin
    Sil.Sink.Connect(FConnection, Self);
    FConnection := nil;
  end;
end;

procedure TSocks5Protocol.FireAuthenticationMethods(var Msg: TSocks5Message);
var
  Event: TAuthenticationMethodsEvent;
  e: IEnumerator;
  i: ISocks5ServerEvents;
  Data: TSocks5Connection;
begin
  if (FServers = nil) or not Socks5ConnectRecv(Msg, Data) then Exit;

  Event.Sender := Self;
  Event.Methods := Data.Methods;
  Event.Result := SUCCEEDED;

  while FServers.Enumerate(e, i, ISocks5ServerEvents) do i.OnAuthenticationMethods(Event);

  if Event.Result = NO_AUTHENTICATION then FServerState := ssRequest else
  if Event.Result = USERNAME_PASSWORD then FServerState := ssLogin;
  FConnection.WriteStr(Socks5SelectMethodSend(Event.Result));
end;

procedure TSocks5Protocol.FireAuthentication(var Msg: TSocks5Message);
var
  Event: TAuthenticationEvent;
  e: IEnumerator;
  i: ISocks5ServerEvents;
  Data: TSocks5Authentication;
begin
  if (FServers = nil) or not Socks5AuthenticationRecv(Msg, Data) then Exit;

  Event.Sender := Self;
  Event.UserName := Data.UserName;
  Event.Password := Data.Password;
  Event.Result := SUCCEEDED;

  while FServers.Enumerate(e, i, ISocks5ServerEvents) do i.OnAuthentication(Event);

  if Event.Result = SUCCEEDED then FServerState := ssRequest;
  FConnection.WriteStr(Socks5AuthenticationResponseSend(Event.Result));
end;

procedure TSocks5Protocol.FireConnect(var Msg: TSocks5Message);
var
  Event: TRequestEvent;
  e: IEnumerator;
  i: ISocks5ServerEvents;
  Data: TSocks5Request;
begin
  if (FServers = nil) or not Socks5RequestRecv(Msg, Data) then Exit;

  Event.Sender := Self;
  Event.Host := Data.Address;
  Event.Port := Data.Port;
  Event.Result.Address := Data.Address;
  Event.Result.Port := Data.Port;
  Event.Result.Reply := SUCCEEDED;

  while FServers.Enumerate(e, i, ISocks5ServerEvents) do i.OnConnect(Event);
  FConnection.WriteStr(Socks5ReplySend(Event.Result.Reply, Event.Host, Event.Port));
end;

procedure TSocks5Protocol.FireBind(var Msg: TSocks5Message);
var
  Event: TRequestEvent;
  e: IEnumerator;
  i: ISocks5ServerEvents;
  Data: TSocks5Request;
begin
  if (FServers = nil) or not Socks5RequestRecv(Msg, Data) then Exit;

  Event.Sender := Self;
  Event.Host := Data.Address;
  Event.Port := Data.Port;
  Event.Result.Address := Data.Address;
  Event.Result.Port := Data.Port;
  Event.Result.Reply := SUCCEEDED;

  while FServers.Enumerate(e, i, ISocks5ServerEvents) do i.OnBind(Event);
  FConnection.WriteStr(Socks5ReplySend(Event.Result.Reply, Event.Host, Event.Port));
end;

procedure TSocks5Protocol.FireUdpAssociate(var Msg: TSocks5Message);
var
  Event: TRequestEvent;
  e: IEnumerator;
  i: ISocks5ServerEvents;
  Data: TSocks5Request;
begin
  if (FServers = nil) or not Socks5RequestRecv(Msg, Data) then Exit;

  Event.Sender := Self;
  Event.Host := Data.Address;
  Event.Port := Data.Port;
  Event.Result.Address := Data.Address;
  Event.Result.Port := Data.Port;
  Event.Result.Reply := SUCCEEDED;

  while FServers.Enumerate(e, i, ISocks5ServerEvents) do i.OnUdpAssociate(Event);
  if Event.Result.Reply = SUCCEEDED then FServerState := ssUdpPacket;
  FConnection.WriteStr(Socks5ReplySend(Event.Result.Reply, Event.Host, Event.Port));
end;

function TSocks5Protocol.Login(const UserName, Password: String): Boolean;
var
  sResp: String;
  Data1: TSocks5SelectedMethod;
  Data2: TSocks5AuthenticationResponse;
begin
  FConnection.WriteStr(Socks5ConnectSend);
  Result := FConnection.ReadStr(sResp) > 0;

  if Result then
  begin
    Result := Socks5SelectMethodRecv(sResp, Data1);
    if Result and (Data1.Method = USERNAME_PASSWORD) then
    begin
      FConnection.WriteStr(Socks5AuthenticationSend(UserName, Password));
      Result := FConnection.ReadStr(sResp) > 0;
      if Result then Result := Socks5AuthenticationResponseRecv(sResp, Data2) and (Data2.Status = SUCCEEDED);
    end;
  end;
end;

function TSocks5Protocol.Connect(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
var
  sResp: String;
  Data1: TSocks5Reply;
begin
  Data.Reply := SERVER_FAILURE;
  FConnection.WriteStr(Socks5RequestSend(REQUEST_CONNECT, Host, Port));
  Result := FConnection.ReadStr(sResp) > 0;

  if Result and Socks5ReplyRecv(sResp, Data1) and (Data1.Reply = SUCCEEDED) then
  begin
    Data.Address := Data1.BoundAddress;
    Data.Port := Data1.BoundPort;
    Data.Reply := Data1.Reply;
    Result := Data.Reply = SUCCEEDED;
  end;
end;

function TSocks5Protocol.Bind(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
var
  sResp: String;
  Data1: TSocks5Reply;
begin
  Data.Reply := SERVER_FAILURE;
  FConnection.WriteStr(Socks5RequestSend(REQUEST_BIND, Host, Port));
  Result := FConnection.ReadStr(sResp) > 0;

  if Result and Socks5ReplyRecv(sResp, Data1) and (Data1.Reply = SUCCEEDED) then
  begin
    Data.Address := Data1.BoundAddress;
    Data.Port := Data1.BoundPort;
    Data.Reply := Data1.Reply;
    Result := Data.Reply = SUCCEEDED;
  end;
end;

function TSocks5Protocol.Accept(out Data: TSocks5Resp): Boolean;
var
  sResp: String;
  Data1: TSocks5Reply;
begin
  Data.Reply := SERVER_FAILURE;
  Result := FConnection.ReadStr(sResp) > 0;

  if Result and Socks5ReplyRecv(sResp, Data1) and (Data1.Reply = SUCCEEDED) then
  begin
    Data.Address := Data1.BoundAddress;
    Data.Port := Data1.BoundPort;
    Data.Reply := Data1.Reply;
    Result := Data.Reply = SUCCEEDED;
  end;
end;

function TSocks5Protocol.UdpAssociate(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
var
  sResp: String;
  Data1: TSocks5Reply;
begin
  Data.Reply := SERVER_FAILURE;
  FConnection.WriteStr(Socks5RequestSend(REQUEST_UDP_ASSOCIATE, Host, Port));

  Result := FConnection.ReadStr(sResp) > 0;

  if Result and Socks5ReplyRecv(sResp, Data1) and (Data1.Reply = SUCCEEDED) then
  begin
    Data.Address := Data1.BoundAddress;
    Data.Port := Data1.BoundPort;
    Data.Reply := Data1.Reply;
    Result := Data.Reply = SUCCEEDED;
  end;
end;

function TSocks5Protocol.SendUdpPacket(const Host: String; Port: LongWord; const Data): Boolean;
begin
  Result := FConnection.WriteStr(Socks5DatagramSend(Host, Port, Data)) > 0;
end;

function TSocks5Protocol.WaitUdpPacket(out Data: TSocks5UdpResp): Boolean;
var
  sResp: String;
  Data1: TSocks5UdpDatagram;
begin
  Result := FConnection.ReadStr(sResp) > 0;

  if Result and Socks5DatagramRecv(sResp, Data1) then
  begin
    Data.Address := Data1.Address;
    Data.Port := Data1.Port;
    Data.Data := Data1.Buffer;
  end;
end;

procedure TSocks5Protocol.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

end.
