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

unit SilSiSocks5Protocol;

interface

uses
  Sil,
  SilSiProtocolBase;

type
  TSocks5Resp = record
    Reply: Byte;
    Address: String;
    Port: LongWord;
  end;

  TSocks5UdpResp = record
    Address: String;
    Port: LongWord;
    Data: String;
  end;

  ISocks5Client = interface (IProtocol)
    ['{CAF52481-080A-11D4-9150-00C0261013CD}']
    function Login(const UserName: String = ''; const Password: String = ''): Boolean;
    function Connect(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function Bind(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function Accept(out Data: TSocks5Resp): Boolean;
    function UdpAssociate(const Host: String; Port: LongWord; out Data: TSocks5Resp): Boolean;
    function SendUdpPacket(const Host: String; Port: LongWord; const Data): Boolean;
    function WaitUdpPacket(out Data: TSocks5UdpResp): Boolean;
  end;

  ISocks5Server = interface (IProtocol)
    ['{BB4ABB82-6D37-11D4-9894-00104B0FA1EF}']
  end;

  TAuthenticationMethodsEvent = record
    Sender: ISocks5Server;
    Methods: TByteArray;
    Result: Byte;
  end;

  TAuthenticationEvent = record
    Sender: ISocks5Server;
    UserName: String;
    Password: String;
    Result: Byte;
  end;

  TRequestEvent = record
    Sender: ISocks5Server;
    Host: String;
    Port: LongWord;
    Result: TSocks5Resp;
  end;

  ISocks5ServerEvents = interface
    ['{BB4ABB81-6D37-11D4-9894-00104B0FA1EF}']
    procedure OnAuthenticationMethods(var Event: TAuthenticationMethodsEvent);
    procedure OnAuthentication(var Event: TAuthenticationEvent);
    procedure OnConnect(var Event: TRequestEvent);
    procedure OnBind(var Event: TRequestEvent);
    procedure OnUdpAssociate(var Event: TRequestEvent);
  end;

implementation

end.
