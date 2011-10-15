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

unit SilSiProtocolBase;

{$I Defines.inc}

interface

uses
  SilSiAbstractConnection,
  SilSiPacketCompletion,
  SilSiPacketBuilder,
  SilSiProtocolPacket,
  SilOsTypes,
  SilSiCommunicationQueue;

const
  SIL_FIRST_PROT_ID = $0400;
  SIL_FIRST_PORT = 23360;

  SIL_LAST_PROT_ID = $8000;
  SIL_LAST_PORT = 43360;

type
  IProtocol = interface
    ['{B68181EA-573E-11D4-988A-00104B0FA1EF}']
    function GetConnection: IAbstractConnection;
    procedure SetConnection(const Value: IAbstractConnection);
    property Connection: IAbstractConnection read GetConnection write SetConnection;
  end;

  IFormatedProtocol = interface (IProtocol)
    ['{C1CA05D1-7F48-11D4-9897-00104B0FA1EF}']
    function GetPacketCompletion: IPacketCompletion;
    procedure SetPacketCompletion(const Value: IPacketCompletion);
    function GetQueue: ICommunicationQueue;
    procedure SetQueue(const Value: ICommunicationQueue);
    function GetProtocolID: LongWord;
    procedure SetProtocolID(Value: LongWord);
    function GetSessionID: LongWord;
    procedure SetSessionID(Value: LongWord);
    function GetName: String;
    function FlushQueue: Boolean;
    property PacketCompletion: IPacketCompletion read GetPacketCompletion write SetPacketCompletion;
    property Queue: ICommunicationQueue read GetQueue write SetQueue;
    property ProtocolID: LongWord read GetProtocolID write SetProtocolID;
    property SessionID: LongWord read GetSessionID write SetSessionID;
    function IsWaiting: Boolean;
    property Name: String read GetName;
  end;

  IProtocolBase = interface (IFormatedProtocol)
    ['{E5043A14-3E45-11D4-9887-00104B0FA1EF}']
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket;
    procedure ReceiveStr(const Buffer: String);
    procedure Receive(Buffer: PChar; Size: LongWord);
    procedure ReceivePacket(const Packet: IProtocolPacket);
    function PacketBuild(const Packet: IProtocolPacket; out Buffer: String): Boolean; 
    procedure Send(const Packet: IProtocolPacket; const ExceptionMessage: String = '');
    procedure SendTo(const Connection: IAbstractConnection; const Packet: IProtocolPacket; const ExceptionMessage: String = '');
    function WaitReply(DataID: LongWord; Timeout: LongWord = INFINITE): IProtocolPacket;
    function WaitReplyFrom({const} Connection: IAbstractConnection; DataID: LongWord; Timeout: LongWord = INFINITE): IProtocolPacket;
    procedure PacketEncode(const Packet: IProtocolPacket);
    procedure PacketDecode(const Packet: IProtocolPacket);
    function GetPacket: IProtocolPacket;
    property Packet: IProtocolPacket read GetPacket;
  end;

  TErrorEvent = record
    Sender: IProtocolBase;
    Buffer: PChar;
    Size: LongWord;
  end;

  TUnknownPacketEvent = record
    Sender: IProtocolBase;
    Packet: IProtocolPacket;
  end;

  TQueuedPacketAction = (qaOk, qaCancel, qaDiscard);

  TQueuedPacketEvent = record
    Sender: IProtocolBase;
    Packet: IProtocolPacket;
    Action: TQueuedPacketAction;
  end;

  IFormatedProtocolEvents = interface
    ['{E5043A15-3E45-11D4-9887-00104B0FA1EF}']
    procedure OnError(var Event: TErrorEvent);
    procedure OnUnknownPacket(var Event: TUnknownPacketEvent);
    procedure OnQueuePacket(var Event: TQueuedPacketEvent);
    procedure OnSendQueuedPacket(var Event: TQueuedPacketEvent);
  end;

  RDebugWaitingPacket = record
    Sender: IProtocolBase;
    DataID: LongWord;
    Timeout: LongWord;
    Packet: IProtocolPacket;
    ExceptionStr: String;
  end;

  IFormatedProtocolDebugEvents = interface
    ['{0F67BCFC-1C77-11D5-98BA-00104B0FA1EF}']
    procedure OnDebugTimeout(var Event: RDebugWaitingPacket);
    procedure OnDebugUnassignedPacket(var Event: RDebugWaitingPacket);
    procedure OnDebugInvalidPacket(var Event: RDebugWaitingPacket);
    procedure OnDebugAsyncPacketID(var Event: RDebugWaitingPacket);
    procedure OnDebugRemoteException(var Event: RDebugWaitingPacket);
    procedure OnDebugConnectionLost(var Event: RDebugWaitingPacket);
  end;

implementation

end.
 