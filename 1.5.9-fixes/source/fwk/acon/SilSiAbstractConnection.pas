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

unit SilSiAbstractConnection;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLiStream,
  SilLiLock,
  SilOiWait,
  SilOiIpc,
  SilOiThread,
  SilSiPacketCompletion;

type
  IPacketCompletion = SilSiPacketCompletion.IPacketCompletion;

type
  IAbstractConnection = interface;

  TConnectionEvent = record
    Sender: IAbstractConnection;
  end;

  TConnectionFailedEvent = record
    Sender: IAbstractConnection;
    ErrorCode: Integer;
  end;

  TConnectionDataEvent = record
    Sender: IAbstractConnection;
    Buffer: PChar;
    Size: LongWord;
  end;

  TConnectionBreakEvent = record
    Sender: IAbstractConnection;
    ConnectionLost: Boolean;
  end;

  // IConnectionEvents
  IConnectingEvents = interface
    ['{18CE6052-C2C6-11D3-9867-00104B0FA1EF}']
    procedure OnConnected(const Event: TConnectionEvent);
    procedure OnFailed(const Event: TConnectionFailedEvent);
  end;

  // IConnectionEstablishedEvents
  IConnectedEvents = interface
    ['{269E7E91-478D-11D4-9889-00104B0FA1EF}']
    procedure OnDataReceived(const Event: TConnectionDataEvent);
    procedure OnDataSent(const Event: TConnectionDataEvent);
    procedure OnDisconnected(const Event: TConnectionBreakEvent);
  end;

  RConnectionRetryEvent = record
    Sender: IAbstractConnection;
    CancelRetry: Boolean;
  end;

  IConnectionTrialEvents = interface
    ['{3434C2B3-9481-11D4-989E-00104B0FA1EF}']
    procedure OnConnecting(var Event: TConnectionEvent);
    procedure OnRetryConnect(var Event: RConnectionRetryEvent);
  end;

  IAbstractConnection = interface
    ['{02CA37D1-F573-11D3-9138-00C0261013CD}']
    function Initialize: Boolean;
    function Connect(const ThreadName: string = ''): Boolean;
    procedure Cleanup;
    procedure Disconnect(Wait: Boolean = false);
    function GetConnected: Boolean;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    function ReadStr(var Buffer: String): LongWord;
    function WriteStr(const Buffer: String): LongWord;
    function GetPacketCompletion: IPacketCompletion;
    procedure SetPacketCompletion(const Value: IPacketCompletion);
    function GetBufferSize: LongWord;
    function GetReady: IEvent;
    function GetRetryInterval: LongWord;
    procedure SetRetryInterval(Value: LongWord);
    procedure CopyEventsOnDisconnect(Value: Boolean);
    function GetRetryCount: LongWord;
    procedure SetRetryCount(Value: LongWord);
    function Thread: IThread;
    procedure SpawnThread(ThreadName: string = '');
    function WaitDataInput: Boolean;
    property IsConnected: Boolean read GetConnected;
    property PacketCompletion: IPacketCompletion read GetPacketCompletion write SetPacketCompletion;
    property Ready: IEvent read GetReady;
    property BufferSize: LongWord read GetBufferSize;
    property RetryInterval: LongWord read GetRetryInterval write SetRetryInterval;
    property RetryCount: LongWord read GetRetryCount write SetRetryCount;
  end;

  IConnectionList = interface(IItemization)
    ['{19CDF561-1217-11D4-987D-00104B0FA1EF}']
    function GetThreadManager: IThreadManager;
    procedure SetThreadManager(const Value: IThreadManager);
    procedure DisconnectAll;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function First: IAbstractConnection;
    function Last: IAbstractConnection;
    function GetItem(Index: Integer): IAbstractConnection;
    function Add(const Item: IAbstractConnection): Integer;
    function IndexOf(const Item: IAbstractConnection): Integer;
    function Remove(const Item: IAbstractConnection): Integer;
    property Items[Index: Integer]: IAbstractConnection read GetItem; default;
    property ThreadManager: IThreadManager read GetThreadManager write SetThreadManager;
  end;

implementation

end.
