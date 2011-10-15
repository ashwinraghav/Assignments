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

unit SilSmProtocolBase;

{$I Defines.inc}

interface

uses
  SysUtils,

  SilLkInterfaced,
  SilLiConnection,
  SilLiEventList,
  SilSiProtocolBase,
  SilSmProtocolPacket,
  SilSiProtocolPacket,
  SilSiPacketBuilder,
  SilSiAbstractConnection,
  SilSiPacketCompletion,
  SilSiCommunicationQueue,
  SilOiIpc,
  SilLiLock,
  SilOsTypes;

type
  TProtocolBaseMessage = record
    Msg: Cardinal;
    Packet: IProtocolPacket;
    Unused1: Integer;
    Unused2: Integer;
  end;

  TProtocolEvent = record
    Sender: IProtocolBase;
    Packet: IPacketData;
  end;

  TErrorMessage = record
    Msg: Cardinal;
    Buffer: PChar;
    Size: LongWord;
    Unused: Integer;
  end;

  TProtocolBase = class (
    // extends
    TSilInterfacedObject,
    // implements
    IProtocol,
    IFormatedProtocol,
    IProtocolBase,
    IConnectedEvents)
  private
    procedure FireError(Buffer: PChar; Size: LongWord);
    procedure FireUnknownPacket(const Packet: IProtocolPacket);
    function FireQueuePacket(const Packet: IProtocolPacket): Boolean;
    function FireSendQueuedPacket(const Packet: IProtocolPacket): TQueuedPacketAction;
    procedure DoSendException(Header: PProtocolHeader; const Msg: String);
    procedure DoDisconnect(Complete: Boolean);
    procedure DoDebugAsyncPacketID(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoDebugConnectionLost(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoDebugInvalidPacket(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoDebugRemoteException(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoDebugTimeout(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoDebugUnassignedPacket(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
    procedure DoGetReply(DataID: LongWord; out Packet: IProtocolPacket);
    procedure DoSetReply(const Packet: IProtocolPacket);
    procedure DoClearReply;
  protected
    FConnection: IAbstractConnection;
    FCompleter: IPacketCompletion;
    FPacket: IProtocolPacket;
    FWaitPacket: IProtocolPacket;
    FWaitDataID: LongWord;
    FReadEvent: IEvent;
    FLock: ILockable;
    FProtocol: LongWord;
    FSession: LongWord;
    FQueue: ICommunicationQueue;
    FWaiting: Boolean;
    FAvoidMsgLock: Boolean;
  protected
    procedure QueuePacket(const Packet: IProtocolPacket);
  protected // IProtocol
    function GetConnection: IAbstractConnection; virtual;
    procedure SetConnection(const Value: IAbstractConnection); virtual;
  protected // IFormatedProtocol
    function GetPacketCompletion: IPacketCompletion;
    procedure SetPacketCompletion(const Value: IPacketCompletion);
    function GetQueue: ICommunicationQueue;
    procedure SetQueue(const Value: ICommunicationQueue);
    function GetProtocolID: LongWord;
    procedure SetProtocolID(Value: LongWord);
    function GetSessionID: LongWord;
    procedure SetSessionID(Value: LongWord);
    function GetName: String; virtual; abstract;
    function FlushQueue: Boolean; virtual;
  protected // IProtocolBase
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket; virtual;
    procedure ReceiveStr(const Buffer: String);
    procedure Receive(Buffer: PChar; Size: LongWord);
    procedure ReceivePacket(const Packet: IProtocolPacket);
    procedure Send(const Packet: IProtocolPacket; const ExceptionMessage: String = '');
    procedure SendTo(const Connection: IAbstractConnection; const Packet: IProtocolPacket; const ExceptionMessage: String = '');
    function WaitReply(DataID: LongWord; Timeout: LongWord = INFINITE): IProtocolPacket;
    function WaitReplyFrom({const} Connection: IAbstractConnection; DataID: LongWord; Timeout: LongWord = INFINITE): IProtocolPacket;
    procedure PacketEncode(const Packet: IProtocolPacket); virtual;
    procedure PacketDecode(const Packet: IProtocolPacket); virtual;
    function PacketBuild(const Packet: IProtocolPacket; out Buffer: String): Boolean; virtual;
    function DiscardPacket(const Header: TProtocolHeader): Boolean; virtual;
    function GetPacket: IProtocolPacket;
    function IsWaiting: Boolean;
  protected // IConnectable
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent); virtual;
    procedure OnDataSent(const Event: TConnectionDataEvent); virtual;
    procedure OnDisconnected(const Event: TConnectionBreakEvent); virtual;
  public
    constructor Create(Protocol: LongWord; Session: LongWord = 0);
    destructor Destroy; override;
    class function DefaultPort: LongWord; virtual;
    procedure DefaultHandler(var Message); override;
  end;

  EProtocolError = class(Exception)
  private
    FPacket: IProtocolPacket;
  public
    constructor Create(const Msg: String; const Packet: IProtocolPacket);
    property Packet: IProtocolPacket read FPacket;
  end;

implementation

uses
  SilOeWait,
  SilOtTool,
  SilLtConnection,
  SilLiEnumerator,
  SilBtError;

{ TProtocolBase }

constructor TProtocolBase.Create(Protocol: LongWord; Session: LongWord);
begin
  inherited Create;
  FAvoidMsgLock := true;
  FProtocol := Protocol;
  FSession := Session;
  FReadEvent := OS.IPC.Event();
  FLock := OS.IPC.CriticalSection();
  FWaitDataID := 0;
end;

destructor TProtocolBase.Destroy;
begin
  FPacket := nil;
  FReadEvent := nil;
  FLock := nil;
  inherited;
end;

procedure TProtocolBase.RemoveListener(const Listener: IUnknown);
var
  bOk: Boolean;
begin
  bOk := Events <> nil;
  inherited;
  if bOk and ((Events = nil) or (Events.Count = 0)) then DoDisconnect(false);
end;

procedure TProtocolBase.FireError(Buffer: PChar; Size: LongWord);
var
  n: IEnumerator;
  i: IFormatedProtocolEvents;
  Event: TErrorEvent;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.Buffer := Buffer;
  Event.Size := Size;

  with Events do
    while Enumerate(n, i, IFormatedProtocolEvents) do
      i.OnError(Event);
end;

procedure TProtocolBase.FireUnknownPacket(const Packet: IProtocolPacket);
var
  n: IEnumerator;
  i: IFormatedProtocolEvents;
  Event: TUnknownPacketEvent;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.Packet := Packet;

  with Events do
    while Enumerate(n, i, IFormatedProtocolEvents) do
      i.OnUnknownPacket(Event);
end;

function TProtocolBase.FireQueuePacket(const Packet: IProtocolPacket): Boolean;
var
  n: IEnumerator;
  i: IFormatedProtocolEvents;
  Event: TQueuedPacketEvent;
begin
  Result := true;
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.Packet := Packet;
  Event.Action := qaOk;

  with Events do
    while Enumerate(n, i, IFormatedProtocolEvents) do
      i.OnQueuePacket(Event);

  Result := Event.Action = qaOk;
end;

function TProtocolBase.FireSendQueuedPacket(const Packet: IProtocolPacket): TQueuedPacketAction;
var
  n: IEnumerator;
  i: IFormatedProtocolEvents;
  Event: TQueuedPacketEvent;
begin
  Result := qaOk;
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.Packet := Packet;
  Event.Action := Result;

  with Events do
    while Enumerate(n, i, IFormatedProtocolEvents) do
      i.OnSendQueuedPacket(Event);

  Result := Event.Action;
end;

function TProtocolBase.GetConnection: IAbstractConnection;
begin
  Result := FConnection;
end;

function TProtocolBase.FlushQueue: Boolean;
var
  Packet: IProtocolPacket;
begin
  Result := false;

  if (FQueue <> nil) and (FConnection <> nil) and FConnection.IsConnected then
    while FQueue.GetPacket(Packet) do
      try
        case FireSendQueuedPacket(Packet) of
          qaOk:     Send(Packet);
          qaCancel: Break;
        end;
        FQueue.RemovePacket(Packet);
        Result := true;
      except
        Break;
      end;
end;

procedure TProtocolBase.QueuePacket(const Packet: IProtocolPacket);
begin
  if (FQueue <> nil) and FireQueuePacket(Packet) then
  begin
    Packet.Flags := Packet.Flags + [pfDelayedWrite];
    FQueue.AppendPacket(Packet);
  end;
end;

procedure TProtocolBase.SetConnection(const Value: IAbstractConnection);
begin
  if Assigned(Value) then
  begin
    FConnection := Value;
    if Assigned(FConnection) and (FConnection.PacketCompletion = nil) then
      FConnection.PacketCompletion := TProtocolPacketCompleter.Create;
  end else
  begin
    if Assigned(FConnection) and Assigned(FConnection.PacketCompletion) then
      FConnection.PacketCompletion := nil; 
    FConnection := nil;
  end;
end;

procedure TProtocolBase.ReceiveStr(const Buffer: String);
begin
  Receive(PChar(Buffer), Length(Buffer));
end;

procedure TProtocolBase.DoSendException(Header: PProtocolHeader; const Msg: String);
var
  Packet: IProtocolPacket;
begin
  Header.DataSize := 0;
  Packet := TProtocolPacket.Create(PChar(Header), SizeOf(TProtocolHeader));
  Packet.Data.WriteString(Msg);
  Packet.Flags := [pfException];
  Send(Packet);
end;

function TProtocolBase.DiscardPacket(const Header: TProtocolHeader): Boolean;
begin
  Result := (Header.ProtoID <> FProtocol) or (Header.SessionID <> FSession);
end;

procedure TProtocolBase.Receive(Buffer: PChar; Size: LongWord);
var
  Packet: IProtocolPacket;
begin
  try
    FLock.Lock;
    try
      if (Size = 0) or (Buffer = nil) then
      begin
        FPacket := nil;
        FWaitPacket := nil;
        FReadEvent.Signal;
        Exit;
      end else
      if (Size < SizeOf(TProtocolHeader)) or DiscardPacket(PProtocolHeader(Buffer)^) then
      begin
        FPacket := nil;
        Exit;
      end;

      Packet := TProtocolPacket.Create(Buffer, Size);
      ReceivePacket(Packet);
    except
      // fire
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TProtocolBase.ReceivePacket(const Packet: IProtocolPacket);
var
  Data: TProtocolBaseMessage;
begin
  FPacket := Packet;

  if FPacket.IsValid then
  begin
    if not (pfException in FPacket.Flags) then
    begin
      Data.Msg := FPacket.DataID;
      Data.Packet := FPacket;

      try
        PacketDecode(FPacket);
        DoSetReply(FPacket);
        Dispatch(Data);

        if (FPacket.ProtoID = FProtocol) and (FPacket.SessionID = FSession) then
          FReadEvent.Signal;
      except
        on e: Exception do
        begin
          DefaultHandler(Data);
          if (FPacket <> nil) and not (pfException in FPacket.Flags) then
            DoSendException(PProtocolHeader(FPacket.GetBufferPtr), e.Message);
        end;
      end;
    end else
    begin
      DoSetReply(FPacket);

      if (FPacket.ProtoID = FProtocol) and (FPacket.SessionID = FSession) then
        FReadEvent.Signal;
    end;
  end else
    FireError(FPacket.GetBufferPtr, FPacket.Size);
end;

function TProtocolBase.GetPacketCompletion: IPacketCompletion;
begin
  Result := FCompleter;
end;

procedure TProtocolBase.SetPacketCompletion(const Value: IPacketCompletion);
begin
  FCompleter := Value;
end;

procedure TProtocolBase.Send(const Packet: IProtocolPacket; const ExceptionMessage: String = '');
begin
  SendTo(FConnection, Packet, ExceptionMessage);
end;

function TProtocolBase.PacketBuild(const Packet: IProtocolPacket; out Buffer: String): Boolean;
begin
  Result := Packet.Build(Buffer);
end;

procedure TProtocolBase.SendTo(const Connection: IAbstractConnection; const Packet: IProtocolPacket; const ExceptionMessage: String = '');
var
  bOk: Boolean;
  sPacket: String;
  pBuf, pEnd: PChar;
  iMaxSize: Integer;
begin
  PacketEncode(Packet);

  bOk := PacketBuild(Packet, sPacket) and (Connection <> nil) and Connection.IsConnected;

  if bOk then
  begin
    iMaxSize := Connection.BufferSize;

    if Length(sPacket) > iMaxSize then
    begin
      pBuf := PChar(sPacket);
      pEnd := pBuf + Length(sPacket);

      while bOk and (pBuf < pEnd) do
      begin
        if pEnd - pBuf < iMaxSize then iMaxSize := pEnd - pBuf;
        bOk := Connection.Write(pBuf^, iMaxSize) > 0;
        Inc(pBuf, iMaxSize);
      end;
    end else
      bOk := Connection.Write(sPacket[1], Length(sPacket)) > 0;
  end;

  if not bOk then
  begin
    if FQueue <> nil then QueuePacket(Packet);
    raise EProtocolError.Create(ExceptionMessage, Packet);
  end;
end;

procedure TProtocolBase.PacketDecode(const Packet: IProtocolPacket);
begin
end;

procedure TProtocolBase.PacketEncode(const Packet: IProtocolPacket);
begin
end;

procedure TProtocolBase.DoSetReply(const Packet: IProtocolPacket);
begin
  try
    FLock.Lock;
    if (pfException in Packet.Flags) or (FWaitDataID = 0) or (Packet.DataID = FWaitDataID) then FWaitPacket := Packet;
  finally
    FLock.Unlock;
  end;
end;

procedure TProtocolBase.DoGetReply(DataID: LongWord; out Packet: IProtocolPacket);
begin
  try
    FLock.Lock;
    FWaitDataID := DataID;
    if FWaitPacket <> nil then
    begin
      if (pfException in FWaitPacket.Flags) or (FWaitPacket.DataID = DataID) then Packet := FWaitPacket;
      FWaitPacket := nil;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TProtocolBase.DoClearReply;
begin
  try
    FLock.Lock;
    FWaitDataID := 0;
    FWaitPacket := nil;
  finally
    FLock.Unlock;
  end;
end;

function TProtocolBase.WaitReply(DataID, Timeout: LongWord): IProtocolPacket;
begin
  Result := WaitReplyFrom(FConnection, DataID, Timeout);
end;

function TProtocolBase.WaitReplyFrom(Connection: IAbstractConnection; DataID, Timeout: LongWord): IProtocolPacket;
var
  sExceptionStr: String;
  WaitPacket: IProtocolPacket;
begin
  Result := nil;
  FWaiting := true;
  DoGetReply(DataID, Result);

  while Connection.IsConnected and (Result = nil) do
  begin
    if (Connection.Thread <> nil) and not Connection.Thread.IsCurrent then
    begin
      if FReadEvent.WaitFor(Timeout, FAvoidMsgLock) <> wrSignaled then
      begin
        DoDebugTimeout(DataID, Timeout, nil, '');
        Error.Throw('TProtocolBase.WaitReplyFrom:timeout(%s:%d)', [ClassName, DataID]);
      end;
    end else
      FConnection.WaitDataInput;

    try
      //FLock.Lock;
      FReadEvent.Reset;
      DoGetReply(DataID, WaitPacket);

      if (WaitPacket <> nil) and WaitPacket.IsValid and ((WaitPacket.DataID = DataID) or (pfException in WaitPacket.Flags)) then
      begin
        WaitPacket.Data.ResetRead;
        Result := WaitPacket;
        if pfException in WaitPacket.Flags then
        begin
          sExceptionStr := WaitPacket.Data.ReadString;
          DoDebugRemoteException(DataID, Timeout, WaitPacket, sExceptionStr);
          raise EProtocolError.Create(sExceptionStr, WaitPacket);
        end;
      end else
      begin
        if WaitPacket = nil then DoDebugUnassignedPacket(DataID, Timeout, nil, '') else
        if not WaitPacket.IsValid then DoDebugInvalidPacket(DataID, Timeout, WaitPacket, '') else
        if WaitPacket.DataID <> DataID then DoDebugAsyncPacketID(DataID, Timeout, WaitPacket, '');
      end;
    finally
      //FLock.Unlock;
    end;
  end;

  DoClearReply;
  FWaiting := false;

  if Result = nil then
  begin
    DoDebugConnectionLost(DataID, Timeout, nil, '');
    raise EProtocolError.CreateFmt('TProtocolBase.WaitReplyFrom(%s:%d)', [ClassName, DataID]);
  end;
end;

function TProtocolBase.IsWaiting: Boolean;
begin
  Result := FWaiting;
end;

function TProtocolBase.CreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := TProtocolPacket.Create;
  Result.ProtoID := FProtocol;
  Result.SessionID := FSession;
  Result.DataID := DataID;
end;

procedure TProtocolBase.DoDebugTimeout(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugTimeout(Event);
end;

procedure TProtocolBase.DoDebugUnassignedPacket(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugUnassignedPacket(Event);
end;

procedure TProtocolBase.DoDebugInvalidPacket(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugInvalidPacket(Event);
end;

procedure TProtocolBase.DoDebugAsyncPacketID(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugAsyncPacketID(Event);
end;

procedure TProtocolBase.DoDebugRemoteException(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugRemoteException(Event);
end;

procedure TProtocolBase.DoDebugConnectionLost(DataID, Timeout: LongWord; const Packet: IProtocolPacket; const ExceptionStr: String);
var
  n: IEnumerator;
  i: IFormatedProtocolDebugEvents;
  Event: RDebugWaitingPacket;
begin
  if Events = nil then Exit;
  Event.Sender := Self;
  Event.DataID := DataID;
  Event.Timeout := Timeout;
  Event.Packet := Packet;
  Event.ExceptionStr := ExceptionStr;

  with Events do
    while Enumerate(n, i, IFormatedProtocolDebugEvents) do
      i.OnDebugConnectionLost(Event);
end;

procedure TProtocolBase.DefaultHandler(var Message);
begin
  if Events <> nil then
    try
      FireUnknownPacket(FWaitPacket);
    except
      FireError(nil, 0);
    end;

  inherited;
end;

procedure TProtocolBase.OnDataReceived(const Event: TConnectionDataEvent);
begin
  Receive(Event.Buffer, Event.Size);
end;

procedure TProtocolBase.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  DoDisconnect(true);
end;

procedure TProtocolBase.DoDisconnect(Complete: Boolean);
begin
  FReadEvent.Signal;
  if FConnection <> nil then
  begin
    Sink.Disconnect(FConnection, Self);
    if Complete then FConnection := nil;
  end;
end;

class function TProtocolBase.DefaultPort: LongWord;
begin
  Result := 0;
end;

function TProtocolBase.GetQueue: ICommunicationQueue;
begin
  Result := FQueue;
end;

procedure TProtocolBase.SetQueue(const Value: ICommunicationQueue);
begin
  FQueue := Value;
end;

procedure TProtocolBase.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

function TProtocolBase.GetProtocolID: LongWord;
begin
  Result := FProtocol;
end;

function TProtocolBase.GetSessionID: LongWord;
begin
  Result := FSession;
end;

procedure TProtocolBase.SetProtocolID(Value: LongWord);
begin
  FProtocol := Value;
end;

procedure TProtocolBase.SetSessionID(Value: LongWord);
begin
  FSession := Value;
end;

function TProtocolBase.GetPacket: IProtocolPacket;
begin
  Result := FPacket;
end;

{ EProtocolError }

constructor EProtocolError.Create(const Msg: String; const Packet: IProtocolPacket);
begin
  inherited Create(Msg);
  FPacket := Packet;
end;

end.
