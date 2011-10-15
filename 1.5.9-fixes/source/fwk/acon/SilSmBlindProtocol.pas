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

unit SilSmBlindProtocol;

interface

uses
  SilBeTypes,
  SilBtInt,
  SilLiLock,
  SilSiBlindProtocol,
  SilSiProtocolPacket,
  SilSiAbstractConnection,
  SilSmAbstractConnection,
  SilSmProtocolBase;

const
  PS_BASE         = $600;
  PS_TEXT         = PS_BASE;
  PS_PROTOCOLID_Q = PS_TEXT         + 1;
  PS_PROTOCOLID_A = PS_PROTOCOLID_Q + 1;

type
  TBlindProtocol = class (
    // extends
    TProtocolBase,
    // implements
    IBlindProtocol)
  private
    FLock: ILockable;
    FFilter: TIntegerArray;
    FInternalSeq: LongWord;
  private
    function DoFindFilter(ProtocolID: LongWord): Integer;
    procedure FireText(var Msg: TProtocolBaseMessage); message PS_TEXT;
    procedure FireProtocolID(var Msg: TProtocolBaseMessage); message PS_PROTOCOLID_Q;
  protected // IProtocolBase
    function GetName: String; override;
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket; override;
    procedure SetConnection(const Value: IAbstractConnection); override;
    function DiscardPacket(const Header: TProtocolHeader): Boolean; override;
  protected // IBlindProtocol
    procedure AddFilter(ProtocolID: Cardinal);
    procedure RemoveFilter(ProtocolID: Cardinal);
    procedure Text(const Buffer: String);
    function Request(const Obj: TGuid; out Id: LongWord; Timeout: LongWord): Boolean;
    function Supports(const Obj: TGuid; Timeout: LongWord): Boolean;
  public
    constructor Create(const Connection: IAbstractConnection = nil);
    destructor Destroy; override;
  end;

implementation

uses
  Sil;

{ TBlindProtocol }

constructor TBlindProtocol.Create(const Connection: IAbstractConnection);
begin
  inherited Create(0);
  FLock := Sil.OS.Ipc.CriticalSection;
  FInternalSeq := 0;
  SetConnection(Connection);
end;

destructor TBlindProtocol.Destroy;
begin
  inherited;
end;

function TBlindProtocol.CreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := inherited CreatePacket(DataID);

  Result.ProtoVer := 1;
  Result.HeaderVer := 1;
  Result.SessionID := 0;
end;

procedure TBlindProtocol.SetConnection(const Value: IAbstractConnection);
begin
  if FConnection <> nil then Sil.Sink.Disconnect(FConnection, Self);
  if Value <> nil then Sil.Sink.Connect(Value, Self);
  inherited SetConnection(Value);
end;

function TBlindProtocol.DoFindFilter(ProtocolID: LongWord): Integer;
begin
  try
    FLock.Lock;

    for Result := 0 to Length(FFilter) - 1 do
      if FFilter[Result] = Integer(ProtocolID) then
        Exit;

    Result := -1;
  finally
    FLock.Unlock;
  end;
end;

function TBlindProtocol.DiscardPacket(const Header: TProtocolHeader): Boolean;
begin
  try
    FLock.Lock;
    Result := (Length(FFilter) > 0) and (DoFindFilter(Header.ProtoID) > -1);
  finally
    FLock.Unlock;
  end;
end;

procedure TBlindProtocol.AddFilter(ProtocolID: Cardinal);
begin
  try
    FLock.Lock;
    if DoFindFilter(ProtocolID) = -1 then Int.ArrayAdd(FFilter, ProtocolID);
  finally
    FLock.Unlock;
  end;
end;

procedure TBlindProtocol.RemoveFilter(ProtocolID: Cardinal);
var
  i: Integer;
begin
  try
    FLock.Lock;
    i := DoFindFilter(ProtocolID);
    if i > -1 then Int.ArrayDelete(FFilter, i);
  finally
    FLock.Unlock;
  end;
end;

procedure TBlindProtocol.Text(const Buffer: String);
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_TEXT);
  Packet.Data.WriteString(Buffer);
  Send(Packet, 'TBlindProtocol.Text');
end;

procedure TBlindProtocol.FireText(var Msg: TProtocolBaseMessage);
var
	n: IEnumerator;
	Adapter: IBlindProtocolEvents;
  Event: RBlindTextEvent;
begin
  if not HasConnections or (Msg.Packet.ProtoID <> FProtocol) then Exit;

  Event.Sender := Self;
  Event.Buffer := Msg.Packet.Data.ReadString;

  while Events.Enumerate(n, Adapter, IBlindProtocolEvents) do Adapter.OnText(Event);
end;

function TBlindProtocol.Supports(const Obj: TGuid; Timeout: LongWord): Boolean;
var
  Dummy: LongWord;
begin
  Result := Request(Obj, Dummy, Timeout);
end;

function TBlindProtocol.Request(const Obj: TGuid; out Id: LongWord; Timeout: LongWord): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_PROTOCOLID_Q);
  Packet.Data.WriteGuid(Obj);
  Send(Packet, 'TBlindProtocol.ProtocolID');
  Packet := WaitReply(PS_PROTOCOLID_A, Timeout);
  Id := Packet.Data.ReadLongWord;
  Result := Id > 0;
end;

procedure TBlindProtocol.FireProtocolID(var Msg: TProtocolBaseMessage);
var
	n: IEnumerator;
	Adapter: IBlindProtocolEvents;
  Event: RBlindProtocolIDEvent;
  Packet: IProtocolPacket;
begin
  if not HasConnections or (Msg.Packet.ProtoID <> FProtocol) then Exit;

  Inc(FInternalSeq);

  Event.Sender := Self;
  Event.Obj := Msg.Packet.Data.ReadGuid;
  Event.Connection := FConnection;
  Event.Id := FInternalSeq;

  if FInternalSeq >= LongWord(-1) then FInternalSeq := 0;

  while Events.Enumerate(n, Adapter, IBlindProtocolEvents) do Adapter.OnRequest(Event);

  Packet := CreatePacket(PS_PROTOCOLID_A);
  Packet.Data.WriteLongWord(Event.Id);
  Send(Packet, 'TBlindProtocol.FireProtocolID');
end;

function TBlindProtocol.GetName: String;
begin
  Result := 'Blind';
end;

end.
