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

unit SilSmEblisProtocol;

interface

uses
  SilBeTypes,
  SilLiEnumerator,
  SilSmProtocolBase,
  SilSiProtocolPacket,
  SilSiAbstractConnection,
  SilSiEblisProtocol;

const
  PI_EBLIS            = 15;
  PS_BASE             = $400;
  PS_NEGOTIATE_Q      = PS_BASE             + 1;
  PS_NEGOTIATE_A      = PS_NEGOTIATE_Q      + 1;

type
  TEblisProtocol = class (
    // extends
    TProtocolBase,
    // implemets
    IUnknown,
    IEblisProtocol)
  private
    FTimeout: LongWord;
  private
    function DoListMod(const List1, List2: TIntegerArray): TIntegerArray;
    function DoListOp(const List1, List2: TIntegerArray): TIntegerArray;
    function DoListToStr(const List: TIntegerArray): String;
    function DoRndList(Size: LongWord): TIntegerArray;
    function DoStrToList(const List: String): TIntegerArray;
  private // IEblisProtocolEvents
    function GetTimeout: LongWord;
    procedure SetTimeout(Value: LongWord);
    procedure FireNegotiate(var Msg: TProtocolBaseMessage); message PS_NEGOTIATE_Q;
  protected // IProtocolBase
    function GetName: String; override;
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket; override;
    procedure SetConnection(const Value: IAbstractConnection); override;
  protected // IEblisProtocol
    function Negotiate(out Key: String; KeyLength: LongWord): Boolean;
  public
    constructor Create(ID: LongWord; const Connection: IAbstractConnection);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilLtConnection;

{ TEblisProtocol }

constructor TEblisProtocol.Create(ID: LongWord; const Connection: IAbstractConnection);
begin
  inherited Create(ID);

  FTimeout := 2000;
  SetConnection(Connection);
  Randomize;
end;

destructor TEblisProtocol.Destroy;
begin
  inherited;
end;

// IProtocolBase

function TEblisProtocol.CreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := inherited CreatePacket(DataID);

  Result.ProtoVer := 1;
  Result.HeaderVer := 1;
  Result.SessionID := 0;
end;

procedure TEblisProtocol.SetConnection(const Value: IAbstractConnection);
begin
  if FConnection <> nil then Sink.Disconnect(FConnection, Self);
  if Value <> nil then Sink.Connect(Value, Self);
  inherited SetConnection(Value);
end;

// helper fns

function TEblisProtocol.DoRndList(Size: LongWord): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Size);
  for i := 0 to Size - 1 do Result[i] := Random(250) + 5;
end;

function TEblisProtocol.DoListToStr(const List: TIntegerArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(List) - 1 do Result := Result + Char(List[i]);
end;

function TEblisProtocol.DoListMod(const List1, List2: TIntegerArray): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List1));
  for i := 0 to Length(List1) - 1 do Result[i] := List1[i] - Trunc(List1[i] / List2[i]) * List2[i];
end;

function TEblisProtocol.DoListOp(const List1, List2: TIntegerArray): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List1));
  for i := 0 to Length(List1) - 1 do Result[i] := List1[i] * List2[i];
end;

function TEblisProtocol.DoStrToList(const List: String): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List));
  for i := 0 to Length(Result) - 1 do Result[i] := Byte(Str.Copy(List, i + 1, 1)[1]);
end;

// IEblisProtocol

function TEblisProtocol.Negotiate(out Key: String; KeyLength: LongWord): Boolean;
var
  Packet: IProtocolPacket;
  cli1, cli2, svr1, tmpcli, tmpsvr: TIntegerArray;
begin
  Result := false;

  cli1 := DoRndList(KeyLength);
  cli2 := DoRndList(KeyLength);
  svr1 := nil;
  tmpsvr := nil;
  tmpcli := nil;

  Packet := CreatePacket(PS_NEGOTIATE_Q);
  Packet.Data.WriteString(DoListToStr(cli1));
  Send(Packet, 'TEblisProtocol.Negotiate.1');
  Packet := WaitReply(PS_NEGOTIATE_A, FTimeout);

  if Packet <> nil then
  begin
    svr1 := DoStrToList(Packet.Data.ReadString);
    tmpsvr := DoStrToList(Packet.Data.ReadString);
    tmpcli := DoListMod(DoListOp(cli1, cli2), svr1);

    Packet := CreatePacket(PS_NEGOTIATE_A);
    Packet.Data.WriteString(DoListToStr(tmpcli));
    Send(Packet, 'TEblisProtocol.Negotiate.2');

    Key := DoListToStr(DoListMod(DoListOp(tmpsvr, cli2), svr1));
    Result := true;
  end;
end;

procedure TEblisProtocol.FireNegotiate(var Msg: TProtocolBaseMessage);
var
	n: IEnumerator;
	Adapter: IEblisProtocolEvents;
  Event: REblisNegotiateEvent;
  Packet: IProtocolPacket;
  cli1, svr1, svr2, tmpcli, tmpsvr: TIntegerArray;
begin
  cli1 := nil;
  svr1 := nil;
  svr2 := nil;
  tmpsvr := nil;
  tmpcli := nil;

  if not HasConnections then Exit;

  cli1 := DoStrToList(Msg.Packet.Data.ReadString);
  svr1 := DoRndList(Length(cli1));
  svr2 := DoRndList(Length(cli1));
  tmpsvr := DoListMod(DoListOp(cli1, svr2), svr1);

  Packet := CreatePacket(PS_NEGOTIATE_A);
  Packet.Data.WriteString(DoListToStr(svr1));
  Packet.Data.WriteString(DoListToStr(tmpsvr));
  Send(Packet, 'TEblisProtocol.FireNegotiate.2');
  Packet := WaitReply(PS_NEGOTIATE_A, FTimeout);

  if Packet <> nil then
  begin
    tmpcli := DoStrToList(Packet.Data.ReadString);
    Event.Key := DoListToStr(DoListMod(DoListOp(tmpcli, svr2), svr1));
    Event.Sender := Self;

    while Events.Enumerate(n, Adapter, IEblisProtocolEvents) do
      Adapter.OnNegotiate(Event);
  end;
end;

function TEblisProtocol.GetName: String;
begin
  Result := 'Eblis';
end;

function TEblisProtocol.GetTimeout: LongWord;
begin
  Result := FTimeout;
end;

procedure TEblisProtocol.SetTimeout(Value: LongWord);
begin
  FTimeout := Value;
end;

end.
