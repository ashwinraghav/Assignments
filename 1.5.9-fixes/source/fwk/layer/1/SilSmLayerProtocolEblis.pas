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

unit SilSmLayerProtocolEblis;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSiLayerProtocolEblis,
  SilSeLayerProtocol,
  SilSmLayerProtocolCustomImate;

const
  PS_BASE             = $600;
  PS_NEGOTIATE        = Succ(PS_BASE);
  PS_NEGOTIATE_REPLY  = Succ(PS_NEGOTIATE);
  PS_NEGOTIATE_FINISH = Succ(PS_NEGOTIATE_REPLY);

type
  TSilEblisProtocol = class (
    TSilCustomImateProtocol,
    IEblisProtocol)
  private
    FHook: IEblisProtocolHook;
    FInProcess: Boolean;
    FSvrCalc1: TIntegerArray;
    FSvrCalc2: TIntegerArray;
  private
    function DoListMod(const List1, List2: TIntegerArray): TIntegerArray;
    function DoListOp(const List1, List2: TIntegerArray): TIntegerArray;
    function DoListToStr(const List: TIntegerArray): String;
    function DoRndList(Size: LongWord): TIntegerArray;
    function DoStrToList(const List: String): TIntegerArray;
  private
    procedure DoFireNegotiate(var Msg: RImateProtocolMessage); message PS_NEGOTIATE;
    procedure DoFireNegotiateFinish(var Msg: RImateProtocolMessage); message PS_NEGOTIATE_FINISH;
  protected
    procedure Initialize(const Hook: IUnknown); override;
    procedure Finalize; override;
  protected // IEblisProtocol
    function Negotiate(out Key: String; KeyLength: LongWord; Timeout: LongWord = INFINITE; const Context: IUnknown = nil): Boolean;
  public
    constructor Create(const Parameters: IParameters);
  end;

implementation

{ TSilEblisProtocol }

constructor TSilEblisProtocol.Create(const Parameters: IParameters);
begin
  inherited Create(Parameters);
  if RandSeed = 0 then Randomize;
end;

procedure TSilEblisProtocol.Initialize(const Hook: IUnknown);
begin
  Ref.GetInterface(Hook, IEblisProtocolHook, FHook);
end;

procedure TSilEblisProtocol.Finalize;
begin
  FHook := nil;
end;

function TSilEblisProtocol.DoRndList(Size: LongWord): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Size);
  for i := 0 to Size - 1 do Result[i] := Random(250) + 5;
end;

function TSilEblisProtocol.DoListToStr(const List: TIntegerArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(List) - 1 do Result := Result + Char(List[i]);
end;

function TSilEblisProtocol.DoListMod(const List1, List2: TIntegerArray): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List1));
  for i := 0 to Length(List1) - 1 do Result[i] := List1[i] - Trunc(List1[i] / List2[i]) * List2[i];
end;

function TSilEblisProtocol.DoListOp(const List1, List2: TIntegerArray): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List1));
  for i := 0 to Length(List1) - 1 do Result[i] := List1[i] * List2[i];
end;

function TSilEblisProtocol.DoStrToList(const List: String): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(List));
  for i := 0 to Length(Result) - 1 do Result[i] := Byte(Str.Copy(List, i + 1, 1)[1]);
end;

function TSilEblisProtocol.Negotiate(out Key: String; KeyLength: LongWord; Timeout: LongWord; const Context: IUnknown): Boolean;
var
  Packet, Reply: IPacket;
  cli1, cli2, SvrCalc1, tmpcli, tmpsvr: TIntegerArray;
begin
  Result := false;

  cli1 := DoRndList(KeyLength);
  cli2 := DoRndList(KeyLength);
  SvrCalc1 := nil;
  tmpsvr := nil;
  tmpcli := nil;

  Packet := Protocol.CreatePacket(PS_NEGOTIATE);
  Packet.Writer.WriteString(DoListToStr(cli1));
  Protocol.SendPacket(Packet, Reply, PS_NEGOTIATE_REPLY, Timeout, Context);

  if Reply <> nil then
  begin
    SvrCalc1 := DoStrToList(Reply.Reader.ReadString);
    tmpsvr := DoStrToList(Reply.Reader.ReadString);
    tmpcli := DoListMod(DoListOp(cli1, cli2), SvrCalc1);

    Packet := Protocol.CreatePacket(PS_NEGOTIATE_FINISH);
    Packet.Writer.WriteString(DoListToStr(tmpcli));
    Protocol.SendPacket(Packet, Context);

    Key := DoListToStr(DoListMod(DoListOp(tmpsvr, cli2), SvrCalc1));
    Result := true;
  end;
end;

procedure TSilEblisProtocol.DoFireNegotiate(var Msg: RImateProtocolMessage);
var
  Packet: IPacket;
  cli1, tmpsvr: TIntegerArray;
begin
  cli1 := nil;
  FSvrCalc1 := nil;
  FSvrCalc2 := nil;
  tmpsvr := nil;
  FInProcess := true;

  cli1 := DoStrToList(Msg.Packet.Reader.ReadString);
  FSvrCalc1 := DoRndList(Length(cli1));

  if Int.ArrayCompare(cli1, FSvrCalc1) = 0 then
  begin
    Randomize;
    FSvrCalc1 := DoRndList(Length(cli1));
  end;

  FSvrCalc2 := DoRndList(Length(cli1));
  tmpsvr := DoListMod(DoListOp(cli1, FSvrCalc2), FSvrCalc1);

  Packet := Protocol.CreatePacket(PS_NEGOTIATE_REPLY);
  Packet.Writer.WriteString(DoListToStr(FSvrCalc1));
  Packet.Writer.WriteString(DoListToStr(tmpsvr));
  Protocol.SendPacket(Packet, Msg.Context);
end;

procedure TSilEblisProtocol.DoFireNegotiateFinish(var Msg: RImateProtocolMessage);
var
  tmpcli: TIntegerArray;
  Event: REblisNegotiateEvent;
begin
  tmpcli := nil;
  if not Assigned(FHook) or not FInProcess then Exit;

  FInProcess := false;
  tmpcli := DoStrToList(Msg.Packet.Reader.ReadString);
  Event.Key := DoListToStr(DoListMod(DoListOp(tmpcli, FSvrCalc2), FSvrCalc1));
  Event.Sender := Self;

  FHook.OnNegotiate(Event);
end;

end.
