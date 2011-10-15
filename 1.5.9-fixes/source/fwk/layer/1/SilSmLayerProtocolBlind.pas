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

unit SilSmLayerProtocolBlind;

interface

{$include Defines.inc}

uses
  Sil,
  SilSeLayerProtocol,
  SilSiLayer,
  SilSiLayerProtocolBlind,
  SilSmLayerProtocolCustomImate;

const
  PS_BASE             = $600;
  PS_TEXT             = PS_BASE;
  PS_REQUEST          = PS_TEXT             + 1;
  PS_REQUEST_REPLY    = PS_REQUEST          + 1;

type
  TSilBlindProtocol = class (
    // extends
    TSilCustomImateProtocol,
    // implements
    IBlindProtocol)
  private
    FHook: IBlindProtocolHook;
    FSequence: LongWord;
  protected
    procedure Initialize(const Hook: IUnknown); override;
    procedure Finalize; override; 
  private
    procedure DoFireText(var Msg: RImateProtocolMessage); message PS_TEXT;
    procedure DoFireRequest(var Msg: RImateProtocolMessage); message PS_REQUEST;
  protected // IBlindProtocol
    procedure SendText(const Buffer: String; const Context: IUnknown = nil);
    function Request(const Protocol: TGuid; out Id: LongWord; Timeout: LongWord; const Context: IUnknown): Boolean;
  end;

implementation

{ TSilBlindProtocol }

procedure TSilBlindProtocol.Initialize(const Hook: IUnknown);
begin
  Ref.GetInterface(Hook, IBlindProtocolHook, FHook);
end;

procedure TSilBlindProtocol.Finalize;
begin
  FHook := nil;
end;

procedure TSilBlindProtocol.SendText(const Buffer: String; const Context: IUnknown = nil);
var
  Packet: IPacket;
begin
  Packet := Protocol.CreatePacket(PS_TEXT);
  Packet.Writer.WriteString(Buffer);
  Protocol.SendPacket(Packet, Context);
end;

procedure TSilBlindProtocol.DoFireText(var Msg: RImateProtocolMessage);
var
  Event: RBlindTextEvent;
begin
  if not Assigned(FHook) then Exit;

  Event.Sender := Self;
  Event.Context := Msg.Context;
  Event.Buffer := Msg.Packet.Reader.ReadString;
  FHook.OnText(Event);
end;

function TSilBlindProtocol.Request(const Protocol: TGuid; out Id: LongWord; Timeout: LongWord; const Context: IUnknown): Boolean;
var
  Packet, Reply: IPacket;
begin
  Packet := Self.Protocol.CreatePacket(PS_REQUEST);
  Packet.Writer.WriteGuid(Protocol);
  Self.Protocol.SendPacket(Packet, Reply, PS_REQUEST_REPLY, Timeout, Context);
  Id := Reply.Reader.ReadLongWord;
  Result := Id <> 0;
end;

procedure TSilBlindProtocol.DoFireRequest(var Msg: RImateProtocolMessage);
var
  Event: RBlindRequestEvent;
  Packet: IPacket;
begin
  if not Assigned(FHook) then Exit;

  Inc(FSequence);
  Event.Sender := Self;
  Event.Context := Msg.Context;
  Event.Protocol := Msg.Packet.Reader.ReadGuid;
  Event.Id := FSequence;
  Event.Result := false;
  FHook.OnRequest(Event);

  Packet := Protocol.CreatePacket(PS_REQUEST_REPLY);

  if Event.Result then
    Packet.Writer.WriteLongWord(Event.Id) else
    Packet.Writer.WriteLongWord(0);

  Protocol.SendPacket(Packet, Event.Context);
end;

end.
