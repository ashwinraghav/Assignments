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

unit SilLayerSmProtocolText;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSkLayer,
  SilLayerSmProtocolImate,
  SilLayerSiProtocolText;

type
  TSilLayerProtocolText = class (TSilLayerProtocolImate, ITextProtocol)
  private
    fhook: ITextProtocolHook;
  private
    procedure DoFireReceive(var Msg: RImateMessage); message PS_TEXT;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
  protected // ITextProtocol
    function send(const buffer: string): boolean;
    procedure sendreply(value: boolean);
  public
    constructor Create(const hook: ITextProtocolHook); reintroduce;
    destructor destroy; override;
  end;

implementation

{ TSilLayerProtocolText }

constructor TSilLayerProtocolText.Create(const hook: ITextProtocolHook);
begin
  inherited Create(1);
  fhook := hook;
end;

destructor TSilLayerProtocolText.destroy;
begin
  //writeln('TSilLayerProtocolText.destroy');
  inherited;
end;

procedure TSilLayerProtocolText.DoActivate(const Context: IUnknown);
begin
end;

procedure TSilLayerProtocolText.DoDeactivate(const Context: IUnknown; Manual: Boolean);
begin
  fhook := nil;
end;

function TSilLayerProtocolText.send(const buffer: string): boolean;
var
  token: ILayerToken;
begin
  token := CreatePacket(PS_TEXT);
  token.writer.WriteString(buffer);
  SendPacket(token);
  Result := true;
end;

procedure TSilLayerProtocolText.DoFireReceive(var Msg: RImateMessage);
var
  buffer: string;
begin
  if fhook <> nil then
  begin
    buffer := Msg.reader.ReadString;
    FHook.receive(buffer);
  end;
end;

procedure TSilLayerProtocolText.sendreply(value: boolean);
var
  token: ILayerToken;
begin
  token := CreatePacket(PS_TEXT_REPLY);
  token.writer.WriteBoolean(value);
  SendPacket(token);
end;

end.
