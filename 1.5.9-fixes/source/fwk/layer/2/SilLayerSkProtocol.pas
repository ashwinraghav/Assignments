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

unit SilLayerSkProtocol;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  SilLayerSiGlobal,
  SilLayerSkLayer;

type
  TSilLayerProtocol = class (TSilLayer)
  private
    factive: boolean;
    fwaits: IVectorInterface;
    procedure DoCancel;
  protected
    function DoCreateWait(const buffer: IMemoryStream; timeout: longword = 0): ILayerWait;
    procedure DoWaitPacket(const wait: ILayerWait; out reply: ILayerToken);
  protected
    function DoProtocolMatch(const packet: ILayerToken): boolean; virtual;
    function DoPacketMatch(const packet: ILayerToken; const wait: ILayerWait): boolean; virtual;
    procedure DoDispatchPacket(const packet: ILayerToken); virtual; abstract;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

type
  TPacketWait = class (TSilObject, ILayerWait)
  private
    fbuffer: IMemoryStream;
    fevent: IEvent;
    ftoken: ILayerToken;
    ftimeout: longword;
  protected
    procedure settoken(const token: ILayerToken);
    function gettoken: ILayerToken;
    function event: IEvent;
    function buffer: IMemoryStream;
    function timeout: longword;
  public
    constructor Create(const buffer: IMemoryStream; timeout: longword);
    destructor Destroy; override;
  end;

{ TSilLayerProtocol }

constructor TSilLayerProtocol.Create;
begin
  inherited;
  fwaits := SilVector.InterfaceList(true);
end;

destructor TSilLayerProtocol.Destroy;
begin
  fwaits := nil;
  inherited;
end;

procedure TSilLayerProtocol.DoActivate(const Context: IInterface);
begin
  factive := true;
end;

procedure TSilLayerProtocol.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  DoCancel;
  factive := false;
end;

function TSilLayerProtocol.DoGetIsActive: Boolean;
begin
  Result := factive;
end;

function TSilLayerProtocol.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
var
  enum: IVectorInterfaceEnumerator;
  wait: ILayerWait;
  waits: IVectorInterface;
  found: boolean;
begin
  waits := fwaits;

  if DoProtocolMatch(packet) then
  begin
    found := false;
    enum := waits.Enumerator;

    while enum.enumerate(wait) do
      if DoPacketMatch(packet, wait) then
      begin
        waits.delete(enum.index);
        wait.settoken(packet);
        wait.event.signal;
        found := true;
        break;
      end;

    if not found then
      DoDispatchPacket(packet);
  end;

  Result := false;
end;

function TSilLayerProtocol.DoProtocolMatch(const packet: ILayerToken): boolean;
begin
  Result := true;
end;

function TSilLayerProtocol.DoPacketMatch(const packet: ILayerToken; const wait: ILayerWait): boolean;
begin
  Result := true;
end;

function TSilLayerProtocol.DoCreateWait(const buffer: IMemoryStream; timeout: longword): ILayerWait;
begin
  Result := TPacketWait.Create(buffer, timeout);
  fwaits.add(result);
end;

procedure TSilLayerProtocol.DoCancel;
var
  enum: IVectorInterfaceEnumerator;
  wait: ILayerWait;
  waits: IVectorInterface;
begin
  waits := fwaits;
  
  if waits <> nil then
  begin
    enum := waits.Enumerator;

    while enum.enumerate(wait) do
      wait.event.signal;

    waits.Clear;
  end;
end;

procedure TSilLayerProtocol.DoWaitPacket(const wait: ILayerWait; out reply: ILayerToken);
begin
  try
    if wait.event.WaitFor(wait.timeout) = wrSignaled then
    begin
      reply := wait.gettoken;
    end else
    begin
      fwaits.remove(wait);
      raise Exception.Create('protocol timeout');
    end;
  except
    // log
    raise;
  end;
end;

{ TPacketWait }

constructor TPacketWait.Create(const buffer: IMemoryStream; timeout: longword);
begin
  inherited Create;
  fbuffer := buffer;
  fevent := Sil.OS.Ipc.Event;
  ftimeout := Int.IfEmpty(timeout, 60000);
end;

destructor TPacketWait.Destroy;
begin
  fbuffer := nil;
  fevent := nil;
  ftoken := nil;
  inherited;
end;

function TPacketWait.buffer: IMemoryStream;
begin
  Result := fbuffer;
end;

procedure TPacketWait.settoken(const token: ILayerToken);
begin
  ftoken := token;
end;

function TPacketWait.event: IEvent;
begin
  Result := fevent;
end;

function TPacketWait.gettoken: ILayerToken;
begin
  Result := ftoken;
end;

function TPacketWait.timeout: longword;
begin
  Result := ftimeout;
end;

end.
