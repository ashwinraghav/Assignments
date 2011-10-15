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

unit SilLayerProtocolSkProtocol;

interface

{$include Sil.inc}

uses
  Sil,
  Sil.Vector,
  UiDef,
  UmLink;

type
  TStreamProtocol = class (TStreamLink)
  private
    factive: boolean;
    fwaits: IVectorInterface;
    procedure DoCancel;
  protected
    function DoCreateWait(const buffer: IMemoryStream; timeout: longword = 0): IStreamWait;
    procedure DoWaitPacket(const wait: IStreamWait; out reply: IStreamToken);
  protected
    function DoProtocolMatch(const packet: IStreamToken): boolean; virtual;
    function DoPacketMatch(const packet: IStreamToken; const wait: IStreamWait): boolean; virtual;
    procedure DoDispatchPacket(const packet: IStreamToken); virtual; abstract;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoRead(const Packet: IStreamToken; out Link: IStreamLink): Boolean; override;
    function DoGetIsActive: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses Sil.Vector.LiVector;

type
  TPacketWait = class (TSilObject, IStreamWait)
  private
    fbuffer: IMemoryStream;
    fevent: IEvent;
    ftoken: IStreamToken;
    ftimeout: longword;
  protected
    procedure settoken(const token: IStreamToken);
    function gettoken: IStreamToken;
    function event: IEvent;
    function buffer: IMemoryStream;
    function timeout: longword;
  public
    constructor Create(const buffer: IMemoryStream; timeout: longword);
    destructor Destroy; override;
  end;

{ TStreamProtocol }

constructor TStreamProtocol.Create;
begin
  inherited;
  fwaits := Sil.Vector.InterfaceList(true);
end;

destructor TStreamProtocol.Destroy;
begin
  fwaits := nil;
  inherited;
end;

procedure TStreamProtocol.DoActivate(const Context: IInterface);
begin
  factive := true;
end;

procedure TStreamProtocol.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  DoCancel;
  factive := false;
end;

function TStreamProtocol.DoGetIsActive: Boolean;
begin
  Result := factive;
end;

function TStreamProtocol.DoRead(const Packet: IStreamToken; out Link: IStreamLink): Boolean;
var
  enum: IVectorInterfaceEnumerator;
  wait: IStreamWait;
  found: boolean;
begin
  if DoProtocolMatch(packet) then
  begin
    found := false;
    enum := fwaits.Enumerator;

    while enum.enumerate(wait) do
      if DoPacketMatch(packet, wait) then
      begin
        fwaits.delete(enum.Iteration);
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

function TStreamProtocol.DoProtocolMatch(const packet: IStreamToken): boolean;
begin
  Result := true;
end;

function TStreamProtocol.DoPacketMatch(const packet: IStreamToken; const wait: IStreamWait): boolean;
begin
  Result := true;
end;

function TStreamProtocol.DoCreateWait(const buffer: IMemoryStream; timeout: longword): IStreamWait;
begin
  Result := TPacketWait.Create(buffer, timeout);
  fwaits.add(result);
end;

procedure TStreamProtocol.DoCancel;
var
  enum: IVectorInterfaceEnumerator;
  wait: IStreamWait;
begin
  if fwaits <> nil then
  begin
    enum := fwaits.Enumerator;

    while enum.enumerate(wait) do
      wait.event.signal;

    fwaits.Clear;
  end;
end;

procedure TStreamProtocol.DoWaitPacket(const wait: IStreamWait; out reply: IStreamToken);
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

procedure TPacketWait.settoken(const token: IStreamToken);
begin
  ftoken := token;
end;

function TPacketWait.event: IEvent;
begin
  Result := fevent;
end;

function TPacketWait.gettoken: IStreamToken;
begin
  Result := ftoken;
end;

function TPacketWait.timeout: longword;
begin
  Result := ftimeout;
end;

end.
