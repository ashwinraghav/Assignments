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

unit SilLayerSmDeviceDatagram;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSkLayer;

type
  TSilLayerDeviceSocketDatagram = class (TSilLayer)
  private
    FDevice: ISocketClient;
    FDefaultDstAddress: ISocketAddress;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  end;

implementation

{ TSilLayerDeviceSocketDatagram }

procedure TSilLayerDeviceSocketDatagram.DoActivate(const Context: IInterface);
var
  srcaddress, dstaddress: string;
  srcport, dstport: word;
begin
  srcaddress := GetParams.Get('srcaddress', '');
  srcport := GetParams.Get('srcport', 1234);
  dstaddress := GetParams.Get('dstaddress', '');
  dstport := GetParams.Get('dstport', 0);

  FDevice := Sil.OS.Socket.CreateClient(stDatagram, spUDP);

  if Str.NotEmpty(srcaddress) or (srcport > 0) then
    FDevice.Bind(srcaddress, srcport);

  if Str.NotEmpty(dstaddress) and (dstport > 0) then
    FDefaultDstAddress := Sil.OS.Socket.IP.Create(dstaddress, dstport);
end;

procedure TSilLayerDeviceSocketDatagram.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  if FDevice <> nil then
  begin
    FDevice.Disconnect;
    FDevice := nil;
  end;
end;

function TSilLayerDeviceSocketDatagram.DoGetIsActive: Boolean;
begin
  Result := FDevice <> nil;
end;

function TSilLayerDeviceSocketDatagram.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
var
  buf: string;
  size: integer;
  srcaddress: ISocketAddress;
  device: ISocketClient;
begin
  device := FDevice;
  SetLength(buf, Int.IfEmpty(Packet.Buffer.Size, 65536));

  size := device.Stream.ReadFrom(buf[1], Length(buf), srcaddress);
  Result := size > 0;

  if result then
  begin
    Packet.Buffer.Position := 0;
    Packet.Buffer.Write(buf[1], size);
    Packet.Params['srcaddress'] := srcaddress;
    link := GetUpper;
  end;
end;

function TSilLayerDeviceSocketDatagram.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
var
  dstaddress: ISocketAddress;
begin
  if not Vart.ToInterface(Packet.Params['dstaddress'], ISocketAddress, dstaddress) then
    dstaddress := FDefaultDstAddress;

  if dstaddress <> nil then
    Result := FDevice.Stream.WriteTo(Packet.Buffer.Memory^, Packet.Buffer.Size, dstaddress) > 0
  else
    Result := false;
end;

end.

