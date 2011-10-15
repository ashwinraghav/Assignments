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

unit SilLayerSmDeviceFile;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSkLayer;

type
  TSilLayerDeviceFile = class (TSilLayer)
  private
    FDevice: IFile;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  end;

implementation

{ TSilLayerDeviceFile }

procedure TSilLayerDeviceFile.DoActivate(const Context: IInterface);
var
  FileName: string;
  Access: TFileAccessMode;
  Share: TFileShareMode;
  MustExists: Boolean;
begin
  FileName := Vart.ToStr(GetParams['filename']);
  Access := TFileAccessMode(Sil.Enum.Value(TypeInfo(TFileAccessMode), GetParams.Get('access', 'ReadWrite'), 'fmAccess'));
  Share := TFileShareMode(Sil.Enum.Value(TypeInfo(TFileShareMode), GetParams.Get('share', 'Read'), 'fmShare'));
  MustExists := GetParams.Get('mustexists', false);
  FDevice := Sil.OS.FileSystem.OpenFile(FileName, Access, Share, MustExists);
end;

procedure TSilLayerDeviceFile.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  if FDevice <> nil then
  begin
    FDevice.FlushBuffer;
    FDevice := nil;
  end;
end;

function TSilLayerDeviceFile.DoGetIsActive: Boolean;
begin
  Result := FDevice <> nil;
end;

function TSilLayerDeviceFile.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
var
  buf: string;
  size: integer;
begin
  SetLength(buf, Int.IfEmpty(Packet.Buffer.Size, 65536));
  size := FDevice.Stream.Read(buf[1], Length(buf));
  Packet.Buffer.Position := 0;
  Packet.Buffer.Write(buf[1], size);

  Link := GetUpper;
  Result := Size > 0;
end;

function TSilLayerDeviceFile.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Result := FDevice.Stream.Write(Packet.Buffer.Memory^, Packet.Buffer.Size) > 0;
end;

end.
 