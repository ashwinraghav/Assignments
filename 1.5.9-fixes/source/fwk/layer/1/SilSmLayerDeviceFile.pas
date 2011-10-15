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

unit SilSmLayerDeviceFile;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerDeviceStream;

type
  TSilFileLayer = class (TSilLayerStream)
  private
    FDevice: IFile;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
  protected // ILayerStream
    function GetDevice: IUnknown; override;
    procedure SetDevice(const Value: IUnknown); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

implementation

{ TSilFileLayer }

constructor TSilFileLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited;
end;

destructor TSilFileLayer.Destroy;
begin
  FDevice := nil;
  inherited;
end;

procedure TSilFileLayer.DoConnect;
var
  FilePath: String;
  Access: TFileAccessMode;
  Share: TFileShareMode;
  MustExists: Boolean;
begin
  FilePath := Parameters['filepath'];

  Access := TFileAccessMode(Sil.Enum.Value(TypeInfo(TFileAccessMode), Parameters.Get('access', 'ReadWrite'), 'fmAccess'));
  Share := TFileShareMode(Sil.Enum.Value(TypeInfo(TFileShareMode), Parameters.Get('share', 'Read'), 'fmShare'));
  MustExists := Parameters.Get('mustexists', true);

  FDevice := Sil.OS.FileSystem.OpenFile(FilePath, Access, Share, MustExists);
  Stream := FDevice.Stream;
  ReadBlockSize := Parameters.Get('buffersize', $FFFF);
end;

procedure TSilFileLayer.DoDisconnect;
begin
  Stream := nil;
  if Assigned(FDevice) then FDevice := nil;
end;

function TSilFileLayer.GetDevice: IUnknown;
begin
  Result := FDevice;
end;

procedure TSilFileLayer.SetDevice(const Value: IInterface);
begin
  FDevice := Value as IFile;
end;

end.

