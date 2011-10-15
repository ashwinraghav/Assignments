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

unit SilSmLayerDeviceNamedPipeClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilOiPipe,
  SilSiLayer,
  SilSmLayerDeviceStream;

type
  TSilNamedPipeClientLayer = class (TSilLayerStream)
  private
    FDevice: INamedPipeClient;
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

uses
  SilSmLayer,
  SilOtPipe;

{ TSilNamedPipeClientLayer }

constructor TSilNamedPipeClientLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Value: Variant;
  Access: TPipeAccess;
  Mode: TPipeMode;
  Type_: TPipeMode;
begin
  inherited;

  if not Self.Parameters.Find('Device', Value) then
  begin
    if Self.Parameters.Find('Access', Value) then
      Access := TPipeAccess(Sil.Enum.Value(TypeInfo(TPipeAccess), Value, 'pa')) else
      Access := paDuplex;

    if Self.Parameters.Find('Mode', Value) then
      Mode := TPipeMode(Sil.Enum.Value(TypeInfo(TPipeMode), Value, 'pm')) else
      Mode := pmStream;

    if Self.Parameters.Find('Type', Value) then
      Type_ := TPipeMode(Sil.Enum.Value(TypeInfo(TPipeMode), Value, 'pm')) else
      Type_ := pmStream;

    FDevice := Sil.OS.Pipe.Open(Self.Parameters['Name'], Access, Type_, Mode);
  end else
    Vart.ToInterface(Value, INamedPipeClient, FDevice);
end;

destructor TSilNamedPipeClientLayer.Destroy;
begin
  FDevice := nil;
  inherited;
end;

procedure TSilNamedPipeClientLayer.DoConnect;
begin
  if Assigned(FDevice) then
  begin
    Parameters['Device'] := FDevice;
    Stream := FDevice.Stream;
    ReadBlockSize := FDevice.Parameters.InBufferSize;
  end;
end;

procedure TSilNamedPipeClientLayer.DoDisconnect;
begin
  Parameters.Remove('Device');
  FDevice := nil;
  Stream := nil;
end;

function TSilNamedPipeClientLayer.GetDevice: IUnknown;
begin
  Result := FDevice;
end;

procedure TSilNamedPipeClientLayer.SetDevice(const Value: IInterface);
begin
  FDevice := Value as INamedPipeClient;
end;

end.

