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

unit SilSmLayerDeviceNamedPipeServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilOiPipe,
  SilSiLayer,
  SilSmLayerDeviceConnectionServer;

type
  TSilNamedPipeServerLayer = class (TSilLayerConnectionServer)
  private
    FDevice: INamedPipeServer;
  protected
    procedure DoActivate; override;
    procedure DoDeactivate(IsBroken: Boolean); override;
    function DoCreateClient(const Client: IUnknown): IUnknown; override;
    function DoAcceptClient(out Client: IUnknown): Boolean; override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
  end;

implementation

uses
  SilSmLayerDeviceNamedPipeClient;

{ TSilNamedPipeServerLayer }

constructor TSilNamedPipeServerLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Value: Variant;
  Access: TPipeAccess;
  PipeMode: TPipeMode;
  PipeType: TPipeMode;
begin
  inherited;

  if not Self.Parameters.Find('Device', Value) then
  begin
    if Self.Parameters.Find('Access', Value) then
      Access := TPipeAccess(Sil.Enum.Value(TypeInfo(TPipeAccess), Value, 'pa')) else
      Access := paDuplex;

    if Self.Parameters.Find('Mode', Value) then
      PipeMode := TPipeMode(Sil.Enum.Value(TypeInfo(TPipeMode), Value, 'pm')) else
      PipeMode := pmStream;

    if Self.Parameters.Find('Type', Value) then
      PipeType := TPipeMode(Sil.Enum.Value(TypeInfo(TPipeMode), Value, 'pm')) else
      PipeType := pmStream;

    FDevice := Sil.OS.Pipe.Create(Self.Parameters['Name'], Access, PipeType, PipeMode);
  end else
    Vart.ToInterface(Value, INamedPipeServer, FDevice);
end;

procedure TSilNamedPipeServerLayer.DoActivate;
begin
  Parameters['Device'] := FDevice;
end;

procedure TSilNamedPipeServerLayer.DoDeactivate(IsBroken: Boolean);
begin
  Parameters.Remove('Device');
  if Assigned(FDevice) then FDevice.Close;
end;

function TSilNamedPipeServerLayer.DoAcceptClient(out Client: IUnknown): Boolean;
var
  Pipe: INamedPipeServerClient;
begin
  Result := FDevice.Connect(Pipe);
  if Result then Client := Pipe;
end;

function TSilNamedPipeServerLayer.DoCreateClient(const Client: IUnknown): IUnknown;
var
  Stream: ILayerStream;
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['device'] := Client;

  Stream := TSilNamedPipeClientLayer.Create(Params);
  Result := Stream;
end;

end.
