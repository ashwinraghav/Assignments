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

unit SilSmLayerDeviceSocketClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerDeviceStream;

type
  TSilSocketClientLayer = class (TSilLayerStream)
  private
    FDevice: ISocketClient;
    FProtocol: TSocketProtocol;
  private
    procedure DoAssignParams;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoStreamWrite(const Buffer; Size: LongWord; const Context: IUnknown); override;
    function GetIsActive: Boolean; override;
  protected // ILayerStream
    function GetDevice: IUnknown; override;
    procedure SetDevice(const Value: IUnknown); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

implementation

{ TSilSocketClientLayer }

constructor TSilSocketClientLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params.Merge(Parameters);

  inherited Create(Params, Controller);
end;

destructor TSilSocketClientLayer.Destroy;
begin
  FDevice := nil;
  inherited;
end;

procedure TSilSocketClientLayer.DoAssignParams;
var
  Host: String;
  Port: Word;
  TypeSpec: TSocketType;
  Value: Variant;
begin
  if not Parameters.Find('Device', Value) or ((FDevice <> nil) and (not FDevice.IsConnected)) then
  begin
    Host := Parameters['Host'];
    Port := Parameters['Port'];

    if Parameters.Find('Type', Value) then
      TypeSpec := TSocketType(Sil.Enum.Value(TypeInfo(TSocketType), Value, 'st')) else
      TypeSpec := stStream;

    if Parameters.Find('Protocol', Value) then
      FProtocol := TSocketProtocol(Sil.Enum.Value(TypeInfo(TSocketProtocol), Value, 'sp')) else
      FProtocol := spTCP;

    if TypeSpec = stUnknown then TypeSpec := stStream;
    if FProtocol = spUnknown then FProtocol := spTCP;

    FDevice := Sil.OS.Socket.CreateClient(TypeSpec, FProtocol, Host, Port);
  end else
  begin
    Vart.ToInterface(Value, ISocketClient, FDevice);
    FProtocol := FDevice.Info.Local.Protocol;
  end;

  if not Parameters.Contains('EnableReceive') and (FProtocol = spUDP) then
    Parameters['EnableReceive'] := false;
end;

procedure TSilSocketClientLayer.DoConnect;
begin
  if (FDevice = nil) or not FDevice.IsConnected then
  begin
    DoAssignParams;
    Parameters['Device'] := FDevice;
    if not FDevice.IsConnected and (FProtocol = spTCP) then FDevice.Connect;
  end;

  if FDevice <> nil then
  begin
    Stream := FDevice.Stream;
    ReadBlockSize := FDevice.Parameters.ReceiveBufferSize;
  end;
end;

procedure TSilSocketClientLayer.DoDisconnect;
begin
  if Assigned(FDevice) and (FProtocol = spTCP) then FDevice.Disconnect;
  Parameters.Remove('Device');
end;

procedure TSilSocketClientLayer.DoStreamWrite(const Buffer; Size: LongWord; const Context: IUnknown);
var
  Address: ISocketAddress;
  HostAddr: String;
begin
  if FProtocol = spUDP then
  begin
    if not Ref.GetInterface(Context, ISocketAddress, Address) then
    begin
      HostAddr := Parameters['address'];
      Address := Sil.OS.Socket.IP.Create(HostAddr, Parameters['port']);
    end;

    FDevice.Stream.WriteTo(Buffer, Size, Address);
  end else
    inherited;
end;

function TSilSocketClientLayer.GetDevice: IUnknown;
begin
  Result := FDevice;
end;

function TSilSocketClientLayer.GetIsActive: Boolean;
begin
  Result := (FDevice <> nil) and FDevice.IsConnected;
end;

procedure TSilSocketClientLayer.SetDevice(const Value: IInterface);
begin
  FDevice := Value as ISocketClient;
end;

end.

