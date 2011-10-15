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

unit SilSmLayerDeviceSocketServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerDeviceConnectionServer;

type
  TSilSocketServerLayer = class (TSilLayerConnectionServer)
  private
    FDevice: ISocketServer;
    FProtocol: TSocketProtocol;
  protected
    procedure DoActivate; override;
    procedure DoDeactivate(IsBroken: Boolean); override;
    function DoCreateClient(const Client: IUnknown): IUnknown; override;
    function DoAcceptClient(out Client: IUnknown): Boolean; override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override; 
  end;

implementation

uses
  SilStLayer,
  SilSmLayerDeviceSocketClient;

{ TSilSocketServerLayer }

constructor TSilSocketServerLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Address: String;
  Port: Word;
  TypeSpec: TSocketType;
  Value: Variant;
begin
  inherited;

  Address := Parameters.Get('Address', '');
  Port := Parameters['Port'];

  if Parameters.Find('Type', Value) then
    TypeSpec := TSocketType(Sil.Enum.Value(TypeInfo(TSocketType), Value, 'st')) else
    TypeSpec := stStream;

  if Parameters.Find('Protocol', Value) then
    FProtocol := TSocketProtocol(Sil.Enum.Value(TypeInfo(TSocketProtocol), Value, 'sp')) else
    FProtocol := spTCP;

  if TypeSpec = stUnknown then TypeSpec := stStream;
  if FProtocol = spUnknown then FProtocol := spTCP;

  FDevice := Sil.OS.Socket.CreateServer(TypeSpec, FProtocol, Address, Port);
end;

destructor TSilSocketServerLayer.Destroy;
begin
  inherited;
end;

procedure TSilSocketServerLayer.DoActivate;
begin
  if FProtocol = spTCP then FDevice.Listen;
end;

procedure TSilSocketServerLayer.DoDeactivate(IsBroken: Boolean);
begin
  if Assigned(FDevice) then FDevice.Cancel;
end;

function TSilSocketServerLayer.DoAcceptClient(out Client: IUnknown): Boolean;
var
  Socket: ISocketClient;
  Buffer: String;
  Address: ISocketAddress;
  Command: ILayerCommand;
  Size: LongWord;
begin
  case FProtocol of
    spTCP:
    begin
      Result := FDevice.IsListening and FDevice.Accept(Socket);
      if Result then Client := Socket;
    end;

    spUDP:
    begin
      SetString(Buffer, nil, FDevice.Parameters.ReceiveBufferSize);
      Size := FDevice.Stream.ReadFrom(Buffer[1], Length(Buffer), Address);
      Result := Size > 0;

      if Result then
      begin
        Command := Cmd.Create(nil, Link, Sil.Stream.Raw.Packet, Vart.Unassigned, Address);
        Command.Packet.Buffer.Write(Buffer[1], Size);
        Client := Command;
      end;
    end;

    else Result := false;
  end;
end;

function TSilSocketServerLayer.DoCreateClient(const Client: IUnknown): IUnknown;
var
  Stream: ILayerStream;
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['device'] := Client;

  Stream := TSilSocketClientLayer.Create(Params);
  Result := Stream;
end;

end.
