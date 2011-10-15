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

unit SilSmSerialConnection;

{$I Defines.inc}

interface

uses
  SilLmInterfaceList,
  SilSiAbstractConnection,
  SilLiStream,
  SilOiHandle,
  SilOiSerialPort,
  SilLiConnection,
  SilSiSerialConnection,
  SilSmAbstractConnection;

type
  TSerialConnection = class (
    // extends
    TAbstractConnection,
    // implements
    IAbstractConnection,
    ISerialConnection)
  private
    FSerial: ISerialPort;
  protected // TAbstractConnection
    function Initialize: Boolean; override;
    procedure DoDisconnect; override;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected // IAbstractConnection
    function GetBufferSize: LongWord; override;
    function GetConnected: Boolean; override;
  protected // ISerialConnection
    function GetHandle: IHandle;
    function GetParameters: ISerialPortParameters;
    function GetTimeouts: ISerialPortTimeouts;
    function GetControlBlock: ISerialPortControlBlock;
    function GetStream: IStream;
    function Open: Boolean;
    procedure Close;
    function EscapeCode(Code: TCommEscapeCode): Boolean;
    function Purge(Value: TCommPurge): Boolean;
  public
    constructor Create(const SerialPort: ISerialPort = nil);
  end;

implementation

uses
  SilLtConnection,
  SilOtTool;

{ TSerialConnection }

procedure TSerialConnection.Close;
begin
  FSerial.Close;
end;

constructor TSerialConnection.Create(const SerialPort: ISerialPort);
begin
  inherited Create;

  if SerialPort = nil then
    FSerial := OS.SerialPort.Create else
    FSerial := SerialPort;

  SetStream(FSerial.Stream);
end;

procedure TSerialConnection.DoDisconnect;
begin
  Close;
end;

function TSerialConnection.GetBufferSize: LongWord;
begin
  Result := FSerial.Parameters.InQueueSize;
end;

function TSerialConnection.EscapeCode(Code: TCommEscapeCode): Boolean;
begin
  Result := FSerial.EscapeCode(Code);
end;

function TSerialConnection.GetConnected: Boolean;
begin
  Result := FConnected and FSerial.Handle.IsValid;
end;

function TSerialConnection.GetHandle: IHandle;
begin
  Result := FSerial.Handle;
end;

function TSerialConnection.GetParameters: ISerialPortParameters;
begin
  Result := FSerial.Parameters;
end;

function TSerialConnection.GetStream: IStream;
begin
  Result := FSerial.Stream;
end;

function TSerialConnection.Initialize: Boolean;
begin
  Result := FSerial.Open;
end;

function TSerialConnection.Open: Boolean;
begin
  Result := Connect;
end;

function TSerialConnection.Purge(Value: TCommPurge): Boolean;
begin
  Result := FSerial.Purge(Value);
end;

procedure TSerialConnection.AddListener(const Listener: IUnknown; KeepRef: Boolean);
begin
  Sink.Connect(FSerial, Listener);
  inherited;
end;

procedure TSerialConnection.RemoveListener(const Listener: IUnknown);
begin
  Sink.Disconnect(FSerial, Listener);
  inherited;
end;

function TSerialConnection.GetControlBlock: ISerialPortControlBlock;
begin
  Result := FSerial.ControlBlock;
end;

function TSerialConnection.GetTimeouts: ISerialPortTimeouts;
begin
  Result := FSerial.Timeouts;
end;

end.
