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

unit SilSmLayerDeviceSerialPort;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerDeviceStream;

type
  TSilSerialPortLayer = class (TSilLayerStream)
  private
    FDevice: ISerialPort;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
  protected // ILayerStream
    function GetDevice: IUnknown; override;
    procedure SetDevice(const Value: IUnknown); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); overload; override; 
    constructor Create(const Device: ISerialPort; const Parameters: IParameters; const Controller: IUnknown); reintroduce; overload; 
    destructor Destroy; override;
  end;

implementation

{ TSilSerialPortLayer }

constructor TSilSerialPortLayer.Create(const Device: ISerialPort; const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited Create(Parameters, Controller);
  FDevice := Device;
end;

constructor TSilSerialPortLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Device: ISerialPort;
  Value: Variant;
  PortParam, PortControl: IParameters;
begin
  Device := Sil.OS.SerialPort.Create;

  if Assigned(Parameters) then
  begin
    PortParam := Parameters.Slice('parameters');
    PortControl := Parameters.Slice('control');

    if PortParam.Count = 0 then PortParam := Parameters;

    with Device.Parameters do
    begin
      PortName := PortParam.Get('portname', 'COM1');
      BaudRate := PortParam.Get('baudrate', 9600);
      Parity := TCommParity(Sil.Enum.Value(TypeInfo(TCommParity), PortParam.Get('parity', 'Even'), 'pa'));
      DataBits := PortParam.Get('databits', 7);
      StopBits := TCommStopBits(Sil.Enum.Value(TypeInfo(TCommStopBits), PortParam.Get('stopbits', 'One'), 'sb'));

      if PortParam.Find('rcvbuffersize', Value) then InQueueSize := Value;
      if PortParam.Find('sndbuffersize', Value) then OutQueueSize := Value;
      if PortParam.Find('writetimeout', Value) then WriteTimeout := Value;
      if PortParam.Find('readtimeout', Value) then ReadTimeout := Value;
      //Device.Parameters.MonitorEvents := Parameters['portname'];
    end;

    if PortControl.Count > 0 then
      with Device.ControlBlock do
      begin
        if PortControl.Find('BinaryMode', Value) then BinaryMode := Value;
        if PortControl.Find('ParityEnabled', Value) then ParityEnabled := Value;
        if PortControl.Find('OutxCtsFlow', Value) then OutxCtsFlow := Value;
        if PortControl.Find('OutxDsrFlow', Value) then OutxDsrFlow := Value;

        if PortControl.Find('DtrControl', Value) then
          DtrControl := TSerialDtrControl(Sil.Enum.Value(TypeInfo(TSerialDtrControl), Value, 'dc'));

        if PortControl.Find('DsrSensitivity', Value) then DsrSensitivity := Value;
        if PortControl.Find('TXContinueOnXoff', Value) then TXContinueOnXoff := Value;
        if PortControl.Find('OutX', Value) then OutX := Value;
        if PortControl.Find('InX', Value) then InX := Value;
        if PortControl.Find('ErrorCharEnabled', Value) then ErrorCharEnabled := Value;
        if PortControl.Find('Null', Value) then Null := Value;

        if PortControl.Find('RtsControl', Value) then
          RtsControl := TSerialRtsControl(Sil.Enum.Value(TypeInfo(TSerialRtsControl), Value, 'rc'));

        if PortControl.Find('AbortOnError', Value) then AbortOnError := Value;
        if PortControl.Find('XOnLim', Value) then XOnLim := Value;
        if PortControl.Find('XOffLim', Value) then XOffLim := Value;
        if PortControl.Find('XOnChar', Value) then XOnChar := Str.ToChr(Vart.ToStr(Value));
        if PortControl.Find('XOffChar', Value) then XOffChar := Str.ToChr(Vart.ToStr(Value));
        if PortControl.Find('ErrorChar', Value) then ErrorChar := Str.ToChr(Vart.ToStr(Value));
        if PortControl.Find('EofChar', Value) then EofChar := Str.ToChr(Vart.ToStr(Value));
        if PortControl.Find('EvtChar', Value) then EvtChar := Str.ToChr(Vart.ToStr(Value));
      end;

    Create(Device, Parameters, Controller);
  end;
end;

destructor TSilSerialPortLayer.Destroy;
begin
  FDevice := nil;
  inherited;
end;

procedure TSilSerialPortLayer.DoConnect;
begin
  if Assigned(FDevice) then
  begin
    if not FDevice.IsOpened then
      if not FDevice.Open then
        raise Error.Create('cannot open device');

    Stream := FDevice.Stream;
    ReadBlockSize := FDevice.Parameters.InQueueSize;
  end;
end;

procedure TSilSerialPortLayer.DoDisconnect;
begin
  Stream := nil;
  if Assigned(FDevice) then FDevice.Close;
end;

function TSilSerialPortLayer.GetDevice: IUnknown;
begin
  Result := FDevice;
end;

procedure TSilSerialPortLayer.SetDevice(const Value: IInterface);
begin
  FDevice := Value as ISerialPort;
end;

end.
 