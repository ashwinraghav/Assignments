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

unit SilOkSerialPort;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiHandle,
  SilOiThread,
  SilOiIpc,
  SilOiSerialPort,
  SilOcTypes,
  SilOsHandled;

type
  TSilSerialPort = class (
    // extends
    TSilOsHandledObject,
    // implements
    IStream,
    ISerialPort,
    ISerialPortParameters,
    ISerialPortTimeouts,
    ISerialPortControlBlock )
  private
    FPortName: String;
    FOpened: Boolean;
    FWriteTimeout: LongWord;
    FReadTimeout: LongWord;
    FHandle: IHandle;
  private
    function DoOpen: Boolean; virtual;
    procedure DoClose; virtual;
  protected // IStream
    function GetSize: LongWord; virtual; abstract;
    function Read(var Buffer; Count: LongWord): LongWord; virtual; abstract;
    function Write(const Buffer; Count: LongWord): LongWord; virtual; abstract;
  protected // ISerialPort
    function GetParameters: ISerialPortParameters;
    function GetTimeouts: ISerialPortTimeouts;
    function GetControlBlock: ISerialPortControlBlock;
    function GetStream: IStream;
    function GetOpened: Boolean;
    function Open: Boolean;
    procedure Close;
    function EscapeCode(Code: TCommEscapeCode): Boolean; virtual; 
    function Purge(Value: TCommPurge): Boolean; virtual; 
  protected // ISerialPortParameters
    function GetPortName: String; virtual;
    procedure SetPortName(const Value: String); virtual;
    function GetBaudRate: LongWord; virtual;
    procedure SetBaudRate(Value: LongWord); virtual;
    function GetParity: TCommParity; virtual;
    procedure SetParity(Value: TCommParity); virtual;
    function GetDataBits: Byte; virtual;
    procedure SetDataBits(Value: Byte); virtual;
    function GetStopBits: TCommStopBits; virtual;
    procedure SetStopBits(Value: TCommStopBits); virtual;
    function GetMonitorEvents: TCommEvents; virtual;
    procedure SetMonitorEvents(Value: TCommEvents); virtual;
    function GetInQueueSize: LongWord; virtual;
    procedure SetInQueueSize(Value: LongWord); virtual;
    function GetOutQueueSize: LongWord; virtual;
    procedure SetOutQueueSize(Value: LongWord); virtual;
    function GetWriteTimeout: LongWord; virtual;
    procedure SetWriteTimeout(Value: LongWord); virtual;
    function GetReadTimeout: LongWord; virtual;
    procedure SetReadTimeout(Value: LongWord); virtual;
  protected // ISerialPortTimeouts
    function GetReadIntervalTimeout: LongWord;
    procedure SetReadIntervalTimeout(Value: LongWord); virtual;
    function GetReadTotalTimeoutMultiplier: LongWord; virtual;
    procedure SetReadTotalTimeoutMultiplier(Value: LongWord); virtual;
    function GetReadTotalTimeoutConstant: LongWord; virtual;
    procedure SetReadTotalTimeoutConstant(Value: LongWord); virtual;
    function GetWriteTotalTimeoutMultiplier: LongWord; virtual;
    procedure SetWriteTotalTimeoutMultiplier(Value: LongWord); virtual;
    function GetWriteTotalTimeoutConstant: LongWord; virtual;
    procedure SetWriteTotalTimeoutConstant(Value: LongWord); virtual;
  protected // ISerialPortControlBlock
    function GetBinaryMode: Boolean; virtual;
    procedure SetBinaryMode(Value: Boolean); virtual;
    function GetParityEnabled: Boolean; virtual;
    procedure SetParityEnabled(Value: Boolean); virtual;
    function GetOutxCtsFlow: Boolean; virtual;
    procedure SetOutxCtsFlow(Value: Boolean); virtual;
    function GetOutxDsrFlow: Boolean; virtual;
    procedure SetOutxDsrFlow(Value: Boolean); virtual;
    function GetDtrControl: TSerialDtrControl; virtual;
    procedure SetDtrControl(const Value: TSerialDtrControl); virtual;
    function GetDsrSensitivity: Boolean; virtual;
    procedure SetDsrSensitivity(Value: Boolean); virtual;
    function GetTXContinueOnXoff: Boolean; virtual;
    procedure SetTXContinueOnXoff(Value: Boolean); virtual;
    function GetOutX: Boolean; virtual;
    procedure SetOutX(Value: Boolean); virtual;
    function GetInX: Boolean; virtual;
    procedure SetInX(Value: Boolean); virtual;
    function GetErrorCharEnabled: Boolean; virtual;
    procedure SetErrorCharEnabled(Value: Boolean); virtual;
    function GetNull: Boolean; virtual;
    procedure SetNull(Value: Boolean); virtual;
    function GetRtsControl: TSerialRtsControl; virtual;
    procedure SetRtsControl(Value: TSerialRtsControl); virtual;
    function GetAbortOnError: Boolean; virtual;
    procedure SetAbortOnError(Value: Boolean); virtual;
    function GetXOnLim: Word; virtual;
    procedure SetXOnLim(Value: Word); virtual;
    function GetXOffLim: Word; virtual;
    procedure SetXOffLim(Value: Word); virtual;
    function GetXOnChar: Char; virtual;
    procedure SetXOnChar(Value: Char); virtual;
    function GetXOffChar: Char; virtual;
    procedure SetXOffChar(Value: Char); virtual;
    function GetErrorChar: Char; virtual;
    procedure SetErrorChar(Value: Char); virtual;
    function GetEofChar: Char; virtual;
    procedure SetEofChar(Value: Char); virtual;
    function GetEvtChar: Char; virtual;
    procedure SetEvtChar(Value: Char); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses SilLkInterfaced;

{ TSilSerialPort }

constructor TSilSerialPort.Create;
begin
  inherited Create;

  FOpened := false;
  FWriteTimeout := INFINITE;
  FReadTimeout := INFINITE;
end;

destructor TSilSerialPort.Destroy;
begin
  Close;
  DoClose;

  inherited;
end;

function TSilSerialPort.DoOpen: Boolean;
begin
end;

procedure TSilSerialPort.DoClose;
begin
end;

procedure TSilSerialPort.Close;
begin
  Locked;

  if FOpened then
  begin
    FOpened := false;
    DoClose;
  end;
end;

function TSilSerialPort.GetBaudRate: LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.GetDataBits: Byte;
begin
  Result := 0;
end;

function TSilSerialPort.GetInQueueSize: LongWord;
begin
  Result := FInQueueSize;
end;

function TSilSerialPort.GetMonitorEvents: TCommEvents;
begin
  Result := FMonitorEvents;
end;

function TSilSerialPort.GetOpened: Boolean;
begin
  Result := FOpened;
end;

function TSilSerialPort.GetOutQueueSize: LongWord;
begin
  Result := FOutQueueSize;
end;

function TSilSerialPort.GetParameters: ISerialPortParameters;
begin
  Result := Self;
end;

function TSilSerialPort.GetParity: TCommParity;
var
  i: TCommParity;
begin
  for i := Low(i) to High(i) do
    if CommParity[i] = FDCB.Parity then
    begin
      Result := i;
      Exit;
    end;

  Result := paNone;
end;

function TSilSerialPort.GetPortName: String;
begin
  Result := FPortName;
end;

function TSilSerialPort.GetSize: LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.GetStopBits: TCommStopBits;
begin
  Result := sbOne;
end;

function TSilSerialPort.GetStream: IStream;
begin
  Result := Self;
end;

function TSilSerialPort.Open: Boolean;
begin
  if not FOpened then
    Result := DoOpen else
    Result := false;
end;

function TSilSerialPort.Read(var Buffer; Count: LongWord): LongWord;
begin
  Result := 0;
end;

procedure TSilSerialPort.SetBaudRate(Value: LongWord);
begin
end;

procedure TSilSerialPort.SetDataBits(Value: Byte);
begin
end;

procedure TSilSerialPort.SetInQueueSize(Value: LongWord);
begin
end;

procedure TSilSerialPort.SetMonitorEvents(Value: TCommEvents);
begin
end;

procedure TSilSerialPort.SetOutQueueSize(Value: LongWord);
begin
end;

procedure TSilSerialPort.SetParity(Value: TCommParity);
begin
end;

procedure TSilSerialPort.SetPortName(const Value: String);
begin
  if not FOpened then FPortName := Value;
end;

procedure TSilSerialPort.SetStopBits(Value: TCommStopBits);
begin
end;

function TSilSerialPort.Write(const Buffer; Count: LongWord): LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.EscapeCode(Code: TCommEscapeCode): Boolean;
begin
  Result := false;
end;

function TSilSerialPort.Purge(Value: TCommPurge): Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetControlBlock: ISerialPortControlBlock;
begin
  Result := Self;
end;

function TSilSerialPort.GetTimeouts: ISerialPortTimeouts;
begin
  Result := Self;
end;

function TSilSerialPort.GetAbortOnError: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetBinaryMode: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetDsrSensitivity: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetDtrControl: TSerialDtrControl;
begin
  Result := dcDisabled;
end;

function TSilSerialPort.GetEofChar: Char;
begin
  Result := #0;
end;

function TSilSerialPort.GetErrorChar: Char;
begin
  Result := #0;
end;

function TSilSerialPort.GetErrorCharEnabled: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetEvtChar: Char;
begin
  Result := #0;
end;

function TSilSerialPort.GetInX: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetNull: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetOutX: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetOutxCtsFlow: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetOutxDsrFlow: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetParityEnabled: Boolean;
begin
  Result := false;
end;

function TSilSerialPort.GetReadIntervalTimeout: LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.GetReadTotalTimeoutConstant: LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.GetReadTotalTimeoutMultiplier: LongWord;
begin
  Result := 0;
end;

function TSilSerialPort.GetRtsControl: TSerialRtsControl;
begin
  Result := TSerialRtsControl((GetDCB(FDCB, cfRtsControl1) shl 1) or GetDCB(FDCB, cfRtsControl2));
end;

function TSilSerialPort.GetTXContinueOnXoff: Boolean;
begin
  Result := GetDCB(FDCB, cfTXContinueOnXoff) > 0;
end;

function TSilSerialPort.GetWriteTotalTimeoutConstant: LongWord;
begin
  Result := FCommTimeouts.WriteTotalTimeoutConstant;
end;

function TSilSerialPort.GetWriteTotalTimeoutMultiplier: LongWord;
begin
  Result := FCommTimeouts.WriteTotalTimeoutMultiplier;
end;

function TSilSerialPort.GetXOffChar: Char;
begin
  Result := FDCB.XoffChar;
end;

function TSilSerialPort.GetXOffLim: Word;
begin
  Result := FDCB.XoffLim;
end;

function TSilSerialPort.GetXOnChar: Char;
begin
  Result := FDCB.XonChar;
end;

function TSilSerialPort.GetXOnLim: Word;
begin
  Result := FDCB.XonLim;
end;

procedure TSilSerialPort.SetAbortOnError(Value: Boolean);
begin
  SetDCB(FDCB, cfAbortOnError, Value);
end;

procedure TSilSerialPort.SetBinaryMode(Value: Boolean);
begin
  SetDCB(FDCB, cfBinaryMode, Value);
end;

procedure TSilSerialPort.SetDsrSensitivity(Value: Boolean);
begin
  SetDCB(FDCB, cfDsrSensitivity, Value);
end;

procedure TSilSerialPort.SetDtrControl(const Value: TSerialDtrControl);
begin
  SetDCB(FDCB, cfDtrControl1, Ord(Value) and $2 > 0);
  SetDCB(FDCB, cfDtrControl2, Ord(Value) and $1 > 0);
end;

procedure TSilSerialPort.SetEofChar(Value: Char);
begin
  FDCB.EofChar := Value;
end;

procedure TSilSerialPort.SetErrorChar(Value: Char);
begin
  FDCB.ErrorChar := Value;
end;

procedure TSilSerialPort.SetErrorCharEnabled(Value: Boolean);
begin
  SetDCB(FDCB, cfErrorCharEnabled, Value);
end;

procedure TSilSerialPort.SetEvtChar(Value: Char);
begin
  FDCB.EvtChar := Value;
end;

procedure TSilSerialPort.SetInX(Value: Boolean);
begin
  SetDCB(FDCB, cfInX, Value);
end;

procedure TSilSerialPort.SetNull(Value: Boolean);
begin
  SetDCB(FDCB, cfNull, Value);
end;

procedure TSilSerialPort.SetOutX(Value: Boolean);
begin
  SetDCB(FDCB, cfOutX, Value);
end;

procedure TSilSerialPort.SetOutxCtsFlow(Value: Boolean);
begin
  SetDCB(FDCB, cfOutxCtsFlow, Value);
end;

procedure TSilSerialPort.SetOutxDsrFlow(Value: Boolean);
begin
  SetDCB(FDCB, cfOutxDsrFlow, Value);
end;

procedure TSilSerialPort.SetParityEnabled(Value: Boolean);
begin
  SetDCB(FDCB, cfParityEnabled, Value);
end;

procedure TSilSerialPort.SetReadIntervalTimeout(Value: LongWord);
begin
  FCommTimeouts.ReadIntervalTimeout := Value;
  if FOpened then DoUpdateCommTimeouts;
end;

procedure TSilSerialPort.SetReadTotalTimeoutConstant(Value: LongWord);
begin
  FCommTimeouts.ReadTotalTimeoutConstant := Value;
  if FOpened then DoUpdateCommTimeouts;
end;

procedure TSilSerialPort.SetReadTotalTimeoutMultiplier(Value: LongWord);
begin
  FCommTimeouts.ReadTotalTimeoutMultiplier := Value;
  if FOpened then DoUpdateCommTimeouts;
end;

procedure TSilSerialPort.SetRtsControl(Value: TSerialRtsControl);
begin
  SetDCB(FDCB, cfRtsControl1, Ord(Value) and $2 > 0);
  SetDCB(FDCB, cfRtsControl2, Ord(Value) and $1 > 0);
end;

procedure TSilSerialPort.SetTXContinueOnXoff(Value: Boolean);
begin
  SetDCB(FDCB, cfTXContinueOnXoff, Value);
end;

procedure TSilSerialPort.SetWriteTotalTimeoutConstant(Value: LongWord);
begin
  FCommTimeouts.WriteTotalTimeoutConstant := Value;
  if FOpened then DoUpdateCommTimeouts;
end;

procedure TSilSerialPort.SetWriteTotalTimeoutMultiplier(Value: LongWord);
begin
  FCommTimeouts.WriteTotalTimeoutMultiplier := Value;
  if FOpened then DoUpdateCommTimeouts;
end;

procedure TSilSerialPort.SetXOffChar(Value: Char);
begin
  FDCB.XoffChar := Value;
end;

procedure TSilSerialPort.SetXOffLim(Value: Word);
begin
  FDCB.XoffLim := Value;
end;

procedure TSilSerialPort.SetXOnChar(Value: Char);
begin
  FDCB.XonChar := Value;
end;

procedure TSilSerialPort.SetXOnLim(Value: Word);
begin
  FDCB.XonLim := Value;
end;

function TSilSerialPort.GetReadTimeout: LongWord;
begin
  Result := FReadTimeout;
end;

function TSilSerialPort.GetWriteTimeout: LongWord;
begin
  Result := FWriteTimeout;
end;

procedure TSilSerialPort.SetReadTimeout(Value: LongWord);
begin
  FReadTimeout := Value;
end;

procedure TSilSerialPort.SetWriteTimeout(Value: LongWord);
begin
  FWriteTimeout := Value;
end;

end.
