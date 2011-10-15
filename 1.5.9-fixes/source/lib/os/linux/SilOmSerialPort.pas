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

unit SilOmSerialPort;

{$I Defines.inc}

interface

uses
  serial,
  libc,
  SysUtils,

  SilOsTypes,

  SilLkInterfaced,
  SilLiStream,
  SilLiStringList,
  SilOiThread,
  SilOiHandle,
  SilLiLock,

  SilOiSerialPort,
  SilOmHandle;

type
  TSerialPort = class (
    // extends
    TSilInterfacedObject,
    // implements
    IHandledObject,
    IStream,
    ISerialPort,
    ISerialPortParameters,
    ISerialPortTimeouts,
    ISerialPortControlBlock)
  private
    FHandle: IHandle;
    FPortName: String;
    FOpened: Boolean;
    FLock: ILockable;
    FBaudRate: longword;
    FByteSize: byte;
    FParity: TParityType;
    FStopBits: byte;
    (*)FCommTimeouts: TCommTimeouts;
    FEvents: IUnknown;
    FInQueueSize: LongWord;
    FOutQueueSize: LongWord;
    FWriteTimeout: LongWord;
    FReadTimeout: LongWord;(*)
  protected
    function DoOpen: Boolean;
    procedure DoUpdateCommTimeouts;
    procedure DoUpdateQueueSize;
    procedure DoUpdateDCB;
  protected // IHandledObject
    function GetHandle: IHandle;
  protected // IStream
    function GetSize: LongWord;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
  protected // ISerialPort
    function GetParameters: ISerialPortParameters;
    function GetTimeouts: ISerialPortTimeouts;
    function GetControlBlock: ISerialPortControlBlock;
    function GetStream: IStream;
    function GetOpened: Boolean;
    function Open: Boolean;
    procedure Close;
    function EscapeCode(Code: TCommEscapeCode): Boolean;
    function Purge(Value: TCommPurge): Boolean;
  protected // ISerialPortParameters
    function GetPortName: String;
    procedure SetPortName(const Value: String);
    function GetBaudRate: LongWord;
    procedure SetBaudRate(Value: LongWord);
    function GetParity: TCommParity;
    procedure SetParity(Value: TCommParity);
    function GetDataBits: Byte;
    procedure SetDataBits(Value: Byte);
    function GetStopBits: TCommStopBits;
    procedure SetStopBits(Value: TCommStopBits);
    function GetMonitorEvents: TCommEvents;
    procedure SetMonitorEvents(Value: TCommEvents);
    function GetInQueueSize: LongWord;
    procedure SetInQueueSize(Value: LongWord);
    function GetOutQueueSize: LongWord;
    procedure SetOutQueueSize(Value: LongWord);
    function GetWriteTimeout: LongWord;
    procedure SetWriteTimeout(Value: LongWord);
    function GetReadTimeout: LongWord;
    procedure SetReadTimeout(Value: LongWord);
  protected // ISerialPortTimeouts
    function GetReadIntervalTimeout: LongWord;
    procedure SetReadIntervalTimeout(Value: LongWord);
    function GetReadTotalTimeoutMultiplier: LongWord;
    procedure SetReadTotalTimeoutMultiplier(Value: LongWord);
    function GetReadTotalTimeoutConstant: LongWord;
    procedure SetReadTotalTimeoutConstant(Value: LongWord);
    function GetWriteTotalTimeoutMultiplier: LongWord;
    procedure SetWriteTotalTimeoutMultiplier(Value: LongWord);
    function GetWriteTotalTimeoutConstant: LongWord;
    procedure SetWriteTotalTimeoutConstant(Value: LongWord);
  protected // ISerialPortControlBlock
    function GetBinaryMode: Boolean;
    procedure SetBinaryMode(Value: Boolean);
    function GetParityEnabled: Boolean;
    procedure SetParityEnabled(Value: Boolean);
    function GetOutxCtsFlow: Boolean;
    procedure SetOutxCtsFlow(Value: Boolean);
    function GetOutxDsrFlow: Boolean;
    procedure SetOutxDsrFlow(Value: Boolean);
    function GetDtrControl: TSerialDtrControl;
    procedure SetDtrControl(const Value: TSerialDtrControl);
    function GetDsrSensitivity: Boolean;
    procedure SetDsrSensitivity(Value: Boolean);
    function GetTXContinueOnXoff: Boolean;
    procedure SetTXContinueOnXoff(Value: Boolean);
    function GetOutX: Boolean;
    procedure SetOutX(Value: Boolean);
    function GetInX: Boolean;
    procedure SetInX(Value: Boolean);
    function GetErrorCharEnabled: Boolean;
    procedure SetErrorCharEnabled(Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(Value: Boolean);
    function GetRtsControl: TSerialRtsControl;
    procedure SetRtsControl(Value: TSerialRtsControl);
    function GetAbortOnError: Boolean;
    procedure SetAbortOnError(Value: Boolean);
    function GetXOnLim: Word;
    procedure SetXOnLim(Value: Word);
    function GetXOffLim: Word;
    procedure SetXOffLim(Value: Word);
    function GetXOnChar: Char;
    procedure SetXOnChar(Value: Char);
    function GetXOffChar: Char;
    procedure SetXOffChar(Value: Char);
    function GetErrorChar: Char;
    procedure SetErrorChar(Value: Char);
    function GetEofChar: Char;
    procedure SetEofChar(Value: Char);
    function GetEvtChar: Char;
    procedure SetEvtChar(Value: Char);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilOtTool,
  SilOmFile;

type
  CommControlBlock = (
    cfBinaryMode, cfParityEnabled, cfOutxCtsFlow, cfOutxDsrFlow,
    cfDtrControl1, cfDtrControl2, cfDsrSensitivity, cfTXContinueOnXoff,
    cfOutX, cfInX, cfErrorCharEnabled, cfNull, cfRtsControl1, cfRtsControl2,
    cfAbortOnError, cfDummy1, cfDummy2);

  CommControlBlocks = set of CommControlBlock;
  PCommControlBlocks = ^CommControlBlocks;

(*)function GetDCB(const DCB: TDCB; Value: CommControlBlock): Byte;
begin
  Result := Ord(Value in PCommControlBlocks(@DCB.Flags)^);
end;(*)

(*)procedure SetDCB(const DCB: TDCB; Value: CommControlBlock; NewSet: Boolean);
begin
  if NewSet then
    Include(PCommControlBlocks(@DCB.Flags)^, Value) else
    Exclude(PCommControlBlocks(@DCB.Flags)^, Value);
end;(*)

{ TSerialPort }

constructor TSerialPort.Create;
begin
  inherited Create;
  FHandle := nil;
  FLock := OS.IPC.CriticalSection();

  FBaudRate := 9600;
  FParity := NoneParity;
  FByteSize := 8;
  FStopBits := 1;

(*)  FInQueueSize := 2048;
  FOutQueueSize := 2048;
  FOpened := false;
  FWriteTimeout := INFINITE;
  FReadTimeout := INFINITE;

  FCommTimeouts.ReadIntervalTimeout := 50;
  FCommTimeouts.ReadTotalTimeoutMultiplier := 0;
  FCommTimeouts.WriteTotalTimeoutMultiplier := 0;
  FCommTimeouts.ReadTotalTimeoutConstant := 0;
  FCommTimeouts.WriteTotalTimeoutConstant := 0;(*)
end;

destructor TSerialPort.Destroy;
begin
  Close;
  inherited;
end;

procedure TSerialPort.Close;
begin
  try
    FLock.Lock;

    if FOpened then
    begin
      FOpened := false;
      FHandle.Close;
    end;
  finally
    FLock.Unlock;
  end;
end;

function TSerialPort.GetBaudRate: LongWord;
begin
  Result := FBaudRate;
end;

function TSerialPort.GetDataBits: Byte;
begin
  Result := FByteSize;
end;

function TSerialPort.GetHandle: IHandle;
begin
  Result := FHandle;
end;

function TSerialPort.GetInQueueSize: LongWord;
begin(*)
  Result := FInQueueSize;
(*)end;

function TSerialPort.GetOpened: Boolean;
begin
  Result := FOpened;
end;

function TSerialPort.GetOutQueueSize: LongWord;
begin(*)
  Result := FOutQueueSize;
(*)end;

function TSerialPort.GetParameters: ISerialPortParameters;
begin
  Result := Self;
end;

function TSerialPort.GetParity: TCommParity;
    begin
  result := TCommParity(Ord(FParity));
    end;

procedure TSerialPort.SetParity(Value: TCommParity);
begin
  if (value in [paNone, paOdd, paEven]) and (GetParity <> Value) then
  begin
    FParity := TParityType(Ord(Value));
    DoUpdateDCB;
  end;
end;

function TSerialPort.GetPortName: String;
begin
  Result := FPortName;
end;

function TSerialPort.GetSize: LongWord;
(*)var
  ComStat: TComStat;
  Dummy: Cardinal;(*)
begin(*)
  if FOpened and ClearCommError(FHandle.Value, Dummy, @ComStat) then
    Result := ComStat.cbInQue else
    Result := 0;
(*)end;

function TSerialPort.GetStopBits: TCommStopBits;
    begin
  if FStopBits = 2 then
    result := sbTwo
  else
    result := sbOne;
    end;

procedure TSerialPort.SetStopBits(Value: TCommStopBits);
begin
  if (value in [sbOne, sbTwo]) and (GetStopBits <> Value) then
  begin
    case Value of
      sbOne:  FStopBits := 1;
      sbTwo:  FStopBits := 2;
    end;
    DoUpdateDCB;
  end;
end;

function TSerialPort.GetStream: IStream;
begin
  Result := Self;
end;

function TSerialPort.Open: Boolean;
begin
  if not FOpened then
    Result := DoOpen else
    Result := false;
  end;

procedure TSerialPort.SetBaudRate(Value: LongWord);
  begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    DoUpdateDCB;
  end;
end;

procedure TSerialPort.SetDataBits(Value: Byte);
  begin
  if FByteSize <> Value then
  begin
    FByteSize := Value;
    DoUpdateDCB;
  end;
end;

procedure TSerialPort.SetInQueueSize(Value: LongWord);
begin(*)
  if FInQueueSize <> Value then
  begin
    FInQueueSize := Value;
    if FOpened then DoUpdateQueueSize;
  end;
(*)end;

function TSerialPort.GetMonitorEvents: TCommEvents;
begin
  result := [];
end;

procedure TSerialPort.SetMonitorEvents(Value: TCommEvents);
  begin
  end;

procedure TSerialPort.SetOutQueueSize(Value: LongWord);
begin(*)
  if FOutQueueSize <> Value then
  begin
    FOutQueueSize := Value;
    if FOpened then DoUpdateQueueSize;
  end;
(*)end;

procedure TSerialPort.SetPortName(const Value: String);
begin
  if not FOpened then FPortName := Value;
end;

function TSerialPort.Read(var Buffer; Count: LongWord): LongWord;
  begin
  Result := libc.__read(FHandle.Value, Buffer, Count);
  end;

function TSerialPort.Write(const Buffer; Count: LongWord): LongWord;
    begin
  Result := libc.__write(FHandle.Value, Buffer, Count);
    end;

procedure TSerialPort.DoUpdateCommTimeouts;
begin(*)
  if not FOpened then Exit;
  SetCommTimeouts(FHandle.Value, FCommTimeouts);
(*)end;

procedure TSerialPort.DoUpdateDCB;
var
  tios: TTermios;
begin
  if FOpened then
  begin
    FillChar(tios, SizeOf(tios), #0);

    case FBaudRate of
      50:     tios.c_cflag := B50;
      75:     tios.c_cflag := B75;
      110:    tios.c_cflag := B110;
      134:    tios.c_cflag := B134;
      150:    tios.c_cflag := B150;
      200:    tios.c_cflag := B200;
      300:    tios.c_cflag := B300;
      600:    tios.c_cflag := B600;
      1200:   tios.c_cflag := B1200;
      1800:   tios.c_cflag := B1800;
      2400:   tios.c_cflag := B2400;
      4800:   tios.c_cflag := B4800;
      9600:   tios.c_cflag := B9600;
      19200:  tios.c_cflag := B19200;
      38400:  tios.c_cflag := B38400;
      57600:  tios.c_cflag := B57600;
      115200: tios.c_cflag := B115200;
      230400: tios.c_cflag := B230400;
    end;

    tios.c_ispeed := tios.c_cflag;
    tios.c_ospeed := tios.c_ispeed;

    tios.c_cflag := tios.c_cflag or CREAD or CLOCAL;

    case FByteSize of
      5:    tios.c_cflag := tios.c_cflag or CS5;
      6:    tios.c_cflag := tios.c_cflag or CS6;
      7:    tios.c_cflag := tios.c_cflag or CS7;
      else  tios.c_cflag := tios.c_cflag or CS8;
    end;

    case FParity of
      OddParity:  tios.c_cflag := tios.c_cflag or PARENB or PARODD;
      EvenParity: tios.c_cflag := tios.c_cflag or PARENB;
    end;

    if FStopBits = 2 then
      tios.c_cflag := tios.c_cflag or CSTOPB;

    //if RtsCtsFlowControl in Flags then
      //tios.c_cflag := tios.c_cflag or CRTSCTS;

    tcflush(FHandle.Value, TCIOFLUSH);
    tcsetattr(FHandle.Value, TCSANOW, tios);
  end;
end;

procedure TSerialPort.DoUpdateQueueSize;
begin(*)
  if not FOpened then SetupComm(FHandle.Value, FInQueueSize, FOutQueueSize);
(*)end;

function TSerialPort.DoOpen: Boolean;
var
  Handle: THandle;
begin
  Handle := __open(PChar(FPortName), O_RDWR + O_NOCTTY + O_NDELAY, 0);
  FHandle := TSilFileHandle.Create(Handle, true);

  FOpened := FHandle.IsValid;
  Result := FOpened;

  if Result then
  begin
    DoUpdateDCB;
    DoUpdateQueueSize;
    DoUpdateCommTimeouts;
  end;
end;

function TSerialPort.EscapeCode(Code: TCommEscapeCode): Boolean;
begin
    Result := false;
end;

function TSerialPort.Purge(Value: TCommPurge): Boolean;
begin
  if FOpened then
    Result := libc.tcflush(FHandle.Value, TCIOFLUSH) > 0
  else
    Result := false;
end;

function TSerialPort.GetControlBlock: ISerialPortControlBlock;
begin(*)
  Result := Self;
(*)end;

function TSerialPort.GetTimeouts: ISerialPortTimeouts;
begin(*)
  Result := Self;
(*)end;

function TSerialPort.GetAbortOnError: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfAbortOnError) > 0;
(*)end;

function TSerialPort.GetBinaryMode: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfBinaryMode) > 0;
(*)end;

function TSerialPort.GetDsrSensitivity: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfDsrSensitivity) > 0;
(*)end;

function TSerialPort.GetDtrControl: TSerialDtrControl;
begin(*)
  Result := TSerialDtrControl((GetDCB(FDCB, cfDtrControl1) shl 1) or GetDCB(FDCB, cfDtrControl2));
(*)end;

function TSerialPort.GetEofChar: Char;
begin(*)
  Result := FDCB.EofChar;
(*)end;

function TSerialPort.GetErrorChar: Char;
begin(*)
  Result := FDCB.ErrorChar;
(*)end;

function TSerialPort.GetErrorCharEnabled: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfErrorCharEnabled) > 0;
(*)end;

function TSerialPort.GetEvtChar: Char;
begin(*)
  Result := FDCB.EvtChar;
(*)end;

function TSerialPort.GetInX: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfInX) > 0;
(*)end;

function TSerialPort.GetNull: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfNull) > 0;
(*)end;

function TSerialPort.GetOutX: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfOutX) > 0;
(*)end;

function TSerialPort.GetOutxCtsFlow: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfOutxCtsFlow) > 0;
(*)end;

function TSerialPort.GetOutxDsrFlow: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfOutxDsrFlow) > 0;
(*)end;

function TSerialPort.GetParityEnabled: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfParityEnabled) > 0;
(*)end;

function TSerialPort.GetReadIntervalTimeout: LongWord;
begin(*)
  Result := FCommTimeouts.ReadIntervalTimeout;
(*)end;

function TSerialPort.GetReadTotalTimeoutConstant: LongWord;
begin(*)
  Result := FCommTimeouts.ReadTotalTimeoutConstant;
(*)end;

function TSerialPort.GetReadTotalTimeoutMultiplier: LongWord;
begin(*)
  Result := FCommTimeouts.ReadTotalTimeoutMultiplier;
(*)end;

function TSerialPort.GetRtsControl: TSerialRtsControl;
begin(*)
  Result := TSerialRtsControl((GetDCB(FDCB, cfRtsControl1) shl 1) or GetDCB(FDCB, cfRtsControl2));
(*)end;

function TSerialPort.GetTXContinueOnXoff: Boolean;
begin(*)
  Result := GetDCB(FDCB, cfTXContinueOnXoff) > 0;
(*)end;

function TSerialPort.GetWriteTotalTimeoutConstant: LongWord;
begin(*)
  Result := FCommTimeouts.WriteTotalTimeoutConstant;
(*)end;

function TSerialPort.GetWriteTotalTimeoutMultiplier: LongWord;
begin(*)
  Result := FCommTimeouts.WriteTotalTimeoutMultiplier;
(*)end;

function TSerialPort.GetXOffChar: Char;
begin(*)
  Result := FDCB.XoffChar;
(*)end;

function TSerialPort.GetXOffLim: Word;
begin(*)
  Result := FDCB.XoffLim;
(*)end;

function TSerialPort.GetXOnChar: Char;
begin(*)
  Result := FDCB.XonChar;
(*)end;

function TSerialPort.GetXOnLim: Word;
begin(*)
  Result := FDCB.XonLim;
(*)end;

procedure TSerialPort.SetAbortOnError(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfAbortOnError, Value);
(*)end;

procedure TSerialPort.SetBinaryMode(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfBinaryMode, Value);
(*)end;

procedure TSerialPort.SetDsrSensitivity(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfDsrSensitivity, Value);
(*)end;

procedure TSerialPort.SetDtrControl(const Value: TSerialDtrControl);
(*)const
  DTR: Cardinal = TIOCM_DTR;(*)
begin(*)
  if State then
    fpioctl(Handle, TIOCMBIS, @DTR)
  else
    fpioctl(Handle, TIOCMBIC, @DTR);

  SetDCB(FDCB, cfDtrControl1, Ord(Value) and $2 > 0);
  SetDCB(FDCB, cfDtrControl2, Ord(Value) and $1 > 0);
(*)end;

procedure TSerialPort.SetEofChar(Value: Char);
begin(*)
  FDCB.EofChar := Value;
(*)end;

procedure TSerialPort.SetErrorChar(Value: Char);
begin(*)
  FDCB.ErrorChar := Value;
(*)end;

procedure TSerialPort.SetErrorCharEnabled(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfErrorCharEnabled, Value);
(*)end;

procedure TSerialPort.SetEvtChar(Value: Char);
begin(*)
  FDCB.EvtChar := Value;
(*)end;

procedure TSerialPort.SetInX(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfInX, Value);
(*)end;

procedure TSerialPort.SetNull(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfNull, Value);
(*)end;

procedure TSerialPort.SetOutX(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfOutX, Value);
(*)end;

procedure TSerialPort.SetOutxCtsFlow(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfOutxCtsFlow, Value);
(*)end;

procedure TSerialPort.SetOutxDsrFlow(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfOutxDsrFlow, Value);
(*)end;

procedure TSerialPort.SetParityEnabled(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfParityEnabled, Value);
(*)end;

procedure TSerialPort.SetReadIntervalTimeout(Value: LongWord);
begin(*)
  FCommTimeouts.ReadIntervalTimeout := Value;
  if FOpened then DoUpdateCommTimeouts;
(*)end;

procedure TSerialPort.SetReadTotalTimeoutConstant(Value: LongWord);
begin(*)
  FCommTimeouts.ReadTotalTimeoutConstant := Value;
  if FOpened then DoUpdateCommTimeouts;
(*)end;

procedure TSerialPort.SetReadTotalTimeoutMultiplier(Value: LongWord);
begin(*)
  FCommTimeouts.ReadTotalTimeoutMultiplier := Value;
  if FOpened then DoUpdateCommTimeouts;
(*)end;

procedure TSerialPort.SetRtsControl(Value: TSerialRtsControl);
begin(*)
  SetDCB(FDCB, cfRtsControl1, Ord(Value) and $2 > 0);
  SetDCB(FDCB, cfRtsControl2, Ord(Value) and $1 > 0);
(*)end;

procedure TSerialPort.SetTXContinueOnXoff(Value: Boolean);
begin(*)
  SetDCB(FDCB, cfTXContinueOnXoff, Value);
(*)end;

procedure TSerialPort.SetWriteTotalTimeoutConstant(Value: LongWord);
begin(*)
  FCommTimeouts.WriteTotalTimeoutConstant := Value;
  if FOpened then DoUpdateCommTimeouts;
(*)end;

procedure TSerialPort.SetWriteTotalTimeoutMultiplier(Value: LongWord);
begin(*)
  FCommTimeouts.WriteTotalTimeoutMultiplier := Value;
  if FOpened then DoUpdateCommTimeouts;
(*)end;

procedure TSerialPort.SetXOffChar(Value: Char);
begin(*)
  FDCB.XoffChar := Value;
(*)end;

procedure TSerialPort.SetXOffLim(Value: Word);
begin(*)
  FDCB.XoffLim := Value;
(*)end;

procedure TSerialPort.SetXOnChar(Value: Char);
begin(*)
  FDCB.XonChar := Value;
(*)end;

procedure TSerialPort.SetXOnLim(Value: Word);
begin(*)
  FDCB.XonLim := Value;
(*)end;

function TSerialPort.GetReadTimeout: LongWord;
begin(*)
  Result := FReadTimeout;
(*)end;

function TSerialPort.GetWriteTimeout: LongWord;
begin(*)
  Result := FWriteTimeout;
(*)end;

procedure TSerialPort.SetReadTimeout(Value: LongWord);
begin(*)
  FReadTimeout := Value;
(*)end;

procedure TSerialPort.SetWriteTimeout(Value: LongWord);
begin(*)
  FWriteTimeout := Value;
(*)end;

end.
