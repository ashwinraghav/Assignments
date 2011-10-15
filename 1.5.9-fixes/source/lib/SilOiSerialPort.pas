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

unit SilOiSerialPort;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiHandle;

type
  TCommParity = (paNone, paOdd, paEven, paMark, paSpace);
  TCommStopBits = (sbOne, sbOneHalf, sbTwo);
  TCommEvent = (evBreak, evCTS, evDSR, evError, evRing, evRlsd, evRxChar, evRxFlag, evTxEmpty);
  TCommEvents = set of TCommEvent;
  TCommEscapeCode = (ecSetXOff, ecSetXOn, ecSetRTS, ecClearRTS, ecSetDTR, ecClearDTR, ecResetDev, ecSetBreak, ecClearBreak);
  TCommPurge = (puTxAbort, puRxAbort, puTxClear, puRxClear);
  TCommPurges = set of TCommPurge;

  ISerialPortTimeouts = interface
    ['{B39F6632-461C-11D4-91AE-00C0261013CD}']
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
    property ReadIntervalTimeout: LongWord read GetReadIntervalTimeout write SetReadIntervalTimeout;
    property ReadTotalTimeoutMultiplier: LongWord read GetReadTotalTimeoutMultiplier write SetReadTotalTimeoutMultiplier;
    property ReadTotalTimeoutConstant: LongWord read GetReadTotalTimeoutConstant write SetReadTotalTimeoutConstant;
    property WriteTotalTimeoutMultiplier: LongWord read GetWriteTotalTimeoutMultiplier write SetWriteTotalTimeoutMultiplier;
    property WriteTotalTimeoutConstant: LongWord read GetWriteTotalTimeoutConstant write SetWriteTotalTimeoutConstant;
  end;

  TSerialDtrControl = (dcEnabled, dcDisabled, dcHandShake);
  TSerialRtsControl = (rcEnabled, rcDisabled, rcHandShake, rcToggle);

  ISerialPortControlBlock = interface
    ['{B39F6633-461C-11D4-91AE-00C0261013CD}']
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
    property BinaryMode: Boolean read GetBinaryMode write SetBinaryMode;
    property ParityEnabled: Boolean read GetParityEnabled write SetParityEnabled;
    property OutxCtsFlow: Boolean read GetOutxCtsFlow write SetOutxCtsFlow;
    property OutxDsrFlow: Boolean read GetOutxDsrFlow write SetOutxDsrFlow;
    property DtrControl: TSerialDtrControl read GetDtrControl write SetDtrControl;
    property DsrSensitivity: Boolean read GetDsrSensitivity write SetDsrSensitivity;
    property TXContinueOnXoff: Boolean read GetTXContinueOnXoff write SetTXContinueOnXoff;
    property OutX: Boolean read GetOutX write SetOutX;
    property InX: Boolean read GetInX write SetInX;
    property ErrorCharEnabled: Boolean read GetErrorCharEnabled write SetErrorCharEnabled;
    property Null: Boolean read GetNull write SetNull;
    property RtsControl: TSerialRtsControl read GetRtsControl write SetRtsControl;
    property AbortOnError: Boolean read GetAbortOnError write SetAbortOnError;
    property XOnLim: Word read GetXOnLim write SetXOnLim;
    property XOffLim: Word read GetXOffLim write SetXOffLim;
    property XOnChar: Char read GetXOnChar write SetXOnChar;
    property XOffChar: Char read GetXOffChar write SetXOffChar;
    property ErrorChar: Char read GetErrorChar write SetErrorChar;
    property EofChar: Char read GetEofChar write SetEofChar;
    property EvtChar: Char read GetEvtChar write SetEvtChar;
  end;

  ISerialPortParameters = interface
    ['{A88D09B3-21D2-11D4-987F-00104B0FA1EF}']
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
    property PortName: String read GetPortName write SetPortName;
    property BaudRate: LongWord read GetBaudRate write SetBaudRate;
    property Parity: TCommParity read GetParity write SetParity;
    property DataBits: Byte read GetDataBits write SetDataBits;
    property StopBits: TCommStopBits read GetStopBits write SetStopBits;
    property InQueueSize: LongWord read GetInQueueSize write SetInQueueSize;
    property OutQueueSize: LongWord read GetOutQueueSize write SetOutQueueSize;
    property MonitorEvents: TCommEvents read GetMonitorEvents write SetMonitorEvents;
    property WriteTimeout: LongWord read GetWriteTimeout write SetWriteTimeout;
    property ReadTimeout: LongWord read GetReadTimeout write SetReadTimeout;
  end;

  ISerialPort = interface(IHandledObject)
    ['{A88D09B1-21D2-11D4-987F-00104B0FA1EF}']
    function GetParameters: ISerialPortParameters;
    function GetTimeouts: ISerialPortTimeouts;
    function GetControlBlock: ISerialPortControlBlock;
    function GetStream: IStream;
    function GetOpened: Boolean;
    function Open: Boolean;
    procedure Close;
    function EscapeCode(Code: TCommEscapeCode): Boolean;
    function Purge(Value: TCommPurge): Boolean;
    property Parameters: ISerialPortParameters read GetParameters;
    property Timeouts: ISerialPortTimeouts read GetTimeouts;
    property ControlBlock: ISerialPortControlBlock read GetControlBlock;
    property Stream: IStream read GetStream;
    property IsOpened: Boolean read GetOpened;
  end;

  TSerialPortEvent = object
    Sender: ISerialPort;
    Events: TCommEvents;
  end;

  ISerialPortEvents = interface
    ['{A20C8581-4141-11D4-9888-00104B0FA1EF}']
    procedure OnCommEvent(const Event: TSerialPortEvent);
  end;

implementation

end.

