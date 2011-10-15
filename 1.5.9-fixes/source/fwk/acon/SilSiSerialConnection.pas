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

unit SilSiSerialConnection;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiHandle,
  SilOiSerialPort,
  SilSiAbstractConnection;

type
  ISerialConnection = interface(IAbstractConnection)
    ['{7324DBD1-CE30-11D3-910D-00C0261013CD}']
    function GetHandle: IHandle;
    function GetParameters: ISerialPortParameters;
    function GetTimeouts: ISerialPortTimeouts;
    function GetControlBlock: ISerialPortControlBlock;
    function GetStream: IStream;
    function Open: Boolean;
    procedure Close;
    function EscapeCode(Code: TCommEscapeCode): Boolean;
    function Purge(Value: TCommPurge): Boolean;
    property Parameters: ISerialPortParameters read GetParameters;
    property Timeouts: ISerialPortTimeouts read GetTimeouts;
    property ControlBlock: ISerialPortControlBlock read GetControlBlock;
    property Stream: IStream read GetStream;
    property Handle: IHandle read GetHandle;
  end;

implementation

end.
 