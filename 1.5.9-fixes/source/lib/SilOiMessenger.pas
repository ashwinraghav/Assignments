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

unit SilOiMessenger;

{$I Defines.inc}

interface

uses
  SilBeError,
  SilLiReference,
  SilOsTypes,
  SilOiHandle,
  SilOiIpc,
  SilOeMessenger;

type
  ISynchronization = interface;

  IMessenger = interface (IHandledObject)
    ['{916645DD-6AE7-4EE6-9556-534026A0307A}']
    function GetOwner: IUnknown;
    procedure SetOwner(const Value: IUnknown);
    procedure SetHandle(const Value: IHandle);
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Pointer = nil; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2: Cardinal = 0; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Cardinal; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2: Pointer = nil; const Timeout: Cardinal = INFINITE): Integer; overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Pointer = nil); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Cardinal); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2: Cardinal); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2: Pointer = nil); overload;
    function Call(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Pointer = nil; Wait: Boolean = False; const Timeout: Cardinal = INFINITE; AvoidMsgLock: Boolean = True; RaiseError: Boolean = True): ISynchronization;
    property Owner: IUnknown read GetOwner write SetOwner;
    property Handle: IHandle read GetHandle write SetHandle;
  end;

  IMessengerHook = interface
    ['{36B2D432-4716-471F-AFB5-9C6D5A0E2105}']
    procedure OnMessengerSending(const Sender: IMessenger; var Msg);
    procedure OnMessengerSent(const Sender: IMessenger; var Msg);
    procedure OnMessengerPost(const Sender: IMessenger; var Msg);
    procedure OnMessengerCall(const Sender: IMessenger; var Msg);
    procedure OnMessengerDispatch(const Sender: IMessenger; var Msg);
  end;

  ISynchronization = interface (IDispatchable)
    ['{B8EEC68E-73C3-4358-B26E-39F1D148AD16}']
    function GetProcessed: IEvent;
    function GetResult: Integer;
    function GetError: Exception;
    function GetFailed: Boolean;
    function GetSucceeded: Boolean;
    property Processed: IEvent read GetProcessed;
    property Result: Integer read GetResult;
    property Error: Exception read GetError;
    property Failed: Boolean read GetFailed;
    property Succeeded: Boolean read GetSucceeded;
  end;

implementation
end.
