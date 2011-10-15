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

unit SilOkMessenger;

interface

{$INCLUDE Defines.inc}

uses
(*)  Messages,(*)

  SilBeTypes,
  SilBeError,
  SilLiReference,
  SilLkInterfaced,
  SilOiHandle,
  SilOiIpc,
  SilOeMessenger,
  SilOiMessenger,

  SilOsTypes,
  SilOsHandled;

type
  TSilMessenger = class(
    TSilOsHandledObject,
    IMessenger )
  private
    FOwner: Pointer;
    FSending: ICriticalSection;
  protected // IMessenger
    function GetOwner: IUnknown;
    procedure SetOwner(const Value: IUnknown);
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer = nil; const Param2: Cardinal = 0; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Cardinal = 0; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Pointer = nil; const Timeout: Cardinal = INFINITE): Integer; overload;
    function Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer = nil; const Param2: Pointer = nil; const Timeout: Cardinal = INFINITE): Integer; overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Cardinal = 0); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal = 0; const Param2: Pointer = nil); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer = nil; const Param2: Cardinal = 0); overload;
    procedure Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer = nil; const Param2: Pointer = nil); overload;
    function Call(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; Wait: Boolean; const Timeout: Cardinal; AvoidMsgLock, RaiseError: Boolean): ISynchronization; virtual;
    function DoSend(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer; virtual; abstract;
    procedure DoPost(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer); virtual; abstract;
  public
    constructor Create(const Handle: IHandle; const Owner: IUnknown); overload;
    constructor Create(const Owner: IUnknown); overload;
    destructor Destroy; override;
  public
    property Owner: IUnknown read GetOwner write SetOwner;
    property Sending: ICriticalSection read FSending;
  end;

function ObjectName(const Obj: IUnknown): string;
  
implementation

uses
  SilBcDebug,
  SilBgDebug,
  SilLtTrace,
  SilLtReference,
  SilOtTool;

type
  TSynchronization = class(
    TSilInterfacedObject,
    ISynchronization )
  private
    FMessenger: TSilMessenger;
    FProcessed: IEvent;
    FID: Cardinal;
    FParam1: Integer;
    FParam2: Pointer;
    FRecv: IDispatchable;
    FResult: Integer;
    FError: Exception;
  private
    procedure DoCall(var Msg); message EV_CALL;
  protected // ISynchronization
    function GetProcessed: IEvent;
    function GetResult: Integer;
    function GetError: Exception;
    function GetFailed: Boolean;
    function GetSucceeded: Boolean;
  public
    constructor Create(const Messenger: TSilMessenger; const Recipient: IDispatchable; const ID: Cardinal; const Param1: Integer; const Param2: Pointer);
    destructor Destroy; override;
  end;

function ObjectName(const Obj: IUnknown): string;
var
  Instance: TObject;
begin
  Instance := Ref.GetInstance(Obj);
  if Assigned(Instance) then
    Result := Instance.ClassName else
    Result := 'nil'
end;

{ TSilMessenger }

constructor TSilMessenger.Create(const Owner: IInterface);
begin
  Create(Os.ToolWindow.Create(Self.Dispatch), Owner);
end;

constructor TSilMessenger.Create(const Handle: IHandle; const Owner: IUnknown);
begin
  inherited Create(Handle);
  Self.Owner := Owner;
  FSending := OS.Ipc.CriticalSection;
end;

destructor TSilMessenger.Destroy;
begin
  Self.Owner := nil;
  FSending := nil;
  inherited;
end;

function TSilMessenger.GetOwner: IUnknown;
begin
  Result := IUnknown(FOwner);
end;

procedure TSilMessenger.Post(const Recipient: IDispatchable; ID: Cardinal; const Param1, Param2: Cardinal);
begin
  FSending.Lock;
  try
    DoPost(Recipient, ID, Cardinal(Param1), Pointer(Param2));
  finally
    FSending.Unlock;
  end;
end;

procedure TSilMessenger.Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2: Cardinal);
begin
  FSending.Lock;
  try
    DoPost(Recipient, ID, Cardinal(Param1), Pointer(Param2));
  finally
    FSending.Unlock;
  end;
end;

procedure TSilMessenger.Post(const Recipient: IDispatchable; ID: Cardinal; const Param1, Param2: Pointer);
begin
  FSending.Lock;
  try
    DoPost(Recipient, ID, Cardinal(Param1), Pointer(Param2));
  finally
    FSending.Unlock;
  end;
end;

procedure TSilMessenger.Post(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer);
begin
  FSending.Lock;
  try
    DoPost(Recipient, ID, Cardinal(Param1), Pointer(Param2));
  finally
    FSending.Unlock;
  end;
end;

function TSilMessenger.Send(const Recipient: IDispatchable; ID: Cardinal; const Param1, Param2, Timeout: Cardinal): Integer;
begin
  FSending.Lock;
  try
    Result := DoSend(Recipient, ID, Cardinal(Param1), Pointer(Param2), Timeout);
  finally
    FSending.Unlock;
  end;
end;

function TSilMessenger.Send(const Recipient: IDispatchable; ID: Cardinal; const Param1, Param2: Pointer; const Timeout: Cardinal): Integer;
begin
  FSending.Lock;
  try
    Result := DoSend(Recipient, ID, Cardinal(Param1), Pointer(Param2), Timeout);
  finally
    FSending.Unlock;
  end;
end;

function TSilMessenger.Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Pointer; const Param2, Timeout: Cardinal): Integer;
begin
  FSending.Lock;
  try
    Result := DoSend(Recipient, ID, Cardinal(Param1), Pointer(Param2), Timeout);
  finally
    FSending.Unlock;
  end;
end;

function TSilMessenger.Send(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer;
begin
  FSending.Lock;
  try
    Result := DoSend(Recipient, ID, Cardinal(Param1), Pointer(Param2), Timeout);
  finally
    FSending.Unlock;
  end;
end;

function TSilMessenger.Call(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; Wait: Boolean; const Timeout: Cardinal; AvoidMsgLock, RaiseError: Boolean): ISynchronization;
begin
  Result := TSynchronization.Create(Self, Recipient, ID, Param1, Param2);
  DoPost(Result, EV_CALL, 0, nil);
  if Wait then
  begin
    Result.Processed.WaitFor(Timeout, AvoidMsgLock);
    if Result.Failed then raise Result.Error;
  end;
end;

procedure TSilMessenger.SetOwner(const Value: IInterface);
begin
  FOwner := Pointer(Value);
end;

{ TSynchronization }

constructor TSynchronization.Create(const Messenger: TSilMessenger; const Recipient: IDispatchable; const ID: Cardinal; const Param1: Integer; const Param2: Pointer);
begin
  inherited Create;
  FMessenger := Messenger;
  FProcessed := Os.IPC.Event();
  FID := ID;
  FRecv := Recipient;
  FParam1 := Param1;
  FParam2 := Param2;
  FResult := 0;
end;

destructor TSynchronization.Destroy;
begin
  FRecv := nil;
  FProcessed := nil;
  FMessenger := nil;
  inherited;
end;

procedure TSynchronization.DoCall(var Msg);
begin
  try
    FResult := FMessenger.DoSend(FRecv, FID, FParam1, FParam2, INFINITE);
  except
    {$IFDEF D60}
    FError := System.AcquireExceptionObject();
    {$ELSE}
    FError := RaiseList;
    {$ENDIF}
  end;
  (*
          {$IFDEF D60}
          ReleaseExceptionObject;
          {$ENDIF}


  *)

  FProcessed.Signal;
end;

function TSynchronization.GetProcessed: IEvent;
begin
  Result := FProcessed;
end;

function TSynchronization.GetError: Exception;
begin
  Result := FError;
end;

function TSynchronization.GetResult: Integer;
begin
  Result := FResult;
end;

function TSynchronization.GetFailed: Boolean;
begin
  Result := Assigned(FError);
end;

function TSynchronization.GetSucceeded: Boolean;
begin
  Result := FProcessed.IsSignaled and not Assigned(FError);
end;

end.
 