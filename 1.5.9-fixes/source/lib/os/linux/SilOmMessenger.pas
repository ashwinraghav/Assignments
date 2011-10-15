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

unit SilOmMessenger;

{$I Defines.inc}

interface

uses
(*)  Messages,(*)
  SilOsTypes,

  SilLiReference,
  SilLkInterfaced,
  SilOiHandle,
  SilOeMessenger,
  SilOiMessenger,
  SilOkMessenger;

type
  RMessage = record
    ID: Cardinal;
    Recipient: IDispatchable;
    (*)Data: PMessage;(*)
    Result: Integer;
  end;

  TSilLinuxMessenger = class(TSilMessenger)
  private
    procedure DoDispatch(var Msg: RMessage); message EV_DISPATCH;
    function DoSendMessage(const Recipient: IDispatchable; const ID: Cardinal; const Param1: Cardinal; const Param2: Pointer): Integer;
    function DoSendTimeout(const Recipient: IDispatchable; const ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer;
  protected // IMessenger
    function DoSend(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer; override;
    procedure DoPost(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer); override;
  public
    procedure DefaultHandler(var Msg); override;
  end;

implementation

uses
  SysUtils,
  SilBcDebug,
  SilBeError,
  SilBgDebug,
  SilLtTrace,
  SilLtReference,
  SilOtTool;

(*)function NewMessage(ID: Cardinal; const Param1: Cardinal; const Param2: Pointer): PMessage;
begin
  New(Result);
  Result.Msg := ID;
  Result.WParam := Param1;
  Result.LParam := Integer(Param2);
  Result.Result := 0;
end;(*)

(*)function MessageID(const Data: PMessage): LongWord;
begin
  if Assigned(Data) then
    Result := Data.Msg else
    Result := 0;
end;(*)

{ TSilLinuxMessenger }

procedure TSilLinuxMessenger.DefaultHandler(var Msg);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxMessenger.DefaultHandler']);
(*)  if Assigned(Self.Handle) then with TMessage(Msg) do
  try
    Linux.DefWindowProc(Self.Handle.Value, Msg, WParam, LPAram);
  except on Ex: Exception do
  end;(*)
end;

function TSilLinuxMessenger.DoSend(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer;
begin
  if Timeout = INFINITE then
    Result := DoSendMessage(Recipient, ID, Param1, Param2) else
    Result := DoSendTimeout(Recipient, ID, Param1, Param2, Timeout);
end;

function TSilLinuxMessenger.DoSendMessage(const Recipient: IDispatchable; const ID: Cardinal; const Param1: Cardinal; const Param2: Pointer): Integer;
(*)var
  Data: PMessage;
  Recv: Pointer;(*)
begin
(*)  Recv := nil;
  if Assigned(Self.Handle) and Self.Handle.IsValid then
  begin
    Data := NewMessage(ID, Param1, Param2);
    IDispatchable(Recv) := Recipient;
    Result := Linux.SendMessage(Self.Handle.Value, EV_DISPATCH, Integer(Recv), Integer(Data))
  end else
    Result := 0;(*)
end;

function TSilLinuxMessenger.DoSendTimeout(const Recipient: IDispatchable; const ID, Param1: Cardinal; const Param2: Pointer; const Timeout: Cardinal): Integer;
(*)var
  Data: PMessage;
  Recv: Pointer;(*)
begin
(*)  Recv := nil;
  if Assigned(Self.Handle) and Self.Handle.IsValid then
  begin
    Data := NewMessage(ID, Param1, Param2);
    IDispatchable(Recv) := Recipient;
    try
      if Linux.SendMessageTimeout(Self.Handle.Value, EV_DISPATCH, Integer(Recv), Integer(Data), SMTO_NORMAL, TimeOut, Cardinal(Result)) = 0 then
        raise Os.Error.Create(GetLastError, 'TSilLinuxMessenger.DoSendTimeout [Linux.SendMessageTimeout]');
    except
      IDispatchable(Recv) := nil;
      Dispose(Data);
      raise;
    end;
  end;(*)
end;

procedure TSilLinuxMessenger.DoPost(const Recipient: IDispatchable; ID: Cardinal; const Param1: Cardinal; const Param2: Pointer);
(*)var
  Data: PMessage;
  Recv: Pointer;(*)
begin
(*)  Recv := nil;
  if Assigned(Self.Handle) then
  begin
    Data := NewMessage(ID, Param1, Param2);
    IDispatchable(Recv) := Recipient;
    if not Linux.PostMessage(Self.Handle.Value, EV_DISPATCH, Integer(Recv), Integer(Data)) then
    begin
      IDispatchable(Recv) := Recipient;
      Dispose(Data);
    end;
  end;(*)
end;

procedure TSilLinuxMessenger.DoDispatch(var Msg: RMessage);
begin
(*)  try
    if Assigned(Msg.Data) then
    try
      if Assigned(Msg.Recipient) then
      try
        Msg.Recipient.Dispatch(Msg.Data^);
        Msg.Result := Msg.Data.Result;
      finally
        Msg.Recipient := nil;
      end;
    finally
      Dispose(Msg.Data);
      Msg.Data := nil;
    end;
  except on Ex: Exception do
  end;(*)
end;

end.
