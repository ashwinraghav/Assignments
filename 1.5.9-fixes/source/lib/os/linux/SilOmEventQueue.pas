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

unit SilOmEventQueue;

{$I Defines.inc}

interface

uses
  SilOiHandle,
  SilOiThread,
  SilOiEventQueue,
  SilOkEventQueue;

const
  EV_QUIT         = {WM_USER + }$1ABA;
  EV_PROCESSEVENT = {WM_USER + }$1ABB;

type
  REvent = packed record
    Msg: Cardinal;
    Event: TEvent;
    Packet: IEventPacket;
    Result: Integer;
  end;

  REventDispatch = packed record
    Msg: Cardinal;
    Packet: IEventPacket;
    Result: Cardinal;
  end;

type
  TSilLinuxEventQueue = class (TSilEventQueue)
  private
    FWindow: IHandle;
  private
    function DoWaitMessage: Boolean;
    procedure DoProcessEvent(var Msg: REvent); message EV_PROCESSEVENT;
  protected
    procedure DoThreadRun(const Thread: IThread); override;
		function DoThreadInitialize(const Thread: IThread): Boolean; override;
		procedure DoThreadFinalize(const Thread: IThread); override;
    procedure DoThreadTerminate; override;
    function DoPostPacket(const Packet: IEventPacket): Boolean; override;
    function DoSendPacket(const Packet: IEventPacket; Timeout: Longword): Integer; override;
  public
    constructor Create(const ASink: IUnknown; Synchronize: Boolean; const ThreadName: String); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Msg); override;
  end;

implementation

uses
  SysUtils,

  SilLtLock,
  SilLtReference,
  SilOtTool;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TSilLinuxEventQueue }

constructor TSilLinuxEventQueue.Create(const ASink: IUnknown; Synchronize: Boolean; const ThreadName: String);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxEventQueue.Create']);
  inherited;
end;

destructor TSilLinuxEventQueue.Destroy;
begin(*)
  with Self.Locked do
  try
    FWindow := nil;
  finally
    Release;
  end;
  inherited;
(*)end;

procedure TSilLinuxEventQueue.DoProcessEvent(var Msg: REvent);
(*)var
  Temp: ILock;(*)
begin(*)
  Temp := Locked;

  if Assigned(FWindow) then
  try
    Temp.Release;
    DoProcess(Msg.Packet)
  finally
    Msg.Packet := nil;
  end;
(*)end;

procedure TSilLinuxEventQueue.DefaultHandler(var Msg);
begin(*)
  Self.Locked;
  if FWindow <> nil then
    with TMessage(Msg) do
      Result := DefWindowProc(FWindow.Value, Msg, wParam, lParam);
(*)end;

function TSilLinuxEventQueue.DoWaitMessage: Boolean;
(*)var
  Msg: TMsg;
  Window: IHandle;(*)
begin(*)
  Window := FWindow;
  Result := (Window <> nil) and Linux.GetMessage(Msg, Window.Value, 0, 0) and (Msg.message <> EV_QUIT);
  if Result then
  begin
    Linux.TranslateMessage(Msg);
    Linux.DispatchMessage(Msg);
  end;
(*)end;

function TSilLinuxEventQueue.DoThreadInitialize(const Thread: IThread): Boolean;
begin(*)
  FWindow := OS.ToolWindow.Create(Self.Dispatch);
  Result := FWindow <> nil;
(*)end;

procedure TSilLinuxEventQueue.DoThreadFinalize(const Thread: IThread);
begin(*)
  FWindow := nil;
(*)end;

procedure TSilLinuxEventQueue.DoThreadRun(const Thread: IThread);
begin(*)
  while DoWaitMessage do;
(*)end;

procedure TSilLinuxEventQueue.DoThreadTerminate;
begin(*)
  if FWindow <> nil then Linux.PostMessage(FWindow.Value, EV_QUIT, 0, 0);
(*)end;

function TSilLinuxEventQueue.DoPostPacket(const Packet: IEventPacket): Boolean;
(*)var
  Data: Cardinal;(*)
begin(*)
  Data := 0;
  IEventPacket(Data) := Packet;
  Result := Assigned(FWindow) and Linux.PostMessage(FWindow.Value, EV_PROCESSEVENT, Packet.Event, Data);
(*)end;

function TSilLinuxEventQueue.DoSendPacket(const Packet: IEventPacket; Timeout: Longword): Integer;
(*)var
  Data: Cardinal;
  Return: Cardinal absolute Result;(*)
begin(*)
  if Assigned(FWindow) then
  begin
    Data := 0;
    IEventPacket(Data) := Packet;
    try
      if Timeout <> 0 then
        begin
          if Linux.SendMessageTimeout(FWindow.Value, EV_PROCESSEVENT, Packet.Event, Cardinal(Packet), SMTO_NORMAL, Timeout, Return) = 0 then
            raise Os.Error.Create(Linux.GetLastError, 'Linux.SendMessageTimeout');
        end
      else
          Return := Linux.SendMessage(FWindow.Value, EV_PROCESSEVENT, Packet.Event, Cardinal(Packet));
    except
      IEventPacket(Data) := nil;
      raise;
    end;
  end;
(*)end;

(*)
function TSilLinuxEventQueue.Post(Event: TEvent; Params: array of Variant): Boolean;
var
  EventPacket: IEventPacket;
begin
  Result := false;
  Lock.Take(Lockable);
  if FWindow = nil then Exit;

  EventPacket := TEventPacket.Create(Event, Params);
  if not Result then EventPacket.Free;
end;

function TSilLinuxEventQueue.Send(Event: TEvent; Params: array of Variant; Timeout: LongWord): Integer;
var
  EventPacket: TEventPacket;
begin
  Result := 0;

  Lock.Take(Lockable);
  if FWindow = nil then Exit;

  EventPacket := TEventPacket.Create(Event, Params);
end;

(*)

end.
