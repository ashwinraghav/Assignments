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

unit SilOmTask;

{$INCLUDE Defines.inc}

interface

uses
  SilOiTask,
  SilOiThread,
  SilOiMessenger,
  SilOeMessenger,
  SilOiIpc,

  SilOkTask;

type
  TSilLinuxTask = class(TSilTask)
  private
    FFlush: IEvent;
  private
    (*)function DoGetMessage(var lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL;(*)
  protected
    procedure DoRun(const Thread: IThread); override;
    procedure DoInitialize(const Thread: IThread); override;
    procedure DoFinalize(const Thread: IThread); override;
    procedure DoTerminate; override;
    procedure DoFlush; override;
  end;

implementation

uses
  SysUtils,
  SilBeError,
  SilBtError,
  SilOsTypes,
  SilBcDebug,
  SilBgDebug,
  SilBtVart,
  SilLiEnumerator,
  SilLtTrace,
  SilLtReference,
  SilOdTask,
  SilOtTool;

{ TSilLinuxTask }

procedure TSilLinuxTask.DoRun(const Thread: IThread);
(*)var
  Msg: TMsg;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxTask.DoRun']);
(*)
  FFlush := OS.Ipc.Event;

  while DoGetMessage(Msg, 0, 0, 0) do
  try
    if Assigned(FHook) then ITaskHook(FHook).OnTaskMessage(Self, Msg);
    if Msg.hwnd <> 0 then
    begin
      Linux.TranslateMessage(Msg);
      Linux.DispatchMessage(Msg);
    end;
    if Msg.message = EV_FLUSH then FFlush.Signal;
  except on Ex: Exception do
  end;
(*)end;

procedure TSilLinuxTask.DoInitialize(const Thread: IThread);
(*)var
  Msg: TMsg;(*)
begin(*)
  Linux.PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
  Self.Handle := Os.ToolWindow.Create(Self.Dispatch);
(*)end;

procedure TSilLinuxTask.DoFinalize(const Thread: IThread);
begin(*)
  Self.Handle := nil;
  FFlush := nil;
(*)end;

procedure TSilLinuxTask.DoTerminate;
begin(*)
  if not Linux.PostThreadMessage(Self.Thread.ThreadID, WM_QUIT, 0, 0) then
    raise Error.Create(SNoTaskWindow);
(*)end;

(*)function TSilLinuxTask.DoGetMessage(var lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL;
begin(*)(*)
  try
    if Assigned(FHook) then ITaskHook(FHook).OnTaskWaiting(Self);
    Result := Linux.GetMessage(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax);
  except
    Result := True;
  end;
(*)(*)end;(*)

procedure TSilLinuxTask.DoFlush;
begin(*)
  if not Linux.PostThreadMessage(Self.Thread.ThreadID, EV_FLUSH, 0, 0) then
    raise Error.Create(SNoTaskWindow);

  FFlush.WaitFor(INFINITE, true);
(*)end;

end.
