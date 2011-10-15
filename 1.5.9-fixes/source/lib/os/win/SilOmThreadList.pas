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

unit SilOmThreadList;

interface

{$INCLUDE Defines.inc}

uses
  Windows, Messages,

  SilBeTypes,
  SilLiEnumerator,
  SilOiHandle,
  SilOiThread,
  SilOkThreadList,

  SilLmInterfaceList;

const
  SIL_EXECPROC = WM_USER + 1;

type
  PCallRec = ^TCallRec;
  TCallRec = record
    Obj: IUnknown;
    Method: TMethod;
    Mode: TThreadCallMode;
    Param: Pointer;
    Unref: Boolean;
  end;

type
  TSilWindowsThreadList = class (
    // extends
    TSilThreadList,
    // implements
    IThreads,
    IThreadList)
  private
    FSynchronizeException: TObject;
    FWindow: IHandle;
    procedure DoPurge(Handle: LongWord);
    procedure DoClose;
    procedure WindowProc(var AMessage);
    procedure DoCall(const Obj: IInterface; Mode: TThreadCallMode; Method: TMethod; Unref: Boolean; Param: Pointer);
  protected // IThreadList
    procedure Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref); override;
    procedure ThreadCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtVart,
  SilOtTool;

{ TSilWindowsThreadList }

constructor TSilWindowsThreadList.Create;
begin
  inherited Create;
  FWindow := OS.ToolWindow.Create(WindowProc);
end;

destructor TSilWindowsThreadList.Destroy;
begin
  DoClose;
  inherited;
end;

procedure TSilWindowsThreadList.DoCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TMethod; Unref: Boolean; Param: Pointer);
var
  CallRec: PCallRec;
  bDispose: Boolean;
begin
  New(CallRec);
  CallRec.Obj := Obj;
  CallRec.Method := TMethod(Method);
  CallRec.Mode := Mode;
  CallRec.Param := Param;
  CallRec.Unref := Unref;

  bDispose := true;

  case Mode of
    tcSync:    SendMessage(FWindow.Value, SIL_EXECPROC, Integer(CallRec), 0);
    tcAsync:   bDispose := not PostMessage(FWindow.Value, SIL_EXECPROC, Integer(CallRec), 0);
  end;

  if bDispose then Dispose(CallRec);
  if Assigned(FSynchronizeException) then raise FSynchronizeException;
end;

procedure TSilWindowsThreadList.Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref);
begin
  DoCall(Obj, Mode, TMethod(Method), true, @Ref);
end;

procedure TSilWindowsThreadList.ThreadCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer);
begin
  DoCall(Obj, Mode, TMethod(Method), false, Param);
end;

procedure TSilWindowsThreadList.DoPurge(Handle: LongWord);
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, Handle, 0, 0, PM_REMOVE) do
    DispatchMessage(Msg);
end;

procedure TSilWindowsThreadList.DoClose;
var
  Handle: LongWord;
begin
  Handle := FWindow.Value;
  FWindow.Close;
  try
    DoPurge(Handle);
  finally
    FWindow := nil;
  end;
end;

procedure TSilWindowsThreadList.WindowProc(var AMessage);
var
  CallRec: PCallRec;
  {$IFNDEF D60}
  RaisePtr: Pointer;
  {$ENDIF}
begin
  with TMessage(AMessage) do
    if Msg = SIL_EXECPROC then
    begin
      CallRec := nil;
      try
        try
          FSynchronizeException := nil;
          CallRec := PCallRec(wParam);

          if CallRec.UnRef then
            TThreadMethod(CallRec.Method)(CallRec.Obj, CallRec.Param^) else
            TThreadCallMethod(CallRec.Method)(CallRec.Obj, CallRec.Param);
        except
          if CallRec.Mode = tcAsync then 
            raise;

          {$IFDEF D60}
          FSynchronizeException := AcquireExceptionObject;
          {$ELSE}
          RaisePtr := RaiseList;

          if RaisePtr <> nil then
          begin
            FSynchronizeException := PRaiseFrame(RaisePtr)^.ExceptObject;
            PRaiseFrame(RaisePtr)^.ExceptObject := nil;
          end;
          {$ENDIF}

          {$IFDEF D60}
          ReleaseExceptionObject;
          {$ENDIF}
        end;
      finally
        // InSendMessage falla si se llama desde el mismo thread
        if CallRec.Mode = tcAsync then Dispose(CallRec);
      end;
    end else
    if FWindow <> nil then
      Result := DefWindowProc(FWindow.Value, Msg, wParam, lParam);
end;

end.
