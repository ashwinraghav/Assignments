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
  SilLiEnumerator,
  SilOiHandle,
  SilOiThread,
  SilOkThreadList,

  SilLmInterfaceList;

const
  SIL_EXECPROC = {WM_USER +} 1;

type
  PCallRec = ^TCallRec;
  TCallRec = record
    Obj: IUnknown;
    //Method: TMethod;
    Mode: TThreadCallMode;
    Param: Pointer;
    Unref: Boolean;
  end;

type
  TSilLinuxThreadList = class (
    // extends
    TSilThreadList,
    // implements
    IThreads,
    IThreadList)
  private
    FSynchronizeException: TObject;
    //FWindow: IHandle;
    procedure WindowProc(var AMessage);
  protected // IThreadList
    procedure Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref); override;
    procedure ThreadCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilBtVart,
  SilOtTool(*),
  SilStGlobalServices(*);

{ TSilLinuxThreadList }

constructor TSilLinuxThreadList.Create;
begin
  inherited Create;
  //FWindow := OS.ToolWindow.Create(WindowProc);
end;

destructor TSilLinuxThreadList.Destroy;
begin
  Clear;
  //FWindow := nil;
  inherited;
end;

procedure TSilLinuxThreadList.Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref);
(*)var
  CallRec: PCallRec;
  bDispose: Boolean;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxThreadList.Call']);
(*)
  New(CallRec);
  CallRec.Thread := Thread;
  CallRec.Method := Method;
  CallRec.Mode := Mode;
  CallRec.Ref := @Ref;

  FSynchronizeException := nil;
  bDispose := true;

  case Mode of
    tcSync:   SendMessage(FWindow.Value, SIL_EXECPROC, Integer(CallRec), 0);
    tcAsync:  bDispose := not PostMessage(FWindow.Value, SIL_EXECPROC, Integer(CallRec), 0);
  end;

  if bDispose then Dispose(CallRec);
  if Assigned(FSynchronizeException) then raise FSynchronizeException;
(*)end;

procedure TSilLinuxThreadList.ThreadCall(const Obj: IInterface; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxThreadList.Call']);
end;

procedure TSilLinuxThreadList.WindowProc(var AMessage);
(*)var
  CallRec: PCallRec;
  RaisePtr: Pointer;(*)
begin(*)
  with TMessage(AMessage) do
    if Msg = SIL_EXECPROC then
    begin
      CallRec := nil;
      try
        try
          FSynchronizeException := nil;
          CallRec := PCallRec(wParam);
          CallRec.Method(CallRec.Thread, CallRec.Ref^);
        except
          {$IFDEF D60}
          RaisePtr := AcquireExceptionObject;
          {$ELSE}
          RaisePtr := RaiseList;
          {$ENDIF}

          if RaisePtr <> nil then
          begin
            FSynchronizeException := PRaiseFrame(RaisePtr)^.ExceptObject;
            PRaiseFrame(RaisePtr)^.ExceptObject := nil;
          end;

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
(*)end;

end.
