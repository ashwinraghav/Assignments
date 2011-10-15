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

unit SilSmLayerDispatcher;

{$include Defines.inc}

interface

uses
  Sil,
  SilScLayer,
  SilSiLayer,
  SilSmLayer;

const
  TD_BASE     = $400;
  TD_RECEIVE  = Succ(TD_BASE);
  TD_WRITE    = Succ(TD_RECEIVE);

type
  TSilLayerDispatcher = class (
    // extends
    TSilLayer,
    // implements
    IDispatchable)
  private
    FReceiveThread: IThread;
    FWriteThread: IThread;
    FReceiveQueue: IInterfaceQueue;
    FWriteQueue: IInterfaceQueue;
  private
    procedure ReceiveThreadRun(var Msg: RThreadRunMessage); message TD_RECEIVE;
    procedure WriteThreadRun(var Msg: RThreadRunMessage); message TD_WRITE;
  protected // ILayerControl
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
  protected // ILayerOperation
    procedure Write(const Command: ILayerCommand); override;
    procedure Receive(const Command: ILayerCommand); override;
  end;

implementation

{ TSilLayerDispatcher }

procedure TSilLayerDispatcher.LayerActivate(const Link: ILayerLink; const Context: IInterface);
var
  Value: Variant;
begin
  if (FReceiveQueue = nil) and (not Parameters.Find('AsyncReceive', Value) or Value) then
  begin
    FReceiveQueue := Sil.List.InterfaceQueue;
    FReceiveThread := Sil.OS.Thread.Spawn(TD_RECEIVE, Self);
  end;

  if (FWriteQueue = nil) and (not Parameters.Find('AsyncWrite', Value) or Value) then
  begin
    FWriteQueue := Sil.List.InterfaceQueue;
    FWriteThread := Sil.OS.Thread.Spawn(TD_Write, Self);
  end;
end;

procedure TSilLayerDispatcher.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  Sil.List.CancelQueue(FReceiveQueue);
  Sil.List.CancelQueue(FWriteQueue);

  Sil.OS.Thread.Wait(FReceiveThread);
  Sil.OS.Thread.Wait(FWriteThread);

  FReceiveQueue := nil;
  FWriteQueue := nil;
  FReceiveThread := nil;
  FWriteThread := nil;
end;

procedure TSilLayerDispatcher.Receive(const Command: ILayerCommand);
begin
  if Assigned(FReceiveQueue) then
    FReceiveQueue.Put(Command) else
    inherited;
end;

procedure TSilLayerDispatcher.Write(const Command: ILayerCommand);
begin
  if Assigned(FWriteQueue) then
    FWriteQueue.Put(Command) else
    inherited;
end;

procedure TSilLayerDispatcher.ReceiveThreadRun(var Msg: RThreadRunMessage);
var
  Command: ILayerCommand;
begin
  try
    while FReceiveQueue.Get(ILayerCommand, Command) do
      inherited Receive(Command);
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.Run');
  end;
end;

procedure TSilLayerDispatcher.WriteThreadRun(var Msg: RThreadRunMessage);
var
  Command: ILayerCommand;
begin
  try
    while FWriteQueue.Get(ILayerCommand, Command) do
      inherited Write(Command);
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.Run');
  end;
end;

end.
 