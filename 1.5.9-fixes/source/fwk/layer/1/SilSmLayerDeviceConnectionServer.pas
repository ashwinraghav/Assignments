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

unit SilSmLayerDeviceConnectionServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilScLayer,
  SilSiLayer,
  SilSmLayer;

const
  THREAD_LISTENER   = 1024;
  THREAD_DISPATCHER = 1025;

type
  TSilLayerConnectionServer = class (
    // extends
    TSilLayer,
    // implements
    IDispatchable,
    ILayerTerminal,
    ILayerActivationEvents)
  private
    FListener: IThread;
    FDispatcher: IThread;
    FQueue: IInterfaceQueue;
    FChainSource: ILayerChainSource;
    FChains: IInterfaceList;
    FStarted: IEvent;
    FChainId: LongWord;
    FLink: Pointer;
  private
    procedure DoDropAll;
    function DoWaitClient(out Client: IInterface): Boolean;
    function DoGetChainId: LongWord;
    function DoFillChain(const Chain: ILayerChain; const Client: IUnknown): Boolean;
    procedure ThreadListener(var Msg: RThreadRunMessage); message THREAD_LISTENER;
    procedure ThreadDispatcher(var Msg: RThreadRunMessage); message THREAD_DISPATCHER;
  protected
    procedure DoActivate; virtual;
    procedure DoDeactivate(IsBroken: Boolean); virtual;
    procedure DoSetupClient(const Client: IUnknown); virtual;
    function DoCreateClient(const Client: IUnknown): IUnknown; virtual; abstract;
    function DoAcceptClient(out Client: IUnknown): Boolean; virtual; abstract;
  protected
    function GetIsActive: Boolean; override;
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
  protected // ILayerActivationEvents
    procedure OnLayerActivated(const Event: RLayerActivated); virtual;
    procedure OnLayerDeactivated(const Event: RLayerDeactivated); virtual;
  protected // ILayerTerminal
    procedure StartLayer(const Context: IUnknown);
    procedure StopLayer(const Context: IUnknown);
    function GetLink: ILayerLink;
    property Link: ILayerLink read GetLink;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLayer,
  SilSmLayerChain,
  SilSmLayerLinkList;

{ TSilLayerConnectionServer }

constructor TSilLayerConnectionServer.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited;

  FChainId := 0;
  FChains := Sil.List.InterfaceList(true);
  FStarted := Sil.OS.Ipc.Event;
end;

destructor TSilLayerConnectionServer.Destroy;
begin
  DoDropAll;
  FChains := nil;
  FChainSource := nil;
  FStarted := nil;

  inherited;
end;

procedure TSilLayerConnectionServer.DoDropAll;
var
  Enum: IEnumerator;
  Chain: ILayerChain;
begin
  if Assigned(FChains) then
    while FChains.Enumerate(Enum, Chain) do
      Chain.Control.Deactivate;

  FChains.Clear;
end;

procedure TSilLayerConnectionServer.LayerActivate(const Link: ILayerLink; const Context: IInterface);
var
  Obj: IUnknown;
begin
  FQueue := Sil.List.InterfaceQueue;

  MakeRef(Link, @FLink);
  DoActivate;

  if Link.Chain.GetAfter(Link, Obj) then
    Ref.GetInterface(Obj, ILayerChainSource, FChainSource);
end;

procedure TSilLayerConnectionServer.StartLayer(const Context: IUnknown);
begin
  try
    if not Assigned(FDispatcher) then
    begin
      FDispatcher := Sil.OS.Thread.Spawn(THREAD_DISPATCHER, 'ConnDisp', Self);
      FListener := Sil.OS.Thread.Spawn(THREAD_LISTENER, 'ConnList', Self);
      FStarted.WaitFor;
    end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.StartLayer');
  end;
end;

procedure TSilLayerConnectionServer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  DoDeactivate(IsBroken);

  Sil.List.CancelQueue(FQueue);
  Sil.OS.Thread.Wait(FListener);
  Sil.OS.Thread.Wait(FDispatcher);

  FDispatcher := nil;
  FListener := nil;
  FQueue := nil;
end;

procedure TSilLayerConnectionServer.StopLayer(const Context: IUnknown);
begin
end;

function TSilLayerConnectionServer.GetLink: ILayerLink;
begin
  Result := ILayerLink(FLink);
end;

procedure TSilLayerConnectionServer.DoActivate;
begin
end;

procedure TSilLayerConnectionServer.DoDeactivate(IsBroken: Boolean);
begin
end;

function TSilLayerConnectionServer.GetIsActive: Boolean;
begin
  Result := FStarted.IsSignaled;
end;

procedure TSilLayerConnectionServer.ThreadListener(var Msg: RThreadRunMessage);
var
  Client: IUnknown;
begin
  try
    FStarted.Signal;

    while DoWaitClient(Client) do
    begin
      if Debug.Check(dlConnection, CDebugLayer) then
        Sil.Trace.Log(ClassName + '.ThreadListener client connect');

      FQueue.Put(Client);
    end;

    FStarted.Reset;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.DoWaitClient');
  end;
end;

function TSilLayerConnectionServer.DoWaitClient(out Client: IUnknown): Boolean;
begin
  try
    Result := DoAcceptClient(Client);
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.DoWaitClient client connect');

    Result := false;
  end;
end;

procedure TSilLayerConnectionServer.ThreadDispatcher(var Msg: RThreadRunMessage);
var
  Client: IUnknown;
  Command: ILayerCommand;
begin
  try
    while FQueue.Get(IUnknown, Client) do
      try
        if Assigned(FChainSource) then
          DoSetupClient(Client) else
        if Ref.GetInterface(Client, ILayerCommand, Command) then
          Cmd.Receive(Command);
      except
        if Debug.Check(dlException, CDebugLayer) then
          Sil.Trace.Exception(ClassName + '.ThreadDispatcher');
      end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.ThreadDispatcher');
  end;
end;

function TSilLayerConnectionServer.DoGetChainId: LongWord;
begin
  Result := FChainId;
  Inc(FChainId);
end;

function TSilLayerConnectionServer.DoFillChain(const Chain: ILayerChain; const Client: IUnknown): Boolean;
begin
  Result := false;

  try
    if Assigned(FChainSource) then
    begin
      FChainSource.CreateChain(Chain, Client);
      Result := true;
      //if not Chain.Control.IsActive then Chain.Control.Activate;
    end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.DoFillChain');
  end;
end;

procedure TSilLayerConnectionServer.DoSetupClient(const Client: IInterface);
var
  Chain: ILayerChain;
begin
  Chain := TSilLayerChain.Create(nil);
  Chain.Id := DoGetChainId;
  Chain.Add(DoCreateClient(Client));

  if DoFillChain(Chain, Client) then
  begin
    FChains.Add(Chain);
    Sil.Sink.Connect(Chain, Self, false);

    if not Chain.Control.IsActive then Chain.Control.Activate(Chain);
  end else
    SilLayer.Tk.DropChain(Chain);
end;

procedure TSilLayerConnectionServer.OnLayerActivated(const Event: RLayerActivated);
begin
end;

procedure TSilLayerConnectionServer.OnLayerDeactivated(const Event: RLayerDeactivated);
var
  Chain: ILayerChain;
begin
  if Debug.Check(dlConnection, CDebugLayer) then
    Sil.Trace.Log(ClassName + '.OnLayerDeactivated client disconnect');

  FChains.Remove(Event.Link);
  Sil.Sink.Disconnect(Event.Link, Self);

  if Ref.GetInterface(Event.Link, ILayerChain, Chain) then
  begin
    if Assigned(FChainSource) then FChainSource.DestroyChain(Chain, Self);
    Chain.Clear(true);
  end;
end;

end.
