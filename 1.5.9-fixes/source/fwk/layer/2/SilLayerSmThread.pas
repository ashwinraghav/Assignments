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

unit SilLayerSmThread;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  SilLayerSiGlobal,
  SilLayerSkLayer;

const
  TH_BASE     = $0400;
  TH_WRITE    = TH_BASE + 1;
  TH_READ     = TH_BASE + 2;

type
  TSilLayerThread = class (TSilLayer, IDispatchable)
  private
    FWrite: IThread;
    FRead: IThread;
    FWriteQueue: IInterfaceQueue;
    FStop: IEvent;
  private
    procedure RunWrite(var Msg: RThreadRunMessage); message TH_WRITE;
    procedure RunRead(var Msg: RThreadRunMessage); message TH_READ;
  protected
    procedure DoActivate(const Context: IUnknown = nil); override;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); override;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; override;
    function DoGetIsActive: Boolean; override;
  end;

implementation

uses
  SilLayerSmToken;

{ TSilLayerThread }

procedure TSilLayerThread.DoActivate(const Context: IInterface);
begin
  FStop := Sil.OS.Ipc.Event;

  if GetParams.Contains('write') then
  begin
    FWriteQueue := Sil.List.InterfaceQueue;
    FWrite := Sil.OS.Thread.Spawn(TH_WRITE, Self);
  end;

  if GetParams.Contains('read') then
    FRead := Sil.OS.Thread.Spawn(TH_READ, Self);
end;

procedure TSilLayerThread.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  if FStop <> nil then
    FStop.Signal;
    
  if FWriteQueue <> nil then
    FWriteQueue.Cancel(true);

  if (FWrite <> nil) and not FWrite.IsCurrent then
  begin
    FWrite.Termination.WaitFor(INFINITE, true);
    FWrite := nil;
    FWriteQueue := nil;
  end;

  if (FRead <> nil) and not FRead.IsCurrent then
  begin
    FRead.Termination.WaitFor(INFINITE, true);
    FRead := nil;
  end;
end;

function TSilLayerThread.DoGetIsActive: Boolean;
begin
  Result := (FWrite <> nil) or (FRead <> nil);
end;

function TSilLayerThread.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  if FWriteQueue <> nil then
    Result := FWriteQueue.Put(Packet)
  else
    Result := inherited DoWrite(Packet, Link);
end;

procedure TSilLayerThread.RunRead(var Msg: RThreadRunMessage);
var
  Link: ILayer;

  procedure DoReceivePacket;
  var
    Packet: ILayerToken;
  begin
    Packet := GetStack.GetToken;
    Link.Action.Read(Packet);
  end;

begin
  try
    if GetStack.GetLowerLink(Link) then
      while not FStop.IsSignaled and Link.Status.IsActive do
        DoReceivePacket;
  except
    on e: Exception do
      Sil.Trace.Exception('TSilLayerThread.RunRead');
  end;
end;

procedure TSilLayerThread.RunWrite(var Msg: RThreadRunMessage);
var
  Packet: ILayerToken;
  Link: ILayer;
begin
  try
    while FWriteQueue.Get(ILayerToken, Packet) do
      if inherited DoWrite(Packet, Link) and (Link <> nil) then
        Link.Action.Write(Packet);
  except
    on e: Exception do
      Sil.Trace.Exception('TSilLayerThread.RunWrite');
  end;
end;

end.

