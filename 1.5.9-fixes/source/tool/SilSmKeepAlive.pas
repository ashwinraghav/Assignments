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

unit SilSmKeepAlive;

interface

{$INCLUDE Defines.inc}

uses
  Sil,
  SilSiKeepAlive;

type
  TSilKeepAlive = class (
    // extends
    TSilInterfacedObject,
    // implements
    IKeepAlive,
    IRunnable)
  private
    FThread: IThread;
    FMustStop: IEvent;
    FInterval: LongWord;
  private
    procedure DoFireKeepAlive(out Success: Boolean);
  protected // IKeepAlive
    procedure Start(Interval: LongWord);
    procedure Stop;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSilKeepAlive }

constructor TSilKeepAlive.Create;
begin
  inherited Create;
end;

destructor TSilKeepAlive.Destroy;
begin
  inherited;
end;

procedure TSilKeepAlive.Start(Interval: LongWord);
begin
  FInterval := Interval;
  FMustStop := Sil.OS.Ipc.Event;
  FThread := Sil.OS.Thread.Spawn('sil$keepalive', Self);
end;

procedure TSilKeepAlive.Stop;
begin
  if Assigned(FThread) then
  begin
    FMustStop.Signal;

    if FThread.ThreadID <> Sil.OS.Thread.ID then
    begin
      FThread.Termination.WaitFor(INFINITE, true);

      FMustStop := nil;
      FThread := nil;
    end;
  end;
end;

procedure TSilKeepAlive.Run(const Thread: IThread);
var
  Success: Boolean;
begin
  repeat
    Success := true;
    DoFireKeepAlive(Success);
  until Success or (FMustStop.WaitFor(FInterval) <> wrTimeout);
end;

procedure TSilKeepAlive.DoFireKeepAlive(out Success: Boolean);
var
  Enum: IEnumerator;
  Sink: IKeepAliveEvents;
begin
  try
    if HasConnections then
      while Events.Enumerate(Enum, Sink, IKeepAliveEvents) do
        Sink.OnKeepAlive(Self, Success);
  except
    Success := false;
  end;
end;

end.
