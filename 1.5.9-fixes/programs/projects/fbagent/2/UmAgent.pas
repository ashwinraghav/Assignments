{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit UmAgent;

interface

{$include Defines.inc}

uses
  Sil,

  UiAgent,
  UiAgentProtocol;

type
  TAgent = class (TSilObject, IAgent, IRunnable)
  private
    FThread: IThread;
    FStop: IEvent;
    FSource: IAgentSource;
    FDispatch: IAgentDispatch;
    FServiceMutex: IMutex;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IAgent
    procedure Start;
    procedure Stop;
  public
    constructor Create(const Source: IAgentSource);
    destructor Destroy; override;
  end;

implementation

uses
  UmAgentDispatch,
  UfConfig;

{ TAgent }

constructor TAgent.Create(const Source: IAgentSource);
begin
  Sil.Trace.Enter(Self, 'Create');
  Sil.Trace.Log('Starting server %s %s', [FBAGENT_GUI_TITLE, FBAGENT_GUI_VERSION]);

  inherited Create;
  FSource := Source;

  Sil.Trace.Leave;
end;

destructor TAgent.Destroy;
begin
  Sil.Trace.Enter(Self, 'Destroy');

  FSource := nil;
  inherited;

  Sil.Trace.Leave;
end;

procedure TAgent.Run(const Thread: IThread);
var
  Data: ISchedule;
begin
  Sil.Trace.Enter(Self, 'Run');

  repeat
    while FSource.Get(Data) do
    begin
      FDispatch.Put(Data);
    end;
  until FStop.WaitFor(60000) <> wrTimeout;

  Sil.Trace.Leave;
end;

procedure TAgent.Start;
begin
  Stop;
  Sil.Trace.Enter(Self, 'Start');

  FDispatch := TAgentDispatch.Create;
  FDispatch.Start;

  FStop := Sil.OS.Ipc.Event;
  FThread := Sil.OS.Thread.Spawn('agent', Self);
  FServiceMutex := Sil.OS.Ipc.Mutex(true, 'FirebirdSQLAgent_Mutex');

  Sil.Trace.Leave;
end;

procedure TAgent.Stop;
begin
  Sil.Trace.Enter(Self, 'Stop');

  FServiceMutex := nil;

  if Assigned(FDispatch) then FDispatch.Stop;
  if Assigned(FStop) then FStop.Signal;
  if Assigned(FThread) then FThread.Termination.WaitFor(INFINITE, true);

  FStop := nil;
  FThread := nil;
  FDispatch := nil;

  Sil.Trace.Leave;
end;

end.
