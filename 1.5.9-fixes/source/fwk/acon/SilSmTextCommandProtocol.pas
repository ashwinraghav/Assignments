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

unit SilSmTextCommandProtocol;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLiConnection,
  SilOiIpc,
  SilLiEventList,
  SilSiProtocolBase,
  SilSiAbstractConnection,
  SilSmTextLineCompleter;

type
  TTextCommandProtocol = class (
    // extends
    TSilInterfacedObject,
    IConnectedEvents,
    IProtocol)
  protected
    FConnection: IAbstractConnection;
    FError: Boolean;
    FIsLogged: Boolean;
    FEvents: IEventList;
    FReadEvent: IEvent;
    FBuffer: String;
    FState: Word;
    FWaitState: Word;
  protected
    procedure ReceiveLine; virtual; abstract;
    function SendCommand(const Text: String; WaitState: Word): Boolean;
    procedure WaitDataInput;
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent); virtual;
    procedure OnDataSent(const Event: TConnectionDataEvent); virtual;
    procedure OnDisconnected(const Event: TConnectionBreakEvent); virtual;
  protected // IProtocol
    function GetConnection: IAbstractConnection;
    procedure SetConnection(const Value: IAbstractConnection); virtual;
  public
    constructor Create(const Connection: IAbstractConnection); virtual;
    destructor Destroy; override;
  end;

const
  ccCRLF = #13#10;
  ccSpc = #32;

implementation

uses
  SilLtConnection,
  SilOtTool,
  SilOiEventQueue,
  SilBtStr,
  SilOsTypes;

constructor TTextCommandProtocol.Create(const Connection: IAbstractConnection);
begin
  inherited Create;

  FReadEvent := OS.IPC.Event();
  SetConnection(Connection);
  FIsLogged := false;
end;

destructor TTextCommandProtocol.Destroy;
begin
  if FConnection <> nil then Sink.Disconnect(FConnection, Self);
  inherited;
end;

procedure TTextCommandProtocol.WaitDataInput;
begin
  if (FConnection.Thread <> nil) and not FConnection.Thread.IsCurrent then
  begin
    FReadEvent.WaitFor(INFINITE, true);
    FReadEvent.Reset;
  end else
    FConnection.WaitDataInput;
end;

function TTextCommandProtocol.SendCommand(const Text: String; WaitState: Word): Boolean;
begin
  Result := false;
  if FConnection = nil then Exit;

  FReadEvent.Reset;
  if Length(Text) > 0 then FConnection.WriteStr(Text + ccCRLF);

  FError := false;
  FBuffer := Str.Null;
  FWaitState := WaitState;

  while FConnection.IsConnected and not FError and not Result do
  begin
    WaitDataInput;

    if not FError then FError := not FConnection.IsConnected or (Length(FBuffer) < 1);
    if not FError then Result := (FState = WaitState);
  end;
end;

function TTextCommandProtocol.GetConnection: IAbstractConnection;
begin
  Result := FConnection;
end;

procedure TTextCommandProtocol.SetConnection(const Value: IAbstractConnection);
begin
  FConnection := Value;
  if FConnection <> nil then FConnection.PacketCompletion := TTextLineCompleter.Create;
end;

procedure TTextCommandProtocol.OnDataReceived(const Event: TConnectionDataEvent);
begin
  SetString(FBuffer, Event.Buffer, Event.Size);
  ReceiveLine;
  FReadEvent.Signal;
end;

procedure TTextCommandProtocol.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  FReadEvent.Signal;
  Sink.Disconnect(Event.Sender, Self);
end;

procedure TTextCommandProtocol.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

end.
