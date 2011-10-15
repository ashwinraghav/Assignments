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

unit SilVmSocketProt;

interface

uses
  SysUtils, Forms, Classes,

  Sil, SilTool,
  SilProtocol,
  SilViConnection,
  SilSiAbstractConnection,
  SilSiSocketConnection;

type
  TSilSocketProtocol = class (
    // extends
    TComponent,
    // implements
    IUnknown,
    IConnectingEvents,
    IConnectedEvents,
    IBlindProtocolEvents)
  private
    FHost: String;
    FPort: Word;
    FTimeout: LongWord;
    FConnection: IClientSocketConnection;
    FBlind: IBlindProtocol;
    FRetryInterval: LongWord;
    FClients: IInterfaceList;
    FAutoConnect: IInterfaceList;
    FConnected: IEvent;
    procedure SetActive(Value: Boolean);
    procedure SetHost(const Value: String);
    procedure SetTimeout(const Value: LongWord);
    function GetBlind: IBlindProtocol;
    function GetSock: IClientSocketConnection;
    procedure SetPort(const Value: Word);
    function DoConnect: Boolean;
    function DoDisconnect: Boolean;
    procedure DoRelease(All: Boolean);
    procedure SetRetryInterval(const Value: LongWord);
    function GetActive: Boolean;
    procedure DoDeactivateChildren;
  protected // IConnectingEvents
		procedure OnConnected(const Event: TConnectionEvent);
		procedure OnFailed(const Event: TConnectionFailedEvent);
  protected // IConnectedEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent);
    procedure OnDataSent(const Event: TConnectionDataEvent);
    procedure OnDisconnected(const Event: TConnectionBreakEvent);
  protected // IBlindProtocolEvents
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindProtocolIDEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClient(const Obj: IUnknown);
    procedure RemoveClient(const Obj: IUnknown);
    procedure Open;
    procedure Close;
    property Blind: IBlindProtocol read GetBlind;
    property Connection: IClientSocketConnection read GetSock;
  published
    property Host: String read FHost write SetHost;
    property Port: Word read FPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
    property Timeout: LongWord read FTimeout write SetTimeout;
    property RetryInterval: LongWord read FRetryInterval write SetRetryInterval;
  end;

procedure Register;

implementation

uses
  SilStSocketConnection;

procedure Register;
begin
  RegisterComponents('SIL', [TSilSocketProtocol]);
end;

{ TSilSocketProtocol }

constructor TSilSocketProtocol.Create(AOwner: TComponent);
begin
  inherited;
  FRetryInterval := 0;
  FClients := Sil.List.InterfaceList(true);
  FAutoConnect := Sil.List.InterfaceList(true);
  FConnected := Sil.OS.Ipc.Event;
end;

destructor TSilSocketProtocol.Destroy;
begin
  while FClients.Count > 0 do
    RemoveClient(FClients.Last);

  SetActive(false);
  DoRelease(true);

  FClients := nil;
  FAutoConnect := nil;

  inherited;
end;

function TSilSocketProtocol.GetBlind: IBlindProtocol;
begin
  Result := FBlind;
end;

function TSilSocketProtocol.GetSock: IClientSocketConnection;
begin
  Result := FConnection;
end;

procedure TSilSocketProtocol.OnDataReceived(const Event: TConnectionDataEvent);
begin
end;

procedure TSilSocketProtocol.OnDataSent(const Event: TConnectionDataEvent);
begin
end;

procedure TSilSocketProtocol.OnRequest(var Event: RBlindProtocolIDEvent);
begin
end;

procedure TSilSocketProtocol.OnText(var Event: RBlindTextEvent);
begin
end;

function TSilSocketProtocol.DoConnect: Boolean;
begin
  FConnection := SilStSocketConnection.SocketConnection.CreateClient(FHost, FPort);
  FConnection.RetryInterval := FRetryInterval;
  Sil.Sink.Connect(FConnection, Self);
  FConnected.Reset;
  FConnection.Connect;
  if FTimeout = 0 then
    FConnected.WaitFor(INFINITE) else
    FConnected.WaitFor(FTimeout);
  Result := FConnection.IsConnected;
end;

procedure TSilSocketProtocol.OnConnected(const Event: TConnectionEvent);
var
  e: IEnumerator;
  Obj: IUnknown;
  Item: IConnectionClient;
begin
  try
    FBlind := SilProtocol.Tk.Blind(Event.Sender);
    FConnected.Signal;

    while FAutoConnect.Enumerate(e, Obj) do
      if Ref.GetInterface(Obj, IConnectionClient, Item) then
        Item.Active := true;

    FAutoConnect.Clear;
  except
    on ex: Exception do
    begin
      // log
    end;
  end;
end;

procedure TSilSocketProtocol.OnDisconnected(const Event: TConnectionBreakEvent);
var
  e: IEnumerator;
  Obj: IUnknown;
  Item: IConnectionClient;
begin
  if Event.ConnectionLost and (FRetryInterval > 0) then
  begin
    while FClients.Enumerate(e, Obj) do
      if Ref.GetInterface(Obj, IConnectionClient, Item) and Item.Active then
        FAutoConnect.Add(Obj);
  end;

  DoRelease(false);
end;                              

procedure TSilSocketProtocol.DoRelease(All: Boolean);
begin
  DoDeactivateChildren;

  if All and (FConnection <> nil) then
  begin
    Sil.Sink.Disconnect(FConnection, Self);
    FConnection := nil;
  end;

  if FBlind <> nil then
  begin
    Sil.Sink.Disconnect(FBlind, Self);
    FBlind := nil;
  end;
end;

function TSilSocketProtocol.DoDisconnect: Boolean;
begin
  if FConnection <> nil then FConnection.Disconnect;
  Result := true
end;

procedure TSilSocketProtocol.OnFailed(const Event: TConnectionFailedEvent);
begin
  FConnected.Signal;
end;

procedure TSilSocketProtocol.DoDeactivateChildren;
var
  i: Integer;
  Item: IConnectionClient;
begin
  for i := FClients.Count - 1 downto 0 do
    if Ref.GetInterface(FClients[i], IConnectionClient, Item) then
      Item.Active := false;
end;

procedure TSilSocketProtocol.SetActive(Value: Boolean);
begin
  if ((FConnection = nil) and Value) or ((FConnection <> nil) and (FConnection.IsConnected <> Value)) then
  begin
    DoDeactivateChildren;

    if Value then
      DoConnect else
      DoDisconnect;
  end;
end;

procedure TSilSocketProtocol.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TSilSocketProtocol.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TSilSocketProtocol.SetTimeout(const Value: LongWord);
begin
  FTimeout := Value;
end;

procedure TSilSocketProtocol.SetRetryInterval(const Value: LongWord);
begin
  FRetryInterval := Value;
end;

procedure TSilSocketProtocol.AddClient(const Obj: IUnknown);
begin
  FClients.Add(Obj);
end;

procedure TSilSocketProtocol.RemoveClient(const Obj: IUnknown);
var
  Item: IConnectionClient;
begin
  if FClients.Remove(Obj) >= 0 then
    if Ref.GetInterface(Obj, IConnectionClient, Item) then
      Item.Cleanup;
end;

function TSilSocketProtocol.GetActive: Boolean;
begin
  Result := (FConnection <> nil) and FConnection.IsConnected;
end;

procedure TSilSocketProtocol.Close;
begin
  Active := false;
end;

procedure TSilSocketProtocol.Open;
begin
  Active := true;
end;

end.

