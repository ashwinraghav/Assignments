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

unit SilSmLayerConnectionImate;

interface

{$include Defines.inc}

uses
  Sil,

  SilSiLayer,
  SilSiLayerProtocolBlind,
  SilSiLayerProtocolEblis,
  SilSiLayerConnection,
  SilSiLayerConnectionImate;

type
  TSilLayerImateConnectionPeer = class (
    TSilObject,
    ILayerConnectionManager,
    ILayerClientConnectionManager,
    ILayerImateConnectionManager,
    IBlindProtocolHook,
    IEblisProtocolHook)
  private
    FManager: ILayerConnectionManager;
    FConnection: ILayerConnection;
    FBlind: IBlindProtocol;
    FEblis: IEblisProtocol;
    FCipherKey: String;
    FSlot: ILayerSlot;
  protected // ILayerConnectionManager
    procedure ILayerConnectionManager.Initialize = DoInitialize;
    procedure DoInitialize(const Connection: ILayerConnection; const Chain: ILayerChain);
    procedure Connected(const Connection: ILayerConnection); virtual;
    procedure Disconnected(const Connection: ILayerConnection); virtual;
    procedure ConnectFailed(const Connection: ILayerConnection); virtual;
  protected // IBlindProtocolHook
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindRequestEvent);
  protected // IEblisProtocolHook
    procedure OnNegotiate(var Event: REblisNegotiateEvent);
  protected
    procedure ProtocolRequest(var Event: RBlindRequestEvent); virtual;
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain); virtual;
  protected // ILayerImateConnectionManager
    function GetConnection: ILayerConnection;
    function InsertProtocol(const Protocol: IUnknown; const Listener: IUnknown; const ID: Integer; AsyncDispatch: Boolean): ILayerChain;
    function RequestProtocol(const Protocol: IUnknown; const IID: TGuid; out Chain: ILayerChain; const Listener: IUnknown; AsyncDispatch: Boolean): Boolean;
    procedure Start(const Params: IParameters);
    procedure Stop;
    property Connection: ILayerConnection read GetConnection;
  public
    constructor Create(const Manager: ILayerConnectionManager);
    destructor Destroy; override;
  end;

implementation

uses
  SilStCipher,

  SilSmLayerSlot,
  SilSmLayerChain,
  SilSmLayerPackerImate,
  SilSmLayerCipher,
  SilSmLayerProtocolBlind,
  SilSmLayerProtocolEblis,
  SilSmLayerProtocolImate,
  SilSmLayerConnection;

{ TSilLayerImateConnectionPeer }

constructor TSilLayerImateConnectionPeer.Create(const Manager: ILayerConnectionManager);
begin
  inherited Create;
  FManager := Manager;
end;

destructor TSilLayerImateConnectionPeer.Destroy;
begin
  FManager := nil;
  inherited;
end;

procedure TSilLayerImateConnectionPeer.DoInitialize(const Connection: ILayerConnection; const Chain: ILayerChain);
begin
  FCipherKey := '';

  if Connection.Mode = cmClientPeer then
    Initialize(Connection, Chain);

  FSlot := TSilLayerSlot.Create(nil);
  Chain.Add(TSilLayerPackerImate.Create(nil));
  Chain.Add(FSlot);

  FBlind := TSilBlindProtocol.Create(nil);

  //if Ref.Supports(Self, IBlindProtocolHook) then

  InsertProtocol(FBlind, IBlindProtocolHook(Self), 0, false);

  if Connection.Mode = cmClientPeer then
    Chain.Control.Activate else
    FSlot.Control.Activate;
end;

procedure TSilLayerImateConnectionPeer.Connected(const Connection: ILayerConnection);
var
  Eblis: IEblisProtocol;
  EblisChain: ILayerChain;
begin
  if Connection.Mode = cmClientPeer then
  begin
    Eblis := TSilEblisProtocol.Create(nil);

    if RequestProtocol(Eblis, IEblisProtocol, EblisChain, nil, true) then
    begin
      Eblis.Negotiate(FCipherKey, 10);
      Eblis := nil;

      FSlot.Remove(EblisChain);

      EblisChain.Control.Deactivate;
      EblisChain.Clear(true);
    end;
  end;

  if Assigned(FManager) then FManager.Connected(Connection);
end;

procedure TSilLayerImateConnectionPeer.Disconnected(const Connection: ILayerConnection);
begin
  if Assigned(FManager) then FManager.Disconnected(Connection);
end;

procedure TSilLayerImateConnectionPeer.Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
begin
  if Assigned(FManager) then FManager.Initialize(Connection, Chain);
end;

procedure TSilLayerImateConnectionPeer.ConnectFailed(const Connection: ILayerConnection);
var
  Client: ILayerClientConnectionManager;
begin
  if Ref.GetInterface(FManager, ILayerClientConnectionManager, Client) then
    Client.ConnectFailed(Connection);
end;

procedure TSilLayerImateConnectionPeer.OnNegotiate(var Event: REblisNegotiateEvent);
begin
  FCipherKey := Event.Key;
end;

procedure TSilLayerImateConnectionPeer.OnRequest(var Event: RBlindRequestEvent);
begin
  Event.Result := false;

  if Guid.Compare(Event.Protocol, IEblisProtocol) = 0 then
  begin
    if not Assigned(FEblis) then
    begin
      FEblis := TSilEblisProtocol.Create(nil);
      InsertProtocol(FEblis, Self, Event.Id, false);
    end;

    Event.Result := true;
  end else
    ProtocolRequest(Event);
end;

procedure TSilLayerImateConnectionPeer.OnText(var Event: RBlindTextEvent);
begin
end;

function TSilLayerImateConnectionPeer.InsertProtocol(const Protocol, Listener: IInterface; const ID: Integer; AsyncDispatch: Boolean): ILayerChain;
var
  Params: IParameterList;
begin
  Result := TSilLayerChain.Create(nil);
  FSlot.Add(Result);

  Params := Sil.List.Parameters;

  if Str.NotEmpty(FCipherKey) then
  begin
    Params['Cipher'] := CipherTool.SimpleMix;
    Params['Key'] := FCipherKey;

    Result.Add(TSilLayerCipher.Create(Params));
    Params.Clear;
  end;

  Params['AsyncDispatch'] := AsyncDispatch;
  Params['Id'] := Id;

  Result.Add(TSilImateProtocolLayer.Create(Params));
  Result.Add(Protocol);

  if Assigned(Listener) then
    Result.Add(Listener);

  if FSlot.Control.IsActive then Result.Control.Activate;
end;

function TSilLayerImateConnectionPeer.RequestProtocol(const Protocol: IInterface; const IID: TGuid; out Chain: ILayerChain; const Listener: IInterface; AsyncDispatch: Boolean): Boolean;
var
  Id: LongWord;
  ObjListener: IUnknown;
begin
  Result := FBlind.Request(IID, Id);

  if Assigned(Listener) then
    ObjListener := Listener else
    ObjListener := Self;

  if Result then Chain := InsertProtocol(Protocol, ObjListener, Id, AsyncDispatch);
end;

procedure TSilLayerImateConnectionPeer.ProtocolRequest(var Event: RBlindRequestEvent);
begin
//
end;

procedure TSilLayerImateConnectionPeer.Start(const Params: IParameters);
begin
  FConnection := TSilLayerConnectionPeer.Create;
  FConnection.Configure(Self);
  FConnection.Start;
end;

procedure TSilLayerImateConnectionPeer.Stop;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Stop;
    FConnection := nil;
  end;

  //FManager := nil;
end;

function TSilLayerImateConnectionPeer.GetConnection: ILayerConnection;
begin
  Result := FConnection;
end;

end.
