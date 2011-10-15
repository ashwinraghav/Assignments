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

unit SilSiLayerConnection;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiLayer;

type
  ILayerConnection = interface;
  ILayerConnectionServer = interface;
  ILayerConnections = interface;
  ILayerConnectionList = interface;
  ILayerConnectionEvents = interface;
  ILayerConnectionManager = interface;
  ILayerClientConnectionManager = interface;
  ILayerConnectionClientEvents = interface;

  TLayerConnectionMode = (cmClientPeer, cmServerPeer, cmServer);

  ILayerConnectionManager = interface
    ['{63D8DFE5-14C8-499F-986D-E5C0A8ECD2BA}']
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
    procedure Connected(const Connection: ILayerConnection);
    procedure Disconnected(const Connection: ILayerConnection);
  end;

  ILayerClientConnectionManager = interface (ILayerConnectionManager)
    ['{037B2FFF-864B-4B1A-ADC2-F8453E9475DD}']
    procedure ConnectFailed(const Connection: ILayerConnection);
  end;

  ILayerConnection = interface
    ['{E2A0C1A8-AE92-4B83-9B00-91FD5B2A51EE}']
    function GetChain: ILayerChain;
    function GetContext: IUnknown;
    function GetManager: ILayerConnectionManager;
    function GetMode: TLayerConnectionMode;
    procedure SetContext(const Value: IUnknown);
    procedure Configure(const Manager: ILayerConnectionManager; const Params: IParameters = nil);
    procedure Start(const Params: IParameters = nil);
    procedure Stop;
    property Chain: ILayerChain read GetChain;
    property Context: IUnknown read GetContext write SetContext;
    property Manager: ILayerConnectionManager read GetManager;
    property Mode: TLayerConnectionMode read GetMode;
  end;

  ILayerConnectionServer = interface (ILayerConnection)
    ['{661AF1D3-4107-4C9E-BB0C-F9DBE261B5DD}']
    function GetClients: ILayerConnections;
    property Clients: ILayerConnections read GetClients;
  end;

  ILayerConnections = interface
    ['{DFBEE1B3-A453-42FD-B980-60847BD53EEC}']
    function GetCount: Integer;
    function GetItem(Index: Integer): ILayerConnection;
    function Locked: ILock;
    function IndexOf(const Value: IUnknown): Integer;
    function Enumerate(var Enum: IEnumerator; out Item: ILayerConnection): Boolean;
    property Items[Index: Integer]: ILayerConnection read GetItem; default;
    property Count: Integer read GetCount;
  end;

  ILayerConnectionList = interface (ILayerConnections)
    ['{87F75335-121A-44CE-BD07-A69B2EEA6662}']
    procedure Delete(Index: Integer);
    function Remove(const Value: IUnknown): Integer;
    procedure Clear;
    function Add(const Value: ILayerConnection): Integer;
  end;

  RLayerConnectionEvent = record
    Sender: ILayerConnection;
  end;

  ILayerConnectionEvents = interface
    ['{30E6698B-5ECA-43B8-9302-0E8AA951A350}']
    procedure OnConnected(const Event: RLayerConnectionEvent);
    procedure OnDisconnected(const Event: RLayerConnectionEvent);
  end;

  ILayerConnectionClientEvents = interface (ILayerConnectionEvents)
    ['{9151E0EB-A394-45FE-8452-50CD73B5ABC0}']
    procedure OnConnectionFailed(const Event: RLayerConnectionEvent);
  end;

implementation

end.
