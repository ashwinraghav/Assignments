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

unit SilVmConnectionListener;

interface

uses
  Classes,
  Sil,
  SilTool,
  SilSiAbstractConnection;

type
  TConnectionProc = procedure(const Event: TConnectionEvent) of object;
  TConnectionFailedProc = procedure(const Event: TConnectionFailedEvent) of object;

  TConnectionListener = class(TComponent, IConnectingEvents, IUnknown)
  private
    FOnConnected: TConnectionProc;
    FOnFailed: TConnectionFailedProc;
    procedure IConnectingEvents.OnConnected = DoConnected;
    procedure IConnectingEvents.OnFailed = DoFailed;
    procedure DoConnected(const Event: TConnectionEvent);
    procedure DoFailed(const Event: TConnectionFailedEvent);
  published
    property OnConnected: TConnectionProc read FOnConnected write FOnConnected;
    property OnFailed: TConnectionFailedProc read FOnFailed write FOnFailed;
  end;

implementation

{ TConnectionListener }

procedure TConnectionListener.DoConnected(const Event: TConnectionEvent);
begin
  if Assigned(FOnConnected) then FOnConnected(Event);
end;

procedure TConnectionListener.DoFailed(const Event: TConnectionFailedEvent);
begin
  if Assigned(FOnFailed) then FOnFailed(Event);
end;

end.
