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

unit SilOiTimer;

{$I Defines.inc}

interface

uses
  SilOiHandle;

type
  ITimer = interface;

  RTimerEvent = record
    Sender: ITimer;
    Time: TDateTime;
  end;

  ITimerEvents = interface
    ['{FD203309-0B70-11D4-9155-00C0261013CD}']
    procedure OnTick(const Event: RTimerEvent);
  end;

  ITimer = interface
    ['{8FE2B644-0B31-11D4-987A-00104B0FA1EF}']
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetID: Cardinal;
    procedure SetID(Value: Cardinal);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetTickCount: Integer;
    procedure SetTickCount(Value: Integer);
    function GetName: String;
    procedure SetName(const Value: String);
    procedure Tick;
    procedure Restart;
    property Interval: Cardinal read GetInterval write SetInterval;
    property ID: Cardinal read GetID write SetID;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property TickCount: Integer read GetTickCount write SetTickCount;
    property Name: String read GetName write SetName;
  end;

implementation

end.
