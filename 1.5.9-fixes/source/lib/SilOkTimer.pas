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

unit SilOkTimer;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,

  SilLiEventList,
  SilLiConnection,
  SilOiHandle,
  SilOiTimer;

type
  TSilTimer = class (
    // extend
    TSilInterfacedObject,
    // implements
    ITimer )
  private
    FID: Cardinal;
    FInterval: Cardinal;
    FEnabled: Boolean;
    FTickCount: Integer;
    FTicks: Integer;
    FName: String;
  protected // ITimer
    function GetInterval: Cardinal;
    function GetID: Cardinal;
    function GetEnabled: Boolean;
    function GetTickCount: Integer;
    procedure SetTickCount(Value: Integer);
    function GetName: String;
    procedure SetInterval(Value: Cardinal); virtual;
    procedure SetID(Value: Cardinal); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetName(const Value: String); virtual;
    procedure Tick;
    procedure Restart;
  protected //-
    procedure DoUpdate(OldState, NewState: Boolean); virtual;
    procedure DoSetTimer; virtual; abstract;
    procedure DoKillTimer; virtual; abstract;
    procedure DoTick; virtual;
    procedure DoFireTick; virtual;
  public
    constructor Create(const ID, Interval: Cardinal; Enabled: Boolean); overload; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure KillTimer;
    procedure SetTimer;
  public
    property ID: Cardinal read FID;
    property Interval: Cardinal read FInterval;
    property Enabled: Boolean read FEnabled;
    property TickCount: Integer read FTickCount;
    property Ticks: Integer read FTicks;
    property Name: String read FName;
  end;

implementation

uses
  SilBtDateTime,
  SilLiEnumerator;

{ TSilTimer }

constructor TSilTimer.Create(const ID, Interval: Cardinal; Enabled: Boolean);
begin
  inherited Create;
  FID := ID;
  FInterval := Interval;
  FEnabled := Enabled;
  FTickCount := -1;
end;

destructor TSilTimer.Destroy;
begin
  inherited;
end;

procedure TSilTimer.AfterConstruction;
begin
  inherited;
  DoUpdate(False, FEnabled);
end;

procedure TSilTimer.BeforeDestruction;
begin
  DoUpdate(FEnabled, false);
  inherited;
end;

procedure TSilTimer.SetTimer;
begin
  DoSetTimer;
  FTicks := FTickCount;
end;

procedure TSilTimer.KillTimer;
begin
  DoKillTimer;
end;

function TSilTimer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TSilTimer.GetID: Cardinal;
begin
  Result := FID;
end;

function TSilTimer.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

procedure TSilTimer.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    DoUpdate(FEnabled, Value);
    FEnabled := Value;
  end;
end;

procedure TSilTimer.SetID(Value: Cardinal);
begin
  if FID <> Value then
  begin
    FID := Value;
    DoUpdate(FEnabled, FEnabled);
  end;
end;

procedure TSilTimer.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    DoUpdate(FEnabled, FEnabled);
  end;
end;

procedure TSilTimer.DoUpdate(OldState, NewState: Boolean);
begin
  if OldState then KillTimer;
  if NewState then SetTimer;
end;

procedure TSilTimer.DoTick;
begin
  if FTicks > -1 then
  begin
    Dec(FTicks);
    if FTicks < 1 then SetEnabled(false);
  end;
  DoFireTick;
end;

procedure TSilTimer.DoFireTick;
var
  Enum: IEnumerator;
  Sink: ITimerEvents;
  Event: RTimerEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Time := DateTime.Now;
    
    while Events.Enumerate(Enum, Sink, ITimerEvents) do
      Sink.OnTick(Event);
  end;
end;

procedure TSilTimer.Tick;
begin
  DoTick;
end;

procedure TSilTimer.Restart;
begin
  FEnabled := true;
  DoUpdate(FEnabled, FEnabled);
end;

function TSilTimer.GetTickCount: Integer;
begin
  Result := FTickCount;
end;

procedure TSilTimer.SetTickCount(Value: Integer);
begin
  FTickCount := Value;
  DoUpdate(FEnabled, FEnabled);
end;

function TSilTimer.GetName: String;
begin
  Result := FName;
end;

procedure TSilTimer.SetName(const Value: String);
begin
  FName := Value;
end;

end.
