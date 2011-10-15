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

unit SilOkWaveAudio;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilOiWaveAudio,
  SilLkInterfaced,
  SilLmInterfaceList;

type
  TSilWaveAudioBufferList = class (TSilInterfaceList, IWaveAudioBufferList)
  private
    FWaveAudio: Pointer;
  protected
    function GetWaveAudio: IWaveAudio;
  protected
    function Get(Index: Integer): IWaveAudioBuffer; virtual;
    procedure Put(Index: Integer; const Value: IWaveAudioBuffer); virtual;
    function Add(const Value: IWaveAudioBuffer): Integer; reintroduce; virtual;
    function First: IWaveAudioBuffer; reintroduce;
    function Last: IWaveAudioBuffer; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Item: IWaveAudioBuffer): Boolean; reintroduce;
  public
    constructor Create(const WaveAudio: IWaveAudio); virtual;
    destructor Destroy; override;
  end;

  TSilWaveAudioDeviceList = class (TSilInterfaceList, IWaveAudioDeviceList)
  protected
    function Get(Index: Integer): IWaveAudioDevice; virtual;
    procedure Put(Index: Integer; const Value: IWaveAudioDevice); virtual;
    function Add(const Value: IWaveAudioDevice): Integer; reintroduce; virtual;
    function First: IWaveAudioDevice; reintroduce;
    function Last: IWaveAudioDevice; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Item: IWaveAudioDevice): Boolean; reintroduce;
  end;

  TSilWaveAudio = class (TSilInterfacedObject, IWaveAudio, IWaveAudioControl)
  private
    FFormat: IWaveAudioFormat;
    FBuffers: IWaveAudioBufferList;
  protected
    FHookIn: IWaveAudioInHook;
    FHookOut: IWaveAudioOutHook;
    FMode: TWaveAudioMode;
    FBufferIndex: Integer;
    FIsRunning: Boolean;
    FIsLoopEnabled: Boolean;
    FIsDiscardEnabled: Boolean;
    FDevice: IWaveAudioDevice;
  protected
    procedure NextBuffer; virtual;
    function PrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean; virtual;
    function UnPrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean; virtual;
    function GetUsable: Boolean; virtual; abstract;
    function DoCreateWaveFormat: IWaveAudioFormat; virtual; abstract;
    function DoCreateWaveBufferList: IWaveAudioBufferList; virtual;
    function GetPosition: LongWord; virtual; abstract;
    function Initialize(Mode: TWaveAudioMode; const Hook: IUnknown): Boolean; virtual; abstract;
    procedure Finalize; virtual; abstract;
    function Capture: Boolean; virtual; abstract;
    function Reproduce: Boolean; virtual; abstract;
    function Reset: Boolean; virtual; abstract;
    function Stop: Boolean; virtual; abstract;
  protected
    function GetMode: TWaveAudioMode;
    function GetBufferIndex: Integer;
    function GetInHook: IWaveAudioInHook;
    function GetOutHook: IWaveAudioOutHook;
    function GetIsRunning: Boolean;
    function GetIsLoopEnabled: Boolean;
    function GetDevice: IWaveAudioDevice;
    procedure SetIsLoopEnabled(Value: Boolean);
    function GetIsDiscardEnabled: Boolean;
    procedure SetIsDiscardEnabled(Value: Boolean);
    function GetFormat: IWaveAudioFormat;
    function GetBuffers: IWaveAudioBufferList;
    property Format: IWaveAudioFormat read GetFormat;
    property Buffers: IWaveAudioBufferList read GetBuffers;
    property Position: LongWord read GetPosition;
  public
    constructor Create(const Device: IWaveAudioDevice);
    destructor Destroy; override;
  end;

implementation

{ TSilWaveAudioBufferList }

constructor TSilWaveAudioBufferList.Create(const WaveAudio: IWaveAudio);
begin
  inherited Create(true);
  MakeRef(WaveAudio, @FWaveAudio);
end;

destructor TSilWaveAudioBufferList.Destroy;
begin
  DropRef(@FWaveAudio);
  inherited;
end;

function TSilWaveAudioBufferList.Add(const Value: IWaveAudioBuffer): Integer;
begin
  Value.WaveAudio := GetWaveAudio;  
  Result := inherited Add(Value);
end;

function TSilWaveAudioBufferList.Enumerate(var Enum: IEnumerator; out Item: IWaveAudioBuffer): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilWaveAudioBufferList.First: IWaveAudioBuffer;
begin
  Result := IWaveAudioBuffer(inherited First);
end;

function TSilWaveAudioBufferList.Get(Index: Integer): IWaveAudioBuffer;
begin
  Result := IWaveAudioBuffer(inherited GetItem(Index));
end;

function TSilWaveAudioBufferList.Last: IWaveAudioBuffer;
begin
  Result := IWaveAudioBuffer(inherited Last);
end;

procedure TSilWaveAudioBufferList.Put(Index: Integer; const Value: IWaveAudioBuffer);
begin
  inherited SetItem(Index, Value);
end;

function TSilWaveAudioBufferList.GetWaveAudio: IWaveAudio;
begin
  Result := IWaveAudio(FWaveAudio);
end;

{ TSilWaveAudio }

constructor TSilWaveAudio.Create(const Device: IWaveAudioDevice);
begin
  inherited Create;

  FMode := wmUnknown;
  FDevice := Device;
  FFormat := DoCreateWaveFormat;
  FBuffers := DoCreateWaveBufferList;
  FBufferIndex := -1;
end;

destructor TSilWaveAudio.Destroy;
begin
  FFormat := nil;
  FBuffers := nil;

  FHookIn := nil;
  FHookOut := nil;

  inherited;
end;

function TSilWaveAudio.DoCreateWaveBufferList: IWaveAudioBufferList;
begin
  Result := TSilWaveAudioBufferList.Create(Self);
end;

function TSilWaveAudio.GetBufferIndex: Integer;
begin
  Result := FBufferIndex;
end;

function TSilWaveAudio.GetBuffers: IWaveAudioBufferList;
begin
  Result := FBuffers;
end;

function TSilWaveAudio.GetDevice: IWaveAudioDevice;
begin
  Result := FDevice;
end;

function TSilWaveAudio.GetFormat: IWaveAudioFormat;
begin
  Result := FFormat;
end;

function TSilWaveAudio.GetInHook: IWaveAudioInHook;
begin
  Result := FHookIn;
end;

function TSilWaveAudio.GetIsDiscardEnabled: Boolean;
begin
  Result := FIsDiscardEnabled;
end;

function TSilWaveAudio.GetIsLoopEnabled: Boolean;
begin
  Result := FIsLoopEnabled;
end;

function TSilWaveAudio.GetIsRunning: Boolean;
begin
  Result := FIsRunning;
end;

function TSilWaveAudio.GetMode: TWaveAudioMode;
begin
  Result := FMode;
end;

function TSilWaveAudio.GetOutHook: IWaveAudioOutHook;
begin
  Result := FHookOut;
end;

procedure TSilWaveAudio.NextBuffer;
begin
  FBuffers.Locked;

  if FBufferIndex >= 0 then
  begin
    if not FIsDiscardEnabled then
    begin
      Inc(FBufferIndex);

      if FIsLoopEnabled and (FBufferIndex >= FBuffers.Count) then
        FBufferIndex := 0;
    end else
      FBuffers.Delete(FBufferIndex);

    FIsRunning := FBufferIndex < FBuffers.Count;
  end else
    FIsRunning := false;
end;

procedure TSilWaveAudio.SetIsDiscardEnabled(Value: Boolean);
begin
  FIsDiscardEnabled := Value;
end;

procedure TSilWaveAudio.SetIsLoopEnabled(Value: Boolean);
begin
  FIsLoopEnabled := Value;
end;

function TSilWaveAudio.PrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
begin
  Result := true;
end;

function TSilWaveAudio.UnPrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
begin
  Result := true;
end;

{ TSilWaveAudioDeviceList }

function TSilWaveAudioDeviceList.Add(const Value: IWaveAudioDevice): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilWaveAudioDeviceList.Enumerate(var Enum: IEnumerator; out Item: IWaveAudioDevice): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilWaveAudioDeviceList.First: IWaveAudioDevice;
begin
  Result := IWaveAudioDevice(inherited First);
end;

function TSilWaveAudioDeviceList.Get(Index: Integer): IWaveAudioDevice;
begin
  Result := IWaveAudioDevice(inherited GetItem(Index));
end;

function TSilWaveAudioDeviceList.Last: IWaveAudioDevice;
begin
  Result := IWaveAudioDevice(inherited Last);
end;

procedure TSilWaveAudioDeviceList.Put(Index: Integer; const Value: IWaveAudioDevice);
begin
  inherited SetItem(Index, Value);
end;

end.

