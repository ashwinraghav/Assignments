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

unit SilOkMixerDevice;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiEnumerator,
  SilOiMixerDevice,
  SilLkObject,
  SilLkInterfaced,
  SilLmInterfaceList;

type
  TSilMixerComponentList = class (TSilInterfaceList, IMixerComponentList)
  protected
    function Get(Index: Integer): IMixerComponent; virtual;
    procedure Put(Index: Integer; const Value: IMixerComponent); virtual;
    function Add(const Value: IMixerComponent): Integer; reintroduce; virtual;
    function First: IMixerComponent; reintroduce;
    function Last: IMixerComponent; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean; reintroduce;
  end;

  TSilMixerRecordingComponentList = class (TSilInterfaceList, IMixerRecordingComponentList)
  protected
    function Get(Index: Integer): IMixerRecordingComponent;
    function Add(const Value: IMixerRecordingComponent): Integer; reintroduce;
    procedure Put(Index: Integer; const Value: IMixerRecordingComponent);
    function First: IMixerRecordingComponent;
    function Last: IMixerRecordingComponent;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean; reintroduce; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerRecordingComponent): Boolean; reintroduce; overload;
    property Items[Index: Integer]: IMixerRecordingComponent read Get write Put; default;
  end;

  TSilMixerVolumeComponentList = class (TSilInterfaceList, IMixerVolumeComponentList)
  protected
    function Get(Index: Integer): IMixerVolumeComponent;
    function Add(const Value: IMixerVolumeComponent): Integer; reintroduce;
    procedure Put(Index: Integer; const Value: IMixerVolumeComponent);
    function First: IMixerVolumeComponent;
    function Last: IMixerVolumeComponent;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean; reintroduce; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerVolumeComponent): Boolean; reintroduce; overload; 
    property Items[Index: Integer]: IMixerVolumeComponent read Get write Put; default;
  end;

  TSilMixerControlList = class (TSilInterfaceList, IMixerControlList)
  protected
    function Get(Index: Integer): IMixerControl; virtual;
    procedure Put(Index: Integer; const Value: IMixerControl); virtual;
    function Add(const Value: IMixerControl): Integer; reintroduce; virtual;
    function First: IMixerControl; reintroduce;
    function Last: IMixerControl; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerControl): Boolean; reintroduce;
    function Find(const Name: String; out Control: IMixerControl): Boolean;
  end;

  TSilMixerDevice = class (TSilObject, IMixerDevice)
  private
    FPlayback: IMixerMasterPlaybackComponent;
    FRecording: IMixerMasterRecordingComponent;
  protected
    procedure DoCheckComponents; virtual;
    procedure DoInitPlayback(const Component: IMixerMasterPlaybackComponent);
    procedure DoInitRecording(const Component: IMixerMasterRecordingComponent);
  protected
    function GetPlayback: IMixerMasterPlaybackComponent;
    function GetRecording: IMixerMasterRecordingComponent;
    function GetName: String; virtual;
    function GetManufacturerId: Word; virtual;
    function GetProductId: Word; virtual;
    function GetDriverVersion: LongWord; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBtText,
  SilBtVart;

{ TSilMixerComponentList }

function TSilMixerComponentList.Add(const Value: IMixerComponent): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilMixerComponentList.Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilMixerComponentList.First: IMixerComponent;
begin
  Result := IMixerComponent(inherited First);
end;

function TSilMixerComponentList.Get(Index: Integer): IMixerComponent;
begin
  Result := IMixerComponent(inherited GetItem(Index));
end;

function TSilMixerComponentList.Last: IMixerComponent;
begin
  Result := IMixerComponent(inherited Last);
end;

procedure TSilMixerComponentList.Put(Index: Integer; const Value: IMixerComponent);
begin
  inherited SetItem(Index, Value);
end;

{ TSilMixerControlList }

function TSilMixerControlList.Add(const Value: IMixerControl): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilMixerControlList.Enumerate(var Enum: IEnumerator; out Item: IMixerControl): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilMixerControlList.Find(const Name: String; out Control: IMixerControl): Boolean;
var
  Enum: IEnumerator;
begin
  while Enumerate(Enum, Control) do
    if Text.Compare(Control.Name, Name) = 0 then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TSilMixerControlList.First: IMixerControl;
begin
  Result := IMixerControl(inherited First);
end;

function TSilMixerControlList.Get(Index: Integer): IMixerControl;
begin
  Result := IMixerControl(inherited GetItem(Index));
end;

function TSilMixerControlList.Last: IMixerControl;
begin
  Result := IMixerControl(inherited Last);
end;

procedure TSilMixerControlList.Put(Index: Integer; const Value: IMixerControl);
begin
  inherited SetItem(Index, Value);
end;

{ TSilMixerRecordingComponentList }

function TSilMixerRecordingComponentList.Add(const Value: IMixerRecordingComponent): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilMixerRecordingComponentList.Enumerate(var Enum: IEnumerator; out Item: IMixerRecordingComponent): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilMixerRecordingComponentList.Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean;
var
  Obj: IUnknown;
begin
  Result := inherited Enumerate(Enum, Obj);
  Item := Obj as IMixerComponent;
end;

function TSilMixerRecordingComponentList.First: IMixerRecordingComponent;
begin
  Result := IMixerRecordingComponent(inherited First);
end;

function TSilMixerRecordingComponentList.Get(Index: Integer): IMixerRecordingComponent;
begin
  Result := IMixerRecordingComponent(inherited GetItem(Index));
end;

function TSilMixerRecordingComponentList.Last: IMixerRecordingComponent;
begin
  Result := IMixerRecordingComponent(inherited Last);
end;

procedure TSilMixerRecordingComponentList.Put(Index: Integer; const Value: IMixerRecordingComponent);
begin
  inherited SetItem(Index, Value);
end;

{ TSilMixerVolumeComponentList }

function TSilMixerVolumeComponentList.Add(const Value: IMixerVolumeComponent): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilMixerVolumeComponentList.Enumerate(var Enum: IEnumerator; out Item: IMixerVolumeComponent): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilMixerVolumeComponentList.Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean;
var
  Obj: IUnknown;
begin
  Result := inherited Enumerate(Enum, Obj);
  Item := Obj as IMixerComponent;
end;

function TSilMixerVolumeComponentList.First: IMixerVolumeComponent;
begin
  Result := IMixerVolumeComponent(inherited First);
end;

function TSilMixerVolumeComponentList.Get(Index: Integer): IMixerVolumeComponent;
begin
  Result := IMixerVolumeComponent(inherited GetItem(Index));
end;

function TSilMixerVolumeComponentList.Last: IMixerVolumeComponent;
begin
  Result := IMixerVolumeComponent(inherited Last);
end;

procedure TSilMixerVolumeComponentList.Put(Index: Integer; const Value: IMixerVolumeComponent);
begin
  inherited SetItem(Index, Value);
end;

{ TSilMixerDevice }

constructor TSilMixerDevice.Create;
begin
  inherited Create;
end;

destructor TSilMixerDevice.Destroy;
begin
  inherited;
end;

procedure TSilMixerDevice.DoInitPlayback(const Component: IMixerMasterPlaybackComponent);
begin
  FPlayback := Component;
end;

procedure TSilMixerDevice.DoInitRecording(const Component: IMixerMasterRecordingComponent);
begin
  FRecording := Component;
end;

function TSilMixerDevice.GetDriverVersion: LongWord;
begin
  Result := 0;
end;

function TSilMixerDevice.GetManufacturerId: Word;
begin
  Result := 0;
end;

function TSilMixerDevice.GetName: String;
begin
  Result := '';
end;

function TSilMixerDevice.GetProductId: Word;
begin
  Result := 0;
end;

function TSilMixerDevice.GetPlayback: IMixerMasterPlaybackComponent;
begin
  if not Assigned(FPlayback) then DoCheckComponents;
  Result := FPlayback;
end;

function TSilMixerDevice.GetRecording: IMixerMasterRecordingComponent;
begin
  if not Assigned(FRecording) then DoCheckComponents;
  Result := FRecording;
end;

procedure TSilMixerDevice.DoCheckComponents;
begin
end;

end.
