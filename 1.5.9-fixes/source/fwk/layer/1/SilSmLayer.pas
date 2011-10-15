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

unit SilSmLayer;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiSharedObject,
  SilSiLayer,
  SilSkSharedObject;

type
  TSilLayer = class (
    TSilSharedObject,
    ILayer,
    ILayerControl,
    ILayerOperation,
    ILayerDuplicate)
  private
    FId: Variant;
    FParameters: IParameterList;
    FFireActivation: Boolean;
  private
    procedure DoFireLayerActivated(const Link: ILayerLink; const Context: IInterface);
    procedure DoFireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
  protected
    procedure FireLayerActivated(const Link: ILayerLink; const Context: IInterface); virtual;
    procedure FireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean); virtual;
  protected // ILayer
    function GetId: Variant; virtual;
    procedure SetId(const Value: Variant); virtual;
    function GetParameters: IParameterList;
    function GetControl: ILayerControl;
    function GetOperation: ILayerOperation;
    property Id: Variant read GetId write SetId;
    property Parameters: IParameterList read GetParameters;
    property Control: ILayerControl read GetControl;
    property Operation: ILayerOperation read GetOperation;
  protected // ILayerControl
    procedure Activate(const Link: ILayerLink; const Context: IUnknown);
    procedure Deactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean);
    procedure Reactivate(const Link: ILayerLink; const Context: IUnknown);
  protected
    function GetIsActive: Boolean; virtual;
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); virtual;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); virtual;
    property IsActive: Boolean read GetIsActive;
  protected // ILayerOperation
    procedure Write(const Command: ILayerCommand); virtual;
    procedure Read(const Command: ILayerCommand); virtual;
    procedure Receive(const Command: ILayerCommand); virtual;
  protected // ILayerDuplicate
    function Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean; virtual;
  public
    constructor CreateShared(const Factory: ISharedFactory; const Owner: IUnknown; const Controller: IUnknown; Param: Pointer); override;
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); reintroduce; virtual; 
    destructor Destroy; override;
  end;

implementation

uses
  SilScLayer,
  SilStLayer;

{ TSilLayer }

constructor TSilLayer.CreateShared(const Factory: ISharedFactory; const Owner, Controller: IInterface; Param: Pointer);
var
  Parameters: IParameters;
begin
  Sil.Ref.GetInterface(IUnknown(Param), IParameters, Parameters);
  Create(Parameters, Controller);
end;

constructor TSilLayer.Create(const Parameters: IParameters; const Controller: IInterface);
begin
  inherited Create(Controller);

  FParameters := Sil.List.Parameters;

  if Assigned(Parameters) then
    FParameters.Merge(Parameters);

  FFireActivation := true;

  if Debug.Check(dlCreation, CDebugLayer) then Sil.Trace.Log(ClassName + '.Create', FParameters);
end;

destructor TSilLayer.Destroy;
begin
  if Debug.Check(dlCreation, CDebugLayer) then Sil.Trace.Log(ClassName + '.Destroy', FParameters);
  FParameters := nil;

  inherited;
end;

procedure TSilLayer.Activate(const Link: ILayerLink; const Context: IUnknown);
begin
  if Debug.Check(dlActivation, CDebugLayer) then Sil.Trace.Log(ClassName + '.Activate');

  LayerActivate(Link, Context);

  if GetIsActive then
    DoFireLayerActivated(Link, Context);
end;

procedure TSilLayer.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
end;

procedure TSilLayer.Deactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean);
begin
  if Debug.Check(dlActivation, CDebugLayer) then Sil.Trace.Log(ClassName + '.Deactivate');

  LayerDeactivate(Link, Context, IsBroken);

  if not GetIsActive then
    DoFireLayerDeactivated(Link, Context, IsBroken);
end;

procedure TSilLayer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
end;

procedure TSilLayer.Reactivate(const Link: ILayerLink; const Context: IUnknown);
begin
  Deactivate(Link, Context, false);
  Activate(Link, Context);
end;

function TSilLayer.GetControl: ILayerControl;
begin
  Result := Self;
end;

function TSilLayer.GetOperation: ILayerOperation;
begin
  Result := Self;
end;

function TSilLayer.GetIsActive: Boolean;
begin
  Result := false;
end;

function TSilLayer.GetId: Variant;
begin
  Result := FId;
end;

procedure TSilLayer.SetId(const Value: Variant);
begin
  FId := Value;
end;

function TSilLayer.GetParameters: IParameterList;
begin
  Result := FParameters;
end;

procedure TSilLayer.Receive(const Command: ILayerCommand);
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Receive size=%d', [Command.Packet.Buffer.Size]);

  Command.Packet.Buffer.Position := 0;
  Cmd.Receive(Command);
end;

procedure TSilLayer.Read(const Command: ILayerCommand);
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Read size=%d', [Command.Packet.Buffer.Size]);

  Command.Packet.Buffer.Position := 0;
  Cmd.Read(Command);
end;

procedure TSilLayer.Write(const Command: ILayerCommand);
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Write size=%d', [Command.Packet.Buffer.Size]);

  Command.Packet.Buffer.Position := 0;
  Cmd.Write(Command);
end;

function TSilLayer.Duplicate(out Obj: IUnknown; const Context: IInterface): Boolean;
begin
  Result := false;
end;

procedure TSilLayer.DoFireLayerActivated(const Link: ILayerLink; const Context: IInterface);
begin
  if FFireActivation then
  begin
    FFireActivation := false;
    FireLayerActivated(Link, Context);
  end;
end;

procedure TSilLayer.DoFireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  if not FFireActivation then
  begin
    FFireActivation := true;
    FireLayerDeactivated(Link, Context, IsBroken);
  end;
end;

procedure TSilLayer.FireLayerActivated(const Link: ILayerLink; const Context: IInterface);
var
  Enum: IEnumerator;
  Sink: ILayerActivationEvents;
  Event: RLayerActivated;
begin
  if HasConnections then
  begin
    Event.Link := Link;
    Event.Context := Context;

    while Events.Enumerate(Enum, Sink, ILayerActivationEvents) do
      Sink.OnLayerActivated(Event);
  end;
end;

procedure TSilLayer.FireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
var
  Enum: IEnumerator;
  Sink: ILayerActivationEvents;
  Event: RLayerDeactivated;
begin
  if HasConnections then
  begin
    Event.Link := Link;
    Event.Context := Context;
    Event.IsBroken := IsBroken;
    Event.Reactivate := false;

    while Events.Enumerate(Enum, Sink, ILayerActivationEvents) do
      Sink.OnLayerDeactivated(Event);
  end;
end;

end.
