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

unit SilLayerSkLayer;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  SilLayerSiGlobal;

type
  TSilLayer = class (TSilObject, ILayer, ILayerStatus, ILayerAction)
  private
    FStack: ILayerStack;
    FLower: ILayer;
    FUpper: ILayer;
    FUid: TGuid;
    FKind: TLayerKind;
    FParams: IParameterList;
    FLogMask: TLayerLogLevelSet;
  protected
    procedure DoActivate(const Context: IUnknown = nil); virtual;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); virtual;
    procedure DoFinalize; virtual;
    function DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean; virtual;
    function DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean; virtual;
    function DoGetIsActive: Boolean; virtual;
  protected // ILayer
    function GetAction: ILayerAction; virtual;
    function GetStack: ILayerStack;
    function GetStatus: ILayerStatus;
    function GetKind: TLayerKind;
    function GetLower: ILayer;
    function GetUid: TGuid;
    function GetUpper: ILayer;
    procedure SetStack(const Value: ILayerStack); virtual;
    procedure SetLower(const Value: ILayer); virtual;
    procedure SetUpper(const Value: ILayer); virtual; 
  protected // ILayerStatus
    function GetIsActive: Boolean;
    function GetParams: IParameterList;
    function GetLogMask: TLayerLogLevelSet;
    procedure SetLogMask(value: TLayerLogLevelSet);
    procedure Activate(const Context: IUnknown = nil);
    procedure Deactivate(const Context: IUnknown = nil; Manual: Boolean = true);
    procedure Finalize;
  protected // ILayerAction
    procedure Write(const Packet: ILayerToken);
    procedure Read(const Packet: ILayerToken);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TSilLayer }

constructor TSilLayer.Create;
begin
  inherited Create;

  FUid := Sil.Guid.Create;
  FKind := lkUnspecified;
end;

destructor TSilLayer.Destroy;
begin
  FStack := nil;
  FLower := nil;
  FUpper := nil;
  FParams := nil;

  inherited;
end;

procedure TSilLayer.Activate(const Context: IInterface);
begin
  if slStatus in FLogMask then Sil.Trace.Enter(self, 'Activate', [GetParams]);
  DoActivate(Context);
  if slStatus in FLogMask then Sil.Trace.Leave;
end;

procedure TSilLayer.Deactivate(const Context: IInterface; Manual: Boolean);
begin
  if slStatus in FLogMask then Sil.Trace.Enter(self, 'Deactivate');
  DoDeactivate(Context, Manual);
  if slStatus in FLogMask then Sil.Trace.Leave;
end;

procedure TSilLayer.DoFinalize;
begin
  // none
end;

procedure TSilLayer.Finalize;
begin
  if slStatus in FLogMask then Sil.Trace.Enter(self, 'Finalize');

  DoFinalize;
  FStack := nil;
  FLower := nil;
  FUpper := nil;
  FParams := nil;

  if slStatus in FLogMask then Sil.Trace.Leave;
end;

function TSilLayer.GetAction: ILayerAction;
begin
  Result := Self;
end;

function TSilLayer.GetStack: ILayerStack;
begin
  Result := FStack;
end;

function TSilLayer.GetStatus: ILayerStatus;
begin
  Result := Self;
end;

function TSilLayer.GetIsActive: Boolean;
begin
  Result := DoGetIsActive;
end;

function TSilLayer.GetKind: TLayerKind;
begin
  Result := FKind;
end;

function TSilLayer.GetLower: ILayer;
begin
  if FStack <> nil then
  begin
    if FLower = nil then
      FStack.GetLowerLink(FLower, Self);
  end else
    FLower := nil;

  Result := FLower;
end;

function TSilLayer.GetParams: IParameterList;
begin
  if FParams = nil then
    FParams := SilVector.ParameterList;

  Result := FParams;
end;

function TSilLayer.GetLogMask: TLayerLogLevelSet;
begin
  Result := FLogMask;
end;

procedure TSilLayer.SetLogMask(value: TLayerLogLevelSet);
begin
  FLogMask := value;
end;

function TSilLayer.GetUid: TGuid;
begin
  Result := FUid;
end;

function TSilLayer.GetUpper: ILayer;
begin
  if FStack <> nil then
  begin
    if FUpper = nil then
      FStack.GetUpperLink(FUpper, Self);
  end else
    FUpper := nil;

  Result := FUpper;
end;

procedure TSilLayer.Read(const Packet: ILayerToken);
var
  Link: ILayer;
begin
  if slAction in FLogMask then Sil.Trace.Enter(self, 'Read');

  if DoRead(Packet, Link) and (Link <> nil) then
    Link.Action.Read(Packet);

  if slAction in FLogMask then Sil.Trace.Leave;
end;

procedure TSilLayer.Write(const Packet: ILayerToken);
var
  Link: ILayer;
begin
  if slAction in FLogMask then Sil.Trace.Enter(self, 'Write', [Mem.Dump(Packet.Buffer.Memory, Packet.Buffer.Size)]);

  if DoWrite(Packet, Link) and (Link <> nil) then
    Link.Action.Write(Packet);

  if slAction in FLogMask then Sil.Trace.Leave;
end;

procedure TSilLayer.SetStack(const Value: ILayerStack);
begin
  FStack := Value;
end;

procedure TSilLayer.SetLower(const Value: ILayer);
begin
  FLower := Value;
end;

procedure TSilLayer.SetUpper(const Value: ILayer);
begin
  FUpper := Value;
end;

procedure TSilLayer.DoActivate(const Context: IInterface);
begin
  // none
end;

procedure TSilLayer.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  // none
end;

function TSilLayer.DoGetIsActive: Boolean;
begin
  Result := false;
end;

function TSilLayer.DoRead(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Link := GetUpper;
  Result := Link <> nil;
end;

function TSilLayer.DoWrite(const Packet: ILayerToken; out Link: ILayer): Boolean;
begin
  Link := GetLower;
  Result := Link <> nil;
end;

end.
