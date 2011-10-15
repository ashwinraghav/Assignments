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

unit SilLayerSmLink;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  UiDef;

type
  TStreamLink = class (TSilObject, IStreamLink, IStreamStatus, IStreamAction)
  private
    FStack: IStreamStack;
    FLower: IStreamLink;
    FUpper: IStreamLink;
    FUid: TGuid;
    FKind: TStreamLinkKind;
    FParams: IParameterList;
    FLogMask: TStreamLogLevelSet;
  protected
    procedure DoActivate(const Context: IUnknown = nil); virtual;
    procedure DoDeactivate(const Context: IUnknown = nil; Manual: Boolean = true); virtual;
    function DoWrite(const Packet: IStreamToken; out Link: IStreamLink): Boolean; virtual;
    function DoRead(const Packet: IStreamToken; out Link: IStreamLink): Boolean; virtual;
    function DoGetIsActive: Boolean; virtual;
  protected // IStreamLink
    function GetAction: IStreamAction; virtual;
    function GetStack: IStreamStack;
    function GetStatus: IStreamStatus;
    function GetKind: TStreamLinkKind;
    function GetLower: IStreamLink;
    function GetUid: TGuid;
    function GetUpper: IStreamLink;
    procedure SetStack(const Value: IStreamStack); virtual;
    procedure SetLower(const Value: IStreamLink); virtual;
    procedure SetUpper(const Value: IStreamLink); virtual;
  protected // IStreamStatus
    function GetIsActive: Boolean;
    function GetParams: IParameterList;
    function GetLogMask: TStreamLogLevelSet;
    procedure SetLogMask(value: TStreamLogLevelSet);
    procedure Activate(const Context: IUnknown = nil);
    procedure Deactivate(const Context: IUnknown = nil; Manual: Boolean = true);
  protected // IStreamAction
    procedure Write(const Packet: IStreamToken);
    procedure Read(const Packet: IStreamToken);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TStreamLink }

constructor TStreamLink.Create;
begin
  inherited Create;

  FUid := Sil.Guid.Create;
  FKind := lkUnspecified;
end;

destructor TStreamLink.Destroy;
begin
  FStack := nil;
  FLower := nil;
  FUpper := nil;
  FParams := nil;

  inherited;
end;

procedure TStreamLink.Activate(const Context: IInterface);
begin
  if slStatus in FLogMask then Sil.Trace.Enter(self, 'Activate', [GetParams]);
  DoActivate(Context);
  if slStatus in FLogMask then Sil.Trace.Leave;
end;

procedure TStreamLink.Deactivate(const Context: IInterface; Manual: Boolean);
begin
  if slStatus in FLogMask then Sil.Trace.Enter(self, 'Deactivate');
  DoDeactivate(Context, Manual);
  if slStatus in FLogMask then Sil.Trace.Leave;
end;

function TStreamLink.GetAction: IStreamAction;
begin
  Result := Self;
end;

function TStreamLink.GetStack: IStreamStack;
begin
  Result := FStack;
end;

function TStreamLink.GetStatus: IStreamStatus;
begin
  Result := Self;
end;

function TStreamLink.GetIsActive: Boolean;
begin
  Result := DoGetIsActive;
end;

function TStreamLink.GetKind: TStreamLinkKind;
begin
  Result := FKind;
end;

function TStreamLink.GetLower: IStreamLink;
begin
  if FStack <> nil then
  begin
    if FLower = nil then
      FStack.GetLowerLink(FLower, Self);
  end else
    FLower := nil;

  Result := FLower;
end;

function TStreamLink.GetParams: IParameterList;
begin
  if FParams = nil then
    FParams := Sil.Vector.ParameterList;

  Result := FParams;
end;

function TStreamLink.GetLogMask: TStreamLogLevelSet;
begin
  Result := FLogMask;
end;

procedure TStreamLink.SetLogMask(value: TStreamLogLevelSet);
begin
  FLogMask := value;
end;

function TStreamLink.GetUid: TGuid;
begin
  Result := FUid;
end;

function TStreamLink.GetUpper: IStreamLink;
begin
  if FStack <> nil then
  begin
    if FUpper = nil then
      FStack.GetUpperLink(FUpper, Self);
  end else
    FUpper := nil;

  Result := FUpper;
end;

procedure TStreamLink.Read(const Packet: IStreamToken);
var
  Link: IStreamLink;
begin
  if slAction in FLogMask then Sil.Trace.Enter(self, 'Read');

  if DoRead(Packet, Link) and (Link <> nil) then
    Link.Action.Read(Packet);

  if slAction in FLogMask then Sil.Trace.Leave;
end;

procedure TStreamLink.Write(const Packet: IStreamToken);
var
  Link: IStreamLink;
begin
  if slAction in FLogMask then Sil.Trace.Enter(self, 'Write', [Mem.Dump(Packet.Buffer.Memory, Packet.Buffer.Size)]);

  if DoWrite(Packet, Link) and (Link <> nil) then
    Link.Action.Write(Packet);

  if slAction in FLogMask then Sil.Trace.Leave;
end;

procedure TStreamLink.SetStack(const Value: IStreamStack);
begin
  FStack := Value;
end;

procedure TStreamLink.SetLower(const Value: IStreamLink);
begin
  FLower := Value;
end;

procedure TStreamLink.SetUpper(const Value: IStreamLink);
begin
  FUpper := Value;
end;

procedure TStreamLink.DoActivate(const Context: IInterface);
begin
  // none
end;

procedure TStreamLink.DoDeactivate(const Context: IInterface; Manual: Boolean);
begin
  // none
end;

function TStreamLink.DoGetIsActive: Boolean;
begin
  Result := false;
end;

function TStreamLink.DoRead(const Packet: IStreamToken; out Link: IStreamLink): Boolean;
begin
  Link := GetUpper;
  Result := Link <> nil;
end;

function TStreamLink.DoWrite(const Packet: IStreamToken; out Link: IStreamLink): Boolean;
begin
  Link := GetLower;
  Result := Link <> nil;
end;

end.
