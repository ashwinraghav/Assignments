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

unit SilSmLayerProtocolCustomImate;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer;

type
  TSilCustomImateProtocol = class (
    // extends
    TSilObject,
    // implements
    IDispatchable,
    ILayerProtocolControl,
    ILayerStatus)
  private
    FProtocol: ILayerProtocol;
    FStatus: ILayerProtocolStatus;
    FParameters: IParameters;
    FIsActive: Boolean;
  protected // ILayerProtocolControl
    function GetParameters: IParameters;
    procedure Activate(const Protocol: ILayerProtocol; const Context: IUnknown = nil);
    procedure Deactivate(const Protocol: ILayerProtocol; const Context: IUnknown = nil);
    function GetStatus: ILayerStatus;
    property Parameters: IParameters read GetParameters;
  protected // ILayerStatus
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;
  protected
    function GetProtocol: ILayerProtocol;
  protected
    procedure Initialize(const Hook: IUnknown); virtual;
    procedure Finalize; virtual;
    property Protocol: ILayerProtocol read GetProtocol;
  public
    constructor Create(const Parameters: IParameters = nil);
    destructor Destroy; override;
  end;

implementation

{ TSilCustomImateProtocol }

constructor TSilCustomImateProtocol.Create(const Parameters: IParameters);
begin
  inherited Create;
  FParameters := Parameters;
end;

destructor TSilCustomImateProtocol.Destroy;
begin
  FParameters := nil;
  inherited;
end;

function TSilCustomImateProtocol.GetParameters: IParameters;
begin
  Result := FParameters;
end;

procedure TSilCustomImateProtocol.Activate(const Protocol: ILayerProtocol; const Context: IUnknown);
var
  Hook: IUnknown;
begin
  if not Assigned(FProtocol) then FProtocol := Protocol;
  if Assigned(FProtocol) then FProtocol.GetHook(Hook);
  Initialize(Hook);

  if Ref.GetInterface(Hook, ILayerProtocolStatus, FStatus) then
    FStatus.OnProtocolActivation(Self);

  FIsActive := true;
end;

procedure TSilCustomImateProtocol.Deactivate(const Protocol: ILayerProtocol; const Context: IUnknown);
begin
  Finalize;

  if Assigned(FStatus) then
  begin
    FStatus.OnProtocolDeactivation(Self);
    FStatus := nil;
  end;

  FProtocol := nil;
  FIsActive := false;
end;

function TSilCustomImateProtocol.GetProtocol: ILayerProtocol;
begin
  if Assigned(FProtocol) then
    Result := FProtocol else
    raise Sil.Error.Create('%s protocol not activated', [ClassName]);
end;

procedure TSilCustomImateProtocol.Initialize(const Hook: IInterface);
begin
end;

procedure TSilCustomImateProtocol.Finalize;
begin
end;

function TSilCustomImateProtocol.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

function TSilCustomImateProtocol.GetStatus: ILayerStatus;
begin
  Result := Self;
end;

end.
