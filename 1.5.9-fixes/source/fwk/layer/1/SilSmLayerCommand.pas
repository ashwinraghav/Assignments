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

unit SilSmLayerCommand;

{$include Defines.inc}

interface

uses
  Sil,
  SilSiLayer,
  SilShLayer;

type
  RCreateParams = record
    Params: IParameters;
    Caller: ILayerLink;
    Link: ILayerLink;
    Packet: IPacket;                                                               
    Context: IInterface;
    Target: Variant;
  end;

type
  TSilLayerCommand = class (
    TSilObject,
    ILayerCommand )
  private
    FCaller: ILayerLink;
    FLink: ILayerLink;
    FPacket: IPacket;
    FContext: IInterface;
    FTarget: Variant;
    FParams: IParameterList;
  protected // ILayerCommand
    function GetCaller: ILayerLink;
    function GetLink: ILayerLink;
    function GetParams: IParameterList;
    function GetContext: IInterface;
    procedure SetContext(const Value: IUnknown);
    function GetPacket: IPacket;
    function GetTarget: Variant;
  protected 
    property Caller: ILayerLink read GetCaller;
    property Link: ILayerLink read GetLink;
    property Context: IInterface read GetContext;
    property Packet: IPacket read GetPacket;
    property Target: Variant read GetTarget;
  public
    constructor Create(const Source: ILayerCommand); overload; 
    constructor Create(const Caller, Link: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IInterface); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmLayerCommandList;

{ TSilLayerCommand }

constructor TSilLayerCommand.Create(const Source: ILayerCommand);
begin
  Create(Source.Caller, Source.Link, Source.Packet, Source.Target, Source.Context);
  FParams := Source.Params;
end;

constructor TSilLayerCommand.Create(const Caller, Link: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IInterface);
begin
  inherited Create;
  FCaller := Caller;
  FLink := Link;
  FPacket := Packet;
  FTarget := Target;
  FContext := Context;
end;

destructor TSilLayerCommand.Destroy;
begin
  FCaller := nil;
  FLink := nil;
  FPacket := nil;
  FContext := nil;
  inherited;
end;

function TSilLayerCommand.GetCaller: ILayerLink;
begin
  Result := FCaller;
end;

function TSilLayerCommand.GetContext: IInterface;
begin
  Result := FContext;
end;

procedure TSilLayerCommand.SetContext(const Value: IInterface);
begin
  FContext := Value; 
end;

function TSilLayerCommand.GetLink: ILayerLink;
begin
  Result := FLink;
end;

function TSilLayerCommand.GetParams: IParameterList;
begin
  if FParams = nil then FParams := sil.list.parameters;
  Result := FParams;
end;

function TSilLayerCommand.GetPacket: IPacket;
begin
  Result := FPacket;
end;

function TSilLayerCommand.GetTarget: Variant;
begin
  Result := FTarget;
end;

end.
