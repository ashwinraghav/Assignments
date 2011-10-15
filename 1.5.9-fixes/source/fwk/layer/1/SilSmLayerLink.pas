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

unit SilSmLayerLink;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer;

type
  TSilLayerLink = class (
    // extends
    TSilInterfacedObject,
    // implements
    ILayerLink,
    ILayerLinkControl,
    ILayerDuplicate)
  private
    FChain: Pointer;
    FLower: ILayerLink;
    FUpper: ILayerLink;
    FLayer: IUnknown;
    FOperation: ILayerOperation;
  private
    procedure DoSetLayer(const Value: IUnknown);
  protected
    function DoDuplicate(const Item: IInterface; out Obj: IInterface; const Context: IInterface): Boolean;
  protected // ILayerLink
    function GetChain: ILayerChain; virtual;
    procedure SetChain(const Value: ILayerChain); virtual;
    function GetLower: ILayerLink; virtual;
    procedure SetLower(const Value: ILayerLink); virtual;
    function GetUpper: ILayerLink; virtual;
    procedure SetUpper(const Value: ILayerLink); virtual;
    function GetOperation: ILayerOperation; virtual;
    //procedure SetOperation(const Value: ILayerOperation); virtual;
    function GetControl: ILayerLinkControl; virtual;
    property Chain: ILayerChain read GetChain write SetChain;
    property Lower: ILayerLink read GetLower write SetLower;
    property Upper: ILayerLink read GetUpper write SetUpper;
    property Operation: ILayerOperation read GetOperation(*) write SetOperation(*);
    property Control: ILayerLinkControl read GetControl;
  protected // ILayerLinkControl
    function GetIsActive: Boolean; virtual;
    procedure Activate(const Context: IUnknown); virtual;
    procedure Deactivate(const Context: IUnknown; IsBroken: Boolean); virtual;
    procedure Reactivate(const Context: IUnknown); virtual;
    property IsActive: Boolean read GetIsActive;
  protected // ILayerDuplicate
    function Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean; virtual;
  public
    constructor Create(const Layer: IUnknown);
    destructor Destroy; override;
  end;

implementation

{ TSilLayerLink }

constructor TSilLayerLink.Create(const Layer: IUnknown);
begin
  inherited Create;
  DoSetLayer(Layer);
//  SetOperation(Operation);
end;

destructor TSilLayerLink.Destroy;
begin
  FChain := nil;
  FLower := nil;
  FUpper := nil;
  DoSetLayer(nil);
  inherited;
end;

function TSilLayerLink.GetChain: ILayerChain;
begin
  Result := ILayerChain(FChain);
end;

procedure TSilLayerLink.SetChain(const Value: ILayerChain);
begin
  ChangeRef(Value, @FChain);
end;

function TSilLayerLink.GetLower: ILayerLink;
begin
  Result := FLower;
end;

procedure TSilLayerLink.SetLower(const Value: ILayerLink);
begin
  FLower := Value;
end;

function TSilLayerLink.GetUpper: ILayerLink;
begin
  Result := FUpper;
end;

procedure TSilLayerLink.SetUpper(const Value: ILayerLink);
begin
  FUpper := Value;
end;

function TSilLayerLink.GetOperation: ILayerOperation;
begin
  Result := FOperation;
end;

(*)procedure TSilLayerLink.SetOperation(const Value: ILayerOperation);
begin
  FOperation := Value;
end;(*)

function TSilLayerLink.GetControl: ILayerLinkControl;
begin
  Result := Self;
end;

procedure TSilLayerLink.Activate(const Context: IUnknown);
begin
  if Assigned(FOperation) then FOperation.Control.Activate(Self, Context);
end;

procedure TSilLayerLink.Deactivate(const Context: IUnknown; IsBroken: Boolean);
begin
  if Assigned(FOperation) then FOperation.Control.Deactivate(Self, Context, IsBroken);
end;

procedure TSilLayerLink.Reactivate(const Context: IUnknown);
begin
  if Assigned(FOperation) then FOperation.Control.Reactivate(Self, Context);
end;

function TSilLayerLink.GetIsActive: Boolean;
begin
  Result := Assigned(FOperation) and FOperation.Control.IsActive;
end;

function TSilLayerLink.DoDuplicate(const Item: IUnknown; out Obj: IUnknown; const Context: IUnknown): Boolean;
var
  Dup: ILayerDuplicate;
begin
  Result :=
    (Ref.GetInterface(Item, ILayerDuplicate, Dup) and Dup.Duplicate(Obj, Context)) or
    Ref.GetInterface(Item, ILayerSelfDuplicate, Obj);
end;

function TSilLayerLink.Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean;
var
  NewOperation: IUnknown;
  Link: ILayerLink;
begin
  Result := Assigned(FOperation) and DoDuplicate(FOperation, NewOperation, Context);

  if Result then
  begin
    Link := TSilLayerLink.Create(NewOperation);
    //Link.Operation := NewOperation as ILayerOperation;
    Obj := Link;
  end;
end;

procedure TSilLayerLink.DoSetLayer(const Value: IInterface);
begin
  if Assigned(FOperation) then FOperation := nil;   
  if Assigned(FLayer) then FLayer := nil;
  if Assigned(Value) then FLayer := Value;
  if Assigned(FLayer) then Sil.Ref.GetInterface(FLayer, ILayerOperation, FOperation);
end;

end.
