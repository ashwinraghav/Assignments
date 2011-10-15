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

unit SilSmLayerBindings;

interface

{$include Defines.inc}

uses
  Sil,
  SilLmParameters,
  SilSiLayer;

type
  TSilLayerBindings = class (TSilParameters, ILayerBindings)
  protected
    function DoGetByIndex(const ID: Integer; out Value: Variant): Boolean; override;
    procedure DoPutByIndex(const ID: Integer; const Value: Variant); override;
  protected // ILayerBindings
    procedure Add(const Name: String; const IID: TGUID; const Getter: IUnknown; const Setter: PUnknown = nil); overload;
    procedure Add(const Name: String; const IID: TGUID; const Setter: PUnknown); overload;
  end;

implementation

type
  ILayerBinding = interface
    ['{CC3F5A26-351A-4D13-9D84-2FD2ECDCD350}']
    function Getter: IUnknown;
    function Setter: PUnknown;
    function IID: TGUID;
  end;

  TLayerBinding = class (TSilObject, ILayerBinding)
  private
    FGetter: IUnknown;
    FSetter: PUnknown;
    FIID: TGUID;
  protected
    function Getter: IUnknown;
    function Setter: PUnknown;
    function IID: TGUID;
  public
    constructor Create(const IID: TGUID; const Getter: IInterface; const Setter: PUnknown);
    destructor Destroy; override;
  end;

{ TSilLayerBindings }

procedure TSilLayerBindings.Add(const Name: String; const IID: TGUID; const Setter: PUnknown);
var
  Getter: IUnknown;
begin
  Add(Name, IID, Getter, Setter);
end;

procedure TSilLayerBindings.Add(const Name: String; const IID: TGUID; const Getter: IInterface; const Setter: PUnknown);
var
  Binding: ILayerBinding;
begin
  Binding := TLayerBinding.Create(IID, Getter, Setter);
  inherited PutItem(Name, Binding);
end;

function TSilLayerBindings.DoGetByIndex(const ID: Integer; out Value: Variant): Boolean;
var
  Binding: ILayerBinding;
begin
  Result := inherited DoGetByIndex(Id, Value);

  if Result and Vart.IsObject(Value) and Ref.GetInterface(Value, ILayerBinding, Binding) then
    Value := Binding.Getter;
end;

procedure TSilLayerBindings.DoPutByIndex(const ID: Integer; const Value: Variant);
var
  Item: Variant;
  Binding: ILayerBinding;
  Result: Boolean;
  Obj: IUnknown;
begin
  Result := inherited DoGetByIndex(Id, Item) and
    Vart.IsObject(Item) and
    Ref.GetInterface(Item, ILayerBinding, Binding);

  if Result then
  begin
    if Ref.GetInterface(Value, Binding.IID, Obj) then
      Binding.Setter^ := Obj;
  end else
    inherited DoPutByIndex(ID, Value);
end;

{ TLayerBinding }

constructor TLayerBinding.Create(const IID: TGUID; const Getter: IInterface; const Setter: PUnknown);
begin
  inherited Create;

  FIID := IID;
  FGetter := Getter;
  FSetter := Setter;
end;

destructor TLayerBinding.Destroy;
begin
  FGetter := nil;
  FSetter := nil;

  inherited;
end;

function TLayerBinding.Getter: IUnknown;
begin
  Result := FGetter;
end;

function TLayerBinding.IID: TGUID;
begin
  Result := FIID;
end;

function TLayerBinding.Setter: PUnknown;
begin
  Result := FSetter;
end;

end.
 