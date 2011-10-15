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

unit SilSmLayerConfig;

{$include Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSiLayer,
  SilSiLayerConfig;

type
  PPVariant = ^PVariant;
  TSilLayerConfigScope = class(
    TSilObject,
    IArguments,
    IArgumentList,
    ILayerConfigScope )
  private
    FParent: ILayerConfigScope;
    FList: IContainerDynamic;
  private
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
    function DoFind(const Name: string; Value: PPVariant = nil): Boolean;  
  protected // IArguments
    function GetCount: Integer;
    function GetItem(const ID: string): Variant;
    function Find(const ID: string; out Value: Variant): Boolean;
  protected // IArgumentList
    procedure PutItem(const ID: string; const Value: Variant);
  protected // ILayerConfigScope
    function GetParent: ILayerConfigScope;
    function Add(const Name: String; const IID: TGUID; const Getter: IUnknown; const Setter: PUnknown = nil): ILayerConfigScope; overload;
    function Add(const Name: String; const IID: TGUID; const Setter: PUnknown): ILayerConfigScope; overload;
  public
    constructor Create(const Parent: ILayerConfigScope);
    destructor Destroy; override;
  end;

implementation

type
  ILayerBinding = interface
    ['{CC3F5A26-351A-4D13-9D84-2FD2ECDCD350}']
    function Getter: IUnknown;
    function Setter: PUnknown;
    function IID: TGUID;
  end;

type
  TSilLayerBinding = class (TSilObject, ILayerBinding)
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

{ TSilLayerConfigScope }

constructor TSilLayerConfigScope.Create(const Parent: ILayerConfigScope);
begin
  inherited Create;
  FList := Vector.Create(TypeInfo(RParameter), DoCompare);
end;

destructor TSilLayerConfigScope.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilLayerConfigScope.GetCount: Integer;
begin
  Result := FList.Items.Count;
end;

function TSilLayerConfigScope.GetItem(const ID: string): Variant;
var
  Item: PVariant;
begin
  if DoFind(ID, @Item) then
    Result := Item^ else
    Result := Sil.Vart.Unassigned;
end;

function TSilLayerConfigScope.Find(const ID: string; out Value: Variant): Boolean;
var
  Item: PVariant;
begin
  Result := DoFind(Id, @Item);
  if Result then Value := Item^;
end;

procedure TSilLayerConfigScope.PutItem(const ID: string; const Value: Variant);
var
  Item: PVariant;
begin
  if DoFind(Id, @Item) then
    Item^ := Value;
end;

function TSilLayerConfigScope.GetParent: ILayerConfigScope;
begin
  Result := FParent;
end;

function TSilLayerConfigScope.Add(const Name: String; const IID: TGUID; const Getter: IInterface; const Setter: PUnknown): ILayerConfigScope;
var
  Item: RParameter;
begin
  Item.Name := Name;
  Item.Value := TSilLayerBinding.Create(IID, Getter, Setter);
  FList.Add(@Item)
end;

function TSilLayerConfigScope.Add(const Name: String; const IID: TGUID; const Setter: PUnknown): ILayerConfigScope;
var
  Item: RParameter;
begin
  Item.Name := Name;
  Item.Value := TSilLayerBinding.Create(IID, nil, Setter);
  FList.Add(@Item)
end;

function TSilLayerConfigScope.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Str.TextCompare(string(Data1^), RParameter(Data2^).Name);
end;

function TSilLayerConfigScope.DoFind(const Name: string; Value: PPVariant): Boolean;
var
  Index: Integer;
begin
  Result := FList.Find(@Name, @Index);
  if Result and Assigned(Value) then Value^ := @RParameter(FList[Index]^).Value;
end;

{ TSilLayerBinding }

constructor TSilLayerBinding.Create(const IID: TGUID; const Getter: IInterface; const Setter: PUnknown);
begin
  inherited Create;
  FIID := IID;
  FGetter := Getter;
  FSetter := Setter;
end;

destructor TSilLayerBinding.Destroy;
begin
  FGetter := nil;
  FSetter := nil;
  inherited;
end;

function TSilLayerBinding.Getter: IUnknown;
begin
  Result := FGetter;
end;

function TSilLayerBinding.IID: TGUID;
begin
  Result := FIID;
end;

function TSilLayerBinding.Setter: PUnknown;
begin
  Result := FSetter;
end;

end.
