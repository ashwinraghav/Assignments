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

unit SilSmLayerLinkList;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerLink;

type
  TSilLayerLinkList = class (
    // extends
    TSilLayerLink,
    // implements
    ILayerLink,
    ILayerLinkList,
    ILayerOperation,
    ILayerDuplicate)
  private
    FId: Variant;
    FList: IInterfaceList;
    FFireActivation: Boolean;
  protected
    function DoCheckValue(const Value: IInterface): IUnknown;
    procedure DoAdd(const Value: IInterface); virtual;
    procedure DoInsert(const Before, Value: IInterface); virtual;
    procedure FireLayerActivated(const Link: ILayerLink; const Context: IInterface); virtual;
    procedure FireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean); virtual;
    property List: IInterfaceList read FList;
  protected // ILayerLink
    function GetOperation: ILayerOperation; override;
    //procedure SetOperation(const Value: ILayerOperation); override;
  protected // ILayerLinkList
    function GetId: Variant;
    procedure SetId(const Value: Variant);
    function GetParameters: IParameterList;
    procedure Add(const Value: IUnknown); overload; virtual;
    procedure Add(const Value: IUnknown; out Obj; const IID: TGuid); overload; virtual;
    procedure Remove(const Value: IUnknown); virtual;
    procedure AddList(const List: ILayerLinkList); virtual;
    procedure Insert(const Before, Value: IUnknown); virtual;
    procedure Clear(Recursive: Boolean); virtual;
    function Enumerate(var Enum: IEnumerator; out Item: IUnknown): Boolean;
    function GetFirst(const IID: TGuid; out Obj; FromIndex: Integer = -1): Boolean;
    function GetLast(const IID: TGuid; out Obj; FromIndex: Integer = -1): Boolean;
    function GetBefore(const Value: IUnknown; out Obj: IUnknown): Boolean;
    function GetAfter(const Value: IUnknown; out Obj: IUnknown): Boolean;
    property Id: Variant read GetId write SetId;
    property Parameters: IParameterList read GetParameters;
  protected // ILayerOperation
    function ILayerOperation.GetControl = DoLayerGetControl;
    function DoLayerGetControl: ILayerControl; virtual;
    procedure Write(const Command: ILayerCommand); virtual;
    procedure Read(const Command: ILayerCommand); virtual;
    procedure Receive(const Command: ILayerCommand); virtual;
  protected // ILayerDuplicate
    function Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean; override;
  protected // ILayerLinkControl
    procedure Activate(const Context: IUnknown); override;
    procedure Deactivate(const Context: IUnknown; IsBroken: Boolean); override;
  public
    constructor Create(const Parameters: IParameters);
    destructor Destroy; override;
  end;

implementation

uses
  SilStLayer,
  SilLkInterfaced, SilLiList, SilLiInterfaceList;

{ TSilLayerLinkList }

constructor TSilLayerLinkList.Create(const Parameters: IParameters);
begin
  inherited Create(Parameters);
  
  FList := Sil.List.InterfaceList(true);
  FFireActivation := true;
end;

destructor TSilLayerLinkList.Destroy;
begin
  Clear(false);
  FList := nil;

  inherited;
end;

function TSilLayerLinkList.GetOperation: ILayerOperation;
begin
  Result := Self;
end;

(*)procedure TSilLayerLinkList.SetOperation(const Value: ILayerOperation);
begin
end;(*)

function TSilLayerLinkList.DoCheckValue(const Value: IInterface): IUnknown;
begin
  if not Ref.GetInterface(Value, ILayerLink, Result) and Ref.GetInterface(Value, ILayer, Result) then
    Result := TSilLayerLink.Create(Value) else
    Result := Value;
end;

procedure TSilLayerLinkList.Add(const Value: IUnknown; out Obj; const IID: TGuid);
begin
  Value.QueryInterface(IID, Obj);
  Add(Value);
end;

procedure TSilLayerLinkList.Add(const Value: IUnknown);
begin
  DoAdd(DoCheckValue(Value));
end;

procedure TSilLayerLinkList.DoAdd(const Value: IUnknown);
begin
  FList.Add(Value);
end;

procedure TSilLayerLinkList.Insert(const Before, Value: IUnknown);
begin
  DoInsert(Before, DoCheckValue(Value));
end;

procedure TSilLayerLinkList.DoInsert(const Before, Value: IUnknown);
var
  i: Integer;
begin
  FList.Locked;

  if Assigned(Before) then
    i := FList.IndexOf(Before) else
    i := 0;

  if i >= 0 then
  begin
    FList.Insert(i, Value);
  end;
end;

procedure TSilLayerLinkList.Remove(const Value: IUnknown);
begin
  FList.Remove(Value);
end;

procedure TSilLayerLinkList.AddList(const List: ILayerLinkList);
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  while List.Enumerate(Enum, Item) do Add(Item);
end;

procedure TSilLayerLinkList.Clear(Recursive: Boolean);
var
  Enum: IEnumerator;
  Item: IUnknown;
  List: ILayerLinkList;
begin
  Control.Deactivate;

  while Enumerate(Enum, Item) do
  begin
    if Recursive and Ref.GetInterface(Item, ILayerLinkList, List) then
      List.Clear(Recursive);

    Remove(Item);
  end;
end;

function TSilLayerLinkList.Enumerate(var Enum: IEnumerator; out Item: IUnknown): Boolean;
begin
  Result := FList.Enumerate(Enum, Item);
end;

function TSilLayerLinkList.Duplicate(out Obj: IUnknown; const Context: IInterface): Boolean;
begin
  Result := false;
end;

procedure TSilLayerLinkList.Read(const Command: ILayerCommand);
begin
end;

procedure TSilLayerLinkList.Receive(const Command: ILayerCommand); 
begin
end;

procedure TSilLayerLinkList.Write(const Command: ILayerCommand); 
begin
end;

function TSilLayerLinkList.GetFirst(const IID: TGuid; out Obj; FromIndex: Integer): Boolean;
var
  i: Integer;
begin
  FList.Locked;
  if FromIndex < 0 then FromIndex := 0;

  for i := FromIndex to FList.Count - 1 do
    if Ref.GetInterface(FList.Items[i], IID, Obj) then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TSilLayerLinkList.GetLast(const IID: TGuid; out Obj; FromIndex: Integer): Boolean;
var
  i: Integer;
begin
  FList.Locked;
  if FromIndex < 0 then FromIndex := FList.Count - 1;

  for i := FromIndex downto 0 do
    if Ref.GetInterface(FList.Items[i], IID, Obj) then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TSilLayerLinkList.GetBefore(const Value: IInterface; out Obj: IUnknown): Boolean;
var
  i: Integer;
begin
  FList.Locked;
  i := FList.IndexOf(Value);
  Result := i > 0;
  if Result then Obj := FList[i - 1];
end;

function TSilLayerLinkList.GetAfter(const Value: IInterface; out Obj: IUnknown): Boolean;
var
  i: Integer;
begin
  FList.Locked;
  i := FList.IndexOf(Value);
  Result := (i >= 0) and (i < FList.Count - 1);
  if Result then Obj := FList[i + 1];
end;

procedure TSilLayerLinkList.FireLayerActivated(const Link: ILayerLink; const Context: IInterface);
var
  Enum: IEnumerator;
  Sink: ILayerActivationEvents;
  Event: RLayerActivated;
begin
  if FFireActivation then
  begin
    FFireActivation := false;

    if HasConnections then
    begin
      Event.Link := Link;
      Event.Context := Context;

      while Events.Enumerate(Enum, Sink, ILayerActivationEvents) do
        Sink.OnLayerActivated(Event);
    end;
  end;
end;

procedure TSilLayerLinkList.FireLayerDeactivated(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
var
  Enum: IEnumerator;
  Sink: ILayerActivationEvents;
  Event: RLayerDeactivated;
begin
  if not FFireActivation then
  begin
    FFireActivation := true;

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
end;

function TSilLayerLinkList.GetId: Variant;
begin
  Result := FId;
end;

function TSilLayerLinkList.GetParameters: IParameterList;
begin
end;

procedure TSilLayerLinkList.SetId(const Value: Variant);
begin
  FId := Value;
end;

function TSilLayerLinkList.DoLayerGetControl: ILayerControl;
begin
  Result := nil;
end;

procedure TSilLayerLinkList.Activate(const Context: IInterface);
begin
  FireLayerActivated(Self, Context);
end;

procedure TSilLayerLinkList.Deactivate(const Context: IInterface; IsBroken: Boolean);
begin
  FireLayerDeactivated(Self, Context, IsBroken);
end;

end.
