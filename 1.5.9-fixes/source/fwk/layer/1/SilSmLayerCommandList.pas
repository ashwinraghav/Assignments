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

unit SilSmLayerCommandList;

{$include Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSiLayer,
  SilShLayer;

type
  TSilLayerCommandList = class(
    TSilObject,
    IEnumerable,
    ILayerCommands,
    ILayerCommandList )
  private
    FList: IContainerDynamic;
    FParams: IParameterList;
  private
    procedure DoCheckList;
    function DoCompareDefault(Data1, Data2: HData; Param: Pointer): Integer;
    function DoCompareCaller(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // ILayerCommands
    function GetCount: Integer;
    function GetFirst: ILayerCommand;
    function GetLast: ILayerCommand;
    function GetParams: IParameterList;
    function GetItem(Index: Integer): ILayerCommand;
    function Enumerate(var Enum: IEnumerator; out Item: ILayerCommand): Boolean;
    function Find(const Layer: IUnknown; out Item: ILayerCommand): Boolean;
  protected // ILayerCommandList
    function Add(const Item: ILayerCommand): Integer;
    function Remove(const Item: ILayerCommand): Integer;
  public
    constructor Create;
    destructor Destroy; override; 
  end;

implementation

uses
  SilLmContainerEnumerator;

{ TSilLayerCommandList }

constructor TSilLayerCommandList.Create;
begin
  inherited Create;
end;

destructor TSilLayerCommandList.Destroy;
begin
  ASSERT(not Assigned(FList) or (FList.Items.Count = 0));
  FList := nil;
  inherited;
end;

function TSilLayerCommandList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList));
end;

function TSilLayerCommandList.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilLayerCommandList.GetFirst: ILayerCommand;
begin
  if Assigned(FList) and (FList.Items.Count > 0) then
    Result := ILayerCommand(FList[FList.Items.First]^) else
    Result := nil;
end;

function TSilLayerCommandList.GetLast: ILayerCommand;
begin
  if Assigned(FList) and (FList.Items.Count > 0) then
    Result := ILayerCommand(FList[FList.Items.Last]^) else
    Result := nil;
end;

function TSilLayerCommandList.GetParams: IParameterList;
begin
  if FParams = nil then FParams := Sil.List.Parameters();
  Result := FParams;
end;

function TSilLayerCommandList.GetItem(Index: Integer): ILayerCommand;
begin
  if Assigned(FList) and (FList.Items.Count > 0) then
    Result := ILayerCommand(FList[Index]^) else
    Result := nil;
end;

function TSilLayerCommandList.Enumerate(var Enum: IEnumerator; out Item: ILayerCommand): Boolean;
var
  Data: HData;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, Lockable <> nil) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
  begin
    if Enum.Get(Data) then
      Item := ILayerCommand(Data^);
  end else
    Enum := nil;
end;

function TSilLayerCommandList.Find(const Layer: IInterface; out Item: ILayerCommand): Boolean;
var
  Index: HItem;
begin
  Result := Assigned(FList) and FList.Find(@Layer, @Index, Compare.Create(DoCompareCaller));
  if Result then Item := ILayerCommand(FList[Index]^);
end;

function TSilLayerCommandList.Add(const Item: ILayerCommand): Integer;
begin
  DoCheckList;
  Result := FList.Add(@Item);
end;

function TSilLayerCommandList.Remove(const Item: ILayerCommand): Integer;
begin
  if Assigned(FList) then
  begin
    Result := FList.Remove(@Item);
    if FList.Items.Count = 0 then FList := nil;
  end else
    Result := -1;
end;

procedure TSilLayerCommandList.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(SizeOf(ILayerCommand), DoCompareDefault);
end;

function TSilLayerCommandList.DoCompareDefault(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Integer(Data1^) - Integer(Data2^);
end;

function TSilLayerCommandList.DoCompareCaller(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Ref.Compare(IUnknown(Data1^), ILayerCommand(Data2^).Caller);
end;

end.
