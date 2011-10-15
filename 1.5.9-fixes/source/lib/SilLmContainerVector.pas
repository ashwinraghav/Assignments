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

unit SilLmContainerVector;

{$I Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiLock,
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLiContainerVector,
  SilLkContainerBase;

type
  TSilBaseVector = class(
    TSilBaseContainer,
    ILockingSet,
    IBaseContainer,
    IBaseVector )
  private
    FList: HData;
    FCapacity: Integer;
  protected
    function DoCalcDelta(Limit: Integer): Integer;
    procedure DoGrow(Limit: Integer);
  protected
    function GetILockingSet: ILockingSet;
  protected
    procedure SetCount(const Value: Integer);
    function GetFirstHandle: HItem;
    function GetLastHandle: HItem;
    function GetData(const Item: HItem): HData;
    function IsValid(const Item: HItem): Boolean;
    procedure Clear;
    function Add(Source: HData; Count: Integer = 1; Return: PData = nil): HItem;
    function Insert(Source: HData; Count: Integer = 1; Item: HItem = 0; Return: PData = nil): HItem;
    procedure Delete(First, Last: HItem);
    procedure Exchange(Item1, Item2: HItem);
    procedure Move(Current, Target: HItem);
    function Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean;
    function GetNext(var Item: HItem; Delta: Integer = 1): Boolean;
  protected // IBaseVector
    function GetList: HData;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  protected
    property Count write SetCount;
    property List: HData read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

implementation

{ TSilBaseVector }

function TSilBaseVector.GetILockingSet: ILockingSet;
begin
  Result := Self;
end;

procedure TSilBaseVector.SetCount(const Value: Integer);
begin
  Locked;
  
  if Value <> FCount then
  begin
    if Value > FCapacity then
      DoGrow(Value);

    if Value > FCount then
      Handler.Initialize(GetData(FCount), Value - FCount) else
      Handler.Finalize(GetData(Value), FCount - Value);

    FCount := Value;
    if FCount = 0 then SetCapacity(0);    
  end;
end;

function TSilBaseVector.GetFirstHandle: HItem;
begin
  Result := 0;
end;

function TSilBaseVector.GetLastHandle: HItem;
begin
  Locked;
  
  Result := FCount - 1;
end;

function TSilBaseVector.GetData(const Item: HItem): HData;
begin
  Result := HData(Integer(FList) + Item * Size);
end;

function TSilBaseVector.IsValid(const Item: HItem): Boolean;
begin
  Result := (Item >= 0) and (Item < FCount);
end;

procedure TSilBaseVector.Clear;
begin
  Locked;
  
  SetCount(0);
end;

function TSilBaseVector.Add(Source: HData; Count: Integer; Return: PData): HItem;
var
  Data: HData;
begin
  Locked;
  
  Result := FCount;
  SetCount(FCount + Count);
  if Assigned(Source) or Assigned(Return) then
  begin
    Data := GetData(Result);
    if Assigned(Source) then Handler.Copy(Data, Source, Count);
    if Assigned(Return) then Return^ := Data;
  end;
end;

function TSilBaseVector.Insert(Source: HData; Count: Integer; Item: HItem; Return: PData): HItem;
var
  New: HItem;
  Data: HData;
begin
  Locked;
  
  if Item <> HNull then
  begin
    Result := Item;
    
    New := Add(nil, Count);
    Data := GetData(Item);

    if Item <> New  then
      Handler.Copy(GetData(New + Count - 1), GetData(New + Count - 2), New - Item, -1);

    if Assigned(Source) or Assigned(Return) then
    begin
      if Assigned(Source) then Handler.Copy(Data, Source, Count);
      if Assigned(Return) then Return^ := Data;    
    end;
    
  end else
    Result := Add(Source, Count, Return);
end;

procedure TSilBaseVector.Delete(First, Last: HItem);
var
  Total: Integer;
  Data: HData;
begin
  Locked;
  
  Inc(Last);
  Total := FCount - Last;
  if Total > 0 then
  begin
    Data := GetData(First);
    Handler.Copy(Data, GetData(Last), Total);
  end;
  SetCount(Total + First);
end;

procedure TSilBaseVector.Exchange(Item1, Item2: HItem);
var
  Data1, Data2: HData;
begin
  Locked;
  
  Data1 := GetData(Item1);
  Data2 := GetData(Item2);
  Handler.Copy(Scratch, Data1);
  Handler.Copy(Data1, Data2);
  Handler.Copy(Data2, Scratch);
end;

procedure TSilBaseVector.Move(Current, Target: HItem);
begin
  Locked;
  
  Handler.Copy(Scratch, GetData(Current));
  Delete(Current, Current + 1);
  Insert(Scratch, 1, Target);
end;

function TSilBaseVector.Find(Data: HData; Item: PItem; const Comparator: ITypeComparator; Param: Pointer): Boolean;
var
  Index: Integer;
  Handler: ITypeComparator;
begin
  Locked;
  Result := False;
  
  if Assigned(Comparator) then
    Handler := Comparator else
    Handler := Self.Handler;
  for Index := 0 to FCount - 1 do
  begin
    Result := Handler.Compare(Data, GetData(Index), Param) = 0;
    if Result then
    begin
      if Assigned(Item) then Item^ := Index;
      Break;
    end;
  end;
  (*)else
    Result := DoSearch(Self, Data, Item, 0, Base.Count-1);(*)
end;

function TSilBaseVector.GetNext(var Item: HItem; Delta: Integer): Boolean;
begin
  if Item <> HNull then
    Inc(Item, Delta) else
    Item := 0;

  Result := IsValid(Item);
end;

function TSilBaseVector.GetList: HData;
begin
  Result := FList;
end;

function TSilBaseVector.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TSilBaseVector.SetCapacity(const Value: Integer);
begin
  Locked;
  
  if Value <> FCapacity then
  begin
    ReallocMem(FList, Value * Size);
    FCapacity := Value;
  end;
end;

function TSilBaseVector.DoCalcDelta(Limit: Integer): Integer;
var
  Modulus: Integer;
begin
  if FCapacity > 64 then
    Modulus := FCapacity div 4
  else if FCapacity > 8 then
    Modulus := 16
  else
    Modulus := 4;

  Result := Modulus;
end;

procedure TSilBaseVector.DoGrow(Limit: Integer);
begin
  SetCapacity(Limit + FCapacity);
end;

end.
