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

unit SilLkAggregation;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLock,
  SilLiObject,
  SilLiEnumerator,
  SilLkAggregable,
  SilLhAggregation,
  SilLiContainerTypes,
  SilLiContainer;

type
  TSilAggregationObject = class(
    TSilAggregableObject,
    IEnumerable,
    IAggregation )
  private
    FList: IContainerDynamic;
  private
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
    procedure DoCheckList;
  protected
    function GetLockable: ILockable; override;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // IAggregation
    function IAggregation.QueryInterface = LookupInterface;
    function LookupInterface(const IID: TGUID; out Obj): Integer;
    function Add(const Contained: IUnknown; IsPublished: Boolean = True): IUnknown; overload;
    function Add(const Contained: IUnknown; const IID: TGUID; Field: PPointer; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableObject; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableObject; const IID: TGUID; Field: PPointer; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableClass; const Owner: IUnknown = nil; IsPublished: Boolean = True; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; overload;
    function Add(Contained: TSilAggregableClass; const IID: TGUID; Field: PPointer; const Owner: IUnknown = nil; IsPublished: Boolean = True; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; overload;
    procedure Remove(Contained: TSilAggregableObject; Field: PPointer = nil); overload;
    procedure Remove(Contained: TSilAggregableClass; Field: PPointer = nil); overload;
    procedure Remove(const Contained: IUnknown; Field: PPointer = nil); overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Obj): Boolean;
    procedure Clear;
  public
    destructor Destroy; override;
  public
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

implementation

uses
  SilOsTypes,
  SilLtReference,
  SilLtContainer,
  SilLmContainerEnumerator;

type
  RData = record
    Instance: IUnknown;
    IsPublished: LongBool;
  end;

{ TSilAggregationObject }

destructor TSilAggregationObject.Destroy;
begin
  Clear;
  FList := nil;
  inherited;
end;

function TSilAggregationObject.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited ObjQueryInterface(IID, Obj);
  if Result <> S_OK then Result := LookupInterface(IID, Obj);
end;

function TSilAggregationObject.GetLockable: ILockable;
begin
  MainInterface.QueryInterface(ILockable, Result);
end;

function TSilAggregationObject.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList), Locked);
end;

function TSilAggregationObject.LookupInterface(const IID: TGUID; out Obj): Integer;
var
  Data: ^RData;
  Cursor: IContainerPointerStatic;
begin
  Result := E_NOINTERFACE;

  if Assigned(FList) and (FList.Items.Count > 0) then
  begin
    Cursor := FList.Cursors.First;

    while (Result <> S_OK) and Cursor.IsValid do
    begin
      Data := Cursor.Data;

      if Assigned(Data.Instance) and Data.IsPublished then
        Result := Data.Instance.QueryInterface(IID, Obj);

      if Result <> S_OK then
        Cursor.Next();
    end;
  end;
end;

function TSilAggregationObject.Add(const Contained: IUnknown; IsPublished: Boolean): IUnknown;
var
  Data: RData;
begin
  if Assigned(Contained) then
  begin
    DoCheckList;
    Data.Instance := Contained;
    Data.IsPublished := IsPublished;
    FList.Add(@Data);
    Result := Contained;
  end else
    Result := nil;
end;

function TSilAggregationObject.Add(const Contained: IUnknown; const IID: TGUID; Field: PPointer; IsPublished: Boolean): IUnknown;
var
  Instance: IUnknown;
begin
  Result := Add(Contained, IsPublished);
  if Assigned(Result) and Assigned(Field) then
  begin
    ASSERT(Result.QueryInterface(IID, Instance) = 0);
    Field^ := Pointer(Instance);
  end;
end;

function TSilAggregationObject.Add(Contained: TSilAggregableObject; IsPublished: Boolean): IUnknown;
begin
  Result := Add(IUnknown(Contained), IsPublished);
end;

function TSilAggregationObject.Add(Contained: TSilAggregableObject; const IID: TGUID; Field: PPointer; IsPublished: Boolean): IUnknown;
begin
  Result := Add(IUnknown(Contained), IID, Field);
end;

function TSilAggregationObject.Add(Contained: TSilAggregableClass; const Owner: IUnknown; IsPublished: Boolean; const Controller: IUnknown; Param: Pointer): IUnknown;
begin
  Result := Add(Contained.Create(GetMainController(Controller), Owner, Param));
end;

function TSilAggregationObject.Add(Contained: TSilAggregableClass; const IID: TGUID; Field: PPointer; const Owner: IUnknown; IsPublished: Boolean; const Controller: IUnknown; Param: Pointer): IUnknown;
begin
  Result := Add(Contained.Create(GetMainController(Controller), Owner, Param), IID, Field);
end;

procedure TSilAggregationObject.Remove(Contained: TSilAggregableObject; Field: PPointer);
var
  Instance: IUnknown;
begin
  if Assigned(Contained) and Assigned(FList) then
  begin
    Instance := Contained;
    FList.Remove(@Instance);
    if Assigned(Field) then Field^ := nil;
  end;
end;

procedure TSilAggregationObject.Remove(Contained: TSilAggregableClass; Field: PPointer);
var
  Data: ^RData;
  Cursor: IContainerPointerStatic;
  Item: IObject;
  Found: Boolean;
begin
  Locked;
  
  if Assigned(Contained) and Assigned(FList) and (FList.Items.Count > 0) then
  begin
    Cursor := FList.Cursors.First;
    Found := False;

    while not Found and Cursor.IsValid do
    begin
      Data := Cursor.Data;

      if not Assigned(Data.Instance) then Continue;

      if Data.Instance.QueryInterface(IObject, Item) = S_OK then
        try
          Found := Item.ClassType.InheritsFrom(Contained);
        finally
          Item := nil;
        end;

      if Found then
      begin
        FList.Items.Delete(Cursor);
        if Assigned(Field) then Field^ := nil;
      end else
        Cursor.Next();
    end;
  end;
end;

procedure TSilAggregationObject.Remove(const Contained: IUnknown; Field: PPointer);
begin
  if Assigned(Contained) and Assigned(FList) then
  begin
    FList.Remove(@Contained);
    if Assigned(Field) then Field^ := nil;
  end;
end;

function TSilAggregationObject.Enumerate(var Enum: IEnumerator; const IID: TGUID; out Obj): Boolean;
var
  Data: ^RData;
begin
  repeat

    if Enum <> nil then
      Result := Enum.Next
    else if GetEnumerator(Enum, Lockable <> nil) then
      Result := Enum.HasMore
    else
      Result := false;

    if Result then
    begin
      Enum.Get(Data);
      if not Assigned(Data) then 
        Continue;
    end else
      Enum := nil;

  until not Result or Ref.Get(Data.Instance, IID, Obj);
end;

procedure TSilAggregationObject.Clear;
begin
  if Assigned(FList) then
    FList.Items.Clear;
end;

function TSilAggregationObject.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Ord(Ref.SameObject(RData(Data1^).Instance, RData(Data2^).Instance)) - 1;
end;

procedure TSilAggregationObject.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(TypeInfo(RData), DoCompare);
end;

end.
