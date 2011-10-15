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

unit SilVmStringsWrapper;

{$I Defines.inc}

interface

uses
  Classes,

  Sil;

type
  TStringsWrapper = class (
    // extends
    TSilInterfacedObject,
    // implements
    IEnumerable,
    ILockable,
    IStrings,
    IStringList,
    IList)
  private
    FFreeList: Boolean;
    FList: TStrings;
    FSection: ILockable;
    FIgnoreCase: Boolean;
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; virtual;
    function GetPtr(Index: Integer): Pointer;
    procedure SetPtr(Index: Integer; Value: Pointer);
    function GetItem(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: String);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function DoGetText: String;
    procedure DoSetText(const Value: String);
    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(Value: Boolean);
  protected
    procedure FireAdd(Index: Integer); virtual;
    procedure FireInsert(Index: Integer); virtual;
    procedure FireDelete(Index: Integer); virtual;
    function Enumerate(var Enum: IEnumerator; out Item): Boolean; 
    function IStrings.Enumerate = DoEnumerate;
    function IStringList.Enumerate = DoEnumerate;
    function DoEnumerate(var Enum: IEnumerator; out Item: string): Boolean;
    function Add(const Value: String; Ptr: Pointer): Integer; virtual;
    procedure AddStrings(const Source: IStringList);
    function AddText(const Value: String; const Separator: string = ''): Integer;
    function GetText(const Separator: string = ''): string;
    function IndexOf(const Value: String): Integer;
    function IndexOfMask(const Value: String): Integer;
    function IndexOfPtr(Value: Pointer): Integer;
    procedure Insert(Index: Integer; const Value: String; Ptr: Pointer); virtual;
    function Remove(const Value: String; All: Boolean = false): Integer;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    property Items[Index: Integer]: String read GetItem write SetItem; default;
    property Ptrs[Index: Integer]: Pointer read GetPtr write SetPtr;
    procedure Clear;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure Sort(Compare: TListSortCompare);
    function First: String; virtual;
    function Last: String; virtual;
    function ItemPtr(Index: Integer): Pointer;
  public
    constructor Create(List: TStrings = nil; Locked: Boolean = false; FreeOnDestroy: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  SilLdContainer;

{ TStringsWrapper }

constructor TStringsWrapper.Create(List: TStrings; Locked, FreeOnDestroy: Boolean);
begin
  inherited Create(Locked);
  FFreeList := not Assigned(List) or FreeOnDestroy;

  if not Assigned(List) then
    FList := Classes.TStringList.Create else
    FList := List;
end;

destructor TStringsWrapper.Destroy;
begin
  if FFreeList then Clear;
  FSection := nil;
  if FFreeList then FList.Free;
  inherited;
end;

function TStringsWrapper.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, FSection <> nil) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
    Enum.Get(Item)
  else
    Enum := nil;
end;

function TStringsWrapper.DoEnumerate(var Enum: IEnumerator; out Item: string): Boolean;
begin
  Result := Enumerate(Enum, Item);
end;

function TStringsWrapper.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStringsWrapper.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := FList.Count > 0;
  if Result then Enum := Sil.List.ListEnumerator(Self, Locked, StringHandler);
end;

function TStringsWrapper.GetItem(Index: Integer): String;
begin
  Result := FList.Strings[Index];
end;

procedure TStringsWrapper.SetItem(Index: Integer; const Value: String);
begin
  FList.Strings[Index] := Value;
end;

function TStringsWrapper.GetPtr(Index: Integer): Pointer;
begin
  Result := FList.Objects[Index];
end;

procedure TStringsWrapper.SetPtr(Index: Integer; Value: Pointer);
begin
  FList.Objects[Index] := Value;
end;

procedure TStringsWrapper.AddStrings(const Source: IStringList);
var
  i: Integer;
begin
  try
    if FSection <> nil then FSection.Lock;
    for i := 0 to Source.Count - 1 do Add(Source.Items[i], Source.Ptrs[i]);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

function TStringsWrapper.AddText(const Value, Separator: string): Integer;
begin
  Result := -1;
end;

function TStringsWrapper.GetText(const Separator: string): string;
begin
  Result := '';
end;

function TStringsWrapper.Add(const Value: String; Ptr: Pointer): Integer;
begin
  try
    if FSection <> nil then FSection.Lock;
    Result := FList.AddObject(Value, Ptr);
    FireAdd(Result);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

function TStringsWrapper.IndexOf(const Value: String): Integer;
begin
  try
    if FSection <> nil then FSection.Lock;
    Result := FList.IndexOf(Value);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

function TStringsWrapper.IndexOfPtr(Value: Pointer): Integer;
begin
  try
    if FSection <> nil then FSection.Lock;
    Result := FList.IndexOfObject(Value);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Insert(Index: Integer; const Value: String; Ptr: Pointer);
begin
  try
    if FSection <> nil then FSection.Lock;
    FList.Insert(Index, Value);
    FList.Objects[Index] := Ptr;
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Clear;
var
  i: Integer;
begin
  try
    if FSection <> nil then FSection.Lock;         
    for i := 0 to FList.Count - 1 do FireDelete(i);
    FList.Clear;
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Delete(Index: Integer);
begin
  try
    if FSection <> nil then FSection.Lock;
    FireDelete(Index);
    FList.Delete(Index);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Exchange(Index1, Index2: Integer);
begin
  try
    if FSection <> nil then FSection.Lock;
    FList.Exchange(Index1, Index2);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Move(CurIndex, NewIndex: Integer);
begin
  try
    if FSection <> nil then FSection.Lock;
    FList.Move(CurIndex, NewIndex);
  finally
    if FSection <> nil then FSection.Unlock;
  end;
end;

procedure TStringsWrapper.Sort(Compare: TListSortCompare);
begin
end;

function TStringsWrapper.DoGetText: String;
begin
  Result := FList.Text;
end;

procedure TStringsWrapper.DoSetText(const Value: String);
begin
  FList.Text := Value;
end;

procedure TStringsWrapper.CheckIndex(Index: Integer);
begin
  if not ValidIndex(Index) then Sil.Error.Throw(SListIndexError, [Index]);
end;

function TStringsWrapper.ValidIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FList.Count);
end;

function TStringsWrapper.GetIgnoreCase: Boolean;
begin
  Result := FIgnoreCase;
end;

procedure TStringsWrapper.SetIgnoreCase(Value: Boolean);
begin
  FIgnoreCase := Value;
end;

procedure TStringsWrapper.FireAdd(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
     while Events.Enumerate(n, l, IListEvents) do l.OnListAdd(Self, Index);
end;

procedure TStringsWrapper.FireDelete(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
     while Events.Enumerate(n, l, IListEvents) do l.OnListDelete(Self, Index);
end;

procedure TStringsWrapper.FireInsert(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
     while Events.Enumerate(n, l, IListEvents) do l.OnListInsert(Self, Index);
end;

function TStringsWrapper.First: String;
begin
  Result := FList[0];
end;

function TStringsWrapper.Last: String;
begin
  Result := FList[FList.Count - 1];
end;

function TStringsWrapper.ItemPtr(Index: Integer): Pointer;
begin
  Result := PChar(FList[Index]);
end;

procedure TStringsWrapper.SetCount(Value: Integer);
begin
  // FList.Count := Value;
end;

function TStringsWrapper.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TStringsWrapper.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

function TStringsWrapper.IndexOfMask(const Value: String): Integer;
begin
  for Result := 0 to FList.Count - 1 do
    if Str.WildCard(FList[Result], Value, FIgnoreCase) then
      Exit;

  Result := -1;
end;

function TStringsWrapper.Remove(const Value: String; All: Boolean): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then Delete(Result);
end;

end.
