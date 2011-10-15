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

unit SilSmResourcePool;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLiInterfaceList,
  SilLiEnumerator,
  SilOiIpc,
  SilSiResourcePool;

type
  TSilResourcePool = class (
    // extends
    TSilObject,
    // implements
    IResourcePool)
  private
    FManager: IPoolItemManager;
    FActive: IInterfaceList;
    FInactive: IInterfaceList;
    FCacheSize: LongWord;
    FSlackSize: LongWord;
    FMaxSize: LongWord;
    FWait: IMutex;
  private
    procedure DoUpdateSize(Size: LongWord);
  protected // IResourcePool
    function GetMaxSize: LongWord;
    procedure SetMaxSize(Value: LongWord);
    function GetSlackSize: LongWord;
    procedure SetSlackSize(Value: LongWord);
    function GetCacheSize: LongWord;
    procedure SetCacheSize(Value: LongWord);
    function GetActualSize: LongWord;
    function GetList: IInterfaceList;
    function Get(out Item: IPoolItem; Timeout: LongWord): Boolean;
    function Select(Param: Pointer; out Item: IPoolItem): Boolean;
    function Release(const Item: IPoolItem): Boolean; reintroduce;
  public
    constructor Create(const Manager: IPoolItemManager; CacheSize: LongWord = 0; MaxSize: LongWord = 0);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtTool,
  SilLtList,
  SilOeWait,
  SilOtTool,
  SilLiLock,
  SilOtIpc;

{ TSilResourcePool }

constructor TSilResourcePool.Create(const Manager: IPoolItemManager; CacheSize: LongWord; MaxSize: LongWord);
begin
  inherited Create;

  FWait := OS.IPC.Mutex();
  FInactive := ListTool.InterfaceList(true);
  FActive := ListTool.InterfaceList(true);

  FManager := Manager;
  FCacheSize := CacheSize;
  FMaxSize := MaxSize;
  FSlackSize := 0;

  DoUpdateSize(FCacheSize);
end;

destructor TSilResourcePool.Destroy;
var
  enum: IEnumerator;
  item: IPoolItem;
begin
  while FActive.Enumerate(enum, item) do
    item.Deactivate;

  FActive.Clear;
  FInactive.Clear;

  inherited;
end;

function TSilResourcePool.GetCacheSize: LongWord;
begin
  Result := FCacheSize;
end;

function TSilResourcePool.GetList: IInterfaceList;
begin
  Result := FActive;
end;

procedure TSilResourcePool.SetCacheSize(Value: LongWord);
var
  Count: LongWord;
begin
  if Value <> FCacheSize then
  begin
    FInactive.Locked;
    FActive.Locked;

    FCacheSize := Value;
    Count := GetActualSize;

    if Count < FCacheSize then
      DoUpdateSize(FSlackSize + FCacheSize - Count);
  end;
end;

function TSilResourcePool.GetSlackSize: LongWord;
begin
  Result := FSlackSize;
end;

procedure TSilResourcePool.SetSlackSize(Value: LongWord);
begin
  if Value <> FSlackSize then
  begin
    FInactive.Locked;
    FSlackSize := Value;

    if LongWord(FInactive.Count) < FSlackSize then
      DoUpdateSize(FSlackSize - LongWord(FInactive.Count));
  end;
end;

function TSilResourcePool.GetActualSize: LongWord;
begin
  Result := FActive.Count + FInactive.Count;
end;

function TSilResourcePool.GetMaxSize: LongWord;
begin
  Result := FMaxSize;
end;

procedure TSilResourcePool.SetMaxSize(Value: LongWord);
begin
  FMaxSize := Value;
end;

procedure TSilResourcePool.DoUpdateSize(Size: LongWord);
var
  Item: IPoolItem;
begin
  FInactive.Locked;

  if Size < FSlackSize then
    Size := FSlackSize;

  while LongWord(FInactive.Count) > Size do
  begin
    FManager.Release(Self, FInactive.Last as IPoolItem);
    FInactive.Delete(FInactive.Count - 1);
  end;

  while (GetActualSize < Size) and ((FMaxSize = 0) or (GetActualSize < FMaxSize)) do
  begin
    Item := FManager.Get(Self);

    if Assigned(Item) then
      FInactive.Add(Item)
    else
      Break;
  end;

  if FWait.IsSignaled then FWait.Release;
end;

function TSilResourcePool.Get(out Item: IPoolItem; Timeout: LongWord): Boolean;
var
  Lock: ILock;
begin
  Result := false;
  Lock := FInactive.Locked;

  if LongWord(FInactive.Count) <= FSlackSize then
    DoUpdateSize(1 + GetActualSize + FSlackSize);

  if FInactive.Count > 0 then
  begin
    Item := FInactive.Last as IPoolItem;
    FInactive.Delete(FInactive.Count - 1);

    if FInactive.Count = 0 then
      FWait.WaitFor(0);

    FActive.Locked;
    FActive.Add(Item);

    Item.Activate;
    Result := true;
  end else
  if Timeout > 0 then
  begin
    Lock.Release;
    Result := FWait.WaitFor(Timeout) = wrSignaled;

    if Result then
    begin
      FInactive.Locked;
      Result := Get(Item, 0);
    end;
  end;
end;

function TSilResourcePool.Select(Param: Pointer; out Item: IPoolItem): Boolean;
begin
  FInactive.Locked;
  FActive.Locked;

  Item := FManager.Select(Self, Param);

  if FInactive.IndexOf(Item) >= 0 then
  begin
    FInactive.Remove(Item);
    FActive.Add(Item);
  end else
  if FActive.IndexOf(Item) < 0 then
    Item := nil;

  Result := Assigned(Item);

  if Result then
    Item.Activate;
end;

function TSilResourcePool.Release(const Item: IPoolItem): Boolean;
begin
  FInactive.Locked;
  FActive.Locked;

  Result := FActive.IndexOf(Item) >= 0;

  if not Result then
    Exit;

  if Item.Deactivate then
  begin
    FInactive.Add(Item);
    FActive.Remove(Item);
    DoUpdateSize(FCacheSize);
  end;
end;

end.

