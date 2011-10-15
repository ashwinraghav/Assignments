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

unit SilOkThreadList;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilOiHandle,
  SilOiThread,

  SilLmInterfaceList;

type
  TSilThreadList = class (
    // extends
    TSilInterfaceList,
    // implements
    IThreads,
    IThreadList)
  private
    FDestroying: Boolean;
  protected // IThreadList
    function GetDestroying: Boolean;
    procedure Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref); virtual;
    procedure ThreadCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer); virtual; 
    procedure RemoveForeign;
    function First: IThread;
    function Last: IThread;
    function GetItem(const Index: Variant): IThread;
    function Add(const Item: IThread): Integer; reintroduce;
    function Remove(const Item: IThread): Integer; reintroduce;
    function IndexOf(const Item: IThread): Integer; reintroduce;
    function FindThreadID(ID: LongWord; out Thread: IThread): Boolean;
  public
    constructor Create; reintroduce; virtual;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  SilBtVart,
  SilOtTool;

{ TSilThreadList }

constructor TSilThreadList.Create;
begin
  inherited Create(true);
end;

procedure TSilThreadList.BeforeDestruction;
begin
  FDestroying := true;
  inherited;
end;

function TSilThreadList.First: IThread;
begin
  Result := GetItem(0);
end;

function TSilThreadList.Last: IThread;
begin
  Result := GetItem(Count - 1);
end;

function TSilThreadList.GetItem(const Index: Variant): IThread;
var
  e: IEnumerator;
  sName: String;
begin
  if Vart.VType(Index) = varString then
  begin
    sName := Index;
    while Enumerate(e, Result) do
      if Result.Name = sName then Exit;
    Result := nil;
  end else
    Result := IThread(inherited GetItem(Index));
end;

function TSilThreadList.GetDestroying: Boolean;
begin
  Result := FDestroying;
end;

function TSilThreadList.Add(const Item: IThread): Integer;
begin
  Result := inherited Add(Item);
end;

function TSilThreadList.IndexOf(const Item: IThread): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TSilThreadList.Remove(const Item: IThread): Integer;
begin
  Result := inherited Remove(Item);
end;

function TSilThreadList.FindThreadID(ID: LongWord; out Thread: IThread): Boolean;
var
  i: Integer;
  Item: Pointer;
begin
  Result := false;

  try
    Lock;

    for i := 0 to Count - 1 do
    begin
      Item := ItemPtr(i);

      if IThread(Item).ThreadID = ID then
      begin
        Thread := IThread(Item);
        Result := true;
        Exit;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TSilThreadList.RemoveForeign;
var
  Enum: IEnumerator;
  Item: IThread;
begin
  while Enumerate(Enum, Item) do
    if not Item.Handle.IsOwned then
      Delete(Enum.Iteration);
end;

procedure TSilThreadList.Call(const Obj: IInterface; Mode: TThreadCallMode; Method: TThreadMethod; const Ref);
begin
end;

procedure TSilThreadList.ThreadCall(const Obj: IInterface; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer);
begin                                                      
end;

end.
