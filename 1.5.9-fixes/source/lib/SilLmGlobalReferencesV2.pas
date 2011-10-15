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

unit SilLmGlobalReferencesV2;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  
  SilLiEnumerator,
  SilLeGlobalServicesV2,
  SilLiGlobalServicesV2,
  SilLmPointerList;

type
  TSilGlobalReferencesV2 = class(
    TSilPointerList,
    IGlobalReferencesV2 )
  private
    FOwner: Pointer;
  private
    function DoGetOwner: IGlobalServiceV2;
  protected
    function Add(Item: Pointer): Integer; overload; override;
    function Remove(Item: Pointer): Integer; overload; override;
  protected // IGlobalReferencesV2
    function Enumerate(var Enum: IEnumerator; out Item: RGlobalRef): Boolean; reintroduce;
    procedure Add(const Ref: PUnknown; Module: LongWord); reintroduce; overload;
    function Remove(const Ref: PUnknown; Module: LongWord): Boolean; reintroduce; overload;
    function Remove(Module: LongWord): Boolean; reintroduce; overload;
  public
    constructor Create(const Owner: IGlobalServiceV2); reintroduce;
    destructor Destroy; override;
  public
    property Owner: IGlobalServiceV2 read DoGetOwner;
  end;

implementation

uses
  SilBkPtr,
  SilLfGlobalServiceV2; 

type
  GlobalRefHandler = class (DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

{ GlobalRefHandler }

class procedure GlobalRefHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  Result := System.New(PGlobalRef);
end;

class procedure GlobalRefHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PGlobalRef(Value));
end;

class procedure GlobalRefHandler.Clear(var Obj; const Data: Pointer);
begin
  if Assigned(RGlobalRef(Obj).Ref) then
    RGlobalRef(Obj).Ref^ := nil;    
end;

class function GlobalRefHandler.Compare(const Value1, Value2; Data: Pointer): Integer;
begin
  Result := Integer(PGlobalRef(Value1).Ref) - Integer(PGlobalRef(Value2).Ref);
end;

class procedure GlobalRefHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PGlobalRef(Dest)^ := PGlobalRef(Source)^;   
end;

{ TSilGlobalReferencesV2 }

constructor TSilGlobalReferencesV2.Create(const Owner: IGlobalServiceV2);
begin
  inherited Create(False, GlobalRefHandler);
  MakeRef(Owner, @FOwner);
end;

destructor TSilGlobalReferencesV2.Destroy;
begin
  DropRef(@FOwner);
  inherited;
end;

function TSilGlobalReferencesV2.Enumerate(var Enum: IEnumerator; out Item: RGlobalRef): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

procedure TSilGlobalReferencesV2.Add(const Ref: PUnknown; Module: LongWord);
var
  Ptr: RGlobalRef;
begin
  Ptr.Ref := Ref;
  Ptr.Module := Module;
  Add(@Ptr);
end;

function TSilGlobalReferencesV2.Remove(const Ref: PUnknown; Module: LongWord): Boolean;
var
  Enum: IEnumerator;
  Ptr: RGlobalRef;
begin
  Result := False;

  while not Result and inherited Enumerate(Enum, Ptr) do
    Result := (Ptr.Ref = Ref) and ((Module = 0) or (Module = Ptr.Module));

  if Result then
    Remove(@Ptr);

  Result := (Count = 0);
end;

function TSilGlobalReferencesV2.Remove(Module: LongWord): Boolean;
var
  Enum: IEnumerator;
  Ptr: RGlobalRef;
begin
  while inherited Enumerate(Enum, Ptr) do
    if (Module = Ptr.Module) then
      Remove(@Ptr);

  Result := (Count = 0);
end;

function TSilGlobalReferencesV2.Add(Item: Pointer): Integer;
var
  Ptr: PGlobalRef absolute Item;
begin
  Result := ItemAdd(Ptr^);
  DoRefAdded(Owner, Ptr.Ref);
end;

function TSilGlobalReferencesV2.Remove(Item: Pointer): Integer;
var
  Ptr: PGlobalRef absolute Item;
begin
  DoRefRemoved(Owner, Ptr.Ref);
  Result := ItemRemove(Ptr);
end;

function TSilGlobalReferencesV2.DoGetOwner: IGlobalServiceV2;
begin
  Result := IGlobalServiceV2(FOwner);
end;

end.
