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

unit SilLtReference;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiReference,
  SilLiTraits,
  SilLiCompare,
  SilLiObject,
  SilLiExtensible,
  SilBkTool,
  SilLhAggregation;

type
  Reference = class(Tool)
    class function GetInstance(const AObject: IUnknown): Pointer;
    class function GetObject(const AObject: IUnknown; const IID: TGUID; AClass: TClass): Pointer;
    class function GetInterface(const AObject: IUnknown; const IID: TGUID; out Obj): Boolean; overload;
    class function GetInterface(const AObject: TObject; const IID: TGUID; out Obj; RaiseError: Boolean = False): Boolean; overload;
    class function SameObject(const Object1, Object2: IUnknown; const IID: TGUID ): Boolean; overload;
    class function SameObject(const Object1, Object2: IUnknown): Boolean; overload;
    class function Compare(const Object1, Object2: IUnknown; const Argument: Pointer = nil): Integer;
    class function Supports(const AObject: IUnknown; const IID: TGUID): Boolean;
    class procedure Free(var Obj);
    class function AddRef(const Instance: TObject): Pointer; overload;
    class procedure Release(var Instance); overload;
    class function AddRef(const Value: IUnknown): Pointer; overload;
    class function Release(const Value: Pointer): Pointer; overload; deprecated; // SE CAMBIA POR DROPREF
    class function DropRef(const Value: Pointer): Pointer; overload;
    class function Extract(const Value: Pointer; const IID: TGUID; out Obj): Boolean;
    class function ExtractInterface(const AObject: IUnknown; const IID: TGUID; out Ptr: Pointer): Boolean;
    class function IsNull(const Value: IUnknown; const Default: IUnknown = nil): IUnknown;
    class function IIf(Condition: Boolean; const IfTrue: IUnknown; const IfFalse: IUnknown = nil): IUnknown;
    class procedure Destroy(var Obj); reintroduce;
    class function Get(const AObject: IUnknown; const IID: TGUID; out Obj; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const AObject: TObject; const IID: TGUID; out Obj; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const AObject: IUnknown; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const AObject: TObject; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const Value: Pointer; const IID: TGUID; out Obj; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const Value: Pointer; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean = False): Boolean; overload;
    class function Get(const AObject: IUnknown; const IID: TGUID; RaiseError: Boolean = False): Pointer; overload;
    class function Get(const AObject: TObject; const IID: TGUID; RaiseError: Boolean = False): Pointer; overload;
    class function Get(const Obj: IUnknown; Ptr: PPointer): Boolean; overload;
    class function MainInterface(const Instance: IUnknown): IUnknown;
    class function Extend(const AObject: IExtensible): IAggregation;
    class procedure Result(ResultPtr: PUnknown; const Value: IUnknown); overload;
    class procedure Result(ResultPtr: PUnknown; Value: PUnknown); overload;
    class function Weak(const Instance: IUnknown): IUnknown; overload;
    class function Adopt(var Reference: RAdoptedRef; Value: IUnknown): Boolean; overload;
    class function Adopt(var Reference: RAdoptedRef; const IID: TGUID; Value: IUnknown): Boolean; overload;
  end;

  Ref = Reference;

implementation

uses
  SilLfTraits,
  SilLkObject,
  SilLkAggregation,
  SilLmWeakReference,
  SilBtInt,
  SilOcTypes,
  SilOsError;

function Succeeded(Code: HResult): Boolean;
begin
  Result := (Code and $80000000 = 0);
end;

function Failed(Code: HResult): Boolean;
begin
  Result := (Code and $80000000 <> 0);
end;

function DoCheck(State: Boolean; RaiseError: Boolean; const Where: string = ''; Code: HResult = E_NOINTERFACE): Boolean; overload;
begin
  Result := State;
  if not Result and RaiseError then
    raise OsError.Create(OsCode(Code), Where);
end;

function DoCheck(Code: HResult; RaiseError: Boolean; const Where: string = ''): Boolean; overload;
begin
  Result := DoCheck(Succeeded(Code), RaiseError, Where, Code);
end;

{ Reference }

class function Reference.GetInstance(const AObject: IUnknown): Pointer;
var
  Ref: IReferenceable;
begin
  if Assigned(AObject) and (AObject.QueryInterface(IReferenceable, Ref) = 0) then
    Result := Ref.Instance else
    Result := nil;
end;

class function Reference.GetObject(const AObject: IUnknown; const IID: TGUID; AClass: TClass): Pointer;
  var Entry: PInterfaceEntry;
begin
  Result := nil;
  if AObject <> nil then
  begin
    Entry := AClass.GetInterfaceEntry(IID);
    if Entry = nil then Exit;
    Result := Pointer(Integer(Pointer(AObject)) - Entry.IOffset);
  end;
end;

class function Reference.GetInterface(const AObject: IUnknown; const IID: TGUID; out Obj): Boolean;
begin
  if Assigned(AObject) then
    Result := Succeeded(AObject.QueryInterface(IID, Obj)) else
    Result := False;
end;

class function Reference.GetInterface(const AObject: TObject; const IID: TGUID; out Obj; RaiseError: Boolean): Boolean;
begin
  Pointer(Obj) := nil;
  if Assigned(AObject) then
    Result := DoCheck(AObject.GetInterface(IID, Obj), RaiseError, 'Reference.GetInterface [IUnknown.QueryInterface]') else
    Result := False;
end;

class function Reference.SameObject(const Object1, Object2: IUnknown; const IID: TGUID): Boolean;
var
  U1, U2: IUnknown;
begin
  Result := GetInterface(Object1, IID, U1) and GetInterface(Object2, IID, U2) and (Pointer(U1) = Pointer(U2));
end;

class function Reference.SameObject(const Object1, Object2: IUnknown): Boolean;
var
  U1, U2: IUnknown;
begin
  Result := GetInterface(Object1, IUnknown, U1) and GetInterface(Object2, IUnknown, U2) and (Pointer(U1) = Pointer(U2));
end;

class function Reference.Compare(const Object1, Object2: IUnknown; const Argument: Pointer): Integer;
var
  Comp1, Comp2: IComparable;
  Res1, Res2: Integer;
begin
  if GetInterface(Object1, IComparable, Comp1) then
    Res1 := + Comp1.CompareTo(Object2) else
    Res1 := -1;

  if GetInterface(Object2, IComparable, Comp2) then
    Res2 := - Comp2.CompareTo(Object1) else
    Res2 := +1;

  if (Comp1 <> nil) or (Comp2 <> nil) then
    begin
      if (Res1 > 0) and (Res2 > 0) then
        Result := Int.Max(Res1, Res2)
      else if (Res1 = 0) or (Res2 = 0) then
        Result := 0
      else
        Result := Int.Min(Res1, Res2);
    end
  else
    Result := Ord(SameObject(Object1, Object2)) - 1;
end;

class function Reference.Supports(const AObject: IUnknown; const IID: TGUID): Boolean;
var
  Ref: IUnknown;
begin
  Result := Assigned(AObject) and Succeeded(AObject.QueryInterface(IID, Ref));
end;

class procedure Reference.Free(var Obj);
var
  P: TObject;
  R: TSilObject;
begin
  if Assigned(Pointer(Obj)) then
    if TObject(Obj) is TSilObject then
    begin
      R := TSilObject(Obj);
      TSilObject(Obj) := nil;
      R.Free;
    end else
    begin
      P := TObject(Obj);
      TObject(Obj) := nil;
      P.Free;
    end;
end;

class function Reference.AddRef(const Instance: TObject): Pointer;
begin
  if Instance is TSilObject then
    TSilObject(Instance)._AddRef;
  Result := Instance;
end;

class procedure Reference.Release(var Instance);
begin
  if (TObject(Instance) <> nil) and (TObject(Instance) is TSilObject) then
    TSilObject(Instance).Release;
end;

class function Reference.AddRef(const Value: IUnknown): Pointer;
begin
  Result := nil;
  IUnknown(Result) := Value;
end;

class function Reference.Release(const Value: Pointer): Pointer;
begin
  Result := Value;
  IUnknown(Result) := nil;
end;

class function Reference.DropRef(const Value: Pointer): Pointer;
begin
  Result := Value;
  IUnknown(Result) := nil;
end;

class function Reference.Extract(const Value: Pointer; const IID: TGUID; out Obj): Boolean;
begin
  Result := Assigned(Value) and (IUnknown(Value).QueryInterface(IID, Obj) = 0);
end;

class function Reference.ExtractInterface(const AObject: IUnknown; const IID: TGUID; out Ptr: Pointer): Boolean;
var
  tmp: IUnknown;
begin
  if Assigned(AObject) then
    Result := Succeeded(AObject.QueryInterface(IID, tmp)) else
    Result := False;

  if Result then
    Ptr := Pointer( tmp ) else
    Ptr := nil;
end;

class function Reference.IsNull(const Value, Default: IInterface): IUnknown;
begin
  if Assigned(Value) then
    Result := Value else
    Result := Default;
end;

class function Reference.IIf(Condition: Boolean; const IfTrue, IfFalse: IInterface): IUnknown;
begin
  if Condition then
    Result := IfTrue else
    Result := IfFalse;
end;

class procedure Reference.Destroy(var Obj);
var
  i: Integer;
begin
  i := IInterface(Obj)._Release;

  if i <> 0 then
    raise OsError.Create('object destroy failed (%d refs left)', [i]) else
    Pointer(Obj) := nil;
end;

class function Reference.Get(const AObject: IInterface; const IID: TGUID; out Obj; RaiseError: Boolean): Boolean;
begin
  Result := Assigned(AObject)
        and DoCheck(AObject.QueryInterface(IID, Obj), RaiseError, 'Reference.Get(IUnknown, @IUnknown) [IUnknown.QueryInterface]');
end;

class function Reference.Get(const AObject: TObject; const IID: TGUID; out Obj; RaiseError: Boolean): Boolean;
begin
  Result := Assigned(AObject)
        and DoCheck(AObject.GetInterface(IID, Obj), RaiseError, 'Reference.Get(TObject, @IUnknown) [TObject.GetInterface]');
end;

class function Reference.Get(const AObject: IInterface; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean): Boolean;
var
  Obj: IUnknown;
begin
  Result := Assigned(AObject) and DoCheck(AObject.QueryInterface(IID, Obj), RaiseError, 'Reference.Get(IUnknown, @Pointer) [IUnknown.QueryInterface]');
  if Assigned(Ptr) then Result := Get(Obj, Ptr);
end;

class function Reference.Get(const AObject: TObject; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean): Boolean;
var
  Obj: IUnknown;
begin
  Result := Assigned(AObject) and DoCheck(AObject.GetInterface(IID, Obj), RaiseError, 'Reference.Get(TObject, @Pointer) [TObject.GetInterface]');
  if Assigned(Ptr) then Result := Get(Obj, Ptr);
end;

class function Reference.Get(const Value: Pointer; const IID: TGUID; out Obj; RaiseError: Boolean): Boolean;
begin
  Result := Assigned(Value)
        and DoCheck(IUnknown(Value).QueryInterface(IID, Obj), RaiseError, 'Reference.Get(Pointer, @IUnknown) [IUnknown.QueryInterface]');
end;

class function Reference.Get(const Value: Pointer; const IID: TGUID; Ptr: PPointer; RaiseError: Boolean): Boolean;
var
  Obj: IUnknown;
begin
  Result := Assigned(Value) and DoCheck(IUnknown(Value).QueryInterface(IID, Obj), RaiseError, 'Reference.Get(Pointer, @Pointer) [IUnknown.QueryInterface]');
  if Assigned(Ptr) then Result := Get(Obj, Ptr);
end;

class function Reference.Get(const AObject: IInterface; const IID: TGUID; RaiseError: Boolean): Pointer;
begin
  Get(AObject, IID, @Result, RaiseError);
end;

class function Reference.Get(const AObject: TObject; const IID: TGUID; RaiseError: Boolean): Pointer;
begin
  Get(AObject, IID, @Result, RaiseError);
end;

class function Reference.Get(const Obj: IUnknown; Ptr: PPointer): Boolean;
begin
  if Assigned(Ptr) then
  begin
    Result := Assigned(Obj);
    if Result then Ptr^ := Pointer(Obj);
  end else
    Result := False;
end;

class function Reference.MainInterface(const Instance: IInterface): IUnknown;
var
  Obj: IObject;
begin
  if Get(Instance, IObject, Obj) then
    Result := Obj.MainInterface else
    Result := Instance;
end;

class function Reference.Extend(const AObject: IExtensible): IAggregation;
begin
  ASSERT(Assigned(AObject));

  if not AObject.HasExtension then
    AObject.Extension := TSilAggregationObject.Create(MainInterface(AObject), AObject, GetInstance(AObject));

  Result := AObject.Extension as IAggregation;
end;

class procedure Reference.Result(ResultPtr: PUnknown; const Value: IInterface);
begin
  Result(ResultPtr, @Value);
end;

class procedure Reference.Result(ResultPtr, Value: PUnknown);
begin
  if Assigned(ResultPtr) then
    if (ResultPtr^ = nil) or (Pointer(ResultPtr^) <> Pointer(Value)) then
      if Assigned(Value) then
        ResultPtr^ := Value^ else
        ResultPtr^ := nil;
end;

class function Reference.Weak(const Instance: IInterface): IUnknown;
begin
  Result := TSilWeakReference.Create(Instance);
end;

class function Reference.Adopt(var Reference: RAdoptedRef; Value: IInterface): Boolean;
begin
  Result := SilLfTraits.Adopt(Reference, Value);
end;

class function Reference.Adopt(var Reference: RAdoptedRef; const IID: TGUID; Value: IInterface): Boolean;
begin
  Result := SilLfTraits.Adopt(Reference, IID, Value);
end;

end.

