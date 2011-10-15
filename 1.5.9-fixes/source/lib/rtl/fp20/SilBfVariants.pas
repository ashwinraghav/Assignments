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

unit SilBfVariants;

{$I Defines.inc}

interface

uses
  SilBfVarUtils;

type
  TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);

function VarIsEmpty(const V: Variant): Boolean;
function VarIsNull(const V: Variant): Boolean;
function VarIsArray(const V: Variant): Boolean;

function VarAsType(const V: Variant; AVarType: TVarType): Variant;
function VarCompareValue(const A, B: Variant): TVariantRelationship;
function Unassigned: Variant;
function Null: Variant;
function EmptyParam: Variant;

function VarGetInfo(const Src: Variant; out Data: Pointer): LongWord;

function VarArrayCreate(const Bounds: array of Integer; AVarType: TVarType): Variant;
function VarArrayOf(const Values: array of Variant): Variant;
function VarArrayLowBound(const A: Variant; Dim: Integer): Integer;
function VarArrayHighBound(const A: Variant; Dim: Integer): Integer;
function VarArrayDimCount(const A: Variant): Integer;
function VarArrayLock(const A: Variant): Pointer;
procedure VarArrayUnlock(const A: Variant);
procedure VarClear(var B: Variant);
procedure VarCopy(var dest: variant; const source: variant);

implementation

uses
  SysUtils,
  SilOsTool;

function VarIsEmpty(const V: Variant): Boolean;
begin
  Result := TVarData(V).VType = varEmpty;
end;

function VarIsNull(const V: Variant): Boolean;
begin
  Result := TVarData(V).VType = varNull;
end;

function VarIsArray(const V: Variant): Boolean;
begin
  Result := TVarData(V).VType and varArray <> 0;
end;

function VarAsType(const V: Variant; AVarType: TVarType): Variant;
begin
  try
    VarCast(Result, V, AVarType);
    //writeln(integer(TVarData(Result).VString), '--', PAnsiString(TVarData(Result).VString)^, '--varastype:', TVarData(Result).VType);
  except
    on e: Exception do
    begin
      writeln('VarAsType:' + e.message);
      raise;
    end;
  end;
end;

function VarCompareValue(const A, B: Variant): TVariantRelationship;
begin
  NotImplemented('VarCompareValue');
end;

function Unassigned: Variant;
begin
  TVarData(Result).VType := varEmpty;
end;

function Null: Variant;
begin
  TVarData(Result).VType := varNull;
end;

function EmptyParam: Variant;
begin
  TVarData(Result).VType := varEmpty;
end;

function VarGetInfo(const Src: Variant; out Data: Pointer): LongWord;

  procedure Assign(Size: LongWord; Ptr: Pointer);
  begin
    if Ptr = nil then
      Size := 0;
    Data := Ptr;
    Result := Size;
  end;

begin
  Result := 0;
  Data := nil;

  case TVarData(Src).VType of
    varByte:      Assign(SizeOf(Byte),     @TVarData(Src).VByte);
    {//$IFDEF D60}
    varWord:      Assign(SizeOf(Word),     @TVarData(Src).VWord);
    {//$ENDIF}
    varSmallint:  Assign(SizeOf(SmallInt), @TVarData(Src).VSmallint);
    varInteger:   Assign(SizeOf(Integer),  @TVarData(Src).VInteger);
    {//$IFDEF D60}
    varLongWord:  Assign(SizeOf(LongWord), @TVarData(Src).VLongWord);
    {//$ENDIF}
    varSingle:    Assign(SizeOf(Single),   @TVarData(Src).VSingle);
    varDouble:    Assign(SizeOf(Double),   @TVarData(Src).VDouble);
    varCurrency:  Assign(SizeOf(Currency), @TVarData(Src).VCurrency);
    varDate:      Assign(SizeOf(Double),   @TVarData(Src).VDate);
    varOleStr:    Assign(OsWStr.Len(TVarData(Src).VOleStr), TVarData(Src).VOleStr);
    varBoolean:   Assign(SizeOf(Boolean),  @TVarData(Src).VBoolean);
    varString:
      if Assigned(TVarData(Src).VString) then
        Assign(StrLen(TVarData(Src).VString), TVarData(Src).VString) else
        Assign(0, TVarData(Src).VString);
  end;
end;

function VarArrayCreate(const Bounds: array of Integer; AVarType: TVarType): Variant;
begin
  NotImplemented('VarArrayCreate');
end;

function VarArrayOf(const Values: array of Variant): Variant;
begin
  NotImplemented('VarArrayOf');
end;

function VarArrayLowBound(const A: Variant; Dim: Integer): Integer;
begin
  NotImplemented('VarArrayLowBound');
end;

function VarArrayHighBound(const A: Variant; Dim: Integer): Integer;
begin
  NotImplemented('VarArrayHighBound');
end;

function VarArrayDimCount(const A: Variant): Integer;
begin
  NotImplemented('VarArrayDimCount');
end;

function VarArrayLock(const A: Variant): Pointer;
begin
  NotImplemented('VarArrayLock');
end;

procedure VarArrayUnlock(const A: Variant);
begin
  NotImplemented('VarArrayUnlock');
end;

procedure VarClear(var B: Variant);
begin
  SilBfVarUtils.VarClear(B);
end;

procedure VarCopy(var dest: variant; const source: variant);
begin
  SilBfVarUtils.VarCopy(dest, source);
end;

end.
