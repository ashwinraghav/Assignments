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

unit SilBtMem;

interface

{$I Defines.inc}

uses
  SysUtils,
  
  SilBeTypes,
  SilBkTool,
  SilBeMemMgr,
  SilBkMemMgr;

type
  Mem = class(Tool)
    class procedure Clear(var Block; Size: Cardinal; Value: Byte {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF});
    class procedure Move(const Src, Dest; SrcSize: Integer; SrcOffset: Integer {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}; DestOffset: Integer {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF});
    class function Get(ASize: Integer; ACount: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}): Pointer;
    class function Alloc(ASize: Integer; ACount: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}): Pointer;
    class procedure Realloc(var P: Pointer; ASize: Integer; ACount: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF});
    class function ToStr(const Buffer: Pointer; Size: Integer; WordLength: Integer = 1; const Separator: string = ','): string;
    class function Dump(const Buffer: Pointer; Size: Integer; WordLength: Integer = 1; const Separator: string = ','): string;
    class function ToByteArray(const Buffer: Pointer; Size: Integer): TByteArray;
    class function ToWordArray(const Buffer: Pointer; Size: Integer): TWordArray;
    class function ToIntegerArray(const Buffer: Pointer; Size: Integer): TIntegerArray;
    class function ToLongArray(const Buffer: Pointer; Size: Integer): TLongArray;
    class procedure Free(var Ptr);
    class function Info: RMemoryInfo;
    class function Use(const Manager: MemoryManagerType): MemoryManagerType;
    class function Scan(const Buffer: Pointer; const Value: Pointer; Size: Integer): Pointer; overload; 
    class function Scan(const Buffer: Pointer; const Value: Word; Size: Integer): Pointer; overload; 
    class function Scan(const Buffer: Pointer; const Value: Byte; Size: Integer): Pointer; overload; 
    class function Compare(const Buffer1, Buffer2: Pointer; Size: Integer): Boolean;
    class function Crc16(const Buffer; Size: Integer): Word;
    class function CheckSum16(Buffer: PChar; Size: Integer): Word; //Size expresado en bytes: debe ser multiplo de 2
  end;

implementation

uses
  SilBtMemMgr,
  SilBtInt,
  SilAfMemScan,
  SilAfMemComp;

class function Mem.Alloc(ASize, ACount: Integer): Pointer;
begin
  Result := AllocMem(ASize * ACount);
end;

class procedure Mem.Clear(var Block; Size: Cardinal; Value: Byte);
begin
  FillChar(Block, Size, Value);
end;

class procedure Mem.Free(var Ptr);
var
  P: Pointer;
begin
  P := Pointer(Ptr);
  Pointer(Ptr) := nil;
  FreeMem(P);
end;

class function Mem.Info: RMemoryInfo;
begin
  Result := SilBtMemMgr.GetManager.Info;
end;

class function Mem.Get(ASize, ACount: Integer): Pointer;
begin
  GetMem(Result, ASize * ACount);
end;

//class procedure Mem.Move(const Src; const Dest; SrcSize, DestOffset: Integer);
class procedure Mem.Move(const Src; const Dest; SrcSize, SrcOffset, DestOffset: Integer);
begin
  //System.Move(Src, (PChar(@Dest) + DestOffset)^, SrcSize);
  System.Move((PChar(@Src) + SrcOffset)^, (PChar(@Dest) + DestOffset)^, SrcSize);
end;

class procedure Mem.Realloc(var P: Pointer; ASize, ACount: Integer);
begin
  ReallocMem(P, ASize * ACount);
end;

class function Mem.Use(const Manager: MemoryManagerType): MemoryManagerType;
begin
  Result := SilBtMemMgr.GetManager;
  SilBtMemMgr.SetManager(Manager);
end;

class function Mem.ToStr(const Buffer: Pointer; Size, WordLength: Integer; const Separator: string): string;
var
  P: PByte;
begin
  Result := '';
  if Buffer <> nil then
  begin
    P := Buffer;
    while Size > 0 do
    begin
      if Result <> '' then Result := Result + Separator;
      Result := Result + Int.ToHex(P^, WordLength);
      Inc(P, WordLength);
      Dec(Size, WordLength);
      if Size < 0 then
      begin
        WordLength := -Size;
        Size := WordLength;
      end;
    end;
  end;
end;

class function Mem.Dump(const Buffer: Pointer; Size, WordLength: Integer; const Separator: string): string;
var
  S: string; 
begin
  Result := '[' + Int.ToStr(Size) + ']';
  S := ToStr(Buffer, Size, WordLength, Separator);
  if S <> '' then Result := Result + ' (' + S + ')';
end;

class function Mem.ToByteArray(const Buffer: Pointer; Size: Integer): TByteArray;
begin
  SetLength(Result, Size div SizeOf(Byte));
  Move(Buffer^, Result[0], Size); 
end;

class function Mem.ToWordArray(const Buffer: Pointer; Size: Integer): TWordArray;
begin
  SetLength(Result, Size div SizeOf(Word));
  Move(Buffer^, Result[0], Size);
end;

class function Mem.ToIntegerArray(const Buffer: Pointer; Size: Integer): TIntegerArray;
begin
  SetLength(Result, Size div SizeOf(Integer));
  Move(Buffer^, Result[0], Size);
end;

class function Mem.ToLongArray(const Buffer: Pointer; Size: Integer): TLongArray;
begin
  SetLength(Result, Size div SizeOf(LongWord));
  Move(Buffer^, Result[0], Size);
end;

class function Mem.Scan(const Buffer, Value: Pointer; Size: Integer): Pointer;
begin
  Result := SilAfMemScan.FindPointer(Buffer, Value, Size);
end;

class function Mem.Scan(const Buffer: Pointer; const Value: Word; Size: Integer): Pointer;
begin
  Result := SilAfMemScan.FindWord(Buffer, Value, Size);
end;

class function Mem.Scan(const Buffer: Pointer; const Value: Byte; Size: Integer): Pointer;
begin
  Result := SilAfMemScan.FindByte(Buffer, Value, Size);
end;

class function Mem.Compare(const Buffer1, Buffer2: Pointer; Size: Integer): Boolean;
begin
  Result := SilAfMemComp.CompareMem(Buffer1, Buffer2, Size);
end;

class function Mem.Crc16(const Buffer; Size: Integer): Word;
var
  I: Integer;
  P: PByte;
  Carry: Boolean;
begin
  Result := $FFFF;
  P := @Buffer;

  while Size > 0 do
  begin
    Result := Result xor P^;
    for I := 1 to 8 do
    begin
      Carry := Odd(Result);
      Result := Result shr 1;
      if Carry then Result := Result xor $A001
    end;
    Dec(Size);
    Inc(P);
  end;
end;

class function Mem.CheckSum16(Buffer: PChar; Size: Integer): Word;
var
  P: PWord;
begin
  Result := 0;
  if Assigned(Buffer) then
  begin
    P := PWord(Buffer);
    Size := Size div 2;
    while Size > 0 do
    begin
      Inc(Result, P^);
      Inc(P);
      Dec(Size);
    end;
  end;
end;

end.
