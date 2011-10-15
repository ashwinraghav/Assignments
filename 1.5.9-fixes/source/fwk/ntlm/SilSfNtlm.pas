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

unit SilSfNtlm;

interface

uses
  Sil;

{$include Defines.inc}

function UnicodeString(const Buffer: String; Upper: Boolean): String;
function GetLMHash(Password: String; const Nonce: array of Byte): String;
function GetNTHash(Password: String; const Nonce: array of Byte): String;
procedure FF(var a: LongWord; b, c, d, x, s: LongWord);
procedure GG(var a: LongWord; b, c, d, x, s: LongWord);
procedure HH(var a: LongWord; b, c, d, x, s: LongWord);
procedure MDEncode(output, input: Pointer; len: LongWord);
procedure MDDecode(output, input: Pointer; len: LongWord);
procedure SetBit(var Data; Index, Value: Byte);
function GetBit(var Data; Index : Byte): Byte;

implementation

uses
  SilSiNtlm,
  SilSeNtlm,
  SilSmNtlm;

function GetBit(var Data; Index: Byte): Byte;
var
  Bits: Array [0..7] of Byte absolute Data;
begin
  Dec( Index );
  if Bits[Index div 8] and ( 128 shr( Index mod 8 ) ) > 0 then
    GetBit := 1
  else
    GetBit := 0;
end;

procedure SetBit( var Data; Index, Value : Byte );
var
  Bits: Array [0..7] Of Byte absolute Data;
  Bit: Byte;
begin
  Dec( Index );
  Bit := 128 shr( Index mod 8 );
  case Value of
    0: Bits[Index div 8] := Bits[Index div 8] and ( not Bit );
    1: Bits[Index div 8] := Bits[Index div 8] or Bit;
  end;
end;

function rol(x: LongWord; y: Byte): LongWord; assembler;
asm
  mov   cl,dl
  rol   eax,cl
end;

function F(x, y, z: LongWord): LongWord; assembler;
asm
  and   edx,eax
  not   eax
  and   eax,ecx
  or    eax,edx
end;

function G(x, y, z: LongWord): LongWord; assembler;
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  or    eax,ecx
  pop   ecx
  and   edx,ecx
  or    eax,edx
end;

function H(x, y, z: LongWord): LongWord; assembler;
asm
  xor eax,edx
  xor eax,ecx
end;

procedure FF(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + (F(b, c, d) + x);
  a := rol(a, s);
end;

procedure GG(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + G(b, c, d) + x + $5a827999;
  a := rol(a, s);
end;

procedure HH(var a: LongWord; b, c, d, x, s: LongWord);
begin
  a := a + H(b, c, d) + x + $6ed9eba1;
  a := rol(a, s);
end;

function UnicodeString(const Buffer: String; Upper: Boolean): String;
var
  WideBuf: WideString;
begin
  if Upper then
    WideBuf := Str.ToUpper(Buffer) else
    WideBuf := Buffer;

  SetLength(Result, Length(WideBuf) * 2);
  Move(WideBuf[1], Result[1], Length(Result));
end;

function GetLMHash(Password: String; const Nonce: array of Byte): String;
const
  Magic: array [0..7] of Byte = ($4B, $47, $53, $21, $40, $23, $24, $25 );
var
  PassHash: String;
  Des: IDesCipher;
begin
  Des := TDesCipher.Create;
  Password := Str.ToUpper(Password);

  if Length(Password) > 14 then
    SetLength(Password, 14)
  else
    while Length(Password) < 14 do
      Password := Password + #0;

  PassHash := Des.EcbEncrypt(Str.Copy(Password, 1, 7), Magic);
  PassHash := PassHash + Des.EcbEncrypt(Str.Copy(Password, 8, 7), Magic);
  PassHash := PassHash + #0#0#0#0#0;

  Result := Des.EcbEncrypt(Str.Copy(PassHash, 1, 7), Nonce);
  Result := Result + Des.EcbEncrypt(Str.Copy(PassHash, 8, 7), Nonce);
  Result := Result + Des.EcbEncrypt(Str.Copy(PassHash, 15, 7), Nonce);
end;

function GetNTHash(Password: String; const Nonce: array of Byte): String;
var
  PassHash: String;
  Context: TMD4Ctx;
  Des: IDesCipher;
begin
  Des := TDesCipher.Create;
  Password := UnicodeString(Password, false);

  Des.MDInit(Context);
  Des.MDUpdate(Context, PChar(Password), Length(Password));
  PassHash := Des.MDFinal(Context);
  PassHash := PassHash + #0#0#0#0#0;

  Result := Des.EcbEncrypt(Str.Copy(PassHash, 1, 7), Nonce);
  Result := Result + Des.EcbEncrypt(Str.Copy(PassHash, 8, 7), Nonce);
  Result := Result + Des.EcbEncrypt(Str.Copy(PassHash, 15, 7), Nonce);
end;

procedure MDEncode(output, input: Pointer; len: LongWord);
var
  i, j: LongWord;
begin
  i := 0;
  j := 0;
  
  while j < len do
  begin
    PBArray(output)^[j] := (PDWordArray(input)^[i] and $ff);
    PBArray(output)^[j + 1] := ((PDWordArray(input)^[i] shr 8) and $ff);
    PBArray(output)^[j + 2] := ((PDWordArray(input)^[i] shr 16) and $ff);
    PBArray(output)^[j + 3] := ((PDWordArray(input)^[i] shr 24) and $ff);
    Inc(i);
    Inc(j, 4);
  end;
end;

procedure MDDecode(output, input: Pointer; len: LongWord);
var
  i, j: LongWord;
begin
  i := 0;
  j := 0;
  
  while j < len do
  begin
    PDWordArray(output)^[i] :=
      PBArray(input)^[j] or
      (PBArray(input)^[j + 1] shl 8) or
      (PBArray(input)^[j + 2] shl 16) or
      (PBArray(input)^[j + 3] shl 24);

    Inc(i);
    Inc(j, 4);
  end;
end;

end.
