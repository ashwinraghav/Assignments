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

unit SilSmNtlm;

interface

uses
  Sil,
  SilSeNtlm,
  SilSiNtlm;

type
  TDesCipher = class (TSilObject, IDesCipher)
  private
    FRoundKeys : Array [1..16, 1..48] of Byte;
    FC: Array [1..28] of Byte;
    FD: Array [1..28] of Byte;
    FInputValue  : Array [1..64] of Byte;
    FOutputValue : Array [1..64] of Byte;
    FL, FR, FfunctionResult : Array [1..32] of Byte;
    FKey: String;
    FSmallBuffer: array [0..63] of Byte;
    procedure SubKey(Round: Byte; var SubKey);
    procedure Shift(var SubKeyPart);
  private
    procedure MD4Transform(var state: array of LongWord; block: Pointer);
    procedure MDDecode(output, input: Pointer; len: LongWord);
    procedure EncipherBlock;
    procedure SetKeys;
    procedure DF( var FK );
  protected
    procedure MDInit(var Context: TMD4Ctx);
    procedure MDUpdate(var Context: TMD4Ctx; Input: Pointer; InputLen: LongWord);
    function MDFinal(var Context: TMD4Ctx): String;
    function EcbEncrypt(const Key: String; const Data: array of Byte): String;
  end;

implementation

uses
  SilSfNtlm;

{ TDesCipher }

procedure TDesCipher.MDInit(var Context: TMD4Ctx);
begin
  Context.Count[0] := 0;
  Context.Count[1] := 0;

  Context.State[0] := $67452301;
  Context.State[1] := $efcdab89;
  Context.State[2] := $98badcfe;
  Context.State[3] := $10325476;
end;

procedure TDesCipher.MDDecode(output, input: Pointer; len: LongWord);
var
  i, j: LongWord;
begin
  i := 0; j := 0;
  while j < len do
  begin
    PDWordArray(output)^[i] :=
      PBArray(input)^[j] or
      (PBArray(input)^[j + 1] shl 8) or
      (PBArray(input)^[j + 2] shl 16) or
      (PBArray(input)^[j + 3] shl 24);
      
    Inc(i); Inc(j, 4);
  end;
end;

procedure TDesCipher.MD4Transform(var state: array of LongWord; block: Pointer);
var
  a, b, c, d: LongWord;
  x: array[0..15] of LongWord;
begin
  a := state[0];
  b := state[1];
  c := state[2];
  d := state[3];

  MDDecode(@x, block, 64);

  FF (a, b, c, d, x[ 0], S11);
  FF (d, a, b, c, x[ 1], S12);
  FF (c, d, a, b, x[ 2], S13);
  FF (b, c, d, a, x[ 3], S14);
  FF (a, b, c, d, x[ 4], S11);
  FF (d, a, b, c, x[ 5], S12);
  FF (c, d, a, b, x[ 6], S13);
  FF (b, c, d, a, x[ 7], S14);
  FF (a, b, c, d, x[ 8], S11);
  FF (d, a, b, c, x[ 9], S12);
  FF (c, d, a, b, x[10], S13);
  FF (b, c, d, a, x[11], S14);
  FF (a, b, c, d, x[12], S11);
  FF (d, a, b, c, x[13], S12);
  FF (c, d, a, b, x[14], S13);
  FF (b, c, d, a, x[15], S14);

  GG (a, b, c, d, x[ 0], S21);
  GG (d, a, b, c, x[ 4], S22);
  GG (c, d, a, b, x[ 8], S23);
  GG (b, c, d, a, x[12], S24);
  GG (a, b, c, d, x[ 1], S21);
  GG (d, a, b, c, x[ 5], S22);
  GG (c, d, a, b, x[ 9], S23);
  GG (b, c, d, a, x[13], S24);
  GG (a, b, c, d, x[ 2], S21);
  GG (d, a, b, c, x[ 6], S22);
  GG (c, d, a, b, x[10], S23);
  GG (b, c, d, a, x[14], S24);
  GG (a, b, c, d, x[ 3], S21);
  GG (d, a, b, c, x[ 7], S22);
  GG (c, d, a, b, x[11], S23);
  GG (b, c, d, a, x[15], S24);

  HH (a, b, c, d, x[ 0], S31);
  HH (d, a, b, c, x[ 8], S32);
  HH (c, d, a, b, x[ 4], S33);
  HH (b, c, d, a, x[12], S34);
  HH (a, b, c, d, x[ 2], S31);
  HH (d, a, b, c, x[10], S32);
  HH (c, d, a, b, x[ 6], S33);
  HH (b, c, d, a, x[14], S34);
  HH (a, b, c, d, x[ 1], S31);
  HH (d, a, b, c, x[ 9], S32);
  HH (c, d, a, b, x[ 5], S33);
  HH (b, c, d, a, x[13], S34);
  HH (a, b, c, d, x[ 3], S31);
  HH (d, a, b, c, x[11], S32);
  HH (c, d, a, b, x[ 7], S33);
  HH (b, c, d, a, x[15], S34);

  state[0] := state[0] + a;
  state[1] := state[1] + b;
  state[2] := state[2] + c;
  state[3] := state[3] + d;
end;

procedure TDesCipher.MDUpdate(var Context: TMD4Ctx; Input: Pointer; InputLen: LongWord);
var
  i, index, partLen: LongWord;
begin
  index := (context.count[0] shr 3) and $3F;
  context.count[0] := context.count[0] + inputLen shl 3;

  if context.count[0] < (inputLen shl 3) then
    Inc(context.count[1]);

  context.count[1] := context.count[1] + inputLen shr 29;
  partLen := 64 - index;

  if inputLen >= partLen then
  begin
    Move(input^, context.buffer[index], partLen);
    MD4Transform(context.state, @context.buffer);
    i := partLen;

    while i + 63 < inputLen do
    begin
      MD4Transform(context.state, @PBArray(input)^[i]);
      Inc(i, 64);
    end;
    index := 0;
  end
  else
    i := 0;

  Move(PBArray(input)^[i], context.buffer[index], inputLen - i);
end;

function TDesCipher.MDFinal(var Context: TMD4Ctx): String;
var
  digest: array[0..15] of Char;
  bits: array[0..7] of Char;
  index, padLen: LongWord;
begin
  MDEncode(@bits, @context.count, 8);

  index := (context.count[0] shr 3) and $3f;
  if (index < 56) then
    padLen := 56 - index
  else
    padLen := 120 - index;

  MDUpdate(context, @MD_PADDING, padLen);

  MDUpdate(context, @bits, 8);
  MDEncode(@digest, @context.state, 16);

  FillChar(context, 0, SizeOf(TMD4Ctx));
  Result := Digest;
end;

procedure TDesCipher.EncipherBlock;
var
  n, b, Round : Byte;
begin
  for n := 1 to 64 do
    FInputValue[n]:=GetBit( FSmallBuffer, n );

  for n := 1 to 64 do
    if n <= 32 then
      FL[n] := FInputValue[IP[n]]
    else
      FR[n-32] := FInputValue[IP[n]];

  for Round := 1 to 16 do
  begin
    DF( FRoundKeys[Round] );
    For n := 1 to 32 do
      FfunctionResult[n] := FfunctionResult[n] xor FL[n];
    FL := FR;
    FR := FfunctionResult;
  end;

  for n := 1 to 64 do
  begin
    b := InvIP[n];
    if b <= 32 then
      FOutputValue[n] := FR[b]
    else
      FOutputValue[n] := FL[b-32];
  end;

  for n := 1 to 64 do
    SetBit( FSmallBuffer, n, FOutputValue[n] );
end;

function TDesCipher.EcbEncrypt(const Key: String; const Data: array of Byte): String;
var
  i, j, t, bit: Integer;
begin
  SetLength( FKey, 8 );

  FKey[1] := Key[1];
  FKey[2] := char( ( ( Byte( Key[1] ) shl 7 ) and $FF ) or ( Byte( Key[2] ) shr 1 ) );
  FKey[3] := char( ( ( Byte( Key[2] ) shl 6 ) and $FF ) or ( Byte( Key[3] ) shr 2 ) );
  FKey[4] := char( ( ( Byte( Key[3] ) shl 5 ) and $FF ) or ( Byte( Key[4] ) shr 3 ) );
  FKey[5] := char( ( ( Byte( Key[4] ) shl 4 ) and $FF ) or ( Byte( Key[5] ) shr 4 ) );
  FKey[6] := char( ( ( Byte( Key[5] ) shl 3 ) and $FF ) or ( Byte( Key[6] ) shr 5 ) );
  FKey[7] := char( ( ( Byte( Key[6] ) shl 2 ) and $FF ) or ( Byte( Key[7] ) shr 6 ) );
  FKey[8] := char( ( ( Byte( Key[7] ) shl 1 ) and $FF ) );

  for i := 1 to 8 do
  begin
    for j := 1 to 7 do
    begin
      bit := 0;
      t := Byte( Fkey[i] ) shl j;
      bit :=( t xor bit) and $1;
    end;
    Fkey[i] := char( ( Byte( Fkey[i] ) and $FE ) or bit );
  end;

  SetKeys;

  SetLength( Result, 8 );
  move( Data, FSmallBuffer, 8 );
  EncipherBlock;
  move( FSmallBuffer, Result[1], 8 );
end;

procedure TDesCipher.DF(var FK);
var
  K : Array [1..48] Of Byte absolute FK;
  Temp1 : Array [1..48] Of Byte;
  Temp2 : Array [1..32] Of Byte;
  n, h, i, j, Row, Column : Integer;
begin
  for n:=1 to 48 do
    Temp1[n]:=FR[E[n]] xor K[n];

  for n:=1 to 8 do
  begin
    i := ( n - 1 ) * 6;
    j := ( n -1 ) * 4;
    Row := Temp1[i+1] * 2 + Temp1[i+6];
    Column := Temp1[i+2] * 8 + Temp1[i+3] * 4 + Temp1[i+4] * 2 + Temp1[i+5];
    for h := 1 to 4 Do
    begin
      case h of
        1: Temp2[j+h] := ( SBoxes[n,Row,Column] and 8 ) div 8;
        2: Temp2[j+h] := ( SBoxes[n,Row,Column] and 4 ) div 4;
        3: Temp2[j+h] := ( SBoxes[n,Row,Column] and 2 ) div 2;
        4: Temp2[j+h] := ( SBoxes[n,Row,Column] and 1 );
      end;
    end;
  end;

  for n := 1 to 32 do
    FfunctionResult[n] := Temp2[P[n]];
end;

procedure TDesCipher.Shift( var SubKeyPart );
var
  SKP: Array [1..28] Of Byte absolute SubKeyPart;
  n, b: Byte;
begin
  b := SKP[1];
  for n := 1 to 27 do
    SKP[n] := SKP[n+1];
  SKP[28] := b;
end;

procedure TDesCipher.SubKey( Round: Byte; var SubKey );
var
  SK : Array [1..48] of Byte absolute SubKey;
  n, b : Byte;
begin
  for n := 1 to ShiftTable[Round] do
  begin
    Shift( FC );
    Shift( FD );
  end;
  for n := 1 to 48 do
  begin
    b := PC_2[n];
    if b <= 28 then
      SK[n] := FC[b]
    else
      SK[n] := FD[b-28];
  end;
end;

procedure TDesCipher.SetKeys;
var
 n: Byte;
 Key: Array [0..7] of Byte;
begin
  move( FKey[1], Key, 8 );
  for n := 1 to 28 do
  begin
    FC[n] := GetBit( Key, PC_1[n] );
    FD[n] := GetBit( Key, PC_1[n+28] );
  end;
  for n := 1 to 16 do
    SubKey( n,FRoundKeys[n] );
end;

end.
