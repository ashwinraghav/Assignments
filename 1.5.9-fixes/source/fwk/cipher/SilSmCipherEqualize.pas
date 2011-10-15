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

unit SilSmCipherEqualize;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiCipher,
  SilSiHash;

type
  TEqualize = class (
    // extends
    TSilInterfacedObject,
    // implements
    ICipher)
  private
    FKey: String;
    FHash: IHash;
  private
    function DoCheckHash: String;
    function Bit1Count(const AMsg: string): integer; overload;
    function Bit1Count(AInt: integer): integer; overload;
    function Bit1Count(AByte: char): integer; overload;
    function BitEqualize(var BDif: integer; AByte: char; C1Cnt: integer): char;
    procedure EncryptStr(var AMsg: string; AKey: string; RoundSize: integer = 0);
    function DecryptStr(var AMsg: string; AKey: string): boolean;
  protected // ICipher
    function GetName: String;
    function GetId: TGuid;
    procedure Initialize(const Key: String; const Hash: IHash = nil);
    procedure Encode(const Source; out Dest: String; Size: LongWord);
    procedure Decode(const Source; out Dest: String; Size: LongWord);
  public
    constructor Create;
    class function Name: String;
  end;

implementation

const
  CId: TGuid = '{04541A45-AE6F-4769-A251-9BAEA1D0F326}';

{ TEqualize }

constructor TEqualize.Create;
begin
  inherited Create;
end;

function TEqualize.GetName: String;
begin
  Result := Name;
end;

class function TEqualize.Name: String;
begin
  Result := 'Equalize';
end;

procedure TEqualize.Initialize(const Key: String; const Hash: IHash);
begin
  FKey := Key;
  FHash := Hash;
end;

function TEqualize.DoCheckHash: String;
begin
  if FHash <> nil then
    FHash.Calculate(FKey, Result) else
    Result := FKey;
end;

procedure TEqualize.Encode(const Source; out Dest: String; Size: LongWord);
begin
  SetLength(Dest, Size);
  Move(Source, Dest[1], Size);
  EncryptStr(Dest, DoCheckHash);
end;

procedure TEqualize.Decode(const Source; out Dest: String; Size: LongWord);
begin
  SetLength(Dest, Size);
  Move(Source, Dest[1], Size);
  DecryptStr(Dest, DoCheckHash);
end;

function TEqualize.Bit1Count(const AMsg: string): integer;
var
  i1: integer;
begin
  result := 0;
  for i1 := 1 to Length( AMsg ) do
    Inc( result, Bit1Count( AMsg[ i1 ] ) );
end;

function TEqualize.Bit1Count(AInt: integer): integer;
var
  s1: string;
begin
  SetLength( s1, sizeof( AInt ) );
  Move( AInt, s1[ 1 ], sizeof( AInt ) );
  result := Bit1Count( s1 );
end;

function TEqualize.Bit1Count(AByte: char): integer;
begin
  result :=
    Byte( AByte ) and $80 shr 7 +
    Byte( AByte ) and $40 shr 6 +
    Byte( AByte ) and $20 shr 5 +
    Byte( AByte ) and $10 shr 4 +
    Byte( AByte ) and $08 shr 3 +
    Byte( AByte ) and $04 shr 2 +
    Byte( AByte ) and $02 shr 1 +
    Byte( AByte ) and $01;
end;

function TEqualize.BitEqualize(var BDif: integer; AByte: char; C1Cnt: integer): char;
begin
  result := char( $A5 );
  Dec( BDif, 2 * C1Cnt - 8 );
  if ( BDif >= 16 ) then
  begin
    result := char( $00 );
    Inc( BDif, - 8 );
  end
  else if ( BDif > 0 ) then
  begin
    result := char( $000F shr ( BDif div 2 ) );
    Inc( BDif, 2 * Bit1Count( byte( result ) ) - 8 );
  end
  else if ( BDif <= -16 ) then
  begin
    result := char( $FF );
    Inc( BDif, 8 );
  end
  else if ( BDif < 0 ) then
  begin
    result := char( $FFF0 shr ( -BDif div 2 ) );
    Inc( BDif, 2 * Bit1Count( byte( result ) ) - 8 );
  end;
end;

procedure TEqualize.EncryptStr(var AMsg: string; AKey: string; RoundSize: integer);
var
  i1, i2, i3, i4, i5, i6,
  bdif, ksize, msize, csize, rsize, nsize: integer;
  ch: char;
begin
  msize := Length( AMsg );
  ksize := Length( AKey );

  // Diferencia bits en 1 - bits en 0
  bdif := 2 * ( Bit1Count( AMsg ) + Bit1Count( msize ) ) -
    8 * ( msize + sizeof( integer ) );
  if ( bdif >= 0 ) then
    csize := ( bdif + 7 ) div 8 else csize := ( -bdif + 7 ) div 8;
  rsize := msize + csize + sizeof( integer );

  // Calcula el tamanio del chunk neutro
  if ( RoundSize > 0 ) then
    nsize := ( RoundSize - rsize mod RoundSize ) mod RoundSize else
    nsize := 0;
  Inc( rsize, nsize );

  // Ajusta y almacena el tamanio del mensaje
  SetLength( AMsg, rsize );
  Move( msize, AMsg[ 1 + rsize - sizeof( integer ) ], sizeof( integer ) );

  // Crea el mesaje destino junto con el chunk equalizador
  if ( bdif > 0 ) then
  begin
    Dec( bdif, 8 * csize );
    FillChar( AMsg[ msize + 1 ], csize, $00 )
  end
  else
  begin
    Inc( bdif, 8 * csize );
    FillChar( AMsg[ msize + 1 ], csize, $FF );
  end;
  if ( bdif <> 0 ) then
    AMsg[ msize + 1 ] :=
      BitEqualize( bdif, AMsg[ msize + 1 ], Bit1Count( AMsg[ msize + 1 ] ) );

  // Almacena el chunk neutro
  if ( nsize > 0 ) then
    for i1 := msize + csize + 1 to msize + csize + nsize do
    begin
      AMsg[ i1 ] := char( Random( 256 ) );
      i2 := Bit1Count( byte( AMsg[ i1 ] ) );
      Inc( bdif, 2 * i2 - 8 );
      if ( bdif < -7 ) or ( 7 < bdif ) or
        ( i1 = msize + csize + nsize ) then
        AMsg[ i1 ] := BitEqualize( bdif, AMsg[ i1 ], i2 );
    end;

  {if ( 2 * Bit1Count( AMsg ) <> 8 * rsize ) then
    raise Exception.Create( 'Todo mal' );{}

  // Mezcla el mensaje
  for i1 := 1 to ( rsize div 4 ) do
  begin
    ch := AMsg[ 2 * i1 ];
    AMsg[ 2 * i1 ] := AMsg[ rsize - 2 * i1 + 1 ];
    AMsg[ rsize - 2 * i1 + 1 ] := ch;
  end;

  // Hace un batido de bits
  for i1 := 1 to rsize do
  begin
    i3 := 1 + ( i1 - 1 ) mod ksize;
    i2 := 1 + ( i1 - 1 + byte( AKey[ i3 ] ) ) mod rsize;
    i4 := byte( AKey[ i3 ] ) and $0F;
    i4 := i4 + i4 shl 4;
    if ( i1 <> i2 ) then
    begin
      i5 := byte( AMsg[ i1 ] ) and not i4 + i4 and not byte( AMsg[ i2 ] );
      i6 := byte( AMsg[ i2 ] ) and not i4 + i4 and not byte( AMsg[ i1 ] );
      AMsg[ i1 ] := char( i6 );
      AMsg[ i2 ] := char( i5 );
    end
    else
      AMsg[ i1 ] := char( byte( AMsg[ i1 ] ) xor i4 );
  end;
end;

function TEqualize.DecryptStr(var AMsg: string; AKey: string): boolean;
var
  i1, i2, i3, i4, i5, i6,
  ksize, msize, rsize: integer;
  dst: string;
  ch: char;
begin
  result := false;
  rsize := Length( AMsg );
  ksize := Length( AKey );

  // Copia el mensaje
  SetLength( dst, rsize );
  Move( AMsg[ 1 ], dst[ 1 ], rsize );

  // Deshace el batido de bits
  for i1 := rsize downto 1 do
  begin
    i3 := 1 + ( i1 - 1 ) mod ksize;
    i2 := 1 + ( i1 - 1 + byte( AKey[ i3 ] ) ) mod rsize;
    i4 := byte( AKey[ i3 ] ) and $0F;
    i4 := i4 + i4 shl 4;
    if ( i1 <> i2 ) then
    begin
      i5 := byte( dst[ i1 ] ) and not i4 + i4 and not byte( dst[ i2 ] );
      i6 := byte( dst[ i2 ] ) and not i4 + i4 and not byte( dst[ i1 ] );
      dst[ i1 ] := char( i6 );
      dst[ i2 ] := char( i5 );
    end
    else
      dst[ i1 ] := char( byte( dst[ i1 ] ) xor i4 );
  end;

  // Varifica la paridad de los bits
  if ( 2 * Bit1Count( dst ) <> 8 * rsize ) then
    exit;

  // Dehace la mezcla el mensaje
  for i1 := 1 to ( rsize div 4 ) do
  begin
    ch := dst[ 2 * i1 ];
    dst[ 2 * i1 ] := dst[ rsize - 2 * i1 + 1 ];
    dst[ rsize - 2 * i1 + 1 ] := ch;
  end;

  // Obtiene y valida el tamanio del mensaje
  Move( dst[ 1 + rsize - sizeof( integer ) ], msize, sizeof( integer ) );
  if ( msize >= rsize - sizeof( integer ) ) then
    exit;

  // Copia el mensaje
  AMsg := dst;
  SetLength( AMsg, msize );
  result := true;
end;

function TEqualize.GetId: TGuid;
begin
  Result := CId;
end;

end.
