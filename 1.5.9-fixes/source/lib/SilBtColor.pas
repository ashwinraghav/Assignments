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

unit SilBtColor;

interface

{$I Defines.inc}

uses
//  {$IFDEF D50} Graphics, {$ELSE} QGraphics, {$ENDIF}
  Math,

  SilOsTypes,
  SilBeTypes,
  SilBkTool;


const
  defMakeA  = 255;
  defFactor =   1;

type
  RColorHsl = record
    Hue: Word;
    Sat: Word;
    Lum: Word;
  end;

  TMorphStep = 0..255;
  
type
  Rgb = class(Tool)
  public
    class function GetBlue(c: OsColor): Integer;
    class function GetGreen(c: OsColor): Integer;
    class function GetRed(c: OsColor): Integer;
    class function GetAlfa(c: OsColor): Integer;
    class function Make(r, g, b: Integer; a: Integer {$IFDEF USE_DEFPARAMS} = defMakeA {$ENDIF} ): OsColor;
    class function MakeChange(var c: OsColor; r, g, b: Integer): Boolean;
    class function MakeBrighter(var Color: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = defFactor {$ENDIF} ): Boolean;
    class function MakeDarker(var Color: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = defFactor {$ENDIF} ): Boolean;
    class function Change(c: OsColor; r, g, b: Integer): OsColor;
    class function Brighter(Color: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = defFactor {$ENDIF} ): OsColor;
    class function Darker(Color: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = defFactor {$ENDIF} ): OsColor;
    class function Brighter2(c: OsColor; Factor: Integer): OsColor;
    class function Morph(Origin, Dest: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF} ): OsColor;
    class function MorphHslRel(Origin, Dest: OsColor; Step: TMorphStep {$IFDEF USE_DEFPARAMS} = 50 {$ENDIF} ): OsColor;
    class function MorphRel(Origin, Dest: OsColor; Step: TMorphStep {$IFDEF USE_DEFPARAMS} = 50 {$ENDIF} ): OsColor;
    class procedure HslToRgb(const Hsl: RColorHsl; out R, G, B: Integer);
    class procedure RgbToHsl(R, G, B: Integer; out Hsl: RColorHsl);
    class function Contrast(c: OsColor; Factor: Integer {$IFDEF USE_DEFPARAMS} = 30 {$ENDIF}): OsColor; overload;
    class function Contrast(c: OsColor; FactorToBright, FactorToDark: Integer): OsColor; overload;
    class procedure ColorToRgb(Color: OsColor; out R, G, B: Integer );
    class procedure ColorToHsl(Color: OsColor; out ColorHsl: RColorHsl);
    class procedure MorphHsl(const Source, Dest: RColorHsl; Step: TMorphStep; out Color: RColorHsl);
    class procedure MorphHslAbs(const Source, Dest: RColorHsl; Factor: integer; out Color: RColorHsl);
    class function HslDistance(const Source, Dest: RColorHsl): integer;
    class procedure HslToColor(const Hsl: RColorHsl; out Color: OsColor);
    class procedure RgbToColor(R, G, B: integer; out Color: OsColor);
    class function ToColor(const Hsl: RColorHsl): OsColor; overload;
    class function ToColor(R, G, B: integer): OsColor; overload;
    class function ToHsl(const Color: OsColor): RColorHsl; overload; 
    class function ToHsl(R, G, B: Integer): RColorHsl; overload; 
  end;

implementation

uses
  SilBtInt,
  SilOfColor;

function BaseColor(Color: OsColor): Longint;
begin
  if Color < 0 then
    Result := SilOfColor.GetSysColor(Color and $000000FF) else
    Result := Color;
end;

class procedure Rgb.ColorToRgb(Color: OsColor; out R, G, B: Integer );
var
  c: OsColor;
begin
  c := BaseColor( Color );
  R := c and $FF;
  G := ( c shr 8 ) and $FF;
  B := ( c shr 16 ) and $FF;
end;

class procedure Rgb.RgbToColor( R, G, B: integer; out Color: OsColor );
begin
  Color := Make( R, G, B );
end;

class procedure Rgb.ColorToHsl(Color: OsColor; out ColorHsl: RColorHsl);
var
  r, g, b: integer;
begin
  ColorToRgb( Color, r, g, b );
  RgbToHsl( r, g, b, ColorHsl );
end;

class procedure Rgb.HslToColor(const Hsl: RColorHsl; out Color: OsColor);
var
  r, g, b: integer;
begin
  HslToRgb( Hsl, r, g, b );
  RgbToColor( r, g, b, Color );
end;

class procedure Rgb.RgbToHsl(R, G, B: Integer; out Hsl: RColorHsl);
var
  max, min, divisor: Integer;
  redc, greenc, bluec: Single;
begin
  if ( R > G ) then max := R else max := G;
  if ( B > max ) then max := B;
  if ( R < G ) then min := R else min := G;
  if ( B < min ) then min := B;

  Hsl.Lum := Round( 240 * ( min + max ) / 2 / 255 );
  if ( max > 0 ) then
  begin
    if ( max + min > 255 ) then
      divisor := ( max + min - 2 * ( max + min - 255 ) ) else
      divisor := ( max + min );

    if ( divisor > 0 ) then
      Hsl.Sat := Round( 240 * ( max - min ) / divisor ) else
      Hsl.Sat := 0;
  end
  else
    Hsl.Sat := 0;

  if ( Hsl.Sat > 0 ) then
  begin
    redc := ( max - R ) / ( max - min );
    greenc := ( max - G ) / ( max - min );
    bluec := ( max - B ) / ( max - min );

    if ( R = max ) then
    begin
      if ( bluec < greenc ) then
        Hsl.Hue := Round( 240 * ( 1 + ( bluec - greenc ) / 6 ) ) else
        Hsl.Hue := Round( 240 * ( bluec - greenc ) / 6 );
    end
    else if ( G = max ) then
      Hsl.Hue := Round( 240 * ( 2 + redc - bluec ) / 6 )
    else
      Hsl.Hue := Round( 240 * ( 4 + greenc - redc ) / 6 );
  end
  else
    Hsl.Hue := 160;
end;

class procedure Rgb.HslToRgb(const Hsl: RColorHsl; out R, G, B: Integer);
var
  dif, divisor, min_p_max, max_m_min: integer;
  min, max: Word;
begin
  if ( Hsl.Lum >= 240 ) then
    min_p_max := 510 else
    min_p_max := Round( Hsl.Lum * 2 * 255 / 240 );
    
  if ( Hsl.Lum > 120 ) then
    divisor := ( min_p_max - 2 * ( min_p_max - 255 ) ) else
    divisor := min_p_max;

  if ( Hsl.Sat >= 240 ) then
    max_m_min := divisor else
    max_m_min := Round( Hsl.Sat * divisor / 240 );

  min := ( min_p_max - max_m_min + 1 ) div 2;
  max := ( min_p_max + max_m_min + 1 ) div 2;

  if ( Hsl.Hue < 40 ) or ( Hsl.Hue > 200 ) then
  begin
    { dif = G - B }
    if ( Hsl.Hue < 40 ) then
      dif := Round( max_m_min * ( Hsl.Hue * 6 / 240 ) ) else
      dif := Round( max_m_min * ( ( Hsl.Hue - 240 ) * 6 / 240 ) );

    R := max;
    if ( dif < 0 ) then
      G := min else
      G := dif + min;
    B := G - dif;
  end
  else if ( Hsl.Hue < 120 ) then
  begin
    { dif = B - R }
    dif := Round( max_m_min * ( Hsl.Hue * 6 / 240 - 2 ) );
    
    if ( dif < 0 ) then
      R := min - dif else
      R := min;
    G := max;
    B := dif + R;
  end
  else
  begin
    { dif = R - G }
    dif := Round( max_m_min * ( Hsl.Hue * 6 / 240 - 4 ) );

    if ( dif < 0 ) then
      R := min else
      R := min + dif;
    G := R - dif;
    B := max;
  end;
end;

class function Rgb.GetBlue(c: OsColor): Integer;
begin
  if c < 0 then c := BaseColor(c);
  Result := (c shr 16) and $ff;
end;

class function Rgb.GetGreen(c: OsColor): Integer;
begin
  if c < 0 then c := BaseColor(c);
  Result := (c shr 8) and $ff;
end;

class function Rgb.GetRed(c: OsColor): Integer;
begin
  if c < 0 then c := BaseColor(c);
  Result := c and $ff;
end;

class function Rgb.GetAlfa(c: OsColor): Integer;
begin
  if c < 0 then c := BaseColor(c);
  Result := (c shr 24) and $ff;
end;

class function Rgb.MakeBrighter(var Color: OsColor; Factor: Integer): Boolean;
var
  c: OsColor;
  src, dst: RColorHsl;
  dif: integer;
begin
  c := BaseColor( Color );
  ColorToHsl( Color, src );
  dst.Hue := src.Hue;
  if ( src.Sat > 120 ) then
  begin
    dif := Int.Min( 240 - src.Sat, Factor );
    dst.Sat := src.Sat + dif;
    dst.Lum := src.Lum + Factor - dif;
  end
  else
  begin
    dif := Int.Min( 240 - src.Lum, Factor );
    dst.Sat := src.Sat + Factor - dif;
    dst.Lum := src.Lum + dif;
  end;
  HslToColor( dst, Color );
  result := ( Color <> c );
end;

class function Rgb.MakeDarker(var Color: OsColor; Factor: Integer): Boolean;
var
  c: OsColor;
  src, dst: RColorHsl;
  dif, dif2: integer;
begin
  c := BaseColor( Color );
  ColorToHsl( Color, src );
  dst.Hue := src.Hue;
  dif := Int.Max( 0, Int.Min( src.Lum - 120, Factor ) ); 
  dst.Lum := src.Lum - dif;
  dif2 := Int.Min( src.Sat, Factor - dif );
  dst.Sat := src.Sat - dif2;
  dst.Lum := dst.Lum - Int.Min( dst.Lum, Factor - dif - dif2 );
  HslToColor( dst, Color );
  result := ( Color <> c );
end;

class function Rgb.MorphRel(Origin, Dest: OsColor; Step: TMorphStep): OsColor;
var
  dr, dg, db: Integer;
begin
  dr := GetRed( Dest ) - GetRed( Origin );
  dg := GetGreen( Dest ) - GetGreen( Origin );
  db := GetBlue( Dest ) - GetBlue( Origin );

  Result := Make(
    GetRed( Origin ) + ( dr * Step ) div 255,
    GetGreen( Origin ) + ( dg * Step ) div 255,
    GetBlue( Origin ) + ( db * Step ) div 255,
    defMakeA );
end;

class function Rgb.Morph(Origin, Dest: OsColor; Factor: Integer): OsColor;
var
  ro, go, bo: integer;
  rd, gd, bd: integer;
  dr, dg, db, sdr, sdg, sdb, cfact: Integer;
begin
  if Origin < 0 then Origin := BaseColor(Origin);
  if Dest < 0 then Dest := BaseColor(Dest);

  ColorToRgb( Origin, ro, go, bo );
  ColorToRgb( Dest, rd, gd, bd );

  dr := Int.AbsSign( rd - ro, sdr );
  dg := Int.AbsSign( gd - go, sdg );
  db := Int.AbsSign( bd - bo, sdb );
  
  if ( dr > 0 ) or ( dg > 0 ) or ( db > 0 ) then
  begin
    cfact := Factor;
    if ( Factor > dr ) then
      Inc( cfact, Factor - dr );
    if ( Factor > dg ) then
      Inc( cfact, Factor - dg );
    if ( Factor > db ) then
      Inc( cfact, Factor - db );

    Result := Make(
      ro + Int.Min( cfact, dr ) * sdr,
      go + Int.Min( cfact, dg ) * sdg,
      bo + Int.Min( cfact, db ) * sdb );
  end
  else
    Result := Dest;
end;

class function Rgb.Make(r, g, b: Integer; a: Integer): OsColor;
begin
  Result :=
//    (a shl 24) or
    (b shl 16) or
    (g shl 8) or
    (r shl 0);
end;

class function Rgb.MakeChange(var c: OsColor; r, g, b: Integer): Boolean;
var
  n: Integer;
begin
  if c < 0 then
    c := BaseColor(c);

  n := Make(
    Int.Range(GetRed(c) + r, 0, 255),
    Int.Range(GetGreen(c) + g, 0, 255),
    Int.Range(GetBlue(c) + b, 0, 255), defMakeA);

  Result := ( n <> c );
  if Result then c := n;
end;

class function Rgb.Brighter(Color: OsColor; Factor: Integer): OsColor;
begin
  Result := Color;
  MakeBrighter(Result, Factor);
end;

class function Rgb.Darker(Color: OsColor; Factor: Integer): OsColor;
begin
  Result := Color;
  MakeDarker(Result, Factor);
end;

class function Rgb.MorphHslRel(Origin, Dest: OsColor; Step: TMorphStep): OsColor;
var
  src, dst, color: RColorHsl;
begin
  ColorToHsl( Origin, src );
  ColorToHsl( Dest, dst );

  MorphHsl( src, dst, Step, color );

  HslToColor( color, result );
end;

class procedure Rgb.MorphHslAbs(const Source, Dest: RColorHsl; Factor: integer; out Color: RColorHsl);
var
  dist: integer;
  step: TMorphStep;
begin
  dist := HslDistance( Source, Dest );
  if ( Factor < dist ) and ( dist > 0 ) then
    step := Round( 255 * Factor / dist ) else
    step := 255;
  MorphHsl( Source, Dest, step, Color );
end;

class procedure Rgb.MorphHsl(const Source, Dest: RColorHsl; Step: TMorphStep; out Color: RColorHsl);
begin
  if ( Step <= 0 ) then
  begin
    Color.Hue := Source.Hue;
    Color.Sat := Source.Sat;
    Color.Lum := Source.Lum;
  end
  else if ( Step >= 255 ) then
  begin
    Color.Hue := Dest.Hue;
    Color.Sat := Dest.Sat;
    Color.Lum := Dest.Lum;
  end
  else
  begin
    if ( Source.Lum = 0 ) then
      Color.Hue := Dest.Hue
    else if ( Dest.Lum = 240 ) then
      Color.Hue := Source.Hue
    else
      Color.Hue := Round( ( Source.Hue * ( 255 - Step ) + Dest.Hue * Step ) / 255 );
    if ( Source.Lum = 0 ) then
      Color.Sat := Dest.Sat
    else if ( Dest.Lum = 240 ) then
      Color.Sat := Source.Sat
    else
      Color.Sat := Round( ( Source.Sat * ( 255 - Step ) + Dest.Sat * Step ) / 255 );

    Color.Lum := Round( ( Source.Lum * ( 255 - Step ) + Dest.Lum * Step ) / 255 );
  end;
end;

class function Rgb.HslDistance(const Source, Dest: RColorHsl): integer;
  function Max(V1, V2: integer): integer;
  begin
    if ( V1 > V2 ) then
      result := V1 else
      result := V2;
  end;
var
  hd, sd, bd: integer;
begin
  hd := Abs( Dest.Hue - Source.Hue );
  sd := Abs( Dest.Sat - Source.Sat );
  bd := Abs( Dest.Lum - Source.Lum );
  result := Max( hd, Max( sd, bd ) );
end;

class function Rgb.Brighter2(c: OsColor; Factor: Integer): OsColor;
begin
  if c < 0 then c := BaseColor(c);
  Result := Make(
    Int.Range(Trunc( GetRed(c) * ( 1 + Factor / 100 ) ), 0, 255),
    Int.Range(Trunc( GetGreen(c) * ( 1 + Factor / 100 ) ), 0, 255),
    Int.Range(Trunc( GetBlue(c) * ( 1 + Factor / 100 ) ), 0, 255), defMakeA);
end;

class function Rgb.Change(c: OsColor; r, g, b: Integer): OsColor;
begin
  Result := c;
  MakeChange(Result, r, g, b);
end;

class function Rgb.Contrast(c: OsColor; Factor: Integer): OsColor;
begin
  result := Rgb.Contrast( c, Factor, Factor );
end;

class function Rgb.Contrast(c: OsColor; FactorToBright, FactorToDark: Integer): OsColor;
begin
  if ( Rgb.GetRed(c) or Rgb.GetGreen(c) or Rgb.GetBlue(c) > 128 ) then
    result := Rgb.MorphRel(c, clBlack, FactorToDark) else
    result := Rgb.MorphRel(c, clWhite, FactorToBright);
end;

class function Rgb.ToColor(const Hsl: RColorHsl): OsColor;
begin
  HslToColor(Hsl, Result);
end;

class function Rgb.ToColor(R, G, B: integer): OsColor;
begin
  RgbToColor(R, G, B, Result);
end;

class function Rgb.ToHsl(const Color: OsColor): RColorHsl;
begin
  ColorToHsl(Color, Result);
end;

class function Rgb.ToHsl(R, G, B: Integer): RColorHsl;
begin
  RgbToHsl(R, G, B, Result);
end;

end.



