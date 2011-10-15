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

unit Digital;

interface

uses
  Sil,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Dialogs;

type
  TDigitalPad = ( dpLeft, dpRight, dpNumber );

  TDigital = class(TCustomControl)
  private
    FBackColor: TColor;
    FForeOnColor: TColor;
    FForeOffColor: TColor;
    FBorderWidth: TBorderWidth;
    FSegmentWidth: Integer;
    FBaseSize: Integer;
    FAutoSegmentWidth: Boolean;
    FAutoDigits: Boolean;
    FBuffer: TBitmap;

    FAlignment: TAlignment;    // Posición de los dígitos
    FDigitCount: Integer;
    FDisplay: array of SmallInt;
    FValue: String;
    FPresicion: Integer;

    procedure SetBackColor(const Value: TColor);
    procedure SetForeOnColor(const Value: TColor);
    procedure SetForeOffColor(const Value: TColor);
    procedure CalcForeOffColor;
    procedure SetSegmentWidth(const Value: Integer);
    procedure SetDigitCount(const Value: Integer);
    procedure SetPresicion(const Value: Integer);
    procedure SetBorderWidth(const Value: TBorderWidth);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAutoDigits(const Value: Boolean);

    procedure SetValue( AValue: String);
    procedure SetPattern( const AValue: String; Pad: TDigitalPad = dpLeft );
    procedure GetDimentions( out PX, PY, PSegSize, PDigWidth,
      PBaseline: Integer );

    procedure DrawHSegment(const x, y, baseline: integer);
    procedure DrawVSegment(const x, y, baseline: integer);
    procedure DrawPoint(const x, y, baseline: integer);
    procedure SegmentAngle( var Points: array of TPoint; baseline: Integer );

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure UpdateMetrics;
    function GetBorderWidth: Integer;
    function GetNumValue: Double;
    procedure SetNumValue(const NValue: Double );

  protected
    procedure Paint; override;

    procedure UpdateDig;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

//		property Display: String read FDisplay write SetDisplay;

  published
    property BackColor: TColor read FBackColor write SetBackColor;
    property ForeOnColor: TColor read FForeOnColor write SetForeOnColor;
    property ForeOffColor: TColor read FForeOffColor write SetForeOffColor;
    property SegmentWidth: Integer read FSegmentWidth write SetSegmentWidth;
    property AutoSegmentWidth: Boolean read FAutoSegmentWidth write FAutoSegmentWidth;
    property AutoDigits: Boolean read FAutoDigits write SetAutoDigits;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property DigitCount: Integer read FDigitCount write SetDigitCount;
    property Value: String read FValue write SetValue;
    property Number: Double read GetNumValue write SetNumValue;
    property Presicion: Integer read FPresicion write SetPresicion;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;

    // Propiedades heredadas
    property Align;
    property Anchors;
    property Cursor;
    property Hint;
    property ShowHint;
    property PopupMenu;
    property Visible;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property OnClick;
    property OnDblClick;
  end;

procedure Register;

implementation

var
  Patterns: array[ Char ] of SmallInt;


procedure Register;
begin
  RegisterComponents('Mariano', [TDigital]);
end;

constructor TDigital.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackColor := clBlack;
  FForeOnColor := clAqua;
  FForeOffColor := clTeal;
  FSegmentWidth := 0;
  FAutoSegmentWidth := true;
  FAutoDigits := true;
  FValue := '';
  FPresicion := 0;

  Height := 42;
  Width := 90;

  FDigitCount := 3;
  FBuffer := TBitmap.Create;

  UpdateMetrics;
end;

destructor TDigital.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TDigital.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := LRESULT(false);
end;

procedure TDigital.CMBorderChanged(var Message: TMessage);
begin
  inherited;

  Invalidate;
end;

procedure TDigital.WMSize(var Message: TWMSize);
begin
  UpdateMetrics;
  inherited;
end;

procedure TDigital.SetBackColor(const Value: TColor);
begin
  FBackColor := Value;
  CalcForeOffColor;
  Invalidate;
end;

procedure TDigital.SetForeOnColor(const Value: TColor);
begin
  FForeOnColor := Value;
  CalcForeOffColor;
  Invalidate;
end;

procedure TDigital.CalcForeOffColor;
begin
  if ( Sil.Rgb.GetRed( FBackColor ) or Sil.Rgb.GetGreen( FBackColor ) or
    Sil.Rgb.GetBlue( FBackColor ) > 128 ) then
    FForeOffColor := Sil.Rgb.MorphRel( FForeOnColor, FBackColor, 90 )
//    FForeOffColor := Sil.Rgb.Brighter( FForeOnColor, $7F + $40 )
  else
    FForeOffColor := Sil.Rgb.MorphRel( FForeOnColor, FBackColor, 50 );
//    FForeOffColor := Sil.Rgb.Darker( FForeOnColor, $7F );
end;

procedure TDigital.SetForeOffColor(const Value: TColor);
begin
  FForeOffColor := Value;
  Invalidate;
end;

procedure TDigital.SetAutoDigits(const Value: Boolean);
begin
  FAutoDigits := Value;
  UpdateMetrics;
end;

procedure TDigital.SetDigitCount(const Value: Integer);
begin
  FDigitCount := Value;
  FAutoDigits := false;
  UpdateDig;
end;

procedure TDigital.SetBorderWidth(const Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Realign;
  Invalidate;
end;

procedure TDigital.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TDigital.SetPresicion(const Value: Integer);
begin
  FPresicion := Value;
  UpdateDig;
end;

procedure TDigital.SetSegmentWidth(const Value: Integer);
begin
  FSegmentWidth := Value;
  Invalidate;
end;

procedure TDigital.SetValue( AValue: String );
begin
  FValue := AValue;
  UpdateDig;
end;

function TDigital.GetNumValue: Double;
begin
  result := Sil.Str.ToFloat( FValue, 0 );
end;

procedure TDigital.SetNumValue( const NValue: Double );
begin
  Value := FloatToStr( NValue );
end;

procedure TDigital.UpdateMetrics;
var
  wd, hg, dig_width, bor: Integer;
begin
  bor := GetBorderWidth;
  wd := Width - 2 * bor;
  hg := Height - 2 * bor;

  FBaseSize := ( hg - 1 ) div 2 - 2;
  if FAutoSegmentWidth then FSegmentWidth := FBaseSize div 8;
  dig_width := FBaseSize + 5 + FBaseSize div 5 + FSegmentWidth * 2;

  // Ajusta la cantidad de dígitos al ancho total
  //
  if FAutoDigits then
  begin
    if FAutoDigits and ( dig_width > 0 ) then
      FDigitCount := wd div dig_width;
  end
  // Ajusta el ancho de los dígitos al ancho total
  //
  else if ( dig_width * FDigitCount > wd ) then
  begin
    if FAutoSegmentWidth then
    begin
      FBaseSize := Round( ( wd / FDigitCount - 5 ) / 1.45 );
      FSegmentWidth := FBaseSize div 8;
    end
    else
      FBaseSize := Round( ( wd / FDigitCount - 5 - FSegmentWidth * 2 ) / 1.2 );
  end;

  UpdateDig;
end;

procedure TDigital.GetDimentions( out PX, PY, PSegSize, PDigWidth,
  PBaseline: Integer );
var
  bor: integer;
begin
  bor := GetBorderWidth;

  PDigWidth := FBaseSize + 5 + FBaseSize div 5 + FSegmentWidth * 2;
  PSegSize := FBaseSize - 2 * FSegmentWidth;

  // Asigna la posición horizontal
  //
  case FAlignment of
  taRightJustify: PX := Width - bor - PDigWidth * FDigitCount;
  taCenter: PX := ( Width - PDigWidth * FDigitCount ) div 2;
  else PX := 1 + FSegmentWidth + bor;
  end;
  PY := ( Height - ( 2 * FBaseSize - 2 * FSegmentWidth ) ) div 2;
  PBaseline := PY + 2 * FBaseSize - 2 * FSegmentWidth;
end;

function TDigital.GetBorderWidth: Integer;
begin
  result := FBorderWidth;
  if ( BevelOuter <> bvNone ) then
    Inc( result, BevelWidth );
  if ( BevelInner <> bvNone ) then
    Inc( result, BevelWidth );
end;

procedure TDigital.UpdateDig;
var
  code: integer;
  num: Double;
  str: string;
begin
  // Determina si es un número
  //
  Val(Sil.Str.Replace(FValue, DecimalSeparator, '.'), num, code);

  // Asigna como cadena o número
  //
  if ( code = 0 ) then
  begin
    str := Format( '%.*f', [ FPresicion, num ] );
    Sil.Str.Replace( str, '.', DecimalSeparator ); 
    SetPattern( str, dpNumber );
  end
  else
    SetPattern( FValue );
end;

procedure TDigital.SetPattern( const AValue: String; Pad: TDigitalPad );
var
  i, j, patt: Integer;
begin
  // Limpia los segmentos
  //
  SetLength( FDisplay, FDigitCount );
  FillChar( FDisplay[ 0 ], Length( FDisplay ) * sizeof( FDisplay[ 0 ] ), 0 );

  // Asigna los segmentos
  //
  i := 1;
  j := 0;
  while ( i <= Length( AValue ) ) do
  begin
    // Regla especial para los puntos decimales
    //
    if ( AValue[ i ] = DecimalSeparator ) and ( j > 0 ) then
    begin
      if ( FDisplay[ j - 1 ] and Patterns[ DecimalSeparator ] ) = 0 then
        Dec( j );
      patt := FDisplay[ j ] or Patterns[ DecimalSeparator ];
    end
    else
      patt := Patterns[ AValue[ i ] ];

    // Verifica si el texto sobrepasa los dígitos
    //
    if ( j >= FDigitCount ) then
    begin
      // Si es un número verifica que entre completo
      //
      if ( Pad = dpNumber ) then
        FillChar( FDisplay[ 0 ], FDigitCount * sizeof( FDisplay[ 0 ] ),
          Patterns[ DecimalSeparator ] );
      break;
    end
    else
      FDisplay[ j ] := patt;

    Inc( j );
    Inc( i );
  end;

  // Evalúa el Pad
  //
  if ( ( Pad = dpRight ) or ( Pad = dpNumber ) ) and ( j < FDigitCount ) then
  begin
    Move( FDisplay[ 0 ], FDisplay[ FDigitCount - j ], j * sizeof( FDisplay[ 0 ] ) );
    FillChar( FDisplay[ 0 ], ( FDigitCount - j ) * sizeof( FDisplay[ 0 ] ), 0 );
  end;

  Invalidate;
end;

procedure TDigital.Paint;
var
  i, j, mult, dig_width, seg_size,
  x, y, baseline: Integer;
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if (Parent = nil) or (Width < 1) or (Height < 1) then Exit;

  if (Width <> FBuffer.Width) or (Height <> FBuffer.Height) then
  begin
    FBuffer.Width := Width;
    FBuffer.Height := Height;
  end;

  // Borra todo el contenido
  //
  with FBuffer.Canvas do
  begin
    Brush.Color := FBackColor;
    FillRect(ClipRect);
  end;

  // Dibuja el contorno
  //
  Rect := Classes.Rect( 0, 0, FBuffer.Width, FBuffer.Height );//GetClientRect;
  if ( BevelOuter <> bvNone ) then
  begin
    AdjustColors(BevelOuter);
    Frame3D(FBuffer.Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  if ( BevelInner <> bvNone ) then
  begin
    AdjustColors(BevelInner);
    Frame3D(FBuffer.Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  // Obtiene las dimensiones de cada elemento
  //
  GetDimentions( x, y, seg_size, dig_width, baseline );

  // Dibuja los segmentos de cada dígito
  //
  for i := 0 to FDigitCount - 1 do
  begin
    if ( FDisplay[i] = 256 ) then
    begin
      FBuffer.Canvas.Pen.Color := FForeOnColor;

      DrawPoint(x + FBaseSize div 16, y + FBaseSize div 2 - FBaseSize div 8, baseline );
      DrawPoint(x + FBaseSize div 16, y + FBaseSize div 2 - FBaseSize div 8 + seg_size, baseline );
      Inc( x, Trunc( FBaseSize / 1.4 ) + 1 );
    end else
    begin
      mult := 1;

      for j := 0 to 7 do
      begin
        if ( FDisplay[i] and mult > 0 ) then
          FBuffer.Canvas.Pen.Color := FForeOnColor else
          FBuffer.Canvas.Pen.Color := FForeOffColor;

        case j of
          0: DrawHSegment( x, y, baseline );
          1: DrawVSegment( x + seg_size + 1, y, baseline );
          2: DrawVSegment( x + seg_size + 1, y + seg_size + 1, baseline );
          3: DrawHSegment( x, y + 2 * seg_size + 2, baseline );
          4: DrawVSegment( x, y + seg_size + 1, baseline );
          5: DrawVSegment( x, y, baseline );
          6: DrawHSegment( x, y + seg_size + 1, baseline );
          7: DrawPoint( x + 3 + FBaseSize, y + 3 + FBaseSize * 2 - FBaseSize div 4 - 2 * FSegmentWidth, baseline );
        end;

        mult := mult * 2;
      end;

      Inc( x, dig_width );
    end;
  end;

  Canvas.CopyRect(FBuffer.Canvas.ClipRect, FBuffer.Canvas, FBuffer.Canvas.ClipRect);
end;

procedure TDigital.DrawHSegment(const x, y, baseline: integer);
var
  points: array[ 0..6 ] of TPoint;
begin
  points[ 0 ].x := x + 1 + FSegmentWidth;
  points[ 1 ].x := x + 1 + 2 * FSegmentWidth;
  points[ 2 ].x := x + FBaseSize - 2 * FSegmentWidth;
  points[ 3 ].x := x + FBaseSize - FSegmentWidth;
  points[ 4 ].x := points[ 2 ].x;
  points[ 5 ].x := points[ 1 ].x;
  points[ 6 ].x := points[ 0 ].x;

  points[ 0 ].y := y + FSegmentWidth;
  points[ 1 ].y := y;
  points[ 2 ].y := points[ 1 ].y;
  points[ 3 ].y := points[ 0 ].y;
  points[ 4 ].y := y + 2 * FSegmentWidth;
  points[ 5 ].y := points[ 4 ].y;
  points[ 6 ].y := points[ 0 ].y;

  SegmentAngle( points, baseline );

  FBuffer.Canvas.Brush.Color := FBuffer.Canvas.Pen.Color;
  FBuffer.Canvas.Polygon( points );
end;

procedure TDigital.DrawVSegment(const x, y, baseline: integer);
var
  points: array[ 0..6 ] of TPoint;
begin
  points[ 0 ].x := x + FSegmentWidth;
  points[ 1 ].x := x;
  points[ 2 ].x := x;
  points[ 3 ].x := points[ 0 ].x;
  points[ 4 ].x := x + 2 * FSegmentWidth;
  points[ 5 ].x := points[ 4 ].x;
  points[ 6 ].x := points[ 0 ].x;

  points[ 0 ].y := y + FBaseSize - FSegmentWidth;
  points[ 1 ].y := points[ 0 ].y - FSegmentWidth;
  points[ 2 ].y := y + 1 + 2 * FSegmentWidth;
  points[ 3 ].y := y + 1 + FSegmentWidth;
  points[ 4 ].y := points[ 2 ].y;
  points[ 5 ].y := points[ 1 ].y;
  points[ 6 ].y := points[ 0 ].y;

  SegmentAngle( points, baseline );

  FBuffer.Canvas.Brush.Color := FBuffer.Canvas.Pen.Color;
  FBuffer.Canvas.Polygon( points );
end;

procedure TDigital.DrawPoint(const x, y, baseline: integer);
var
  points: array[ 0..4 ] of TPoint;
begin
  points[ 0 ].x := x;
  points[ 1 ].x := x + FBaseSize div 4 + 1;
  points[ 2 ].x := points[ 1 ].x;
  points[ 3 ].x := points[ 0 ].x;
  points[ 4 ].x := points[ 0 ].x;

  points[ 0 ].y := y;
  points[ 1 ].y := points[ 0 ].y;
  points[ 2 ].y := y + FBaseSize div 4 + 1;
  points[ 3 ].y := points[ 2 ].y;
  points[ 4 ].y := points[ 0 ].y;

  SegmentAngle( points, baseline );

  FBuffer.Canvas.Brush.Color := FBuffer.Canvas.Pen.Color;
  FBuffer.Canvas.Polygon( points );
end;

procedure TDigital.SegmentAngle( var Points: array of TPoint; baseline: Integer );
var
  i1: Integer;
begin
  for i1 := 0 to Length( Points ) - 1 do
  begin
    Points[ i1 ].x := Points[ i1 ].x + ( baseline - Points[ i1 ].y ) div 10;
  end;
end;


initialization
begin
  // Crea los patrones
  //
  Patterns[ '0' ] := $3F;
  Patterns[ '1' ] := $06;
  Patterns[ '2' ] := $5B;
  Patterns[ '3' ] := $4F;
  Patterns[ '4' ] := $66;
  Patterns[ '5' ] := $6D;
  Patterns[ '6' ] := $7D;
  Patterns[ '7' ] := $07;
  Patterns[ '8' ] := $7F;
  Patterns[ '9' ] := $6F;
  Patterns[ '-' ] := $40;
  Patterns[ DecimalSeparator ] := $80;
  Patterns[ ':' ] := $100;
  Patterns[ '_' ] := $08;
  Patterns[ '~' ] := $01;
  Patterns[ '=' ] := $48;
  Patterns[ '''' ] := $20;
end;

end.


