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

unit sClock;

interface

uses
  Messages, Windows, SysUtils, Classes, Controls, Forms, Graphics,
  StdCtrls, Consts, Math, ExtCtrls, Dialogs, Sil;

type
  scClock = class;
  scClockLabel = class;
  scClockDate = class;
  scClockHands = class;

  scCustomGaugeScale = class;
  scGaugeScale = class;
  scGaugeScalePoints = class;
  scGaugeHand = class;
  scShape = class;

  stTPoints = array of TPoint;
  stPPoints = ^stTPoints;
  stTColors = array of TColor;

  scPen = class ( TPen )
    property Color nodefault;
  end;

  scBrush = class ( TBrush )
    property Color nodefault;
  end;

  //
  // Escala
  //
  scCustomGaugeScale = class ( TCustomControl )

  private
    fHandsCircular: boolean;

    // Elementos a dibujar
    fShape: scShape;
    fCenterShape: scShape;
    fPoints: scGaugeScalePoints;
    fHands: TOwnedCollection;

    fDrawBuff: TBitmap;

    fOnChange: TNotifyEvent;

    fColorChanged: boolean;

  protected
    procedure Changed( Sender: TObject ); virtual;

    procedure SetHandsCircular( Value: boolean );
    procedure SetHands( Value: TOwnedCollection );
    procedure SetShape( Value: scShape );
    procedure SetCenterShape( Value: scShape );
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

    function GetTranspColor( ExcludeColors: array of TColor ): TColor;
    procedure Paint; override;
    procedure PaintTo( DstCanvas: TCanvas; Rect: TRect );

    procedure CreateParams( var Params: TCreateParams ); override;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property Shape: scShape read fShape write SetShape;
    property CenterShape: scShape read fCenterShape write SetCenterShape;
    property Hands: TOwnedCollection read fHands write SetHands;
    property Points: scGaugeScalePoints read fPoints write fPoints;

    property HandsCircular: boolean read fHandsCircular write SetHandsCircular;

  published
    property Align;
    property Cursor;
    property Font;
    property Hint;
    property PopupMenu;
    property Visible;

    property OnClick;
    property OnDblClick;
  end;

  // Escala con atributos publicados
  //
  scGaugeScale = class ( scCustomGaugeScale )

  published
    property Shape;
    property CenterShape;
    property Points;
    property HandsCircular;
    property Hands;
  end;

  //
  // Reloj
  //
  scClock = class ( scCustomGaugeScale )

  private
    FInterval: Cardinal;
    FWindowHandle: IHandle;
    FOnTimer: TNotifyEvent;
    FEnabled: boolean;
    FDate: scClockDate;
    fLastDate: string;

    fClkHands: scClockHands;

  protected
    procedure Changed( Sender: TObject ); override;

    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetClkHands(const Value: scClockHands);
    procedure SetDate(const Value: scClockDate );

    procedure WndProcMsg(var Msg);
    procedure UpdateTimer;
    procedure Timer; dynamic;
    procedure SetEnabled(Value: boolean); override;
    procedure Paint; override;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Loaded; override;

  published
    property Shape;
    property CenterShape;
    property Points;

    property Hands: scClockHands read fClkHands write SetClkHands;
    property Date: scClockDate read FDate write SetDate;
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  // Texto con atributos
  //
  scClockLabel = class ( TPersistent )

  private
    fVisible: boolean;
    fFont: TFont;
    fOnChange: TNotifyEvent;

  protected
    procedure Changed( Sender: TObject );

    procedure SetVisible( const Value: boolean );
    procedure SetFont( const Value: TFont );
    procedure SetOnChange( const Value: TNotifyEvent );

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property OnChange: TNotifyEvent read fOnChange write fOnChange;

  published
    property Visible: boolean read fVisible write SetVisible;
    property Font: TFont read fFont write SetFont;
  end;

  // Texto de la fecha
  //
  scClockDate = class ( scClockLabel )

  private
    fFormat: string;

    procedure SetFormat(const Value: string);

  published
    property Format: string read fFormat write SetFormat;
  end;

  // Agujas
  //
  scClockHands = class( TPersistent )

  private
    fScale: scCustomGaugeScale;

  protected
    function GetCircular: boolean;
    function GetHour: scGaugeHand;
    function GetMinutes: scGaugeHand;
    function GetSeconds: scGaugeHand;
    procedure SetCircular(const Value: boolean);
    procedure SetHour(const Value: scGaugeHand);
    procedure SetMinutes(const Value: scGaugeHand);
    procedure SetSeconds(const Value: scGaugeHand);

  public
    constructor Create( Scale: scCustomGaugeScale );

  published
    property Circular: boolean read GetCircular write SetCircular;
    property Hour: scGaugeHand read GetHour write SetHour;
    property Minutes: scGaugeHand read GetMinutes write SetMinutes;
    property Seconds: scGaugeHand read GetSeconds write SetSeconds;
  end;

  //
  // Aguja
  //
  scGaugeHand = class ( TCollectionItem )

  private
    // Parámetros de aspecto
    //
    fTail: integer;
    fWidth: integer;
    fArrowAspect: integer;
    fLength: integer;
    fBrush: scBrush;
    fPen: scPen;
    fVisible: boolean;

    // Parámetros de posición
    //
    fAngle: Single;

    fOnChange: TNotifyEvent;

  protected
    procedure Changed( Sender: TObject );

    function GetDegAng(): Single;
    procedure SetDegAng( Angle: Single );

    procedure SetTail( Value: integer );
    procedure SetWidth( Value: integer );
    procedure SetArrowAspect( Value: integer );
    procedure SetLength( Value: integer );
    procedure SetBrush( Value: scBrush );
    procedure SetPen( Value: scPen );
    procedure SetVisible( Value: boolean );

    function GetDisplayName: string; override;

  public
    constructor Create( Collection: TCollection ); override;
    destructor Destroy; override;

    procedure PaintTo( DstCanvas: TCanvas; Position: TPoint;
      Height, Width: integer; Radial: boolean; ForceCircular: boolean );

    property OnChange: TNotifyEvent read fOnChange write fOnChange;

  published
    property Visible: boolean read fVisible write SetVisible;
    property Angle: Single read GetDegAng write SetDegAng;

    property Tail: integer read fTail write SetTail;
    property Width: integer read fWidth write SetWidth;
    property ArrowAspect: integer read fArrowAspect write SetArrowAspect;
    property Length: integer read fLength write SetLength;
    property Brush: scBrush read fBrush write SetBrush stored true;
    property Pen: scPen read fPen write SetPen stored true;
  end;

  // Puntos de la hora
  //
  scGaugeScalePoints = class( TPersistent )

  private
    // Puntos de la escala
    //
    fVisible: boolean;
    fPosition: integer;
    fSize: integer;
    fTexts: string;
    fColor: TColor;
    fMarksWidth: integer;
    fMinutes: boolean;

    fOnChange: TNotifyEvent;

  protected
    procedure Changed( Sender: TObject ); virtual;

    procedure SetVisible( const Value: boolean );
    procedure SetPosition( const Value: integer );
    procedure SetSize( const Value: integer );
    procedure SetTexts( const Value: string );
    procedure SetColor( const Value: TColor );
    procedure SetMinutes( const Value: boolean );
    procedure SetMarksWidth( const Value: integer );

  public
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

  published
    property Visible: boolean read fVisible write SetVisible;
    property Size: integer read fSize write SetSize;
    property Texts: string read fTexts write SetTexts;
    property MarksWidth: integer read fMarksWidth write SetMarksWidth;
    property Minutes: boolean read fMinutes write SetMinutes;
    property Position: integer read fPosition write SetPosition;
    property Color: TColor read fColor write SetColor;
  end;

  //
  // Forma
  //
  scShape = class ( TPersistent )

  private
    fVisible: boolean;

    fRelSize: integer;

    fPen: TPen;
    fBrush: TBrush;
    fShape: TShapeType;

    fOnChange: TNotifyEvent;

  protected
    procedure Changed( Sender: TObject );

    procedure SetVisible( Value: boolean );
    procedure SetRelSize( Value: integer );
    procedure SetBrush( Value: TBrush );
    procedure SeTPen( Value: TPen );
    procedure SetShape( Value: TShapeType );

  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw( ACanvas: TCanvas; const Rect: TRect );
    function IsSquare: boolean;

    property OnChange: TNotifyEvent read fOnChange write fOnChange;

  published
    property Visible: boolean read fVisible write SetVisible;
    property RelativeSize: integer read fRelSize write SetRelSize;
    property Brush: TBrush read fBrush write SetBrush;
    property Pen: TPen read fPen write SeTPen;
    property ShapeType: TShapeType read fShape write SetShape;
  end;

  // Clase para definición de funciones auxiliares
  //
  scGraphics = class
    class function PointGetStr( Point: integer; Str: string; var Line: boolean ): string;
    class function CalcX( Width: integer; Angle, SizeRel: Single ): Single;
    class function CalcY( Height: integer; Angle, SizeRel: Single ): Single;
    class function GetTranspColor( Exclude: array of TColor; TryColor: TColor = clFuchsia ): TColor;
    class function ColorConcat( Arr1: stTColors; Arr2: array of TColor ): stTColors;
  end;

  // Componentes definidos
  //
  TSGaugeScale = class( scGaugeScale );
  TSClock = class( scClock );

implementation

class function scGraphics.CalcX( Width: integer; Angle, SizeRel: Single ): Single;
begin
  result := Width * SizeRel / 100 * Cos( Angle );
end;

class function scGraphics.CalcY( Height: integer; Angle, SizeRel: Single ): Single;
begin
  result := Height * SizeRel / 100 * Sin( Angle );
end;

class function scGraphics.PointGetStr( Point: integer; Str: string; var Line: boolean ): string;
var
  i1, pt, off: integer;
  s1: string;
begin
  pt := 0;
  off := 0;
  s1 := '';

  Point := ( Point + 11 ) mod 12;
  for i1 := 1 to Length( Str ) do
  begin
    if ( Str[ i1 ] = ',' ) then
      Inc( pt )
    else if ( pt = Point ) and ( off = 0 ) then
      off := i1;

    if ( pt = Point + 1 ) or ( i1 = Length( Str ) ) then
    begin
      if ( off > 0 ) then
        if ( i1 = Length( Str ) ) then
          s1 := Copy( Str, off, i1 + 1 - off )
        else
          s1 := Copy( Str, off, i1 - off );
      break;
    end;
  end;

  line := ( Length( s1 ) > 0 ) and ( s1[ 1 ] = '&' );
  if line then
    result := PChar( s1 ) + 1
  else
    result := s1;
end;

class function scGraphics.GetTranspColor( Exclude: array of TColor; TryColor: TColor = clFuchsia ): TColor;
const
  cl_arr: array [ 0..17 ] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple,
    clTeal, clGray, clSilver, clRed, clLime, clYellow,
    clBlue, clFuchsia, clAqua, clLtGray, clDkGray, clWhite );
var
  i1, i2, off: integer;
  valid: boolean;
begin
  off := 0;
  for i1 := 0 to Length( cl_arr ) - 1 do
  begin
    if ( cl_arr[ i1 ] = TryColor ) then
    begin
      off := i1;
      break;
    end;
  end;

  for i1 := 0 to Length( cl_arr ) - 1 do
  begin
    if ( i1 = 0 ) then
      result := TryColor
    else
      result := cl_arr[ ( i1 + off ) mod Length( cl_arr ) ];

    valid := true;

    // Valida el color
    //
    for i2 := 0 to Length( Exclude ) - 1 do
    begin
      if ( result = Exclude[ i2 ] ) then
      begin
        valid := false;
        break;
      end;
    end;

    if valid then break;
  end;

  if not valid then
    result := TryColor;
end;

class function scGraphics.ColorConcat( Arr1: stTColors; Arr2: array of TColor ): stTColors;
var
  i1: integer;
  arr: stTColors;
begin
  arr := Arr1;
  SetLength( arr, Length( Arr1 ) + Length( Arr2 ) );
  for i1 := 0 to Length( Arr2 ) - 1 do
  begin
    arr[ i1 + Length( Arr1 ) ] := Arr2[ i1 ];
  end;

  result := arr;
end;



{ scGaugeHand }


constructor scGaugeHand.Create( Collection: TCollection );
begin
  inherited Create( Collection );

  fVisible := true;

  fTail := 10;
  fWidth := 5;
  fLength := 35;
  fArrowAspect := 100;

  fBrush := scBrush.Create();
  fBrush.OnChange := Changed;
  fPen := scPen.Create();
  fPen.OnChange := Changed;
end;

destructor scGaugeHand.Destroy;
begin
  fPen.Free();
  fBrush.Free();

  inherited Destroy();
end;

procedure scGaugeHand.Changed(Sender: TObject);
begin
  if Assigned( fOnChange ) then
    fOnChange( Self );
end;

function scGaugeHand.GetDegAng(): Single;
begin
  result := fAngle * 180 / PI;
end;

procedure scGaugeHand.SetDegAng( Angle: Single );
begin
  fAngle := Angle * PI / 180;
  Changed( Self );
end;

procedure scGaugeHand.SetTail( Value: integer );
begin
  fTail := Value;
  Changed( Self );
end;

procedure scGaugeHand.SetWidth( Value: integer );
begin
  fWidth := Value;
  Changed( Self );
end;

procedure scGaugeHand.SetArrowAspect( Value: integer );
begin
  fArrowAspect := Value;
  Changed( Self );
end;

procedure scGaugeHand.SetLength( Value: integer );
begin
  fLength := Value;
  Changed( Self );
end;

procedure scGaugeHand.SetBrush( Value: scBrush );
begin
  fBrush.Assign( Value );
  Changed( self );
end;

procedure scGaugeHand.SetPen( Value: scPen );
begin
  fPen.Assign( Value );
  Changed( self );
end;

procedure scGaugeHand.SetVisible( Value: boolean );
begin
  fVisible := Value;
  Changed( Self );
end;

function scGaugeHand.GetDisplayName: string;
begin
  case Index of
  0: result := 'Hour';
  1: result := 'Minutes';
  2: result := 'Seconds';
  else
    result := 'Hand #' + IntToStr( Index );
  end;

end;

procedure scGaugeHand.PaintTo( DstCanvas: TCanvas; Position: TPoint;
  Height, Width: integer; Radial: boolean; ForceCircular: boolean );
var
  puntos: stTPoints;
  x, y, wd, hg, dx, dy, np: integer;
  pen_tmp: TPen;
  brush_tmp: TBrush;
begin
  if not Visible then
    exit;

  // Determina las medidas sobre las que dibuja
  //
  wd := Width;
  hg := Height;
  if ForceCircular and ( hg < wd ) then wd := hg;
  if ForceCircular and ( wd < hg ) then hg := wd;
  x := Position.x;
  y := Position.y;

  if ( fWidth > 0 ) then
  begin
    np := 7;
    System.SetLength( puntos, np );
    dx := Round( scGraphics.CalcX( wd, fAngle - PI / 2, fLength * fWidth / 100 ) );
    dy := Round( scGraphics.CalcY( hg, fAngle - PI / 2, fLength * fWidth / 100 ) );
    puntos[ 0 ].x := x + dx;
    puntos[ 0 ].y := y - dy;
    puntos[ 1 ].x := x + Round( scGraphics.CalcX( wd, fAngle, fLength ) );
    puntos[ 1 ].y := y - Round( scGraphics.CalcY( hg, fAngle, fLength ) );

    puntos[ 2 ].x := puntos[ 1 ].x - ( dx * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 2 ].y := puntos[ 1 ].y + ( dy * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 1 ].x := puntos[ 1 ].x + ( dx * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 1 ].y := puntos[ 1 ].y - ( dy * ( 100 - fArrowAspect ) ) div 100;

    puntos[ 3 ].x := x - dx;
    puntos[ 3 ].y := y + dy;
    puntos[ 4 ].x := x + Round( scGraphics.CalcX( wd, fAngle + PI, fLength * fTail / 100 ) );
    puntos[ 4 ].y := y - Round( scGraphics.CalcY( hg, fAngle + PI, fLength * fTail / 100 ) );

    puntos[ 5 ].x := puntos[ 4 ].x + ( dx * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 5 ].y := puntos[ 4 ].y - ( dy * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 4 ].x := puntos[ 4 ].x - ( dx * ( 100 - fArrowAspect ) ) div 100;
    puntos[ 4 ].y := puntos[ 4 ].y + ( dy * ( 100 - fArrowAspect ) ) div 100;

    puntos[ 6 ].x := puntos[ 0 ].x;
    puntos[ 6 ].y := puntos[ 0 ].y;
  end
  else
  begin
    np := 2;
    System.SetLength( puntos, np );
    puntos[ 0 ].x := x;
    puntos[ 0 ].y := y;
    puntos[ 1 ].x := x + Round( scGraphics.CalcX( wd, fAngle, fLength ) );
    puntos[ 1 ].y := y - Round( scGraphics.CalcY( hg, fAngle, fLength ) );
  end;

  // Dibuja el polígono
  //
  brush_tmp := DstCanvas.Brush;
  pen_tmp := DstCanvas.Pen;
  DstCanvas.Brush := fBrush;
  DstCanvas.Pen := fPen;
  DstCanvas.Polygon( puntos );
  DstCanvas.Brush := brush_tmp;
  DstCanvas.Pen := pen_tmp;
end;


{ scGaugeScale }


constructor scCustomGaugeScale.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  // Crea la figura del contorno
  //
  fShape := scShape.Create();
  fShape.OnChange := Changed;
  fShape.fShape := stRoundRect;

  // Crea la figura central
  //
  fCenterShape := scShape.Create();
  fCenterShape.OnChange := Changed;
  fCenterShape.fRelSize := 10;
  fCenterShape.fShape := stCircle;

  // Crea el contenedor de agujas, los puntos y el
  // buffer de dibujo
  //
  fHands := TOwnedCollection.Create( AOwner, scGaugeHand );
  fPoints := scGaugeScalePoints.Create();
  fDrawBuff := TBitmap.Create();

  // Asigna las variables por default
  //
  Height := 40;
  Width := 40;
  fHandsCircular := true;
  fPoints.Visible := true;
  fPoints.Position := 80;
  fPoints.Size := 15;
  fPoints.Color := clBlue;
  fPoints.Minutes := true;
  fPoints.MarksWidth := 3;
  fPoints.OnChange := Changed;

  fColorChanged := true;
end;

destructor scCustomGaugeScale.Destroy;
begin
  fShape.Free();
  fCenterShape.Free();
  fHands.Free();
  fPoints.Free();
  fDrawBuff.Free();

  inherited Destroy();
end;

function scCustomGaugeScale.GetTranspColor( ExcludeColors: array of TColor): TColor;
var
  i1: integer;
  cl_arr: stTColors;
begin
  // Determina el color transparente
  //
  SetLength( cl_arr, fHands.Count * 2 + Length( ExcludeColors ) );
  for i1 := 0 to fHands.Count - 1 do
  begin
    cl_arr[ i1 * 2 ] := scGaugeHand( fHands.Items[ i1 ] ).Brush.Color;
    cl_arr[ i1 * 2 + 1 ] := scGaugeHand( fHands.Items[ i1 ] ).Pen.Color;
  end;
  for i1 := 0 to Length( ExcludeColors ) - 1 do
  begin
    cl_arr[ i1 + 2 * fHands.Count ] := ExcludeColors[ i1 ];
  end;

  result := scGraphics.GetTranspColor( scGraphics.ColorConcat( cl_arr,
    [ fShape.Pen.Color, fShape.Brush.Color,
    fCenterShape.Pen.Color, fCenterShape.Brush.Color,
    fPoints.Color, Font.Color ] ) );
end;

procedure scCustomGaugeScale.Paint;
var
  tmp: TColor;
  cv: TCanvas;
begin
//	if not Visible then exit;

  // Actualiza la medida del bitmap
  //
  if fDrawBuff.Width <> Width then
    fDrawBuff.Width := Width;
  if fDrawBuff.Height <> Height then
    fDrawBuff.Height := Height;

  // Toma variables para el dibujo
  //
  cv := fDrawBuff.Canvas;

  // Configura el color transparente
  //
  fDrawBuff.Transparent := true;
  fDrawBuff.TransparentMode := tmFixed;
  if fColorChanged then
  begin
    fDrawBuff.TransparentColor := GetTranspColor( [] );
    fColorChanged := false;
  end;

  // Dibuja un rectángulo transparente
  //
  cv.Brush.Color := fDrawBuff.TransparentColor;
  cv.FillRect( ClientRect );

  // Pinta en el canvas temporario
  //
  PaintTo( cv, ClientRect );

  // Vuelca el bitmap a la ventana
  //
  tmp := Canvas.Brush.Color;
  Canvas.Brush.Style := bsClear;
  Canvas.Draw( 0, 0, fDrawBuff );
//	Canvas.CopyRect( ClientRect, cv, ClientRect );
//	Canvas.BrushCopy( ClientRect, fDrawBuff, ClientRect, cl_transp );
  Canvas.Brush.Color := tmp;
end;

procedure scCustomGaugeScale.PaintTo( DstCanvas: TCanvas; Rect: TRect );
var
  square, line: boolean;
  x_off, y_off, wd, hg, x, y, dx, dy, dx2, dy2, i1: integer;
  ang: Single;
  pos: TPoint;
  ss: string;
  ts: TSize;
  tmp: TColor;
begin
  // Dibujas la formas
  //
  fShape.Draw( DstCanvas, Rect );
  fCenterShape.Draw( DstCanvas, Rect );
  square := fShape.IsSquare();

  // Obtiene el área de circunscripción
  //
  hg := ( ( Rect.Bottom - Rect.Top ) * fShape.fRelSize div 100 )
    - 2 * fShape.Pen.Width;
  wd := ( ( Rect.Right - Rect.Left ) * fShape.fRelSize div 100 )
    - 2 * fShape.Pen.Width;
  if square and ( hg < wd ) then wd := hg;
  if square and ( wd < hg ) then hg := wd;
  x_off := ( Rect.Right - Rect.Left - wd ) div 2;
  y_off := ( Rect.Bottom - Rect.Top - hg ) div 2;

  // Toma variables de dibujo
  //
  DstCanvas.Font := Font;
  DstCanvas.Pen.Width := 1;
  DstCanvas.Pen.Color := fPoints.Color;
  DstCanvas.Brush.Color := fPoints.Color;

  // Dibuja los puntos de los minutos
  //
  dx := Round( fPoints.Size * wd / 1000 );
  dy := Round( fPoints.Size * hg / 1000 );
  if fPoints.Visible and fPoints.Minutes then
  begin
    for i1 := 0 to 59 do
    begin
      if ( i1 mod 5 <> 0 ) then
      begin
        ang := ( 90 - 6 * i1 ) * PI / 180;
        if fHandsCircular and ( Cos( ang ) <> 0 ) then
        begin
          if ( Cos( ang ) < 0 ) then
            ang := ArcTan( Tan( ang ) * wd / hg ) + PI
          else
            ang := ArcTan( Tan( ang ) * wd / hg );
        end;
        x := x_off + Round( wd / 2 + scGraphics.CalcX( wd, ang, fPoints.Position div 2 ) );
        y := y_off + Round( hg / 2 - scGraphics.CalcY( hg, ang, fPoints.Position div 2 ) );
        DstCanvas.Rectangle( x - dx div 2, y - dy div 2, x + dx div 2, y + dy div 2 );
      end;
    end;
  end;

  // Dibuja los puntos de las horas
  //
  if fPoints.Visible then
  begin
    for i1 := 0 to 11 do
    begin
      ang := ( 90 - 6 * i1 * 5 ) * PI / 180;
      if fHandsCircular and ( Cos( ang ) <> 0 ) then
      begin
        if ( Cos( ang ) < 0 ) then
          ang := ArcTan( Tan( ang ) * wd / hg ) + PI
        else
          ang := ArcTan( Tan( ang ) * wd / hg );
      end;
      x := x_off + Round( wd / 2 + scGraphics.CalcX( wd, ang, fPoints.Position div 2 ) );
      y := y_off + Round( hg / 2 - scGraphics.CalcY( hg, ang, fPoints.Position div 2 ) );

      ss := scGraphics.PointGetStr( i1, fPoints.Texts, line );
      if ( Length( ss ) > 0 ) then
      begin
        ts := DstCanvas.TextExtent( ss );
        SetBkMode( DstCanvas.Handle, TRANSPARENT );
        DstCanvas.TextOut( x - ts.cX div 2, y - ts.cY div 2, ss );
        SetBkMode( DstCanvas.Handle, OPAQUE );
      end
      else
        DstCanvas.Rectangle( x - dx, y - dy, x + dx, y + dy );

      if line then
      begin
        x := x_off + Round( wd / 2 + scGraphics.CalcX( wd, ang, ( 3 * fPoints.Position ) div 8 ) );
        y := y_off + Round( hg / 2 - scGraphics.CalcY( hg, ang, ( 3 * fPoints.Position ) div 8 ) );
        dx2 := Round( scGraphics.CalcX( wd, ang, fPoints.Size / 10 ) );
        dy2 := Round( scGraphics.CalcY( hg, ang, fPoints.Size / 10 ) );
        tmp := DstCanvas.Pen.Width;
        DstCanvas.Pen.Width := fPoints.MarksWidth;
        DstCanvas.MoveTo( x - dx2, y + dy2 );
        DstCanvas.LineTo( x + dx2, y - dy2 );
        DstCanvas.Pen.Width := tmp;
      end;
    end;
  end;

  // Dibuja las agujas
  //
  pos.x := x_off + wd div 2;
  pos.y := y_off + hg div 2;
  for i1 := 0 to fHands.Count - 1 do
  begin
    scGaugeHand( fHands.Items[ i1 ] ).PaintTo( fDrawBuff.Canvas, pos, hg, wd, true,
      square or fHandsCircular );
  end;
end;

procedure scCustomGaugeScale.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or ws_Ex_Transparent;
  Params.WindowClass.hbrBackground := GetStockObject( NULL_BRUSH );
end;

procedure scCustomGaugeScale.SetHandsCircular( Value: boolean );
begin
  fHandsCircular := Value;
  Changed( Self );
end;

procedure scCustomGaugeScale.SetHands( Value: TOwnedCollection );
begin
  fHands := Value;
  Changed( Self );
end;

procedure scCustomGaugeScale.SetShape( Value: scShape );
begin
  fShape.Assign( Value );
  Changed( Self );
end;

procedure scCustomGaugeScale.SetCenterShape( Value: scShape );
begin
  fCenterShape.Assign( Value );
  Changed( Self );
end;

procedure scCustomGaugeScale.Changed(Sender: TObject);
begin
  fColorChanged := true;
  if Assigned( fOnChange ) then
    fOnChange( Self );
  Invalidate();
end;

procedure scCustomGaugeScale.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure scCustomGaugeScale.WMMove(var Message: TWMMove);
begin
  inherited;
  //Changed( nil );
end;


{ scClock }


constructor scClock.Create( AOwner: TComponent );
var
  hand: scGaugeHand;
begin
  inherited Create( AOwner );

  FEnabled := True;
  FInterval := 1000;
  FWindowHandle := Sil.Os.ToolWindow.Create(WndProcMsg);

  // Crea el objeto de la fecha
  //
  FDate := scClockDate.Create();
  FDate.OnChange := Changed;
  FDate.Visible := false;
  FDate.Format := 'dd/mm/yyyy';

  // Inicializa otras variables
  //
  Height := 40;
  Width := 40;

  fClkHands := scClockHands.Create( Self );

  // Crea la aguja de la hora
  //
  hand := scGaugeHand( fHands.Add() );
  hand.Tail := 20;
  hand.Width := 15;
  hand.Length := 25;
  hand.Brush.Color := clRed;
  hand.Pen.Color := clNavy;

  // Crea la aguja de los minutos
  //
  hand := scGaugeHand( fHands.Add() );
  hand.Tail := 15;
  hand.Width := 5;
  hand.Length := 35;
  hand.Brush.Color := clBlue;
  hand.Pen.Color := clBackground;

  // Crea la aguja de los segundos
  //
  hand := scGaugeHand( fHands.Add() );
  hand.Tail := 15;
  hand.Width := -1;
  hand.Length := 35;
  hand.Pen.Color := clGreen;

  fShape.RelativeSize := 100;
  fShape.Brush.Color := clMaroon;
  fShape.Pen.Color := clNavy;
  fShape.Pen.Width := 2;
  fShape.ShapeType := stRoundRect;

  fCenterShape.Visible := True;
  fCenterShape.RelativeSize := 90;
  fCenterShape.Brush.Color := clInfoBk;
  fCenterShape.Pen.Color := clWhite;
  fCenterShape.ShapeType := stEllipse;

  Font.Color := clMaroon;

  UpdateTimer;
end;

destructor scClock.Destroy;
begin
  FEnabled := False;
  UpdateTimer;

  FDate.Free();
  FWindowHandle := nil;

  fClkHands.Free();

  inherited Destroy();
end;

procedure scClock.Loaded;
begin
  inherited Loaded();

  scGaugeHand( fHands.Items[ 0 ] ).OnChange := Changed;
  scGaugeHand( fHands.Items[ 1 ] ).OnChange := Changed;
  scGaugeHand( fHands.Items[ 2 ] ).OnChange := Changed;

  Timer();
end;

procedure scClock.WndProcMsg(var Msg);
begin
  try
    if ( TMessage(Msg).Msg = WM_TIMER ) then
    begin
      Timer;
    end
    else if ( FWindowHandle <> nil ) then
      with TMessage(Msg) do
        DefWindowProc( FWindowHandle.Value, Msg, WParam, LParam );
  except
  end;
end;

procedure scClock.UpdateTimer;
begin
  KillTimer(FWindowHandle.Value, 1);
  if (FInterval <> 0) and FEnabled then
    if SetTimer(FWindowHandle.Value, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create( SNoTimers );
end;

procedure scClock.SetEnabled(Value: boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure scClock.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure scClock.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  Invalidate();
end;

procedure scClock.Timer;
begin
  Invalidate();
end;

procedure scClock.Paint;
var
  ahora: TDateTime;
  hora, min, seg, msec: Word;
  style_tmp: TBrushStyle;
  ts: TSize;
  rect: TRect;

  date_str: string;
begin
  // Obtiene y decodifica el tiempo
  //
  ahora := Now();
  DecodeTime( ahora, hora, min, seg, msec );
  if ( fHands.Count > 0 ) then
    scGaugeHand( fHands.Items[ 0 ] ).fAngle := ( 90 + 360 - 30 * hora - min div 2 ) * PI / 180;
  if ( fHands.Count > 1 ) then
    scGaugeHand( fHands.Items[ 1 ] ).fAngle := ( 90 + 360 - 6 * min - seg div 10 ) * PI / 180;
  if ( fHands.Count > 2 ) then
    scGaugeHand( fHands.Items[ 2 ] ).fAngle := ( 90 + 360 - 6 * seg ) * PI / 180;

  // Actualiza la medida del bitmap
  //
  if fDrawBuff.Width <> Width then
    fDrawBuff.Width := Width;
  if fDrawBuff.Height <> Height then
    fDrawBuff.Height := Height;

  // Configura el color transparente
  //
  fDrawBuff.Transparent := true;
  fDrawBuff.TransparentMode := tmFixed;
  if fColorChanged then
  begin
    fDrawBuff.TransparentColor := GetTranspColor( [] );
    fColorChanged := false;
  end;

  // Dibuja un rectángulo transparente
  //
  fDrawBuff.Canvas.Brush.Color := fDrawBuff.TransparentColor;
  fDrawBuff.Canvas.FillRect( ClientRect );

  // Pinta la escala
  //
  rect := ClientRect;
  if FDate.Visible then
  begin
    try
      date_str := FormatDatetime( FDate.Format, Now() );
    except
      date_str := FormatDatetime( 'dd/mm/yyyy', Now() );
    end;
    if ( fLastDate <> date_str ) then
    begin
      fLastDate := date_str;
      Changed( nil );
    end;
    fDrawBuff.Canvas.Font := FDate.Font;
    ts := fDrawBuff.Canvas.TextExtent( date_str );

    // Agrega la fecha
    //
    if ( ClientRect.Bottom - ClientRect.Top - ts.cy > 20 ) then
    begin
      rect.Bottom := rect.Bottom - ts.cy;
      fDrawBuff.Canvas.TextOut( ( rect.Right - rect.Left - ts.cx ) div 2,
        rect.Bottom, date_str );
    end;
  end;

  PaintTo( fDrawBuff.Canvas, rect );

  // Vuelca el bitmap a la ventana
  //
  style_tmp := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Draw( 0, 0, fDrawBuff );
  Canvas.Brush.Style := style_tmp;

  if Assigned(FOnTimer) then FOnTimer(Self);
end;


procedure scClock.SetDate(const Value: scClockDate);
begin
  FDate.Assign( Value );
end;

procedure scClock.Changed(Sender: TObject);
begin
  fColorChanged := true;
  if ( Parent <> nil ) then
    Parent.Invalidate();
  Invalidate();
end;

procedure scClock.SetClkHands(const Value: scClockHands);
begin
  fClkHands.Assign( Value );
end;

{ scShape }

constructor scShape.Create;
begin
  inherited Create();

  fVisible := true;
  fRelSize := 100;
  fPen := TPen.Create();
  fPen.OnChange := Changed;
  fBrush := TBrush.Create();
  fBrush.OnChange := Changed;
end;

destructor scShape.Destroy;
begin
  fPen.Free();
  fBrush.Free();

  inherited Destroy();
end;

procedure scShape.Changed(Sender: TObject);
begin
  if @fOnChange <> nil then
    fOnChange( Self );
end;

function scShape.IsSquare: boolean;
begin
  result := ( fShape in [ stSquare, stRoundSquare, stCircle ] )
end;

procedure scShape.SetShape( Value: TShapeType );
begin
  fShape := Value;
  Changed( Self );
end;

procedure scShape.SeTPen( Value: TPen );
begin
  fPen.Assign( Value );
  Changed( Self );
end;

procedure scShape.SetVisible( Value: boolean );
begin
  fVisible := Value;
  Changed( Self );
end;

procedure scShape.SetRelSize( Value: integer );
begin
  fRelSize := Value;
  Changed( Self );
end;

procedure scShape.SetBrush( Value: TBrush );
begin
  fBrush.Assign( Value );
  Changed( Self );
end;

procedure scShape.Draw( ACanvas: TCanvas; const Rect: TRect );
var
  X, Y, W, H, S: integer;
begin
  if ( not fVisible ) then exit;

  ACanvas.Pen := fPen;
  ACanvas.Brush := fBrush;

  W := ( ( Rect.Right - Rect.Left ) * fRelSize div 100 ) - ACanvas.Pen.Width + 1;
  H := ( ( Rect.Bottom - Rect.Top ) * fRelSize div 100 ) - ACanvas.Pen.Width + 1;
  if ACanvas.Pen.Width = 0 then
  begin
    Dec( W, 2 );
    Dec( H, 2 );
  end;
  X := ( Rect.Right - Rect.Left - W ) div 2;
  Y := ( Rect.Bottom - Rect.Top - H ) div 2;

  if W < H then S := W else S := H;
  if fShape in [stSquare, stRoundSquare, stCircle] then
  begin
    Inc( X, (W - S) div 2 );
    Inc( Y, (H - S) div 2 );
    W := S;
    H := S;
  end;

  case fShape of
  stRectangle, stSquare:
    ACanvas.Rectangle( X, Y, X + W, Y + H );
  stRoundRect, stRoundSquare:
    ACanvas.RoundRect( X, Y, X + W, Y + H, S div 4, S div 4 );
  stCircle, stEllipse:
    ACanvas.Ellipse( X, Y, X + W, Y + H );
  end;
end;


{ scClockLabel }

procedure scClockLabel.Changed(Sender: TObject);
begin
  if Assigned( fOnChange ) then
    fOnChange( Self );
end;

constructor scClockLabel.Create;
begin
  fFont := TFont.Create();
  fFont.OnChange := Changed;
  fOnChange := nil;
end;

destructor scClockLabel.Destroy;
begin
  fFont.Free();

  inherited Destroy;
end;

procedure scClockLabel.SetFont(const Value: TFont);
begin
  fFont.Assign( Value );
  Changed( Self );
end;

procedure scClockLabel.SetOnChange(const Value: TNotifyEvent);
begin
  fOnChange := Value;
end;

procedure scClockLabel.SetVisible(const Value: boolean);
begin
  fVisible := Value;
  Changed( Self );
end;

{ scClockDate }

procedure scClockDate.SetFormat(const Value: string);
begin
  fFormat := Value;
  Changed( Self );
end;


{ scGaugeScalePoints }

procedure scGaugeScalePoints.SetVisible(const Value: boolean);
begin
  fVisible := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetSize( const Value: integer );
begin
  fSize := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetTexts( const Value: string );
begin
  fTexts := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetMinutes( const Value: boolean );
begin
  fMinutes := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetPosition( const Value: integer );
begin
  fPosition := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetColor( const Value: TColor );
begin
  fColor := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.SetMarksWidth(const Value: integer);
begin
  fMarksWidth := Value;
  Changed( Self );
end;

procedure scGaugeScalePoints.Changed(Sender: TObject);
begin
  if @fOnChange <> nil then
    fOnChange( Self );
end;


{ scClockHands }

constructor scClockHands.Create(Scale: scCustomGaugeScale);
begin
  fScale := Scale;
end;

function scClockHands.GetCircular: boolean;
begin
  result := fScale.HandsCircular;
end;

function scClockHands.GetHour: scGaugeHand;
begin
  result := scGaugeHand( fScale.fHands.Items[ 0 ] );
end;

function scClockHands.GetMinutes: scGaugeHand;
begin
  result := scGaugeHand( fScale.fHands.Items[ 1 ] );
end;

function scClockHands.GetSeconds: scGaugeHand;
begin
  result := scGaugeHand( fScale.fHands.Items[ 2 ] );
end;

procedure scClockHands.SetCircular(const Value: boolean);
begin
  fScale.HandsCircular := Value;
end;

procedure scClockHands.SetHour(const Value: scGaugeHand);
begin
  fScale.fHands.Items[ 0 ] := TCollectionItem( Value );
end;

procedure scClockHands.SetMinutes(const Value: scGaugeHand);
begin
  fScale.fHands.Items[ 1 ] := TCollectionItem( Value );
end;

procedure scClockHands.SetSeconds(const Value: scGaugeHand);
begin
  fScale.fHands.Items[ 2 ] := TCollectionItem( Value );
end;


end.

