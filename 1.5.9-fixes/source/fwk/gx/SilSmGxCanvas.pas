{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@str.com.ar               *
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

unit SilSmGxCanvas;

interface

uses
  Sil,
  Windows, Graphics,
  SilSiGx;

type
  TGxVclCanvas = class
  (
    TSilObject,
    IGxCanvas
  )
  private
    FCanvas: TCanvas;
  private // IGxCanvas
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
    procedure Draw(X, Y: double; const Graphic: IGxGraphic);
    procedure Ellipse(X1, Y1, X2, Y2: double);
    procedure FillRect(X1, Y1, X2, Y2: double); overload;
    procedure FillRect( Rect: TGxRect ); overload;
    procedure MoveTo(X, Y: double);
    procedure LineTo(X, Y: double);
    procedure Line(X1, Y1, X2, Y2: double);
    procedure Rectangle(X1, Y1, X2, Y2: double);
    procedure FrameRect(X1, Y1, X2, Y2: double);
    procedure Frame3D(X1, Y1, X2, Y2, Width: double; TopColor, BottomColor: TColor);
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: double);
    procedure StretchDraw(X1, Y1, X2, Y2: double; const Graphic: IGxGraphic);
    procedure TextOut(X, Y: double; const Text: string);
    procedure TextRect(X1, Y1, X2, Y2, X, Y: double; const Text: string);
    procedure Polygon(const Points: array of TGxPoint);
    procedure Polyline(const Points: array of TGxPoint);
    //
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): double;
    function TextWidth(const Text: string): double;
    //
    function GetVclCanvas: TCanvas;
    function GetBrush: TBrush;
    function GetCopyMode: TCopyMode;
    function GetFont: TFont;
    function GetPen: TPen;
    //
    function GetPixelWidth( Pixels: integer = 1 ): double;
    function GetPixelHeight( Pixels: integer = 1 ): double;
  public
    constructor Create( Canvas: TCanvas ); reintroduce;
    destructor Destroy; override;
  end;

  TGxCanvasView = class
  (
    TSilObject,
    IGxCanvas,
    IGxView
  )
  private
    FActive: boolean;
    FCanvas: IGxCanvas;
  protected
    function DoTranslateXY( X1, Y1: double; out X2, Y2: double ): boolean; virtual;
    function DoUntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean; virtual;
    function DoTranslateWidth( Width1: double; out Width2: double ): boolean; virtual;
    function DoTranslateHeight( Height1: double; out Height2: double ): boolean; virtual;
    function DoUntranslateWidth( Width2: double; out Width1: double ): boolean; virtual;
    function DoUntranslateHeight( Height2: double; out Height1: double ): boolean; virtual;
  protected // IGxCanvas
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
    procedure Draw(X, Y: double; const Graphic: IGxGraphic);
    procedure Ellipse(X1, Y1, X2, Y2: double);
    procedure FillRect(X1, Y1, X2, Y2: double); overload;
    procedure FillRect( Rect: TGxRect ); overload;
    procedure MoveTo(X, Y: double);
    procedure LineTo(X, Y: double);
    procedure Line(X1, Y1, X2, Y2: double);
    procedure Rectangle(X1, Y1, X2, Y2: double);
    procedure FrameRect(X1, Y1, X2, Y2: double);
    procedure Frame3D(X1, Y1, X2, Y2, Width: double; TopColor, BottomColor: TColor);
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: double);
    procedure StretchDraw(X1, Y1, X2, Y2: double; const Graphic: IGxGraphic);
    procedure TextOut(X, Y: double; const Text: string);
    procedure TextRect(X1, Y1, X2, Y2, X, Y: double; const Text: string);
    procedure Polygon(const Points: array of TGxPoint);
    procedure Polyline(const Points: array of TGxPoint);
    //
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): double;
    function TextWidth(const Text: string): double;
    //
    function GetVclCanvas: TCanvas;
    function GetBrush: TBrush;
    function GetCopyMode: TCopyMode;
    function GetFont: TFont;
    function GetPen: TPen;
    //
    function GetPixelWidth( Pixels: integer = 1 ): double;
    function GetPixelHeight( Pixels: integer = 1 ): double;
  protected // IGxView
    function TranslateXY( X1, Y1: double; out X2, Y2: double ): boolean;
    function UntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean;
    function GetCanvas: IGxCanvas;
    procedure SetCanvas(const Canvas: IGxCanvas);
  protected
    property Active: boolean read FActive write FActive;
  public
    constructor Create(const Canvas: IGxCanvas); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TGxView2D = class
  (
    TGxCanvasView,
    IGxView2D
  )
  private
    FScale: double;
    FWidth: double;
    FHeight: double;
    FPanX: double;
    FPanY: double;
  protected
    procedure FireViewChanged;
  protected
    procedure OnViewChanged; virtual;
  protected
    function DoTranslateXY( X1, Y1: double; out X2, Y2: double ): boolean; override;
    function DoUntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean; override;
    function DoTranslateWidth( Width1: double; out Width2: double ): boolean; override;
    function DoTranslateHeight( Height1: double; out Height2: double ): boolean; override;
    function DoUntranslateWidth( Width2: double; out Width1: double ): boolean; override;
    function DoUntranslateHeight( Height2: double; out Height1: double ): boolean; override;
  protected // IGxView2D
    function GetScale: double;
    function GetWidth: double;
    function GetHeight: double;
    function GetFocusX: double;
    function GetFocusY: double;
    function GetPanX: double;
    function GetPanY: double;
    function GetVisibleRect: TGxRect;
    procedure SetScale(Value: double);
    procedure SetWidth(Value: double);
    procedure SetHeight(Value: double);
    procedure SetFocusX(Value: double);
    procedure SetFocusY(Value: double);
    procedure SetPanX(Value: double);
    procedure SetPanY(Value: double);
  public
    constructor Create(const Canvas: IGxCanvas); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmGxEntities;
  
{ TGxVclCanvas }

constructor TGxVclCanvas.Create( Canvas: TCanvas );
begin
  inherited Create;
  FCanvas := Canvas; 
end;

destructor TGxVclCanvas.Destroy;
begin

  inherited;
end;

procedure TGxVclCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
begin
  FCanvas.Arc(
    Round( X1 ), Round( Y1 ),
    Round( X2 ), Round( Y2 ),
    Round( X3 ), Round( Y3 ),
    Round( X4 ), Round( Y4 ) );
end;

procedure TGxVclCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
begin
  FCanvas.Chord(
    Round( X1 ), Round( Y1 ),
    Round( X2 ), Round( Y2 ),
    Round( X3 ), Round( Y3 ),
    Round( X4 ), Round( Y4 ) );
end;

procedure TGxVclCanvas.Draw(X, Y: double; const Graphic: IGxGraphic);
begin
  FCanvas.Draw( Round( X ), Round( Y ), Graphic.VclGraphic );
end;

procedure TGxVclCanvas.Ellipse(X1, Y1, X2, Y2: double);
begin
  FCanvas.Ellipse( Round( X1 ), Round( Y1 ),
    Round( X2 ), Round( Y2 ) );
end;

procedure TGxVclCanvas.FillRect(X1, Y1, X2, Y2: double);
var
  rect: TRect;
begin
  rect.Left := Round( X1 );
  rect.Top := Round( Y1 );
  rect.Right := Round( X2 );
  rect.Bottom := Round( Y2 );
  FCanvas.FillRect( rect );
end;

procedure TGxVclCanvas.FillRect( Rect: TGxRect ); 
var
  rect2: TRect;
begin
  rect2.Left := Round( Rect.Left );
  rect2.Top := Round( Rect.Top );
  rect2.Right := Round( Rect.Right );
  rect2.Bottom := Round( Rect.Bottom );
  FCanvas.FillRect( rect2 );
end;

function TGxVclCanvas.GetVclCanvas: TCanvas;
begin
  result := FCanvas;
end;

function TGxVclCanvas.GetBrush: TBrush;
begin
  result := FCanvas.Brush;
end;

function TGxVclCanvas.GetCopyMode: TCopyMode;
begin
  result := FCanvas.CopyMode;
end;

function TGxVclCanvas.GetFont: TFont;
begin
  result := FCanvas.Font;
end;

function TGxVclCanvas.GetPen: TPen;
begin
  result := FCanvas.Pen;
end;

procedure TGxVclCanvas.LineTo(X, Y: double);
begin
  FCanvas.LineTo( Round( X ), Round( Y ) );
end;

procedure TGxVclCanvas.MoveTo(X, Y: double);
begin
  FCanvas.MoveTo( Round( X ), Round( Y ) );
end;

procedure TGxVclCanvas.Line(X1, Y1, X2, Y2: double);
begin
  MoveTo( X1, Y1 );
  LineTo( X2, Y2 );
end;

procedure TGxVclCanvas.Rectangle(X1, Y1, X2, Y2: double);
begin
  FCanvas.Rectangle(
    Round( X1 ), Round( Y1 ),
    Round( X2 ), Round( Y2 ) );
end;

procedure TGxVclCanvas.FrameRect(X1, Y1, X2, Y2: double);
var
  rect: TRect;
begin
  rect.Left := Round( X1 );
  rect.Top := Round( Y1 );
  rect.Right := Round( X2 );
  rect.Bottom := Round( Y2 );
  FCanvas.FrameRect( rect );
end;

procedure TGxVclCanvas.Frame3D(X1, Y1, X2, Y2, Width: double; TopColor, BottomColor: TColor);
var
  wd: integer;
  tl, tr, br, bl: TPoint; 
begin
  tl.X := Round( X1 );
  tl.Y := Round( Y1 );
  tr.X := Round( X2 ) - 1;
  tr.Y := tl.Y;
  br.X := tr.X;
  br.Y := Round( Y2 ) - 1;
  bl.X := tl.X;
  bl.Y := br.Y;
  wd := Round( Width );

  while ( wd > 0 ) do
  begin
    with FCanvas do
    begin
      Pen.Style := psSolid;
      Pen.Width := 1;
      Pen.Color := TopColor;
      Polyline( [ bl, tl, tr ] );
      Pen.Color := BottomColor;
      //dec( bl.X );
      Polyline( [ tr, br, bl ] );
    end;
    inc( tl.X ); inc( tl.Y );
    dec( tr.X ); inc( tr.Y );
    dec( br.X ); dec( br.Y );
    inc( bl.X ); dec( bl.Y );
    dec( wd );
  end;
end;

procedure TGxVclCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: double);
begin
  FCanvas.RoundRect(
    Round( X1 ), Round( Y1 ),
    Round( X2 ), Round( Y2 ),
    Round( X3 ), Round( Y3 ) );
end;

procedure TGxVclCanvas.StretchDraw(X1, Y1, X2, Y2: double; const Graphic: IGxGraphic);
var
  rect: TRect;
  gr: TBitmap;
begin
  if Assigned( Graphic ) and Assigned( Graphic.VclGraphic ) and ( X2 > X1 ) and ( Y2 > Y1 ) then
  begin
    if Graphic.GetVclStretched( Round( X2 - X1 ), Round( Y2 - Y1 ), gr ) then
    begin
      FCanvas.Draw( Round( X1 ), Round( Y1 ), gr );
    end
    else
    begin
      rect.Left := Round( X1 );
      rect.Top := Round( Y1 );
      rect.Right := Round( X2 );
      rect.Bottom := Round( Y2 );
      FCanvas.StretchDraw( rect, Graphic.VclGraphic );
    end;
  end;
end;

function TGxVclCanvas.TextExtent(const Text: string): TSize;
begin
  result := FCanvas.TextExtent( Text );
end;

function TGxVclCanvas.TextHeight(const Text: string): double;
begin
  result := FCanvas.TextHeight( Text );
end;

procedure TGxVclCanvas.TextOut(X, Y: double; const Text: string);
begin
  FCanvas.TextOut( Round( X ), Round( Y ), Text );
end;

procedure TGxVclCanvas.TextRect(X1, Y1, X2, Y2, X, Y: double; const Text: string);
var
  rect: TRect;
begin
  rect.Left := Round( X1 );
  rect.Top := Round( Y1 );
  rect.Right := Round( X2 );
  rect.Bottom := Round( Y2 );
  FCanvas.TextRect( rect, Round( X ), Round( Y ), Text );
end;

procedure TGxVclCanvas.Polygon(const Points: array of TGxPoint);
var
  i1: integer;
  arr: array of TPoint;
begin
  SetLength( arr, Length( Points ) );
  for i1 := 0 to Length( arr ) - 1 do
  begin
    arr[ i1 ].X := Round( Points[ i1 ].X );
    arr[ i1 ].Y := Round( Points[ i1 ].Y );
  end;
  FCanvas.Polygon( arr );
end;

procedure TGxVclCanvas.Polyline(const Points: array of TGxPoint);
var
  i1: integer;
  arr: array of TPoint;
begin
  SetLength( arr, Length( Points ) );
  for i1 := 0 to Length( arr ) - 1 do
  begin
    arr[ i1 ].X := Round( Points[ i1 ].X );
    arr[ i1 ].Y := Round( Points[ i1 ].Y );
  end;
  FCanvas.Polyline( arr );
end;

function TGxVclCanvas.TextWidth(const Text: string): double;
begin
  result := FCanvas.TextWidth( Text );
end;

function TGxVclCanvas.GetPixelHeight(Pixels: integer): double;
begin
  result := 1;
end;

function TGxVclCanvas.GetPixelWidth(Pixels: integer): double;
begin
  result := 1;
end;

{ TGxCanvasView }

constructor TGxCanvasView.Create(const Canvas: IGxCanvas);
begin
  inherited Create;
  FCanvas := Canvas;
end;

destructor TGxCanvasView.Destroy;
begin
  FCanvas := nil;
  inherited;
end;

function TGxCanvasView.GetVclCanvas: TCanvas;
begin
  result := FCanvas.VclCanvas;
end;

function TGxCanvasView.GetBrush: TBrush;
begin
  result := FCanvas.Brush;
end;

function TGxCanvasView.GetCopyMode: TCopyMode;
begin
  result := FCanvas.CopyMode;
end;

function TGxCanvasView.GetFont: TFont;
begin
  result := FCanvas.Font;
end;

function TGxCanvasView.GetPen: TPen;
begin
  result := FCanvas.Pen;
end;

procedure TGxCanvasView.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
    DoTranslateXY( X3, Y3, X3, Y3 );
    DoTranslateXY( X4, Y4, X4, Y4 );
  end;

  FCanvas.Arc( X1, Y1, X2, Y2, X3, Y3, X4, Y4 )
end;

procedure TGxCanvasView.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
    DoTranslateXY( X3, Y3, X3, Y3 );
    DoTranslateXY( X4, Y4, X4, Y4 );
  end;

  FCanvas.Chord( X1, Y1, X2, Y2, X3, Y3, X4, Y4 );
end;

procedure TGxCanvasView.Draw(X, Y: double; const Graphic: IGxGraphic);
begin
  if FActive then
    DoTranslateXY( X, Y, X, Y );

  FCanvas.Draw( X, Y, Graphic );
end;

procedure TGxCanvasView.Ellipse(X1, Y1, X2, Y2: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
  end;

  FCanvas.Ellipse( X1, Y1, X2, Y2 );
end;

procedure TGxCanvasView.FillRect(X1, Y1, X2, Y2: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
  end;

  FCanvas.FillRect( X1, Y1, X2, Y2 );
end;

procedure TGxCanvasView.FillRect(Rect: TGxRect);
begin
  FillRect( Rect.Left, Rect.Top, Rect.Right, Rect.Bottom );
end;

procedure TGxCanvasView.LineTo(X, Y: double);
begin
  if FActive then
    DoTranslateXY( X, Y, X, Y );

  FCanvas.LineTo( X, Y );
end;

procedure TGxCanvasView.MoveTo(X, Y: double);
begin
  if FActive then
    DoTranslateXY( X, Y, X, Y );

  FCanvas.MoveTo( X, Y );
end;

procedure TGxCanvasView.Line(X1, Y1, X2, Y2: double);
begin
  MoveTo( X1, Y1 );
  LineTo( X2, Y2 );
end;

procedure TGxCanvasView.Rectangle(X1, Y1, X2, Y2: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
  end;

  FCanvas.Rectangle( X1, Y1, X2, Y2 );
end;

procedure TGxCanvasView.FrameRect(X1, Y1, X2, Y2: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
  end;

  FCanvas.FrameRect( X1, Y1, X2, Y2 );
end;

procedure TGxCanvasView.Frame3D(X1, Y1, X2, Y2, Width: double; TopColor, BottomColor: TColor);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
    DoTranslateWidth( Width, Width );
  end;

  FCanvas.Frame3D( X1, Y1, X2, Y2, Width, TopColor, BottomColor );
end;

procedure TGxCanvasView.RoundRect(X1, Y1, X2, Y2, X3, Y3: double);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
    DoTranslateWidth( X3, X3 );
    DoTranslateHeight( Y3, Y3 );
  end;

  FCanvas.RoundRect( X1, Y1, X2, Y2, X3, Y3 );
end;

procedure TGxCanvasView.StretchDraw(X1, Y1, X2, Y2: double; const Graphic: IGxGraphic);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
  end;

  FCanvas.StretchDraw( X1, Y1, X2, Y2, Graphic );
end;

function TGxCanvasView.TextExtent(const Text: string): TSize;
var
  x1, y1: double;
begin
  result := FCanvas.TextExtent( Text );
  if FActive and
    DoUntranslateWidth( result.cx, x1 ) and
    DoUntranslateHeight( result.cy, y1 ) 
  then
  begin
    result.cx := Round( x1 );
    result.cy := Round( y1 );
  end;
end;

function TGxCanvasView.TextHeight(const Text: string): double;
begin
  result := FCanvas.TextHeight( Text );
  if FActive then DoUntranslateHeight( result, result );
end;

procedure TGxCanvasView.TextOut(X, Y: double; const Text: string);
begin
  if FActive then
    DoTranslateXY( X, Y, X, Y );

  FCanvas.TextOut( X, Y, Text );
end;

procedure TGxCanvasView.TextRect(X1, Y1, X2, Y2, X, Y: double; const Text: string);
begin
  if FActive then
  begin
    DoTranslateXY( X1, Y1, X1, Y1 );
    DoTranslateXY( X2, Y2, X2, Y2 );
    DoTranslateXY( X, Y, X, Y );
  end;

  FCanvas.TextRect( X1, Y1, X2, Y2, X, Y, Text );
end;

function TGxCanvasView.TextWidth(const Text: string): double;
begin
  result := FCanvas.TextWidth( Text );
  if FActive then DoUntranslateWidth( result, result );
end;

procedure TGxCanvasView.Polygon(const Points: array of TGxPoint);
var
  i1: integer;
  arr: array of TGxPoint;
  x, y: double;
begin
  if FActive then
  begin
    SetLength( arr, Length( Points ) );
    for i1 := 0 to Length( arr ) - 1 do
    begin
      DoTranslateXY( Points[ i1 ].X, Points[ i1 ].Y, X, Y );
      arr[ i1 ].X := x;
      arr[ i1 ].Y := y;
    end;
    FCanvas.Polygon( arr );
  end
  else
    FCanvas.Polygon( Points );
end;

procedure TGxCanvasView.Polyline(const Points: array of TGxPoint);
var
  i1: integer;
  arr: array of TGxPoint;
  x, y: double;
begin
  if FActive then
  begin
    SetLength( arr, Length( Points ) );
    for i1 := 0 to Length( arr ) - 1 do
    begin
      DoTranslateXY( Points[ i1 ].X, Points[ i1 ].Y, X, Y );
      arr[ i1 ].X := x;
      arr[ i1 ].Y := y;
    end;
    FCanvas.Polyline( arr );
  end
  else
    FCanvas.Polyline( Points );
end;

function TGxCanvasView.DoTranslateXY(X1, Y1: double; out X2, Y2: double): boolean;
begin
  result := false;
end;

function TGxCanvasView.DoUntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean;
begin
  result := false;
end;

function TGxCanvasView.DoTranslateHeight(Height1: double; out Height2: double): boolean;
begin
  result := false;
end;

function TGxCanvasView.DoTranslateWidth(Width1: double; out Width2: double): boolean;
begin
  result := false;
end;

function TGxCanvasView.DoUntranslateHeight(Height2: double; out Height1: double): boolean;
begin
  result := false;
end;

function TGxCanvasView.DoUntranslateWidth(Width2: double; out Width1: double): boolean;
begin
  result := false;
end;

function TGxCanvasView.TranslateXY(X1, Y1: double; out X2, Y2: double): boolean;
begin
  result := DoTranslateXY( X1, Y1, X2, Y2 );
end;

function TGxCanvasView.UntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean;
begin
  result := DoUntranslateXY( X2, Y2, X1, Y1 );
end;

function TGxCanvasView.GetPixelHeight(Pixels: integer): double;
begin
  if FActive then
  begin
    if not DoUntranslateHeight( Pixels, result ) then
      raise Exception.Create( 'Can''t Get Pixel Height' );
  end
  else
    result := Pixels;
end;

function TGxCanvasView.GetPixelWidth(Pixels: integer): double;
begin
  if FActive then
  begin
    if not DoUntranslateWidth( Pixels, result ) then
      raise Exception.Create( 'Can''t Get Pixel Width' );
  end
  else
    result := Pixels;
end;

function TGxCanvasView.GetCanvas: IGxCanvas;
begin
  result := self;
end;

procedure TGxCanvasView.SetCanvas(const Canvas: IGxCanvas);
begin
  FCanvas := Canvas;
end;

{ TGxView2D }

constructor TGxView2D.Create(const Canvas: IGxCanvas);
begin
  inherited Create( Canvas );
  FScale := 1;
end;

destructor TGxView2D.Destroy;
begin
  inherited;
end;

function TGxView2D.GetHeight: double;
begin
  result := FHeight;
end;

function TGxView2D.GetWidth: double;
begin
  result := FWidth;
end;

function TGxView2D.GetFocusX: double;
begin
  result := FWidth / FScale / 2 + FPanX;
end;

function TGxView2D.GetFocusY: double;
begin
  result := FHeight / FScale / 2 + FPanY;
end;

function TGxView2D.GetPanX: double;
begin
  result := FPanX;
end;

function TGxView2D.GetPanY: double;
begin
  result := FPanY;
end;

function TGxView2D.GetScale: double;
begin
  result := FScale;
end;

procedure TGxView2D.SetHeight(Value: double);
begin
  if ( FHeight <> Value ) then
  begin
    FHeight := Value;
    FireViewChanged;
  end;
end;

procedure TGxView2D.SetWidth(Value: double);
begin
  if ( FWidth <> Value ) then
  begin
    FWidth := Value;
    FireViewChanged;
  end;
end;

procedure TGxView2D.SetPanX(Value: double);
begin
  if ( FPanX <> Value ) then
  begin
    FPanX := Value;
    FireViewChanged;
  end;
end;

procedure TGxView2D.SetPanY(Value: double);
begin
  if ( FPanY <> Value ) then
  begin
    FPanY := Value;
    FireViewChanged;
  end;
end;

procedure TGxView2D.SetFocusX(Value: double);
begin
  SetPanX( Value - FWidth / FScale / 2 );
end;

procedure TGxView2D.SetFocusY(Value: double);
begin
  SetPanY( Value - FHeight / FScale / 2 );
end;

procedure TGxView2D.SetScale(Value: double);
begin
  if ( FScale <> Value ) then
  begin
    FScale := Value;
    FireViewChanged;
  end;
end;

function TGxView2D.DoTranslateXY(X1, Y1: double; out X2, Y2: double): boolean;
begin
  X2 := FScale * ( X1 - FPanX );
  Y2 := FScale * ( Y1 - FPanY );
  result := true;
end;

function TGxView2D.DoUntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean;
begin
  X1 := X2 / FScale + FPanX;
  Y1 := Y2 / FScale + FPanY;
  result := true;
end;

function TGxView2D.DoTranslateHeight(Height1: double; out Height2: double): boolean;
begin
  Height2 := Height1 * FScale;
  result := true;
end;

function TGxView2D.DoTranslateWidth(Width1: double; out Width2: double): boolean;
begin
  Width2 := Width1 * FScale;
  result := true;
end;

function TGxView2D.DoUntranslateHeight(Height2: double; out Height1: double): boolean;
begin
  result := ( FScale <> 0 );
  if result then
    Height1 := Height2 / FScale;
end;

function TGxView2D.DoUntranslateWidth(Width2: double; out Width1: double): boolean;
begin
  result := ( FScale <> 0 );
  if result then
    Width1 := Width2 / FScale;
end;

procedure TGxView2D.FireViewChanged;
begin
  self.Active := ( FScale <> 1 ) or ( FPanX <> 0 ) or ( FPanY <> 0 );

  OnViewChanged;
end;

procedure TGxView2D.OnViewChanged;
begin

end;

function TGxView2D.GetVisibleRect: TGxRect;
begin
  result.Left := FPanX;
  result.Top := FPanY;
  result.Right := FPanX + FWidth / FScale;
  result.Bottom := FPanY + FHeight / FScale;
end;

end.

