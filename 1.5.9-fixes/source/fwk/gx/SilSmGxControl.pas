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

unit SilSmGxControl;

interface

uses
  Sil, SilOtPerformance,

  Windows, Classes, Controls, Graphics, SysUtils, Messages,

  SilViControls, SilVkControl, SilVm3dShape,

  SilSmGxCanvas, SilSmGxEntity, SilSmGxEntities, SilSiGx;

const
  kPerfInterval_us = 1e6;

type
  TGxControl = class
  (
    TSilControl,
    IGxEntityEvents
  )
  private
    FEntitiesValid: boolean;
    FEntities: IGxEntityBuilder;
    FCanvas: IGxCanvas;
    FViewActive: boolean;
    FView: IGxView2D;
    FUseMouseWheel: boolean;
  private
    FInvalidatedRect: TGxRect;
    FEntitiesUpdating: boolean;
    FInvalidateCount: integer;
  private { Perfomance Checking variables }
    FPerfomanceFrecuency: LargeInt;
    FPerfStart: LargeInt;
    FPaintCount: integer;
    FFPS: integer;
    FPaintTimeAcum: single;
    FEventCount: integer;
  private
    FUpdateCount: integer;
    FMustInvalidate: boolean;
    FMustCheckUpdating: boolean;
  private
    FFocusedEntity: IGxEntity;
  private
    FCursorPos: TPoint;
    procedure WMLButtonUpx(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  private
    FOnFocusChanged: TNotifyEvent;
  private
    function GetView: IGxView2D;
    procedure DoCheckEntitiesUpdating;
    procedure DoEvaluateEntitiesUpdating(Sender: TObject);
    procedure DoSetEntitiesUpdating(Value: boolean);
    function GetCanvas: IGxCanvas;
    procedure DoCheckView;
    function GetPaintTime: integer;
    procedure SetFocusedEntity(const Value: IGxEntity);
  protected
    procedure DoUpdateEntities;
    procedure DoMouseEvent(MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure InvalidateEntity( const Sender: IGxEntity );
  protected
    procedure DoPaint(const Control: ISilControl; Canvas: TCanvas); override;
  protected
    procedure UpdateEntities; virtual;
    procedure EntitiesChanged;
    function GetEntities: IGxEntityListEdit;
  protected
    property EntityBuilder: IGxEntityBuilder read FEntities;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
  protected // IGxEntityEvents
    procedure OnBeginEvents( const Sender: IGxEntity );
    procedure OnEndEvents( const Sender: IGxEntity );
    procedure OnShapeChanged( const Sender: IGxEntity );
    procedure OnPositionChanged( const Sender: IGxEntity );
    procedure OnVisibleChanged( const Sender: IGxEntity );
    procedure OnStateChanged( const Sender: IGxEntity );
    procedure OnEntityNew( const Sender: IGxEntity );
    procedure OnEntityRemove( const Sender: IGxEntity );
  public
    constructor Create( AOwner: TComponent ); reintroduce;
    destructor Destroy; override;
  public
    procedure PaintAt(const Canvas: IGxCanvas; Rect: TGxRect); virtual;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
    procedure Invalidate; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetTimeStampUs: LargeInt;
  public
    property Entities: IGxEntityListEdit read GetEntities;
    property FocusedEntity: IGxEntity read FFocusedEntity write SetFocusedEntity;
    property Canvas: IGxCanvas read GetCanvas;
    property View: IGxView2D read GetView;
    property InvalidateCount: integer read FInvalidateCount;
    property PaintCount: integer read FPaintCount;
    property FramesPerSec: integer read FFPS;
    property PaintTime: integer read GetPaintTime;
    property EventCount: integer read FEventCount;
  public
    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
  public
    property Color;
  end;

implementation

uses Types;

constructor TGxControl.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  Windows.QueryPerformanceFrequency( FPerfomanceFrecuency );

  FUseMouseWheel := true;
  DoUpdateEntities;
end;

destructor TGxControl.Destroy;
begin
  FEntitiesValid := true;
  if Assigned( FEntities ) then
    Sil.Sink.Disconnect( FEntities, self );
  FEntities := nil;
  FView := nil;
  FCanvas := nil;

  inherited;        
end;

procedure TGxControl.Invalidate;
begin
  inc( FInvalidateCount );
  if not FViewActive then
  begin
    FInvalidatedRect.Left := 0;
    FInvalidatedRect.Top := 0;
    FInvalidatedRect.Right := Width;
    FInvalidatedRect.Bottom := Height;
  end
  else
    FInvalidatedRect := FView.VisibleRect;

  inherited;
end;

procedure TGxControl.InvalidateEntity(const Sender: IGxEntity);
(*)var
  rect: TRect;
  lf, tp, rg, bt: double;(*)
begin
  Invalidate;
  {FInvalidatedRect := Sender.ScreenRect;
  inc( FInvalidateCount );
  inherited Invalidate;}

  (*)if FViewActive then
  begin
    if not FView.UntranslateXY( FInvalidatedRect.Left, FInvalidatedRect.Top, lf, tp ) or
      not FView.UntranslateXY( FInvalidatedRect.Right, FInvalidatedRect.Bottom, rg, bt ) then
      exit;
  end
  else
  begin
    lf := FInvalidatedRect.Left;
    tp := FInvalidatedRect.Top;
    rg := FInvalidatedRect.Right;
    bt := FInvalidatedRect.Bottom;
  end;

  rect.Left := Round( lf - 0.99 );
  rect.Top := Round( tp - 0.99 );
  rect.Right := Round( rg + 0.99 );
  rect.Bottom := Round( bt + 0.99 );
  InvalidateRect( WindowHandle, @rect, false );(*)
end;

procedure TGxControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if Assigned( FView ) then
  begin
    FView.Width := self.Width;
    FView.Height := self.Height;
  end;
end;

function TGxControl.GetView: IGxView2D;
begin
  DoCheckView;
  result := FView;
end;

function TGxControl.GetTimeStampUs: LargeInt;
begin
  Windows.QueryPerformanceCounter( result );
  result := Round( 1e6 * result / FPerfomanceFrecuency );
end;

procedure TGxControl.DoPaint(const Control: ISilControl; Canvas: TCanvas);
var
  rect: TGxRect;
  t1, dif, dif2: LargeInt;
  cv: IGxCanvas;
const
  k_us_to_datetime_percent: double = 100 / 86400 / 1e6;
begin
  { Inicio de control de perfomance }
  t1 := GetTimeStampUs;
  dif := t1 - FPerfStart;
  if ( dif >= kPerfInterval_us ) then
  begin
    if ( FPerfStart <> 0 ) then
      FFPS := Round( 1e6 * FPaintCount / dif ) {else
      dif := 0};

    FPerfStart := t1;
    FPaintCount := 0;
    FPaintTimeAcum := 0;
  end;

  inherited;

  if not Assigned( FCanvas ) or ( FCanvas.VclCanvas <> Canvas ) then
  begin
    FCanvas := TGxVclCanvas.Create( Canvas );
    DoCheckView;
    FView.Canvas := FCanvas;
  end;

  rect := FInvalidatedRect;

  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := self.Color;
  FCanvas.Pen.Style := psClear;
  FCanvas.FillRect( 0, 0, Width, Height );{}
  //FCanvas.FillRect( rect );

  if FViewActive then
    cv := FView.Canvas else
    cv := FCanvas;

  PaintAt( cv, rect );

  { Fin de control de perfomance }
  dif2 := GetTimeStampUs - t1;
  inc( FPaintCount );
  //inc( dif, dif2 );
  FPaintTimeAcum := FPaintTimeAcum + dif2;

  {with cv.VclCanvas do
  begin
    Font.Color := clNavy;
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    if ( FPaintCount > 1 ) then
      TextOut( 0, 0, Format( 'PaintTime [ms]=%d, FPS=%d, CPU=%d%%',
        [ GetPaintTime, FFPS, Round( 100 * FPaintTimeAcum / dif ) ] ) ) else
      TextOut( 0, 0, Format( 'PaintTime [ms]=%d', [ GetPaintTime ] ) );
  end;{}
end;

function TGxControl.GetPaintTime: integer;
begin
  if ( FPaintCount > 0 ) then
    result := Round( FPaintTimeAcum / 1000 / FPaintCount ) else
    result := 0;  
end;

procedure TGxControl.PaintAt(const Canvas: IGxCanvas; Rect: TGxRect);
begin
  if not Assigned( FEntities ) or not FEntitiesValid then
    DoUpdateEntities;

  if Assigned( FEntities ) then
    FEntities.PaintAt( Canvas, 0, 0, Rect );
end;

procedure TGxControl.DoCheckView;
begin
  if not Assigned( FView ) then
  begin
    FView := TGxView2D.Create( FCanvas );
    FView.Width := self.Width;
    FView.Height := self.Height;
    FView.Scale := 1;
    FViewActive := true;
  end;
end;

procedure TGxControl.DoUpdateEntities;
begin
  if not Assigned( FEntities ) then
  begin
    FEntities := TGxEntityBuilder.Create;
    Sil.Sink.Connect( FEntities, self, false );
  end
  else
    FEntities.Clear;

  UpdateEntities;

  FEntitiesValid := true;
end;

procedure TGxControl.UpdateEntities;
begin

end;

procedure TGxControl.EntitiesChanged;
begin
  FEntitiesValid := false;
  Invalidate;
end;

function TGxControl.GetEntities: IGxEntityListEdit;
begin
  result := FEntities;
end;

procedure TGxControl.OnBeginEvents( const Sender: IGxEntity );
begin
  BeginUpdate;
end;

procedure TGxControl.OnEndEvents( const Sender: IGxEntity );
begin
  EndUpdate;
end;

procedure TGxControl.BeginUpdate;
begin
  inc( FUpdateCount );
end;

procedure TGxControl.EndUpdate;
begin
  dec( FUpdateCount );
  if ( FUpdateCount = 0 ) then
  begin
    if FMustInvalidate then
      Invalidate;
    FMustInvalidate := false;
                              
    if FMustCheckUpdating then
      DoCheckEntitiesUpdating;
    FMustCheckUpdating := false;
  end;
end;

procedure TGxControl.OnPositionChanged(const Sender: IGxEntity);
begin
  if ( FUpdateCount = 0 ) then
    InvalidateEntity( Sender ) else
    FMustInvalidate := true;
end;

procedure TGxControl.OnShapeChanged(const Sender: IGxEntity);
begin
  if ( FUpdateCount = 0 ) then
    InvalidateEntity( Sender ) else
    FMustInvalidate := true;
end;

procedure TGxControl.OnStateChanged(const Sender: IGxEntity);
var
  prevfocus: IGxEntity;
begin
  if ( FUpdateCount = 0 ) then
  begin
    InvalidateEntity( Sender );
    DoCheckEntitiesUpdating;
  end
  else
    FMustCheckUpdating := true;

  if Assigned( Sender ) then
  begin
    if ( esFocused in Sender.State ) then
    begin
      if not Sil.Ref.SameObject( Sender, FFocusedEntity ) then
      begin
        prevfocus := FFocusedEntity;

        FFocusedEntity := Sender;

        if Assigned( prevfocus ) then
          prevfocus.ClearState( esFocused );

        if Assigned( FOnFocusChanged ) then
          FOnFocusChanged( self );
      end;
    end
    else if Sil.Ref.SameObject( Sender, FFocusedEntity ) then
    begin
      FFocusedEntity := nil;
      if Assigned( FOnFocusChanged ) then
        FOnFocusChanged( self );
    end;
  end;
end;

procedure TGxControl.OnEntityNew(const Sender: IGxEntity);
begin
  if ( FUpdateCount = 0 ) then
    InvalidateEntity( Sender ) else
    FMustInvalidate := true;
end;

procedure TGxControl.OnEntityRemove(const Sender: IGxEntity);
begin
  if ( FUpdateCount = 0 ) then
    InvalidateEntity( Sender ) else
    FMustInvalidate := true;
end;

procedure TGxControl.OnVisibleChanged(const Sender: IGxEntity);
begin
  if ( FUpdateCount = 0 ) then
    InvalidateEntity( Sender ) else
    FMustInvalidate := true;
end;

procedure TGxControl.DoCheckEntitiesUpdating;
var
  updating: boolean;
  enum: IEnumerator;
  entity: IGxEntity;
begin
  updating := false;
  while not updating and FEntities.Enumerate( enum, entity ) do
    updating := ( esUpdating in entity.State );

  DoSetEntitiesUpdating( updating );
end;

procedure TGxControl.DoEvaluateEntitiesUpdating(Sender: TObject);
var
  enum: IEnumerator;
  entity: IGxEntity;
  any: boolean;
begin
  any := false;
  while FEntities.Enumerate( enum, entity ) do
    if ( esUpdating in entity.State ) then
    begin
      entity.Evaluate;
      any := true;
    end;

  if not any then
    DoSetEntitiesUpdating( false );
end;

procedure TGxControl.DoSetEntitiesUpdating( Value: boolean );
begin
  if ( FEntitiesUpdating <> Value ) then
  begin
    if Value then
      self.AddEvent( DoEvaluateEntitiesUpdating, 50, 0 ) else
      self.RemoveEvent( DoEvaluateEntitiesUpdating );

    FEntitiesUpdating := Value;
  end;
end;

procedure TGxControl.WMLButtonUpx(var Message: TWMLButtonUp);
begin
  FCursorPos.X := Message.XPos;
  FCursorPos.Y := Message.YPos;
  inherited;
end;

procedure TGxControl.Click;
begin
  inherited;
  DoMouseEvent( meClick, mbLeft, [], FCursorPos.X, FCursorPos.Y );
end;

procedure TGxControl.DblClick;
begin
  inherited;
  DoMouseEvent( meDblClick, mbLeft, [], FCursorPos.X, FCursorPos.Y );
end;

procedure TGxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  DoMouseEvent( meDown, Button, Shift, X, Y );
end;

procedure TGxControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  DoMouseEvent( meMove, mbLeft, Shift, X, Y );
end;

procedure TGxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  DoMouseEvent( meUp, Button, Shift, X, Y );
end;

procedure TGxControl.DoMouseEvent(MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i1: integer;
  entity: IGxEntity;
  stop: boolean;
  xx, yy: double;
  canvas: IGxCanvas;
begin
  inc( FEventCount );

  if FViewActive then
  begin
    canvas := FView.Canvas;
    if not FView.UntranslateXY( X, Y, xx, yy ) then
      exit;
  end
  else
  begin
    canvas := FCanvas;
    xx := x;
    yy := y;
  end;

  stop := false;
  if Assigned( canvas ) then
  begin
    i1 := FEntities.Count - 1;
    while not stop and ( i1 >= 0 ) do
    begin
      entity := FEntities.Entity[ i1 ];
      if ( MouseEvent in entity.MouseFilter ) then
        entity.EvalMouseEvent( canvas, MouseEvent, Button, Shift, xx, yy, stop );

      dec( i1 );
    end;
  end;
end;

function TGxControl.GetCanvas: IGxCanvas;
begin
  if FViewActive then
    result := FView.Canvas else
    result := FCanvas;
end;

function TGxControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  px, py, scale: double;
begin
  result := FUseMouseWheel;

  if FUseMouseWheel then
  begin
    { Desplazamiento Horizontal }
    if ssShift in Shift then
    begin
      FView.PanX := FView.PanX - WheelDelta / FView.Scale / 5;
      Invalidate;
    end
    { Zoom }
    else if ssCtrl in Shift then
    begin
      if ( WheelDelta > 0 ) then
        scale := FView.Scale * 1.1 else
        scale := FView.Scale / 1.1;

      if ( 0.1 <= scale ) and ( scale <= 100 ) then
      begin
        MousePos := ScreenToClient( MousePos );
        px := MousePos.X / FView.Scale + FView.PanX;
        py := MousePos.Y / FView.Scale + FView.PanY;
        {px := FView.FocusX;
        py := FView.FocusY;}
        FView.Scale := scale;
        {FView.FocusX := px;
        FView.FocusY := py;}
        FView.PanX := px - MousePos.X / scale;
        FView.PanY := py - MousePos.Y / scale;
        Invalidate;
      end;
    end
    { Desplazamiento Vertical }
    else
    begin
      FView.PanY := FView.PanY - WheelDelta / FView.Scale / 5;
      Invalidate;
    end;
  end;
end;

procedure TGxControl.SetFocusedEntity(const Value: IGxEntity);
begin
  if not Sil.Ref.SameObject( FFocusedEntity, Value ) then
  begin
    if Assigned( Value ) then
      Value.SetState( esFocused )
    else if Assigned( FFocusedEntity ) then
      FFocusedEntity.ClearState( esFocused );
  end;
end;

end.

