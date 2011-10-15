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

unit SilSmGxEntity;

interface

uses
  Sil,
  Windows, Classes, Graphics, Controls,
  SilSiGx;

type
  TUpdateEvent = class
  (
    TSilObject,
    IGxUpdateEvent
  )
  private
    FCode: integer;
    FEntity: IGxEntity;
  protected // IGxUpdateEvent
    function GetCode: integer;
    function GetEntity: IGxEntity;
  public
    constructor Create(Code: integer; const Entity: IGxEntity); reintroduce;
  end;

type
  TGxEntity = class
  (
    TSilInterfacedObject,
    IGxEntity,
    IGxEntityList,
    IGxEntityListEdit,
    IGxEntityEvents
  )
  private
    FParent: Pointer; // IGxEntity
    FVisible: boolean;
    FState: TGxEntityStates;
    FLeft: double;
    FTop: double;
    FHeight: double;
    FWidth: double;
    FMouseFilter: TMouseEvents;
    FPixelHeight: double;
  private
    //FLastRect: TGxRect;
  private
    FChildsState: TGxEntityStates;
    FChildsVisible: boolean;
    FChildsMouseFilter: TMouseEvents;
    FChilds: Sil.IInterfaceList;
  private
    FUpdateCount: integer;
    FUpdateEvents: Sil.IInterfaceList;
  private
    FPen: IGxPen;
    FBrush: IGxBrush;
  private
    procedure DoCheckState;
  protected
    function DoGetPointReference(X, Y, NearDelta: double): TPointReference; virtual;
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); virtual;
    procedure DoEvaluate; virtual;
    procedure DoPixelHeightChanged; virtual;
    procedure DoMouseEvent( const Canvas: IGxCanvas; MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: double; out Stop: boolean ); virtual;
    procedure DoAddUpdateEvent( Code: integer; const Entity: IGxEntity; Mode: TGxUpdateEventMode ); overload;
    procedure DoAddUpdateEvent(const UpdateEvent: IGxUpdateEvent; Mode: TGxUpdateEventMode); overload;
    procedure DoFireUpdateEvents;
    procedure DoClearUpdateEvents;
    procedure DoFireUpdateEvent(const UpdateEvent: IGxUpdateEvent; out Fired: boolean); virtual;
    function GetUpdating: boolean;
    procedure DoAssign(const Source: TObject); virtual;
    function GetBrush: IGxBrush;
    function GetPen: IGxPen;
  protected
    procedure DoFireBeginEvents;
    procedure DoFireEndEvents;
    procedure FireShapeChanged( const Sender: IGxEntity );
    procedure FirePositionChanged( const Sender: IGxEntity );
    procedure FireVisibleChanged( const Sender: IGxEntity );
    procedure FireStateChanged( const Sender: IGxEntity );
  protected
    procedure DoChildEntityNew( const Sender: IGxEntity ); virtual;
    procedure DoChildEntityRemove( const Sender: IGxEntity ); virtual;
    procedure DoChildEntityMoved( const Sender: IGxEntity; Order: integer ); virtual;
  protected
    function DoCheckPen( var Pen: IGxPen ): IGxPen;
    function DoCheckBrush( var Brush: IGxBrush ): IGxBrush;
    function DoCheckFont( var Font: IGxFont ): IGxFont;
    procedure DoPaintChilds(const Canvas: IGxCanvas; X, Y: double; ClipRect: TGxRect);
    procedure DoEvaluateChilds;
    procedure DoSetState(Value: TGxEntityStates);
  protected // IGxEntity
    ///function GetParent: IGxEntity;
    function GetState: TGxEntityStates;
    function GetMouseFilter: TMouseEvents;
    function GetVisible: boolean;
    function GetLeft: double;
    function GetTop: double;
    function GetHeight: double;
    function GetWidth: double;
    function GetChilds: IGxEntityList;
    function GetChildsEdit: IGxEntityListEdit;
    function GetPointReference( X, Y, NearDelta: double ): TPointReference;
    ///function GetScreenRect: TGxRect;
    ///procedure SetParent(const Value: IGxEntity);
    procedure SetMouseFilter(Value: TMouseEvents);
    procedure EvalMouseEvent( const Canvas: IGxCanvas; MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: double; out Stop: boolean );
    procedure SetVisible(Value: boolean);
    procedure SetLeft(Value: double);
    procedure SetTop(Value: double);
    procedure SetHeight(Value: double);
    procedure SetWidth(Value: double);
    procedure MoveTo(X, Y: double);
    procedure PaintAt(const Canvas: IGxCanvas; OffsetX, OffsetY: double; ClipRect: TGxRect);
    procedure Evaluate;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearState( State: TGxEntityState );
    // properties
    ///property Parent: IGxEntity read GetParent write SetParent;
    ///property ScreenRect: TGxRect read GetScreenRect;
    property MouseFilter: TMouseEvents read GetMouseFilter write SetMouseFilter;
    property Visible: boolean read GetVisible write SetVisible;
    property Left: double read GetLeft write SetLeft;
    property Top: double read GetTop write SetTop;
    property Height: double read GetHeight write SetHeight;
    property Width: double read GetWidth write SetWidth;
    property Brush: IGxBrush read GetBrush;
    property Pen: IGxPen read GetPen;
    property Childs: IGxEntityList read GetChilds;
    property PointReference[ X, Y, NearDelta: double ]: TPointReference read GetPointReference;
  protected
    procedure SetState( Value: TGxEntityState );
    property State: TGxEntityStates read GetState;
  protected // IGxEntityListEdit
    procedure Add( const Entity: IGxEntity; Order: integer = -1 );
    procedure Remove( const Entity: IGxEntity );
    procedure SetEntityOrder( const Entity: IGxEntity; Order: integer );
    procedure Clear;
    procedure Delete( Index: integer );
    function IndexOf(const Entity: IGxEntity): integer;
    function GetEntity( Index: integer ): IGxEntity;
    function GetEntityOrder( const Entity: IGxEntity ): integer;
    function Enumerate( var Enum: IEnumerator; out Entity: IGxEntity ): boolean;
    function GetCount: integer;
    function FindEntityAtPos( X, Y, NearDelta: double; out Entity: IGxEntity ): boolean;
    // properties
    property EntityOrder[ const Entity: IGxEntity ]: integer read GetEntityOrder write SetEntityOrder;
    //
    function GetChildsVisible: boolean;
    procedure SetChildsVisible(Value: boolean);
    function IGxEntityList.GetVisible = GetChildsVisible;
    procedure IGxEntityList.SetVisible = SetChildsVisible;
  protected // IGxEntityEvents
    procedure OnBeginEvents( const Sender: IGxEntity );
    procedure OnEndEvents( const Sender: IGxEntity );
    procedure OnShapeChanged( const Sender: IGxEntity );
    procedure OnPositionChanged( const Sender: IGxEntity );
    procedure OnVisibleChanged( const Sender: IGxEntity );
    procedure OnStateChanged( const Sender: IGxEntity );
    procedure OnEntityNew( const Sender: IGxEntity );
    procedure OnEntityRemove(const Sender: IGxEntity);
  protected
    function GetRight: double;
    function GetBottom: double;
    //
    property Right: double read GetRight;
    property Bottom: double read GetBottom;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
  public
    property Updating: boolean read GetUpdating;
    property PixelHeight: double read FPixelHeight;
  end;

implementation

uses
  SilSmGxPen, SilSmGxCanvas;

{ TGxEntity }

constructor TGxEntity.Create;
begin
  inherited Create;
  FVisible := true;
  FState := [];
  FChildsState := [];
  FLeft := 0;
  FTop := 0;
  FMouseFilter := [];
  FChildsVisible := true;
  FChildsMouseFilter := [];
end;

destructor TGxEntity.Destroy;
begin
  Clear;
  FChilds := nil;
  inherited;
end;

{function TGxEntity.GetParent: IGxEntity;
begin
  result := IGxEntity( FParent );
end;}

{procedure TGxEntity.SetParent(const Value: IGxEntity);
var
  parent: IGxEntity;
begin
  parent := IGxEntity( FParent );
  if not Sil.Ref.SameObject( parent, Value ) then
  begin
    FParent := nil;
    if Assigned( parent ) then
      parent.Childs.Remove( self );

    FParent := Pointer( Value );

    if Assigned( FParent ) then
      IGxEntity( FParent ).Childs.Add( self );
  end;
end;}

{procedure TGxEntity.PaintAt(const Canvas: IGxCanvas; OffsetX, OffsetY: double );
begin
  if FVisible then
    DoPaintAt( Canvas, FLeft + OffsetX, FTop + OffsetY );

  if FChildsVisible then
    DoPaintChilds( Canvas, FLeft + OffsetX, FTop + OffsetY );
end;}

procedure TGxEntity.PaintAt(const Canvas: IGxCanvas; OffsetX, OffsetY: double; ClipRect: TGxRect);
var
  pixh: double;
begin
  if FVisible and
    ( ( FWidth > 0 ) or ( FHeight > 0 ) ) and
    ( ClipRect.Left <= OffsetX + FLeft + Width ) and
    ( OffsetX + FLeft <= ClipRect.Right ) and
    ( ClipRect.Top <= OffsetY + FTop + Height ) and
    ( OffsetY + FTop <= ClipRect.Bottom )  
  then
  begin
    if Assigned( FPen ) and not FPen.UseParentPen then
      Canvas.Pen.Assign( FPen.VclPen ) else
      Canvas.Pen.Style := psClear;

    if Assigned( FBrush ) and not FBrush.UseParentBrush then
      Canvas.Brush.Assign( FBrush.VclBrush );

    {FLastRect.Left := OffsetX + FLeft;
    FLastRect.Top := OffsetY + FTop;
    FLastRect.Right := OffsetX + FLeft + FWidth;
    FLastRect.Bottom := OffsetY + FTop + FHeight;}

    pixh := Canvas.GetPixelHeight( 1 );
    if ( pixh <> FPixelHeight ) then
    begin
      FPixelHeight := pixh;
      DoPixelHeightChanged;
    end;

    DoPaintAt( Canvas, FLeft + OffsetX, FTop + OffsetY );
  end;

  if FChildsVisible then
    DoPaintChilds( Canvas, FLeft + OffsetX, FTop + OffsetY, ClipRect );
end;

function TGxEntity.GetVisible: boolean;
begin
  result := FVisible;
end;

function TGxEntity.GetChildsVisible: boolean;
begin
  result := FChildsVisible;
end;

function TGxEntity.GetState: TGxEntityStates;
begin
  result := FState;
  if ( esUpdating in FChildsState ) then
    include( result, esUpdating );
end;

function TGxEntity.GetLeft: double;
begin
  result := FLeft;
end;

function TGxEntity.GetTop: double;
begin
  result := FTop;
end;

function TGxEntity.GetBottom: double;
begin
  result := FTop + FHeight;
end;

function TGxEntity.GetRight: double;
begin
  result := FLeft + FWidth;
end;

function TGxEntity.GetHeight: double;
begin
  result := FHeight;
end;

function TGxEntity.GetWidth: double;
begin
  result := FWidth;
end;

function TGxEntity.GetBrush: IGxBrush;
begin
  result := DoCheckBrush( FBrush ); 
end;

function TGxEntity.GetPen: IGxPen;
begin
  result := DoCheckPen( FPen );
end;

function TGxEntity.GetMouseFilter: TMouseEvents;
begin
  result := FMouseFilter + FChildsMouseFilter;
end;

function TGxEntity.GetChilds: IGxEntityList;
begin
  result := self;
end;

function TGxEntity.GetChildsEdit: IGxEntityListEdit;
begin
  result := self;
end;

procedure TGxEntity.MoveTo(X, Y: double);
begin
  if ( FLeft <> X ) or ( FTop <> Y ) then
  begin
    FLeft := X;
    FTop := Y;
    FirePositionChanged( self );
  end;
end;

function TGxEntity.DoCheckPen(var Pen: IGxPen): IGxPen;
begin
  if not Assigned( Pen ) then
    Pen := TGxPen.Create;

  result := Pen;
end;

function TGxEntity.DoCheckBrush(var Brush: IGxBrush): IGxBrush;
begin
  if not Assigned( Brush ) then
    Brush := TGxBrush.Create;

  result := Brush;
end;

function TGxEntity.DoCheckFont(var Font: IGxFont): IGxFont;
begin
  if not Assigned( Font ) then
    Font := TGxFont.Create;

  result := Font;
end;

procedure TGxEntity.SetVisible(Value: boolean);
begin
  if ( FVisible <> Value ) then
  begin
    FVisible := Value;
    FireVisibleChanged( self );
  end;
end;

procedure TGxEntity.SetChildsVisible(Value: boolean);
begin
  if ( FChildsVisible <> Value ) then
  begin
    FChildsVisible := Value;
    FireVisibleChanged( self );
  end;
end;

procedure TGxEntity.SetLeft(Value: double);
begin
  if ( FLeft <> Value ) then
  begin
    FLeft := Value;
    FirePositionChanged( self );
  end;
end;

procedure TGxEntity.SetTop(Value: double);
begin
  if ( FTop <> Value ) then
  begin
    FTop := Value;
    FirePositionChanged( self );
  end;
end;

procedure TGxEntity.SetHeight(Value: double);
begin
  if ( FHeight <> Value ) then
  begin
    FHeight := Value;
    FireShapeChanged( self );
  end;
end;

procedure TGxEntity.SetWidth(Value: double);
begin
  if ( FWidth <> Value ) then
  begin
    FWidth := Value;
    FireShapeChanged( self );
  end;
end;

procedure TGxEntity.SetState(Value: TGxEntityState);
begin
  if not ( Value in FState ) then
  begin
    Include( FState, Value );
    FireStateChanged( self );
  end;
end;

procedure TGxEntity.ClearState( State: TGxEntityState );
begin
  if ( State in FState ) then
  begin
    Exclude( FState, State );
    FireStateChanged( self );
  end;
end;

procedure TGxEntity.SetMouseFilter(Value: TMouseEvents);
begin
  FMouseFilter := Value;
end;

procedure TGxEntity.Evaluate;
begin
  DoEvaluate;
  DoEvaluateChilds;
end;

procedure TGxEntity.DoFireBeginEvents;
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnBeginEvents( self );
end;

procedure TGxEntity.DoFireEndEvents;
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnEndEvents( self );
end;

procedure TGxEntity.FirePositionChanged( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if ( FUpdateCount > 0 ) then
    DoAddUpdateEvent( uePositionChanged, Sender, uemKeepLast )
  else if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnPositionChanged( Sender );
end;

procedure TGxEntity.FireVisibleChanged( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if ( FUpdateCount > 0 ) then
    DoAddUpdateEvent( ueVisibleChanged, Sender, uemKeepLast )
  else if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnVisibleChanged( Sender );
end;

procedure TGxEntity.FireShapeChanged( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if ( FUpdateCount > 0 ) then
    DoAddUpdateEvent( ueShapeChanged, Sender, uemKeepLast )
  else if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnShapeChanged( Sender );
end;

procedure TGxEntity.FireStateChanged( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  if ( FUpdateCount > 0 ) then
    DoAddUpdateEvent( ueStateChanged, Sender, uemKeepLast )
  else if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnStateChanged( Sender );
end;

(*)procedure TGxEntity.FireChildEntityNew( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  DoChildEntityNew( Sender );

  if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnEntityNew( Sender );
end;

procedure TGxEntity.FireChildEntityRemove( const Sender: IGxEntity );
var
  enum: IEnumerator;
  sink: IGxEntityEvents;
begin
  DoChildEntityRemove( Sender );

  if Assigned( Events ) then
    while Events.Enumerate( enum, sink, IGxEntityEvents ) do
      sink.OnEntityRemove( Sender );
end;(*)

procedure TGxEntity.DoPaintChilds(const Canvas: IGxCanvas; X, Y: double; ClipRect: TGxRect);
var
  enum: IEnumerator;
  entity: IGxEntity;
begin
  if Assigned( FChilds ) then
    while FChilds.Enumerate( enum, entity ) do
      entity.PaintAt( Canvas, X, Y, ClipRect );
end;

procedure TGxEntity.DoEvaluateChilds;
var
  enum: IEnumerator;
  entity: IGxEntity;
begin
  if Assigned( FChilds ) then
    while FChilds.Enumerate( enum, entity ) do
      if ( esUpdating in entity.State ) then
        entity.Evaluate;
end;

procedure TGxEntity.DoSetState(Value: TGxEntityStates);
begin
  if ( FState <> Value ) then
  begin
    FState := Value;
    FireStateChanged( self );
  end;
end;

function TGxEntity.GetEntity( Index: integer ): IGxEntity;
begin
  if Assigned( FChilds ) and ( 0 <= Index ) and ( Index < FChilds.Count ) then
    result := FChilds[ Index ] as IGxEntity else
    result := nil;
end;

function TGxEntity.GetEntityOrder(const Entity: IGxEntity): integer;
begin
  if Assigned( FChilds ) then
    result := FChilds.IndexOf( Entity ) else
    result := -1;
end;

procedure TGxEntity.SetEntityOrder(const Entity: IGxEntity; Order: integer);
var
  i1: integer;
begin
  if Assigned( FChilds ) then
  begin
    i1 := FChilds.IndexOf( Entity );

    if ( i1 >= 0 ) then
    begin
      if ( Order < 0 ) then
        Order := 0
      else if ( Order >= FChilds.Count ) then
        Order := FChilds.Count - 1;

      if ( i1 <> Order ) then
      begin
        FChilds.Delete( i1 );
        if ( Order = FChilds.Count ) then
          FChilds.Add( Entity ) else
          FChilds.Insert( Order, Entity );

        DoChildEntityMoved( Entity, Order ); 
      end;
    end;
  end;
end;

procedure TGxEntity.Clear;
begin
  DoClearUpdateEvents;
  
  if Assigned( FChilds ) then
    while ( FChilds.Count > 0 ) do
      Remove( FChilds.Last as IGxEntity );
end;

procedure TGxEntity.Delete(Index: integer);
begin
  if Assigned( FChilds ) then
    Remove( FChilds[ Index ] as IGxEntity );
end;

function TGxEntity.IndexOf(const Entity: IGxEntity): integer;
begin
  if Assigned( FChilds ) then
    result := FChilds.IndexOf( Entity ) else
    result := -1;
end;

procedure TGxEntity.Add(const Entity: IGxEntity; Order: integer);
begin
  if Assigned( Entity ) then
  begin
    if not Assigned( FChilds ) then
      FChilds := Sil.List.InterfaceList;

    if ( FChilds.IndexOf( Entity ) = -1 ) then
    begin
      if ( Order = -1 ) or ( Order >= FChilds.Count ) then
        FChilds.Add( Entity ) else
        FChilds.Insert( Order, Entity );
        
      Sil.Sink.Connect( Entity, self, false );
      DoCheckState;
      DoChildEntityNew( Entity );(*)(*)
      //FireChildEntityNew( Entity );
    end;

    ///Entity.Parent := self;
  end;
end;

procedure TGxEntity.Remove(const Entity: IGxEntity);
begin
  if Assigned( Entity ) then
  begin
    if Assigned( FChilds ) and ( FChilds.IndexOf( Entity ) >= 0 ) then
    begin
      DoChildEntityRemove( Entity );(*)(*)
      //FireChildEntityRemove( Entity );
      Sil.Sink.Disconnect( Entity, self );
      FChilds.Remove( Entity );
      DoCheckState;
    end;

    ///Entity.Parent := nil;
  end;
end;

function TGxEntity.Enumerate(var Enum: IEnumerator; out Entity: IGxEntity): boolean;
begin
  if Assigned( FChilds ) then
    result := FChilds.Enumerate( Enum, Entity ) else
    result := false;
end;

function TGxEntity.FindEntityAtPos(X, Y, NearDelta: double; out Entity: IGxEntity): boolean;
var
  {enum: IEnumerator;}
  i1: integer;
begin
  result := false;
  if Assigned( FChilds ) then
  begin
    i1 := FChilds.Count - 1;
    while not result and ( i1 >= 0 ) {FChilds.Enumerate( enum, Entity )} do
    begin
      Entity := FChilds.Items[ i1 ] as IGxEntity;
      result := ( Entity.GetPointReference( X, Y, NearDelta ) in [ prNear, prBorder, prInside ] );
      dec( i1 );
    end;
  end;
end;

procedure TGxEntity.OnBeginEvents( const Sender: IGxEntity );
begin
end;

procedure TGxEntity.OnEndEvents( const Sender: IGxEntity );
begin
end;

procedure TGxEntity.OnPositionChanged(const Sender: IGxEntity);
begin
  FirePositionChanged( Sender );
end;

procedure TGxEntity.OnShapeChanged(const Sender: IGxEntity);
begin
  FireShapeChanged( Sender );
end;

procedure TGxEntity.OnStateChanged(const Sender: IGxEntity);
begin
  DoCheckState;
  FireStateChanged( Sender );
end;

procedure TGxEntity.OnEntityNew(const Sender: IGxEntity);
begin
  //FireChildEntityNew( Sender );
end;

procedure TGxEntity.OnEntityRemove(const Sender: IGxEntity);
begin
  //FireChildEntityRemove( Sender );
end;

procedure TGxEntity.OnVisibleChanged(const Sender: IGxEntity);
begin
  FireVisibleChanged( Sender );
end;

procedure TGxEntity.DoCheckState;
var
  enum: IEnumerator;
  entity: IGxEntity;
  prev: TGxEntityStates;
begin
  if Assigned( FChilds ) then
  begin
    prev := GetState;
    
    FChildsMouseFilter := [];
    FChildsState := [];
    while FChilds.Enumerate( enum, entity ) do
    begin
      FChildsMouseFilter := FChildsMouseFilter + entity.MouseFilter;
      FChildsState := FChildsState + entity.State;
    end;

    if ( prev <> GetState ) then
      FireStateChanged( self );
  end;
end;

function TGxEntity.GetCount: integer;
begin
  if Assigned( FChilds ) then
    result := FChilds.Count else
    result := 0;
end;

procedure TGxEntity.EvalMouseEvent( const Canvas: IGxCanvas; MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: double; out Stop: boolean );
var
  entity: IGxEntity;
  i1: integer;
begin
  Stop := false;

  if FChildsVisible and Assigned( FChilds ) then
  begin
    i1 := FChilds.Count - 1;
    while not Stop and ( i1 >= 0 ) do
    begin
      entity := FChilds.Items[ i1 ] as IGxEntity;
      if MouseEvent in entity.MouseFilter then
        entity.EvalMouseEvent( Canvas, MouseEvent, Button, Shift, X - Left, Y - Top, Stop );

      dec( i1 );
    end;
  end;

  if FVisible and not Stop and ( DoGetPointReference( X, Y, 0 ) in [ prBorder, prInside ] ) then
    DoMouseEvent( Canvas, MouseEvent, Button, Shift, X, Y, Stop );
end;

function TGxEntity.GetPointReference(X, Y, NearDelta: double): TPointReference;
begin
  result := DoGetPointReference( X, Y, NearDelta );
end;

{function TGxEntity.GetScreenRect: TGxRect;
var
  newrect: TGxRect;
begin
  result := FLastRect;
  if Assigned( FParent ) then
  begin
    newrect := IGxEntity( FParent ).ScreenRect;
    newrect.Left := result.Left + FLeft;
    newrect.Top := result.Top + FTop;
    newrect.Right := result.Left + FLeft + FWidth;
    newrect.Bottom := result.Top + FTop + FHeight;
  end
  else
  begin
    newrect.Left := FLeft;
    newrect.Top := FTop;
    newrect.Right := FLeft + FWidth;
    newrect.Bottom := FLeft + FHeight;
  end;

  if ( newrect.Left < result.Left ) then result.Left := newrect.Left;
  if ( newrect.Top < result.Top ) then result.Top := newrect.Top;
  if ( newrect.Right > result.Right ) then result.Right := newrect.Right;
  if ( newrect.Bottom > result.Bottom ) then result.Bottom := newrect.Bottom;
end;}

function TGxEntity.DoGetPointReference( X, Y, NearDelta: double): TPointReference;
begin
  if
    ( FLeft <= X ) and ( X <= FLeft + FWidth ) and
    ( FTop <= Y ) and ( Y <= FTop + FHeight ) then
  begin
    if ( ( X = FLeft ) or ( X = FLeft + FWidth ) ) and ( ( Y = FTop ) or ( Y = FTop + FHeight ) ) then
      result := prBorder else
      result := prInside;
  end
  else if ( NearDelta > 0 ) and
    ( FLeft - NearDelta <= X ) and ( X <= FLeft + FWidth + NearDelta ) and
    ( FTop - NearDelta <= Y ) and ( Y <= FTop + FHeight + NearDelta ) then
    result := prNear
  else
    result := prOutside;
end;

procedure TGxEntity.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  (*) IMPLEMENTADA EN DESCENDIENTES (*)
end;

procedure TGxEntity.DoEvaluate;
begin
  (*) IMPLEMENTADA EN DESCENDIENTES (*)
end;

procedure TGxEntity.DoMouseEvent( const Canvas: IGxCanvas; MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: double; out Stop: boolean );
begin
  if ( MouseEvent in [ meClick, meDblClick ] ) then
  begin
    if not ( esFocused in FState ) then
    begin
      include( FState, esFocused );
      FireStateChanged( self );
    end;
  end;
  Stop := true;
end;

procedure TGxEntity.DoPixelHeightChanged;
begin
  (*) IMPLEMENTADA EN DESCENDIENTES (*)
end;

procedure TGxEntity.DoChildEntityNew(const Sender: IGxEntity);
begin
  FireShapeChanged( self );
end;

procedure TGxEntity.DoChildEntityRemove(const Sender: IGxEntity);
begin
  FireShapeChanged( self );
end;

procedure TGxEntity.DoChildEntityMoved( const Sender: IGxEntity; Order: integer );
begin
  FireShapeChanged( self );
end;

procedure TGxEntity.BeginUpdate;
begin
  inc( FUpdateCount );
end;

procedure TGxEntity.EndUpdate;
begin
  dec( FUpdateCount );

  DoFireUpdateEvents;
end;

procedure TGxEntity.DoAddUpdateEvent(Code: integer; const Entity: IGxEntity; Mode: TGxUpdateEventMode);
begin
  DoAddUpdateEvent( TUpdateEvent.Create( Code, Entity ), Mode );
end;

procedure TGxEntity.DoAddUpdateEvent(const UpdateEvent: IGxUpdateEvent; Mode: TGxUpdateEventMode);

  function FindSimilar: integer;
  begin
    result := FUpdateEvents.Count - 1;
    while result >= 0 do
    begin
      with ( FUpdateEvents.Items[ result ] as IGxUpdateEvent ) do
        if ( Code = UpdateEvent.Code ) and (
          not Assigned( Entity ) and not Assigned( UpdateEvent.Entity ) or
          Sil.Ref.SameObject( Entity, UpdateEvent.Entity ) ) then
            break;

      dec( result );
    end;
  end;

var
  index: integer;
begin
  if not Assigned( FUpdateEvents ) then
    FUpdateEvents := Sil.List.InterfaceList( true );

  case Mode of
  uemKeepAll: FUpdateEvents.Add( UpdateEvent );
  uemKeepFirst:
    begin
      if ( FindSimilar = -1 ) then
        FUpdateEvents.Add( UpdateEvent );
    end;
  uemKeepLast:
    begin
      index := FindSimilar;
      if ( index >= 0 ) then FUpdateEvents.Delete( index );
      FUpdateEvents.Add( UpdateEvent );
    end;
  end;
end;

procedure TGxEntity.DoClearUpdateEvents;
begin
  if Assigned( FUpdateEvents ) then
    FUpdateEvents.Clear;
end;

procedure TGxEntity.DoFireUpdateEvents;
var
  fired, multiple: boolean;
begin
  if ( FUpdateCount = 0 ) and Assigned( FUpdateEvents ) then
  begin
    multiple := ( FUpdateEvents.Count > 1 );
    if multiple then DoFireBeginEvents;
    
    while ( FUpdateEvents.Count > 0 ) do
    begin
      DoFireUpdateEvent( FUpdateEvents.Items[ 0 ] as IGxUpdateEvent, fired );
      FUpdateEvents.Delete( 0 );
    end;

    if multiple then DoFireEndEvents;
  end;
end;

procedure TGxEntity.DoFireUpdateEvent( const UpdateEvent: IGxUpdateEvent; out Fired: boolean );
begin
  if not Fired then
  begin
    Fired := true;
    case UpdateEvent.Code of
    ueShapeChanged: FireShapeChanged( UpdateEvent.Entity );
    uePositionChanged: FirePositionChanged( UpdateEvent.Entity );
    ueVisibleChanged: FireVisibleChanged( UpdateEvent.Entity );
    ueStateChanged: FireStateChanged( UpdateEvent.Entity );
    else Fired := false;
    end;
  end;
end;

function TGxEntity.GetUpdating: boolean;
begin
  result := ( FUpdateCount > 0 );
end;

procedure TGxEntity.DoAssign(const Source: TObject);
begin
  if ( TObject( Source ) is TGxEntity ) then
  begin
    FParent := TGxEntity( Source ).FParent;
    FVisible := TGxEntity( Source ).FVisible;
    FState := TGxEntity( Source ).FState;
    FLeft := TGxEntity( Source ).FLeft;
    FTop := TGxEntity( Source ).FTop;
    FHeight := TGxEntity( Source ).FHeight;
    FWidth := TGxEntity( Source ).FWidth;
    FMouseFilter := TGxEntity( Source ).FMouseFilter;
    FPixelHeight := TGxEntity( Source ).FPixelHeight;
    // FLastRect
    FChildsVisible := TGxEntity( Source ).FChildsVisible;
    FChildsMouseFilter := TGxEntity( Source ).FChildsMouseFilter;
    //FChilds
    //FUpdateCount: integer;
    //FUpdateEvents: Sil.IInterfaceList;
  end;
end;

{ TUpdateEvent }

constructor TUpdateEvent.Create(Code: integer; const Entity: IGxEntity);
begin
  inherited Create;
  FCode := Code;
  FEntity := Entity;
end;

function TUpdateEvent.GetCode: integer;
begin
  result := FCode;
end;

function TUpdateEvent.GetEntity: IGxEntity;
begin
  result := FEntity;
end;

end.

