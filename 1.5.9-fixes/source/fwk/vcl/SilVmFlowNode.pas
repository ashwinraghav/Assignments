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

unit SilVmFlowNode;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  Messages, Classes, Graphics, Controls,
  SilViControls,
  SilVmFlowThread,
  SilVkControl;

type
  TFlowActivateEvent = procedure ( Sender: TObject; out Value: integer ) of object;
  TFlowExceptionEvent = procedure ( Sender: TObject; Ex: Exception ) of object;

  TFlowNodeState = ( nsInactive, nsWorking, nsActive, nsError );
  TFlowOption = ( noAutoKind );
  TFlowOptions = set of TFlowOption;

  TChildsIndex = 0..1;

  TFlowNodeKind = ( nkDesition, nkThrow, nkEnd );
  TFlowThreadMode = ( tmNone, tmParent, tmSelf );

  TCustomFlowNode = class
  (
    TSilControl
  )
  private
    FState: TFlowNodeState;
    FThread: IFlowThread;
    FValue: integer;
    FParent: TCustomFlowNode;
    FChilds: IPointerList;
    FOnActivate: TFlowActivateEvent;
    FOnCompletion: TNotifyEvent;
    FOnException: TFlowExceptionEvent;
    FKind: TFlowNodeKind;
    FOptions: TFlowOptions;
    FThreadMode: TFlowThreadMode;
    FParentIndex: integer;
    FElevation: integer;
  private
    FImageSource: ISil3dImageSource;
  private
    procedure SetThreadMode(const Value: TFlowThreadMode);
    procedure SetState(const Value: TFlowNodeState);
    procedure SetThread(const Value: IFlowThread);
    procedure SetOnActivate(const Value: TFlowActivateEvent);
    procedure SetOnCompletion(const Value: TNotifyEvent);
    procedure SetOnException(const Value: TFlowExceptionEvent);
    procedure SetKind(const Value: TFlowNodeKind);
    procedure CheckKind;
    procedure AddChild(const Child: TCustomFlowNode);
    procedure CheckThread;
    function GetParentNode: TCustomFlowNode;
    procedure RemoveChild(const Child: TCustomFlowNode);
    procedure SetParentNode(const Value: TCustomFlowNode);
    procedure SetParentNodeIndex(const Value: integer);
    function GetParentNodeIndex: integer;
    function IndexOfChild(Child: TCustomFlowNode): integer;
    procedure CheckChilds;
    procedure DoChildsChanged;
    procedure MoveChild(Child: TCustomFlowNode; Index: integer);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function _StoreKind: Boolean;
    procedure SetOptions(const Value: TFlowOptions);
    procedure SetElevation(const Value: integer);
    procedure Check3d;
  private
    procedure DoActivate( Sender: TObject );
    procedure DoActivateOk(const Value: integer); overload;
    procedure DoActivateOk(const Sender: IUnknown; Param: Pointer); overload;
    procedure DoActivateError(Ex: Exception); overload;
    procedure DoActivateError(const Sender: IUnknown; Param: Pointer); overload;
  protected
    function DoGetCurrentColor: TColor; override;
    procedure DoPaint(const Control: ISilControl; Canvas: TCanvas); override;
  protected
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
  protected
    procedure DoDrawFigure( Canvas: TCanvas ); virtual;
    procedure DoPropagate; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  protected
    property Elevation: integer read FElevation write SetElevation;
    property Active: boolean read GetActive write SetActive;
    property State: TFlowNodeState read FState;
    property Value: integer read FValue;
    property Options: TFlowOptions read FOptions write SetOptions;
    property Kind: TFlowNodeKind read FKind write SetKind stored _StoreKind;
    property ParentNode: TCustomFlowNode read GetParentNode write SetParentNode;
    property ParentIndex: integer read GetParentNodeIndex write SetParentNodeIndex;
    property ThreadMode: TFlowThreadMode read FThreadMode write SetThreadMode;
    property Thread: IFlowThread read FThread write SetThread;
    property OnActivate: TFlowActivateEvent read FOnActivate write SetOnActivate;
    property OnCompletion: TNotifyEvent read FOnCompletion write SetOnCompletion;
    property OnException: TFlowExceptionEvent read FOnException write SetOnException;
  end;

  TSilFlowNode = class
  (
    TCustomFlowNode
  )
  published
    property Elevation;
    property Active;
    property State;
    property Options;
    property Kind;
    property ParentNode;
    property ParentIndex;
    property ThreadMode;
    property Thread;
    property OnActivate;
    property OnCompletion;
    property OnException;
  published
    property TextVertical;
    property Enabled;
  published
    property Image;
    property Highlight;
    property Color;
    property BackgroundColor;
    property AutoBackgroundColor;
    property OnMouseEnter;
    property OnMouseLeave;
  published
    property Anchors;
    property Align;
    property Caption;
    property DragCursor;
    property DragMode;
    property DockSite;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  SysUtils,
  SilVm3dShape;

{ TCustomFlowNode }

constructor TCustomFlowNode.Create(Owner: TComponent);
begin
  inherited;

  FOptions := [ noAutoKind ];
  FKind := nkEnd;
  FThreadMode := tmParent;
  FElevation := 15;

  Check3d;
end;

destructor TCustomFlowNode.Destroy;
begin
  Active := false;
  FChilds := nil;
  FThread := nil;
  inherited;
end;

function TCustomFlowNode.GetActive: boolean;
begin
  result := ( FState <> nsInactive );
end;

procedure TCustomFlowNode.SetElevation(const Value: integer);
begin
  if ( FElevation <> Value ) then
  begin
    FElevation := Value;
    Check3d;
    Invalidate;
  end;
end;

procedure TCustomFlowNode.SetActive(const Value: boolean);
begin
  if Value and ( FState = nsInactive ) then
  begin
    SetState( nsWorking );

    CheckThread;

    if Assigned( FThread ) then
      FThread.Activate( self, DoActivate ) else
      DoActivate( self );
  end
  else if not Value and ( FState > nsInactive ) then
  begin
    SetState( nsInactive );
    DoPropagate;
  end;
end;

procedure TCustomFlowNode.SetOnActivate(const Value: TFlowActivateEvent);
begin
  FOnActivate := Value;
end;

procedure TCustomFlowNode.SetOnCompletion(const Value: TNotifyEvent);
begin
  FOnCompletion := Value;
end;

procedure TCustomFlowNode.SetOnException(const Value: TFlowExceptionEvent);
begin
  FOnException := Value;
end;

procedure TCustomFlowNode.SetThread(const Value: IFlowThread);
begin
  FThread := Value;
end;

procedure TCustomFlowNode.CheckThread;
begin
  case FThreadMode of
  tmNone:
    FThread := nil;
  tmSelf:
    if not Assigned( FThread ) then
      FThread := FlowThread.Create;
  tmParent:
    if Assigned( FParent ) then
      FThread := FParent.Thread else
      FThread := nil;
  else
    FThread := nil;
  end;
end;

procedure TCustomFlowNode.Check3d;
begin
  if ( FElevation <> 0 ) then
  begin
    if not Assigned( FImageSource ) then
    begin
      FImageSource := ImageTool.Create3dShape;

      with FImageSource.Params do
      begin
        ResetLights;
        AddLight( -30, -30, 30 );
        AddLight( 0, 0, 60 );
      end;
    end;

    with FImageSource.Params do
    begin
      Elevation := FElevation;

      case FKind of
      nkThrow: Shape := ksRoundRect;
      nkDesition: Shape := ksRhombus;
      else Shape := ksEllipse;
      end;
    end;
  end
  else
    FImageSource := nil;
end;

procedure TCustomFlowNode.DoActivate( Sender: TObject );
var
  value: integer;
begin
  try
    value := 0;
    if Assigned( FOnActivate ) then
      FOnActivate( self, value );

    Sil.Os.Thread.SyncCall( DoActivateOk, Pointer( value ) );

  except on ex: Exception do
    Sil.Os.Thread.SyncCall( DoActivateError, ex );
  end;
end;

procedure TCustomFlowNode.DoActivateError(const Sender: IUnknown; Param: Pointer);
begin
  DoActivateError( Exception( Param ) );
end;

procedure TCustomFlowNode.DoActivateError( Ex: Exception );
begin
  SetState( nsError );

  if Assigned( Ex ) and Assigned( FOnException ) then
    try
      FOnException( self, Ex );
    except
    end;
end;

procedure TCustomFlowNode.DoActivateOk(const Sender: IUnknown; Param: Pointer);
begin
  DoActivateOk( integer( Param ) );
end;

procedure TCustomFlowNode.DoActivateOk( const Value: integer );
begin
  FValue := Value;
  SetState( nsActive );
  DoPropagate;

  if Assigned( FOnCompletion ) then
    try
      FOnCompletion( self );
    except
    end;
end;

procedure TCustomFlowNode.SetState(const Value: TFlowNodeState );
begin
  FState := Value;
  Invalidate;
end;

procedure TCustomFlowNode.DoPropagate;
var
  enum: IEnumerator;
  child: TCustomFlowNode;
begin
  if Assigned( FChilds ) then
    while FChilds.Enumerate( enum, child ) do
      begin
        if ( FState = nsActive ) then
          case FKind of
          nkThrow: child.Active := true;
          nkDesition: child.Active := ( enum.Iteration = Value );
          end
        else
          child.Active := false;
      end;
end;

procedure TCustomFlowNode.DoPaint(const Control: ISilControl; Canvas: TCanvas);
begin
  if Assigned( FImageSource ) then
    FImageSource.PaintTo( Height, Width, CurrentColor, BackgroundColor,
      false, Transparent, TransparentColor, Canvas, ClientRect ) else
      DoDrawFigure( Canvas );

  Canvas.Brush.Color := BackgroundColor;
  Canvas.Brush.Style := bsClear;
  CaptionProp.Draw( Canvas, ClientRect, clBlack, clWhite );
end;

function TCustomFlowNode.DoGetCurrentColor: TColor;
var
  col: TColor;
begin
  case FState of
  nsInactive: col := self.Color; //clBtnFace;
  nsWorking: col := clYellow;
  nsActive: col := clLime;
  nsError: col := clRed;
  else col := self.Color; //clBtnFace;
  end;

  result := CalcCurrentColor( col, Sil.Rgb.Brighter( col, 100 ) );
end;

procedure TCustomFlowNode.DoDrawFigure( Canvas: TCanvas );
var
  points: array [ 0..4 ] of TPoint;
begin
  Canvas.Brush.Color := BackgroundColor;
  Canvas.FillRect( ClientRect );

  Canvas.Brush.Color := self.CurrentColor;

  case FKind of
  nkThrow:
    Canvas.RoundRect( 0, 0, Width - 1, Height - 1, 10, 10 );

  nkDesition:
    begin
      points[ 0 ].X := 0;
      points[ 0 ].Y := Height div 2;
      points[ 1 ].X := Width div 2;
      points[ 1 ].Y := 0;
      points[ 2 ].X := 2 * points[ 1 ].X;
      points[ 2 ].Y := Height div 2;
      points[ 3 ].X := Width div 2;
      points[ 3 ].Y := 2 * points[ 0 ].Y;
      points[ 4 ].X := points[ 0 ].X;
      points[ 4 ].Y := points[ 0 ].Y;

      Canvas.Polygon( points );
    end;

  else
    Canvas.Ellipse( ClientRect );
  end;
end;

procedure TCustomFlowNode.Notification(Component: TComponent; Operation: TOperation);
var
  enum: IEnumerator;
  child: TCustomFlowNode;
  changed: boolean;
begin
  inherited;

  if ( Operation = opRemove ) then
  begin
    if ( FParent = Component ) then
      FParent := nil;

    changed := false;    
    if Assigned( FChilds ) then
      while FChilds.Enumerate( enum, child ) do
        if ( child = Component ) then
        begin
          FChilds.Remove( child );
          changed := true;
        end;

    if changed then
      DoChildsChanged;
  end;
end;

function TCustomFlowNode.GetParentNode: TCustomFlowNode;
begin
  result := FParent;
end;

procedure TCustomFlowNode.SetParentNode(const Value: TCustomFlowNode);
begin
  if ( FParent <> Value ) then
  begin
    FThread := nil;

    if Assigned( FParent ) then
      FParent.RemoveChild( self );

    FParent := Value;

    if Assigned( FParent ) then
    begin
      FParent.AddChild( self );
      FParent.MoveChild( self, FParentIndex );
    end;
  end;
end;

procedure TCustomFlowNode.AddChild(const Child: TCustomFlowNode);
begin
  if Assigned( Child ) then
  begin
    CheckChilds;
    FChilds.Add( Child );
    DoChildsChanged;
  end;
end;

procedure TCustomFlowNode.RemoveChild(const Child: TCustomFlowNode);
begin
  if Assigned( FChilds ) then
  begin
    FChilds.Remove( Child );
    DoChildsChanged;
  end;
end;

function TCustomFlowNode.IndexOfChild(Child: TCustomFlowNode): integer;
begin
  if Assigned( FChilds ) then
    result := FChilds.IndexOf( Child ) else
    result := 0;
end;

procedure TCustomFlowNode.CheckChilds;
begin
  if not Assigned( FChilds ) then
    FChilds := Sil.List.PointerList;
end;

procedure TCustomFlowNode.DoChildsChanged;
begin
  CheckKind;
  Invalidate;
end;

procedure TCustomFlowNode.SetOptions(const Value: TFlowOptions);
begin
  if ( FOptions <> Value ) then
  begin
    FOptions := Value;
    CheckKind;
  end;
end;

procedure TCustomFlowNode.CheckKind;
begin
  if ( noAutoKind in FOptions ) then
  begin
    if Assigned( FChilds ) then
      case FChilds.Count of
      0: FKind := nkEnd;
      1: FKind := nkThrow;
      else FKind := nkDesition;
      end
    else
      FKind := nkEnd;

    Check3d;
  end;
end;

function TCustomFlowNode._StoreKind: Boolean;
begin
  result := not ( noAutoKind in FOptions );
end;

procedure TCustomFlowNode.SetKind(const Value: TFlowNodeKind);
begin
  if ( FKind <> Value ) then
  begin
    FOptions := FOptions - [ noAutoKind ];
    FKind := Value;
    DoPropagate;
    Check3d;
    Invalidate;
  end;
end;

procedure TCustomFlowNode.SetThreadMode(const Value: TFlowThreadMode);
begin
  if ( FThreadMode <> Value ) then
  begin
    FThreadMode := Value;
    FThread := nil;
  end;
end;

function TCustomFlowNode.GetParentNodeIndex: integer;
begin
  if Assigned( FParent ) then
    result := FParent.IndexOfChild( self ) else
    result := 0;
end;

procedure TCustomFlowNode.SetParentNodeIndex(const Value: integer);
begin
  FParentIndex := Value;

  if Assigned( FParent ) then
    FParent.MoveChild( self, FParentIndex );
end;

procedure TCustomFlowNode.MoveChild(Child: TCustomFlowNode; Index: integer);
var
  idx: integer;
begin
  if Assigned( FChilds ) then
  begin
    idx := FChilds.IndexOf( Child );

    if ( Index > idx ) and ( Index = FChilds.Count - 1 ) then
    begin
      FChilds.Add( Child );
      FChilds.Delete( idx );
    end
    else if ( Index > idx ) and ( Index < FChilds.Count ) then
    begin
      FChilds.Insert( Index + 1, Child );
      FChilds.Delete( idx );
    end
    else if ( Index < idx ) and ( Index >= 0 ) then
    begin
      FChilds.Insert( Index, Child );
      FChilds.Delete( idx + 1 );
    end;
  end;
end;

end.

