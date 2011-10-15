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

unit SilSiGx;

interface

uses
  Sil,
  Windows, Classes, Graphics, Controls;

type
  TMouseButton = Controls.TMouseButton;
  TShiftState = Classes.TShiftState;
  
type
  IGxPen = interface;
  IGxBrush = interface;
  IGxFont = interface;
  IGxGraphic = interface;
  IGxCanvas = interface;

  TGxPoint = packed record
    X: double;
    Y: double;
  end;

  TGxRect = record
    Left: double;
    Top: double;
    Right: double;
    Bottom: double;
  end;
  
  IGxCanvas = interface
    ['{F542FEBB-2D25-403C-B31E-9C62464860AC}']
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
    // properties
    property VclCanvas: TCanvas read GetVclCanvas;
    property Brush: TBrush read GetBrush;
    property CopyMode: TCopyMode read GetCopyMode;
    property Font: TFont read GetFont;
    property Pen: TPen read GetPen;
  end;

  IGxPen = interface
    ['{6EEC436C-7ACE-407B-9934-C9AB4CAEFE91}']
    function GetUseParentPen: boolean;
    function GetVclPen: TPen;
    procedure SetUseParentPen(Value: boolean);
    procedure SetVclPen(const Value: TPen);
    // properties
    property UseParentPen: boolean read GetUseParentPen write SetUseParentPen;
    property VclPen: TPen read GetVclPen write SetVclPen;
  end;

  IGxBrush = interface
    ['{12949427-9F30-48C2-8904-C9DA88A669FE}']
    function GetUseParentBrush: boolean;
    function GetVclBrush: TBrush;
    procedure SetUseParentBrush(Value: boolean);
    procedure SetVclBrush(const Value: TBrush);
    // properties
    property UseParentBrush: boolean read GetUseParentBrush write SetUseParentBrush;
    property VclBrush: TBrush read GetVclBrush write SetVclBrush;
  end;

  IGxFont = interface
    ['{F05FD852-8EED-4BA7-A815-72D5B2AECBA5}']
    function GetUseParentFont: boolean;
    function GetVclFont: TFont;
    procedure SetUseParentFont(Value: boolean);
    procedure SetVclFont(const Value: TFont);
    // properties
    property UseParentFont: boolean read GetUseParentFont write SetUseParentFont;
    property VclFont: TFont read GetVclFont write SetVclFont;
  end;

  IGxGraphic = interface
    ['{C35111A8-E134-4844-B547-30EF1E845AF3}']
    function GetVclGraphic: TGraphic;
    procedure SetVclGraphic(Value: TGraphic);
    function GetVclStretched( Width, Height: integer; out Bitmap: TBitmap ): boolean;
    // properties
    property VclGraphic: TGraphic read GetVclGraphic write SetVclGraphic;
  end;

type
  IGxView = interface;
  IGxView2D = interface;

  IGxView = interface
    ['{C0ABBF2A-9D5A-4A15-893C-23EFD71E9411}']
    function TranslateXY( X1, Y1: double; out X2, Y2: double ): boolean;
    function UntranslateXY( X2, Y2: double; out X1, Y1: double ): boolean;
    function GetCanvas: IGxCanvas;
    procedure SetCanvas(const Canvas: IGxCanvas);
    // properties
    property Canvas: IGxCanvas read GetCanvas write SetCanvas;
  end;

  IGxView2D = interface ( IGxView )
    ['{60273CAB-6EEB-4A85-B8F2-3EC179699BD9}']
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
    // properties
    property Scale: double read GetScale write SetScale;
    property Width: double read GetWidth write SetWidth;
    property Height: double read GetHeight write SetHeight;
    property FocusX: double read GetFocusX write SetFocusX;
    property FocusY: double read GetFocusY write SetFocusY;
    property PanX: double read GetPanX write SetPanX;
    property PanY: double read GetPanY write SetPanY;
    property VisibleRect: TGxRect read GetVisibleRect;
  end;

const
  ueShapeChanged = 1;
  uePositionChanged = 2;
  ueVisibleChanged = 3;
  ueStateChanged = 4;
  ueCustom = $100;

type
  IGxEntity = interface;
  IGxEntityList = interface;
  IGxEntityListEdit = interface;

  TMouseEvent = ( meEnter, meMove, meDown, meUp, meClick, meDblClick, meLeave );
  TMouseEvents = set of TMouseEvent;
  TPointReference = ( prUnknown, prOutside, prNear, prBorder, prInside );

  TGxEntityState =
  (
    esUpdating,
    esFocused,
    esSelected,
    esHighlighted
  );
  TGxEntityStates = set of TGxEntityState;
  
  TGxUpdateEventMode = ( uemKeepAll, uemKeepFirst, uemKeepLast );

  IGxUpdateEvent = interface
    ['{8BBDCFFF-FAE5-40A3-BE4B-E76645D51648}']
    function GetCode: integer;
    function GetEntity: IGxEntity;
    // properties
    property Code: integer read GetCode;
    property Entity: IGxEntity read GetEntity;
  end;

  IGxEntity = interface
    ['{DDA0FA1E-2B32-4427-BA24-3BC638B7B976}']
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
    procedure SetMouseFilter(Value: TMouseEvents);
    procedure SetState(Value: TGxEntityState);
    procedure SetVisible(Value: boolean);
    procedure SetLeft(Value: double);
    procedure SetTop(Value: double);
    procedure SetHeight(Value: double);
    procedure SetWidth(Value: double);
    procedure PaintAt(const Canvas: IGxCanvas; OffsetX, OffsetY: double; ClipRect: TGxRect);
    procedure EvalMouseEvent( const Canvas: IGxCanvas; MouseEvent: TMouseEvent; Button: TMouseButton; Shift: TShiftState; X, Y: double; out Stop: boolean );
    procedure Evaluate;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearState( State: TGxEntityState );
    // properties
    property State: TGxEntityStates read GetState;
    property MouseFilter: TMouseEvents read GetMouseFilter write SetMouseFilter;
    property Visible: boolean read GetVisible write SetVisible;
    property Left: double read GetLeft write SetLeft;
    property Top: double read GetTop write SetTop;
    property Height: double read GetHeight write SetHeight;
    property Width: double read GetWidth write SetWidth;
    property Childs: IGxEntityList read GetChilds;
    property ChildsEdit: IGxEntityListEdit read GetChildsEdit;
    property PointReference[ X, Y, NearDelta: double ]: TPointReference read GetPointReference;
  end;

  IGxEntityEvents = interface
    ['{94C657E9-7666-4BAD-B847-C36D107A52B1}']
    procedure OnBeginEvents( const Sender: IGxEntity );
    procedure OnEndEvents( const Sender: IGxEntity );
    procedure OnShapeChanged( const Sender: IGxEntity );
    procedure OnPositionChanged( const Sender: IGxEntity );
    procedure OnVisibleChanged( const Sender: IGxEntity );
    procedure OnStateChanged( const Sender: IGxEntity );
    (*)procedure OnEntityNew( const Sender: IGxEntity );
    procedure OnEntityRemove( const Sender: IGxEntity );(*)
  end;

  IGxEntityList = interface ( IGxEntity )
    ['{8D706EB4-75BC-4096-9919-FF04ED8EAB9F}']
    function Enumerate( var Enum: IEnumerator; out Entity: IGxEntity ): boolean;
  end;

  IGxEntityListEdit = interface ( IGxEntityList )
    ['{F10D8899-96DF-4409-B749-0DBB4F68B510}']
    procedure Add( const Entity: IGxEntity; Order: integer = -1 );
    procedure Remove( const Entity: IGxEntity );
    procedure SetEntityOrder( const Entity: IGxEntity; Order: integer );
    procedure Clear;
    procedure Delete( Index: integer );
    function GetCount: integer;
    function IndexOf(const Entity: IGxEntity): integer;
    function GetEntity( Index: integer ): IGxEntity;
    function FindEntityAtPos( X, Y, NearDelta: double; out Entity: IGxEntity ): boolean;
    function GetEntityOrder( const Entity: IGxEntity ): integer;
    //
    property Count: integer read GetCount;
    property Entity[ Index: integer ]: IGxEntity read GetEntity;
    property EntityOrder[ const Entity: IGxEntity ]: integer read GetEntityOrder write SetEntityOrder;
  end;

type
  IGxEntityBuilder = interface;
  IGxDot = interface;
  IGxLine = interface;
  IGxRectangle = interface;
  IGxEllipse = interface;
  IGxText = interface;
  IGxImage = interface;

  IGxEntityBuilder = interface ( IGxEntityListEdit )
    ['{418C6D62-1209-45B3-9985-9E155B6BDCDA}']
    function NewDot: IGxDot;
    function NewLine( Width, Height: double ): IGxLine;
    function NewRectangle( Width, Height: double ): IGxRectangle;
    function NewEllipse: IGxEllipse;
    function NewCircle( Radious: double ): IGxEllipse;
    function NewImage( Width, Height: double; VclImage: TGraphic ): IGxImage;
    function NewBlock: IGxEntityList; 
  end;

  IGxDot = interface ( IGxEntity )
    ['{452DCBAF-4443-488E-9FAF-9491C000B0C8}']
    function GetPen: IGxPen;
    // properties
    property Pen: IGxPen read GetPen;
  end;

  IGxLine = interface ( IGxEntity )
    ['{C8BEF710-2B7D-4E0E-B135-5692750303E4}']
    function GetPen: IGxPen;
    // properties
    property Pen: IGxPen read GetPen;
  end;

  IGxRectangle = interface ( IGxLine )
    ['{E8D0B7C0-67F8-4EB1-AC45-955C759BEA4F}']
    function GetBrush: IGxBrush;
    // properties
    property Brush: IGxBrush read GetBrush;
  end;

  IGxEllipse = interface ( IGxRectangle )
    ['{D96E9EDC-3B8C-4CE0-B4B1-1B2520959A28}']
  end;

  IGxText = interface ( IGxEntity )
    ['{B1A340A9-E35C-42D6-8A06-91E8B5BCA174}']
    function GetText: string;
    function GetFont: IGxFont;
    procedure SetText(const Value: string);
    // properties
    property Text: string read GetText write SetText;
    property Font: IGxFont read GetFont;
  end;

  IGxImage = interface ( IGxRectangle )
    ['{F05EBFE3-7501-410A-8811-B9B79A32542A}']
    function GetGraphic: IGxGraphic;
    procedure SetGraphic(const Value: IGxGraphic);
    // properties
    property Graphic: IGxGraphic read GetGraphic write SetGraphic;
  end;

implementation

    { Quedaron afuera de Canvas }
    (*)
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap; const Source: TRect; Color: TColor);
    procedure CopyRect(const Dest: TRect; Canvas: TCanvas; const Source: TRect);
    procedure DrawFocusRect(const Rect: TRect);
    procedure Ellipse(const Rect: TRect); overload;
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);
    procedure FrameRect(const Rect: TRect);
    procedure PolyBezier(const Points: array of TPoint);
    procedure PolyBezierTo(const Points: array of TPoint);
    procedure Rectangle(const Rect: TRect); overload;

    function HandleAllocated: Boolean;
    procedure Lock;
    procedure Refresh;
    function TryLock: Boolean;
    procedure Unlock;
    property ClipRect: TRect read GetClipRect;
    property Handle: HDC read GetHandle write SetHandle;
    property LockCount: Integer read FLockCount;
    property CanvasOrientation: TCanvasOrientation read GetCanvasOrientation;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property TextFlags: Longint read FTextFlags write FTextFlags;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    (*)

end.

