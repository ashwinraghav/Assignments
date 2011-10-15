{------------------------------------------------------------------------------}
{                                                                              }
{  TGradient v2.5                                                              }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit SilVmGradientPanel;

{$I Defines.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Menus,

  SilVmCustomPanel, SilViControls;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;

  TGradientColors = array[0..255] of TRGBTriple;

  TGradientShift = -100..100;
  TGradientRotation = -100..100;

  TGradientStyle = (gsCustom, gsRadialC, gsRadialT, gsRadialB, gsRadialL,
    gsRadialR, gsRadialTL, gsRadialTR, gsRadialBL, gsRadialBR, gsLinearH,
    gsLinearV, gsReflectedH, gsReflectedV, gsDiagonalLF, gsDiagonalLB,
    gsDiagonalRF, gsDiagonalRB, gsArrowL, gsArrowR, gsArrowU, gsArrowD,
    gsDiamond, gsButterfly);

  TCustomGradientEvent = procedure(Sender: TObject; const Colors: TGradientColors;
    Pattern: TBitmap) of object;

  TSilGradientPanel = class
  (
    TSilCustomPanel
  )
  private
    fColorBegin: TColor;
    fColorEnd: TColor;
    fStyle: TGradientStyle;
    fShift: TGradientShift;
    fRotation: TGradientRotation;
    fReverse: Boolean;
    fPattern: TBitmap;
    fOnCustom: TCustomGradientEvent;
    FUpdateCount: Integer;
    FUpdatePending: Boolean;
    FDirty: Boolean;
  private
    procedure SetColorBegin(Value: TColor);
    procedure SetColorEnd(Value: TColor);
    procedure SetStyle(Value: TGradientStyle);
    procedure SetShift(Value: TGradientShift);
    procedure SetRotation(Value: TGradientRotation);
    procedure SetReverse(Value: Boolean);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure GroupIndexChanged; override;
    procedure GroupChanged; override;
    procedure UpdatePattern; virtual;
    property Pattern: TBitmap read fPattern;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidatePattern;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Anchors;
    property ColorBegin: TColor read fColorBegin write SetColorBegin default clWhite;
    property ColorEnd: TColor read fColorEnd write SetColorEnd default clBtnFace;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property GroupIndex;
    property Height default 100;
    property ParentShowHint;
    property PopupMenu;
    property Reverse: Boolean read fReverse write SetReverse default False;
    property Rotation: TGradientRotation read fRotation write SetRotation default 0;
    property Shift: TGradientShift read fShift write SetShift default 0;
    property ShowHint;
    property Style: TGradientStyle read fStyle write SetStyle default gsRadialC;
    property Visible;
    property Width default 100;
    property OnClick;
    property OnCustom: TCustomGradientEvent read fOnCustom write fOnCustom;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  published
    property Alignment;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    {$IFDEF D60}
    property ParentBackground;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnResize;
    property OnUnDock;
  end;

implementation

uses
  Types;

type
  RBuildParams = record
    Pattern: TBitmap;
    Group: ISilGroupingProperty;
    Translate: boolean;
    OwnerLeft: integer;
    OwnerTop: integer;
    OwnerHeight: integer;
    OwnerWidth: integer;
    OffsetX: integer;
    ScaleX1: double;
    ScaleX2: double;
    OffsetY: integer;
    ScaleY1: double;
    ScaleY2: double;
    Colors: TGradientColors;
  end;

function TranslateX(const Params: RBuildParams; const X1: integer): integer;
begin
  result := Round( ( Params.OffsetX + X1 * Params.ScaleX2 ) * Params.ScaleX1 );
  if ( result >= Params.Pattern.Width ) then result := Params.Pattern.Width - 1;
  if ( result < 0 ) then result := 0;
end;

function TranslateY(const Params: RBuildParams; const Y1: integer): integer;
begin
  result := Round( ( Params.OffsetY + Y1 * Params.ScaleY2 ) * Params.ScaleY1 );
  if ( result >= Params.Pattern.Height ) then result := Params.Pattern.Height - 1;
  if ( result < 0 ) then result := 0;
end;

procedure Translate(const Params: RBuildParams; const X1, Y1: integer; out X2, Y2: integer);
begin
  if Params.Translate then
  begin
    X2 := TranslateX( Params, X1 );
    Y2 := TranslateY( Params, Y1 );
  end
  else
  begin
    X2 := X1;
    Y2 := Y1;
  end;
end;

procedure DimPattern(var Params: RBuildParams; const Width, Height: integer);
begin
  Params.Pattern.Width := Width;
  Params.Pattern.Height := Height;

  if Assigned( Params.Group ) then
  begin
    Params.Translate := true;
    Params.ScaleX1 := Width / ( Params.Group.Rect.Right - Params.Group.Rect.Left + 1 );
    Params.ScaleX2 := Params.OwnerWidth / Width;
    Params.OffsetX := Params.OwnerLeft - Params.Group.Rect.Left;
    Params.ScaleY1 := Height / ( Params.Group.Rect.Bottom - Params.Group.Rect.Top + 1 );
    Params.ScaleY2 := Params.OwnerHeight / Height;
    Params.OffsetY := Params.OwnerTop - Params.Group.Rect.Top;
  end
  else
    Params.Translate := false;
end;

function Sector(Pattern: TBitmap; const X, Y: integer): integer;
{
  0 1
  2 3
}
begin
  if ( X < Pattern.Width div 2 ) then
    result := 0 else
    result := 1;

  if ( Y >= Pattern.Height div 2 ) then
    Inc( result, 2 );
end;

procedure RadialCentral(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 362, 362 );
  for Y1 := 0 to 361 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 361 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (180 - Y2) * (180 - Y2)))];
      1: Row[X1] := Params.Colors[Round(Sqrt((X2 - 181) * (X2 - 181) + (180 - Y2) * (180 - Y2)))];
      2: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (180 - Y2) * (180 - Y2)))];
      3: Row[X1] := Params.Colors[Round(Sqrt((X2 - 181) * (X2 - 181) + (180 - Y2) * (180 - Y2)))];
      end;
    end;
  end;
end;

procedure RadialTop(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 362, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 361 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,2: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + Y2 * Y2))];
      1,3: Row[X1] := Params.Colors[Round(Sqrt((X2 - 181) * (X2 - 181) + Y2 * Y2))];
      end;
    end;
  end;
end;

procedure RadialBottom(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 362, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 361 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,2: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (180 - Y2) * (180 - Y2)))];
      1,3: Row[X1] := Params.Colors[Round(Sqrt((X2 - 181) * (X2 - 181) + (180 - Y2) * (180 - Y2)))];
      end;
    end;
  end;
end;

procedure RadialLeft(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 362 );
  for Y1 := 0 to 361 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,1: Row[X1] := Params.Colors[Round(Sqrt(X2 * X2 + (180 - Y2) * (180 - Y2)))];
      2,3: Row[X1] := Params.Colors[Round(Sqrt(X2 * X2 + (Y2 - 181) * (Y2 - 181)))];
      end;
    end;
  end;
end;

procedure RadialRight(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 362 );
  for Y1 := 0 to 361 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,1: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (180 - Y2) * (180 - Y2)))];
      2,3: Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (Y2 - 181) * (Y2 - 181)))];
      end; 
    end;
  end;
end;

procedure RadialTopLeft(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[Round(Sqrt(X2 * X2 + Y2 * Y2))];
    end;
  end;
end;

procedure RadialTopRight(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + Y2 * Y2))];
    end;
  end;
end;

procedure RadialBottomLeft(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[Round(Sqrt(X2 * X2 + (180 - Y2) * (180 - Y2)))];
    end;
  end;
end;

procedure RadialBottomRight(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 181, 181 );
  for Y1 := 0 to 180 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 180 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[Round(Sqrt((180 - X2) * (180 - X2) + (180 - Y2) * (180 - Y2)))];
    end;
  end;
end;

procedure LinearHorizontal(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 1, 256 );
  X1 := 0;
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    Translate( Params, X1, Y1, X2, Y2 );
    Row[0] := Params.Colors[Y2];
  end;
end;

procedure LinearVertical(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 1 );
  Y1 := 0;
  Row := PRGBTripleArray(Params.Pattern.ScanLine[0]);
  for X1 := 0 to 255 do
  begin
    Translate( Params, X1, Y1, X2, Y2 );
    Row[X1] := Params.Colors[X2];
  end;
end;

procedure ReflectedHorizontal(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 1, 512 );
  X1 := 0;
  for Y1 := 0 to 511 do
  begin
    Translate( Params, X1, Y1, X2, Y2 );
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    case Sector(Params.Pattern, X2, Y2) of
    0,1: Row[0] := Params.Colors[255 - Y2];
    2,3: Row[0] := Params.Colors[Y2 - 256];
    end;
  end;
end;

procedure ReflectedVertical(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 512, 1 );
  Y1 := 0;
  Row := PRGBTripleArray(Params.Pattern.ScanLine[0]);
  for X1 := 0 to 511 do
  begin
    Translate( Params, X1, Y1, X2, Y2 );
    case Sector(Params.Pattern, X2, Y2) of
    0,2: Row[X1] := Params.Colors[255 - X2];
    1,3: Row[X1] := Params.Colors[X2 - 256];
    end;
  end;
end;

procedure DiagonalLinearForward(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 128, 129 );
  for Y1 := 0 to 128 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 127 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[127 + (Y2 - X2)];
    end;
  end;
end;

procedure DiagonalLinearBackward(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 128, 129 );
  for Y1 := 0 to 128 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 127 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      Row[X1] := Params.Colors[X2 + Y2];
    end;
  end;
end;

procedure DiagonalReflectedForward(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      if X2 > Y2 then
        Row[X1] := Params.Colors[X2 - Y2]
      else
        Row[X1] := Params.Colors[Y2 - X2];
    end;
  end;
end;

procedure DiagonalReflectedBackward(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      if X2 + Y2 < 255 then
        Row[X1] := Params.Colors[255 - (X2 + Y2)]
      else
        Row[X1] := Params.Colors[(Y2 + X2) - 255];
    end;
  end;
end;

procedure ArrowLeft(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 129, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 128 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,1: Row[X1] := Params.Colors[255 - (X2 + Y2)];
      2,3: Row[X1] := Params.Colors[Y2 - X2];
      end;
    end;
  end;
end;

procedure ArrowRight(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 129, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 128 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,1: Row[X1] := Params.Colors[(X2 - Y2) + 127];
      2,3: Row[X1] := Params.Colors[(X2 + Y2) - 128];
      end;
    end;
  end;
end;

procedure ArrowUp(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 129 );
  for Y1 := 0 to 128 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,2: Row[X1] := Params.Colors[255 - (X2 + Y2)];
      1,3: Row[X1] := Params.Colors[X2 - Y2];
      end;
    end;
  end;
end;

procedure ArrowDown(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 129 );
  for Y1 := 0 to 128 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0,2: Row[X1] := Params.Colors[127 + (Y2 - X2)];
      1,3: Row[X1] := Params.Colors[(X2 + Y2) - 128];
      end;
    end;
  end;
end;

procedure Diamond(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0: Row[X1] := Params.Colors[255 - (X2 + Y2)];
      1: Row[X1] := Params.Colors[X2 - Y2];
      2: Row[X1] := Params.Colors[Y2 - X2];
      3: Row[X1] := Params.Colors[(X2 + Y2) - 255];
      end;
    end;
  end;
end;

procedure Butterfly(var Params: RBuildParams);
var
  X1, Y1, X2, Y2: Integer;
  Row: PRGBTripleArray;
begin
  DimPattern( Params, 256, 256 );
  for Y1 := 0 to 255 do
  begin
    Row := PRGBTripleArray(Params.Pattern.ScanLine[Y1]);
    for X1 := 0 to 255 do
    begin
      Translate( Params, X1, Y1, X2, Y2 );
      case Sector(Params.Pattern, X2, Y2) of
      0: Row[X1] := Params.Colors[ (X2 - Y2) + 128 ];
      1: Row[X1] := Params.Colors[ 383 - (X2 + Y2) ];
      2: Row[X1] := Params.Colors[ (X2 + Y2) - 128 ];
      3: Row[X1] := Params.Colors[ 128 + (Y2 - X2) ];
      end;
    end;
  end;
end;

{ TSilGradientPanel }

type
  TPatternBuilder = procedure(var Params: RBuildParams);

const
  PatternBuilder: array[TGradientStyle] of TPatternBuilder = (nil,
    RadialCentral, RadialTop, RadialBottom, RadialLeft, RadialRight,
    RadialTopLeft, RadialTopRight, RadialBottomLeft, RadialBottomRight,
    LinearHorizontal, LinearVertical, ReflectedHorizontal, ReflectedVertical,
    DiagonalLinearForward, DiagonalLinearBackward, DiagonalReflectedForward,
    DiagonalReflectedBackward, ArrowLeft, ArrowRight, ArrowUp, ArrowDown,
    Diamond, Butterfly);

constructor TSilGradientPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 100;
  Height := 100;
  fColorBegin := clWhite;
  fColorEnd := clBtnFace;
  fStyle := gsRadialC;
  fShift := 0;
  fRotation := 0;
  fReverse := False;
  fPattern := TBitmap.Create;
  fPattern.PixelFormat := pf24bit;
  UpdatePattern;
end;

destructor TSilGradientPanel.Destroy;
begin
  fPattern.Free;
  inherited Destroy;
end;

procedure TSilGradientPanel.Loaded;
begin
  inherited Loaded;
  UpdatePattern;
end;

procedure TSilGradientPanel.Paint;
begin
  if not FDirty then Canvas.StretchDraw(ClientRect, Pattern);
end;

procedure TSilGradientPanel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSilGradientPanel.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FUpdatePending then
    UpdatePattern;
end;

procedure TSilGradientPanel.InvalidatePattern;
begin
  UpdatePattern;
end;

procedure TSilGradientPanel.SetColorBegin(Value: TColor);
begin
  if fColorBegin <> Value then
  begin
    fColorBegin := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.SetColorEnd(Value: TColor);
begin
  if fColorEnd <> Value then
  begin
    fColorEnd := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.SetStyle(Value: TGradientStyle);
begin
  if fStyle <> Value then
  begin
    fStyle := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.SetShift(Value: TGradientShift);
begin
  if Value < Low(TGradientShift) then
    Value := Low(TGradientShift)
  else if Value > High(TGradientShift) then
    Value := High(TGradientShift);

  if fShift <> Value then
  begin
    fShift := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.SetRotation(Value: TGradientRotation);
begin
  if Value < Low(TGradientRotation) then
    Value := Low(TGradientRotation)
  else if Value > High(TGradientRotation) then
    Value := High(TGradientRotation);

  if fRotation <> Value then
  begin
    fRotation := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.SetReverse(Value: Boolean);
begin
  if fReverse <> Value then
  begin
    fReverse := Value;
    UpdatePattern;
  end;
end;

procedure TSilGradientPanel.GroupChanged;
begin
  UpdatePattern;
end;

procedure TSilGradientPanel.GroupIndexChanged;
begin
  UpdatePattern;
end;

procedure TSilGradientPanel.UpdatePattern;
var
  Params: RBuildParams;
  dRed, dGreen, dBlue: Integer;
  RGBColor1, RGBColor2: TColor;
  RGB1, RGB2: TRGBTriple;
  //UpdatedRect: TRect;
  Index: Integer;
  M: Integer;
begin
  FUpdatePending := True;

  if (csLoading in ComponentState) or (FUpdateCount <> 0) then Exit;

  FUpdatePending := False;

  if Reverse then
  begin
    RGBColor1 := ColorToRGB(ColorEnd);
    RGBColor2 := ColorToRGB(ColorBegin);
  end
  else
  begin
    RGBColor1 := ColorToRGB(ColorBegin);
    RGBColor2 := ColorToRGB(ColorEnd);
  end;

  RGB1.rgbtRed := GetRValue(RGBColor1);
  RGB1.rgbtGreen := GetGValue(RGBColor1);
  RGB1.rgbtBlue := GetBValue(RGBColor1);

  RGB2.rgbtRed := GetRValue(RGBColor2);
  RGB2.rgbtGreen := GetGValue(RGBColor2);
  RGB2.rgbtBlue := GetBValue(RGBColor2);

  if Shift > 0 then
  begin
    Inc(RGB1.rgbtRed, MulDiv(RGB2.rgbtRed - RGB1.rgbtRed, Shift, 100));
    Inc(RGB1.rgbtGreen, MulDiv(RGB2.rgbtGreen - RGB1.rgbtGreen, Shift, 100));
    Inc(RGB1.rgbtBlue, MulDiv(RGB2.rgbtBlue - RGB1.rgbtBlue, Shift, 100));
  end
  else if Shift < 0 then
  begin
    Inc(RGB2.rgbtRed, MulDiv(RGB2.rgbtRed - RGB1.rgbtRed, Shift, 100));
    Inc(RGB2.rgbtGreen, MulDiv(RGB2.rgbtGreen - RGB1.rgbtGreen, Shift, 100));
    Inc(RGB2.rgbtBlue, MulDiv(RGB2.rgbtBlue - RGB1.rgbtBlue, Shift, 100));
  end;

  dRed := RGB2.rgbtRed - RGB1.rgbtRed;
  dGreen := RGB2.rgbtGreen - RGB1.rgbtGreen;
  dBlue := RGB2.rgbtBlue - RGB1.rgbtBlue;

  M := MulDiv(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Params.Colors[Index] do
      begin
        rgbtRed := RGB1.rgbtRed + (Index * dRed) div 255;
        rgbtGreen := RGB1.rgbtGreen + (Index * dGreen) div 255;
        rgbtBlue := RGB1.rgbtBlue + (Index * dBlue) div 255;
      end
  else if M > 0 then
  begin
    M := 255 - M;
    for Index := 0 to M - 1 do
      with Params.Colors[Index] do
      begin
        rgbtRed := RGB1.rgbtRed + (Index * dRed) div M;
        rgbtGreen := RGB1.rgbtGreen + (Index * dGreen) div M;
        rgbtBlue := RGB1.rgbtBlue + (Index * dBlue) div M;
      end;
    for Index := M to 255 do
      with Params.Colors[Index] do
      begin
        rgbtRed := RGB1.rgbtRed + ((255 - Index) * dRed) div (255 - M);
        rgbtGreen := RGB1.rgbtGreen + ((255 - Index) * dGreen) div (255 - M);
        rgbtBlue := RGB1.rgbtBlue + ((255 - Index) * dBlue) div (255 - M);
      end;
  end
  else if M < 0 then
  begin
    M := -M;
    for Index := 0 to M do
      with Params.Colors[Index] do
      begin
        rgbtRed := RGB2.rgbtRed - (Index * dRed) div M;
        rgbtGreen := RGB2.rgbtGreen - (Index * dGreen) div M;
        rgbtBlue := RGB2.rgbtBlue - (Index * dBlue) div M;
      end;
    for Index := M + 1 to 255 do
      with Params.Colors[Index] do
      begin
        rgbtRed := RGB2.rgbtRed - ((255 - Index) * dRed) div (255 - M);
        rgbtGreen := RGB2.rgbtGreen - ((255 - Index) * dGreen) div (255 - M);
        rgbtBlue := RGB2.rgbtBlue - ((255 - Index) * dBlue) div (255 - M);
      end;
  end;

  FDirty := True;
  try
    Params.Translate := false;
    Params.Pattern := Pattern; 
    Params.Group := GetGrouping;
    Params.OwnerLeft := self.Left;
    Params.OwnerTop := self.Top;
    Params.OwnerHeight := self.Height;
    Params.OwnerWidth := self.Width;

    if @PatternBuilder[Style] <> nil then
      PatternBuilder[Style](Params)
    else if Assigned(fOnCustom) then
      fOnCustom(Self, Params.Colors, Pattern)
    else
    begin
      Pattern.Width := 2;
      Pattern.Height := 2;
      Pattern.Canvas.Pixels[0, 0] := RGBColor1;
      Pattern.Canvas.Pixels[0, 1] := RGBColor2;
      Pattern.Canvas.Pixels[1, 0] := RGBColor2;
      Pattern.Canvas.Pixels[1, 1] := RGBColor1;
    end;
  finally
    FDirty := False;
  end;

  {if (Parent <> nil) and Parent.HandleAllocated then
  begin
    UpdatedRect := BoundsRect;
    InvalidateRect(Parent.Handle, @UpdatedRect, False);
    if csDesigning in ComponentState then Parent.Update;
  end
  else}
  Invalidate;
end;

end.

