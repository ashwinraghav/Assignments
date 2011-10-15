unit SilVm3dShape;

{$I Defines.inc}

interface

uses
  Sil, SilBeTypes,
  SilViControls,
  SysUtils, Math, Graphics;

const
  kIgnoreColor = $010203;

type
  ImageTool = class
    class procedure ImageFilter( SrcBitmap: TBitmap; var DstBitmap: TBitmap; Color: TColor; AutoBalance: boolean = false; IgnoreColor: TColor = kIgnoreColor; BackGroundColor: TColor = kIgnoreColor );
    class procedure ImageMix( SrcBitmap: TBitmap; var DstBitmap: TBitmap; Color: TColor; IgnoreColor: TColor = kIgnoreColor; BackGroundColor: TColor = kIgnoreColor );
    class function CreateBitmap(Width, Height: integer): TBitmap;
    class function Create3dShape: ISil3dImageSource;
  end;

implementation

const
  cImageUpID: TGUID = '{99F5886A-C6C3-47A3-8ACC-9D7232635A37}';
  cImageDnID: TGUID = '{20BAEAC5-7714-4FB7-B43A-A2E7BFC3C7D9}';

type
  ISilImage = interface;
  ISilImagePool = interface;

  ISilImage = interface
    ['{837CC719-57F7-44FA-B7C8-D2BDDB612553}']
    function GetBitmap: TBitmap;
    function GetID: TGUID;
    function GetParams: IUnknown;
    procedure SetID(const Value: TGUID);
    procedure SetParams(const Value: IUnknown);
    // properties
    property Bitmap: TBitmap read GetBitmap;
    property ID: TGUID read GetID write SetID;
    property Params: IUnknown read GetParams write SetParams;
  end;

  ISilImagePool = interface
    ['{921DE5B9-D19F-49A3-82EF-237797958FDA}']
    function RegisterNew( const ID: TGUID; const Params: IUnknown; const Bitmap: TBitmap ): ISilImage;
    function Enumerate(var Enum: IEnumerator; out Image: ISilImage): boolean;
  end;

type
  VectTool = class
    class procedure EscalarProduct( const V1, V2: RVector; out Value: double );
    class procedure VectorialProduct( const V1, V2: RVector; out V3: RVector );
  end;

type
  TVector = class
  private
    FCoords: RVector;
    FModuleValid: boolean;
    FModule: double;
  protected
    procedure DoCheckModule;
    procedure DoMakeVersor;
  protected
    function GetX: double; virtual;
    function GetY: double; virtual;
    function GetZ: double; virtual;
    function GetVector: RVector; virtual;
    procedure SetX(const Value: double); virtual;
    procedure SetY(const Value: double); virtual;
    procedure SetZ(const Value: double); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create( X, Y, Z: double ); overload; virtual;
    constructor Create( const Vector: RVector ); overload; virtual;
  public
    procedure Assign( const Vector: RVector );
  public
    property Vector: RVector read GetVector;
    property X: double read GetX write SetX;
    property Y: double read GetY write SetY;
    property Z: double read GetZ write SetZ;
  end;

  TVersor = class ( TVector )
  protected
    function GetVector: RVector; override;
    function GetX: double; override;
    function GetY: double; override;
    function GetZ: double; override;
  public
    constructor Create( X, Y, Z: double ); override;
  end;

type
  T3dShape = class;
  TSil3dParams = class;

  T3dShape = class
  (
    TSilInterfacedObject,
    ISil3dImageSource
  )
  private
    FParams: ISil3dImageParams;
  private
    FView: RVector;
  private
    FImagePool: ISilImagePool;
    FDrawBuffUp: ISilImage;
    FDrawBuffDn: ISilImage;
    FCurrentBuff: TBitmap;
  private
    procedure Invalidate;
    function DoGetBitmap( ID: TGUID; Down: boolean; var Image: ISilImage ): TBitmap;
    procedure DoDraw( Bitmap: TBitmap; Down: boolean );
    procedure CheckImagePool;
  protected // ISil3dImageSource
    function GetParams: ISilImageParams;
    procedure GetBitmap(Height, Width: integer; Color, BackgroundColor: TColor;
      Down, Transparent: boolean; TransparentColor: TColor; var Bitmap: TBitmap);
    procedure PaintTo(Height, Width: integer; Color, BackgroundColor: TColor;
      Down, Transparent: boolean; TransparentColor: TColor; const Canvas: TCanvas; Rect: TRect);
    function Get3dParams: ISil3dImageParams;
    function ISil3dImageSource.GetParams = Get3dParams;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSil3dParams = class
  (
    TSilInterfacedObject,
    ISilImageParams,
    ISil3dImageParams
  )
  public
    FOwner: T3dShape;
    FHeight: integer;
    FWidth: integer;
    FShape: TSil3dShape;
    FElevation: integer;
    FAlbedo: double;
    FLights: IPointerList;
  protected
    procedure Invalidate;
    procedure DoCheckPoint( const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos; Normal: TVersor ); overload;
    procedure DoCheckPoint( const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos ); overload;
  protected // ISilImageParams
    function SameParams(const Source: IUnknown): boolean;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetWidth(const Value: integer);
    procedure SetHeight(const Value: integer);
    // properties
    property Width: integer read Getwidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  protected // ISil3dImageParams
    procedure CheckPoint( const View: RVector; const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos; out Light: double );
    function GetAlbedo: double;
    function GetShape: TSil3dShape;
    function GetElevation: integer;
    procedure SetShape(const Value: TSil3dShape);
    procedure SetAlbedo(const Value: double);
    procedure SetElevation(const Value: integer);
    procedure ResetLights;
    procedure AddLight( X, Y, Z: double );
  public
    constructor Create(const Owner: T3dShape; const Source: ISil3dImageParams = nil);
  end;

type
  TSilImage = class;
  TSilImagePool = class;

  TSilImage = class
  (
    TSilInterfacedObject,
    ISilImage
  )
  private
    FOwner: TSilImagePool;
    FBitmap: TBitmap;
    FID: TGUID;
    FParams: IUnknown;
  private
    procedure CheckBitmap;
  protected // ISilImage
    function GetBitmap: TBitmap;
    function GetID: TGUID;
    function GetParams: IUnknown;
    procedure SetID(const Value: TGUID);
    procedure SetParams(const Value: IUnknown);
    // properties
    property Bitmap: TBitmap read GetBitmap;
    property ID: TGUID read GetID write SetID;
    property Params: IUnknown read GetParams write SetParams;
  public
    constructor Create( const Owner: TSilImagePool; Bitmap: TBitmap ); reintroduce;
    destructor Destroy; override;
  end;

  TSilImagePool = class
  (
    TSilInterfacedObject,
    ISilImagePool
  )
  private
    FList: IPointerList;
    FRef: PPointer;
  private
    procedure CheckList;
  protected // ISilImagePool
    function RegisterNew( const ID: TGUID; const Params: IUnknown; const Bitmap: TBitmap ): ISilImage;
    function Enumerate(var Enum: IEnumerator; out Image: ISilImage): boolean;
  public
    constructor Create(var Ref: Pointer); reintroduce;
    destructor Destroy; override;
  public
    procedure Remove(const Image: ISilImage);
  end;

var
  _ImagePool: Pointer;
  
{ VectTool }

class procedure VectTool.EscalarProduct(const V1, V2: RVector; out Value: double);
begin
  Value := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
end;

class procedure VectTool.VectorialProduct(const V1, V2: RVector; out V3: RVector);
begin
  V3.X := V1.Y * V2.Z - V1.Z * V2.Y;
  V3.Y := V1.Z * V2.X - V1.X * V2.Z;
  V3.Z := V1.X * V2.Y - V1.Y * V2.X;
end;

{ ImageTool }

class function ImageTool.CreateBitmap(Width, Height: integer): TBitmap;
begin
  result := TBitmap.Create();
  result.PixelFormat := pf24bit;
  result.Width := Width;
  result.Height := Height;
end;

class function ImageTool.Create3dShape: ISil3dImageSource;
begin
  result := T3dShape.Create;
end;

class procedure ImageTool.ImageFilter( SrcBitmap: TBitmap; var DstBitmap: TBitmap; Color: TColor; AutoBalance: boolean; IgnoreColor, BackGroundColor: TColor );
var
  r, g, b: double;
  x, y: integer;
  sr, sg, sb, ir, ig, ib, mx: byte;
  dstptr, srcptr: PByte;
begin
  if not Assigned( DstBitmap ) then
    DstBitmap := CreateBitmap( SrcBitmap.Width, SrcBitmap.Height );

  b := Sil.Rgb.GetBlue( Color ) / 255;
  g := Sil.Rgb.GetGreen( Color ) / 255;
  r := Sil.Rgb.GetRed( Color ) / 255;
  ib := Sil.Rgb.GetBlue( IgnoreColor );
  ig := Sil.Rgb.GetGreen( IgnoreColor );
  ir := Sil.Rgb.GetRed( IgnoreColor );

  if AutoBalance then
  begin
    mx := 0;
    y := 0;
    while ( mx < 255 ) and ( y < SrcBitmap.Height ) do
    begin
      srcptr := SrcBitmap.ScanLine[ y ];
      x := 0;
      while ( x < SrcBitmap.Width * 3 ) do
      begin
        sb := srcptr^;
        Inc( srcptr );
        inc( x );
        if ( sb > mx ) then mx := sb;
      end;

      inc( y );
    end;

    if ( mx > 0 ) then
    begin
      b := b * 255 / mx;
      g := g * 255 / mx;
      r := r * 255 / mx;
    end;
  end;
  
  for y := 0 to SrcBitmap.Height - 1 do
  begin
    dstptr := DstBitmap.ScanLine[ y ];
    srcptr := SrcBitmap.ScanLine[ y ];
    for x := 0 to SrcBitmap.Width - 1 do
    begin
      sb := srcptr^; Inc( srcptr );
      sg := srcptr^; Inc( srcptr );
      sr := srcptr^; Inc( srcptr );
      if ( sb = ib ) and ( sg = ig ) and ( sr = ir ) then
      begin
        dstptr^ := Sil.Rgb.GetBlue( BackGroundColor ); Inc( dstptr );
        dstptr^ := Sil.Rgb.GetGreen( BackGroundColor ); Inc( dstptr );
        dstptr^ := Sil.Rgb.GetRed( BackGroundColor ); Inc( dstptr );
      end
      else
      begin
        dstptr^ := Round( sb * b ); Inc( dstptr );
        dstptr^ := Round( sg * g ); Inc( dstptr );
        dstptr^ := Round( sr * r ); Inc( dstptr );
      end;
    end;
  end;
end;

class procedure ImageTool.ImageMix( SrcBitmap: TBitmap; var DstBitmap: TBitmap; Color: TColor; IgnoreColor: TColor = kIgnoreColor; BackGroundColor: TColor = kIgnoreColor );
var
  r, g, b, x, y: integer;
  sr, sg, sb, ir, ig, ib: byte;
  dstptr, srcptr: PByte;
begin
  if not Assigned( DstBitmap ) then
    DstBitmap := CreateBitmap( SrcBitmap.Width, SrcBitmap.Height );

  b := Sil.Rgb.GetBlue( Color );
  g := Sil.Rgb.GetGreen( Color );
  r := Sil.Rgb.GetRed( Color );
  ib := Sil.Rgb.GetBlue( IgnoreColor );
  ig := Sil.Rgb.GetGreen( IgnoreColor );
  ir := Sil.Rgb.GetRed( IgnoreColor );

  for y := 0 to SrcBitmap.Height - 1 do
  begin
    dstptr := DstBitmap.ScanLine[ y ];
    srcptr := SrcBitmap.ScanLine[ y ];
    for x := 0 to SrcBitmap.Width - 1 do
    begin
      sb := srcptr^; Inc( srcptr );
      sg := srcptr^; Inc( srcptr );
      sr := srcptr^; Inc( srcptr );
      if ( sb = ib ) and ( sg = ig ) and ( sr = ir ) then
      begin
        dstptr^ := Sil.Rgb.GetBlue( BackGroundColor ); Inc( dstptr );
        dstptr^ := Sil.Rgb.GetGreen( BackGroundColor ); Inc( dstptr );
        dstptr^ := Sil.Rgb.GetRed( BackGroundColor ); Inc( dstptr );
      end
      else
      begin
        dstptr^ := ( sb + b ) div 2; Inc( dstptr );
        dstptr^ := ( sg + g ) div 2; Inc( dstptr );
        dstptr^ := ( sr + r ) div 2; Inc( dstptr );
      end;
    end;
  end;
end;

{ TVector }

constructor TVector.Create;
begin
  inherited Create;
end;

constructor TVector.Create(const Vector: RVector);
begin
  Create( Vector.X, Vector.Y, Vector.Z );
end;

constructor TVector.Create(X, Y, Z: double);
begin
  inherited Create;

  FCoords.X := X;
  FCoords.Y := Y;
  FCoords.Z := Z;
  FModuleValid := false;
end;

function TVector.GetX: double;
begin
  result := FCoords.X;
end;

function TVector.GetY: double;
begin
  result := FCoords.Y;
end;

function TVector.GetZ: double;
begin
  result := FCoords.Z;
end;

procedure TVector.Assign(const Vector: RVector);
begin
  Move( Vector, FCoords, sizeof( FCoords ) );
  FModuleValid := false;
end;

procedure TVector.SetX(const Value: double);
begin
  FCoords.X := Value;
  FModuleValid := false;
end;

procedure TVector.SetY(const Value: double);
begin
  FCoords.Y := Value;
  FModuleValid := false;
end;

procedure TVector.SetZ(const Value: double);
begin
  FCoords.Z := Value;
  FModuleValid := false;
end;

function TVector.GetVector: RVector;
begin
  result := FCoords;
end;

procedure TVector.DoMakeVersor;
begin
  if not FModuleValid or ( FModule <> 1 ) then
  begin
    DoCheckModule;
    if ( FModule > 0 ) then
      with FCoords do
      begin
        X := X / FModule;
        Y := Y / FModule;
        Z := Z / FModule;
        FModule := 1;
      end;
  end;
end;

procedure TVector.DoCheckModule;
begin
  if not FModuleValid then
  begin
    with FCoords do
      FModule := Math.Hypot( X, Math.Hypot( Y, Z ) );
    FModuleValid := true;
  end;
end;

{ TVersor }

constructor TVersor.Create(X, Y, Z: double);
begin
  inherited;
  DoMakeVersor;
end;

function TVersor.GetVector: RVector;
begin
  DoMakeVersor;
  result := inherited GetVector;
end;

function TVersor.GetX: double;
begin
  DoMakeVersor;
  result := inherited GetX;
end;

function TVersor.GetY: double;
begin
  DoMakeVersor;
  result := inherited GetY;
end;

function TVersor.GetZ: double;
begin
  DoMakeVersor;
  result := inherited GetZ;
end;

{ T3dShape }

constructor T3dShape.Create;
begin
  inherited Create;

  FView.X := 0;
  FView.Y := 0;
  FView.Z := 1;

  FParams := TSil3dParams.Create( self );
end;

destructor T3dShape.Destroy;
begin
  FDrawBuffUp := nil;
  FDrawBuffDn := nil;
  FImagePool := nil;
  FreeAndNil( FCurrentBuff );
  FParams := nil;

  inherited;
end;

procedure T3dShape.PaintTo( Height, Width: integer; Color, BackgroundColor: TColor;
  Down, Transparent: boolean; TransparentColor: TColor;
  const Canvas: TCanvas; Rect: TRect );
begin
  GetBitmap( Height, Width, Color, BackgroundColor, Down, Transparent, TransparentColor, FCurrentBuff );
  Canvas.CopyRect( Rect, FCurrentBuff.Canvas, Rect );
end;

procedure T3dShape.Invalidate;
begin
  FreeAndNil( FCurrentBuff );
  FDrawBuffUp := nil;
  FDrawBuffDn := nil;
end;

procedure T3dShape.GetBitmap(Height, Width: integer; Color, BackgroundColor: TColor;
  Down, Transparent: boolean; TransparentColor: TColor; var Bitmap: TBitmap);
var
  srcbmp: TBitmap;
  bkgcolor: TColor;
begin
  if ( FParams.Width <> Width ) or ( FParams.Height <> Height ) then
  begin
    FParams.Width := Width;
    FParams.Height := Height;
    Invalidate;
  end;

  if Down then
    srcbmp := DoGetBitmap( cImageDnID, true, FDrawBuffDn ) else
    srcbmp := DoGetBitmap( cImageUpID, false, FDrawBuffUp );

  if Transparent then
    bkgcolor := TransparentColor else
    bkgcolor := BackgroundColor;

  ImageTool.ImageFilter( srcbmp, Bitmap, Color, false, kIgnoreColor, bkgcolor );
end;

function T3dShape.DoGetBitmap( ID: TGUID; Down: boolean; var Image: ISilImage ): TBitmap;

  function FindImage( ID: TGUID; out Image: ISilImage ): boolean;
  var
    enum: IEnumerator;
  begin
    result := false;
    while not result and FImagePool.Enumerate( enum, Image ) do
      result := Sil.Guid.IsEqual( Image.ID, ID ) and FParams.SameParams( Image.Params );
  end;

var
  bitmap: TBitmap;
begin
  CheckImagePool;

  if not Assigned( Image ) and not FindImage( ID, Image ) then
  begin
    bitmap := ImageTool.CreateBitmap( FParams.Width, FParams.Height );

    DoDraw( bitmap, Down );
    Image := FImagePool.RegisterNew( ID, TSil3dParams.Create( self, FParams ), bitmap );
  end;
  
  result := Image.Bitmap;
end;

procedure T3dShape.CheckImagePool;
begin
  if not Assigned( _ImagePool ) then
    FImagePool := TSilImagePool.Create( _ImagePool ) else
    FImagePool := IUnknown( _ImagePool ) as ISilImagePool;
end;

procedure T3dShape.DoDraw( Bitmap: TBitmap; Down: boolean );
var
  z, light: double;
  x, y, saturation, cr, cg, cb: integer;
  rp: T2dRelPos;
  lineptr: PByte;
begin
  with Bitmap, Canvas do
    for y := 0 to FParams.Height - 1 do
    begin
      lineptr := ScanLine[ y ];
      for x := 0 to FParams.Width - 1 do
      begin
        FParams.CheckPoint( FView, Down, x, y, z, rp, light );
        if ( rp = rpInside ) then
        begin
          saturation := Round( Sqrt( light * 100 ) );

          if ( saturation > 100 ) then
            saturation := 100
          else if ( saturation < 0 ) then
            saturation := 0;

          cr := Round( 255 * saturation / 100 );
          cg := cr;
          cb := cr;
        end
        else
        begin
          cb := Sil.Rgb.GetBlue( kIgnoreColor );
          cg := Sil.Rgb.GetGreen( kIgnoreColor );
          cr := Sil.Rgb.GetRed( kIgnoreColor );
        end;

        lineptr^ := cb;
        Inc( lineptr );
        lineptr^ := cg;
        Inc( lineptr );
        lineptr^ := cr;
        Inc( lineptr );
      end;
    end;
end;

function T3dShape.Get3dParams: ISil3dImageParams;
begin
  result := FParams;
end;

function T3dShape.GetParams: ISilImageParams;
begin
  result := FParams;
end;

{ TSilImage }

constructor TSilImage.Create(const Owner: TSilImagePool; Bitmap: TBitmap);
begin
  inherited Create;
  FOwner := Owner;
  FBitmap := Bitmap;
end;

destructor TSilImage.Destroy;
begin
  FOwner.Remove( self );
  FBitmap.Free;
  inherited;
end;

function TSilImage.GetBitmap: TBitmap;
begin
  CheckBitmap;
  result := FBitmap;
end;

procedure TSilImage.CheckBitmap;
begin
  if not Assigned( FBitmap ) then
    FBitmap := TBitmap.Create;
end;

function TSilImage.GetID: TGUID;
begin
  result := FID;
end;

function TSilImage.GetParams: IUnknown;
begin
  result := FParams;
end;

procedure TSilImage.SetID(const Value: TGUID);
begin
  FID := Value;
end;

procedure TSilImage.SetParams(const Value: IInterface);
begin
  FParams := Value;
end;

{ TSilImagePool }

constructor TSilImagePool.Create(var Ref: Pointer);
begin
  inherited Create;
  FRef := @Ref;
  Ref := Pointer( self as IUnknown );
end;

destructor TSilImagePool.Destroy;
begin
  FRef^ := nil;
  inherited;
end;

procedure TSilImagePool.CheckList;
begin
  if not Assigned( FList ) then
    FList := Sil.List.PointerList;
end;

function TSilImagePool.Enumerate(var Enum: IEnumerator; out Image: ISilImage): boolean;
var
  ptr: Pointer;
begin
  if Assigned( FList ) then
  begin
    result := FList.Enumerate( Enum, ptr ) and
      Sil.Ref.GetInterface( IUnknown( ptr ), ISilImage, Image );
  end
  else
    result := false;
end;

function TSilImagePool.RegisterNew( const ID: TGUID; const Params: IUnknown; const Bitmap: TBitmap ): ISilImage;
begin
  CheckList;
  result := TSilImage.Create( self, Bitmap );
  result.ID := ID;
  result.Params := Params;
  FList.Add( Pointer( result ) );
end;

procedure TSilImagePool.Remove(const Image: ISilImage);
var
  enum: IEnumerator;
  ptr: Pointer;
begin
  if Assigned( FList ) then
    while FList.Enumerate( enum, ptr ) do
      if Sil.Ref.SameObject( IUnknown( ptr ), Image ) then
      begin
        FList.Remove( ptr );
        break;
      end;
end;

{ TSil3dParams }

constructor TSil3dParams.Create(const Owner: T3dShape; const Source: ISil3dImageParams);
begin
  inherited Create;

  FOwner := Owner;
  FLights := Sil.List.PointerList;
  if Assigned( Source ) then
  begin
    FHeight := Source.Height;
    FWidth := Source.Width;
    FShape := Source.Shape;
    FElevation := Source.Elevation;
    FAlbedo := Source.Albedo;
  end;
end;

function TSil3dParams.SameParams(const Source: IUnknown): boolean;
var
  params: ISil3dImageParams;
begin
  result :=
    Sil.Ref.GetInterface( Source, ISil3dImageParams, params ) and
    ( FHeight = params.Height ) and
    ( FWidth = params.Width ) and
    ( FShape = params.Shape ) and
    ( FElevation = params.Elevation ) and
    ( FAlbedo = params.Albedo );
end;

procedure TSil3dParams.ResetLights;
begin
  FLights.Clear;
  Invalidate;
end;

procedure TSil3dParams.AddLight(X, Y, Z: double);
begin
  FLights.Add( TVector.Create( X, Y, Z ) );
  Invalidate;
end;

function TSil3dParams.GetAlbedo: double;
begin
  result := FAlbedo;
end;

procedure TSil3dParams.SetAlbedo(const Value: double);
var
  val: double;
begin
  if ( Value > 1 ) then
    val := 1 else
    val := Value;

  if ( FAlbedo <> val ) then
  begin
    FAlbedo := val;
    Invalidate;
  end;
end;

function TSil3dParams.GetShape: TSil3dShape;
begin
  result := FShape;
end;

procedure TSil3dParams.SetShape(const Value: TSil3dShape);
begin
  if ( FShape <> Value ) then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

function TSil3dParams.GetHeight: integer;
begin
  result := FHeight;
end;

procedure TSil3dParams.SetHeight(const Value: integer);
begin
  if ( FHeight <> Value ) then
  begin
    FHeight := Value;
    Invalidate;
  end;
end;

function TSil3dParams.GetWidth: integer;
begin
  result := FWidth;
end;

procedure TSil3dParams.SetWidth(const Value: integer);
begin
  if ( FWidth <> Value ) then
  begin
    FWidth := Value;
    Invalidate;
  end;
end;

function TSil3dParams.GetElevation: integer;
begin
  result := FElevation;
end;

procedure TSil3dParams.SetElevation(const Value: integer);
begin
  if ( FElevation <> Value ) then
  begin
    FElevation := Value;
    Invalidate;
  end;
end;

procedure TSil3dParams.CheckPoint( const View: RVector; const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos; out Light: double );
var
  normal: TVersor;
  lightamount, reflection: double;
  enum: IEnumerator;
  lv: TVector;
begin
  normal := TVersor.Create;
  try
    DoCheckPoint( Down, X, Y, Z, RelPos, normal );
    Light := 0;

    if ( RelPos = rpInside ) then
      while FLights.Enumerate( enum, lv ) do
      begin
        VectTool.EscalarProduct( normal.Vector, lv.Vector, lightamount );
        if ( FAlbedo > 0 ) then
        begin
          VectTool.EscalarProduct( normal.Vector, View, reflection );
          reflection := ( 1 - FAlbedo ) + FAlbedo * reflection;
        end
        else
          reflection := 1;
        if ( lightamount > 0 ) then Light := Light + lightamount * reflection;
      end;

  finally
    normal.Free;
  end;
end;

procedure TSil3dParams.DoCheckPoint( const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos; Normal: TVersor );
var
  z1, z2: double;
  rp: T2dRelPos;
  v1, v2, v3: RVector;
begin
  DoCheckPoint( Down, X, Y, Z, RelPos );
  if Assigned( Normal ) then
  begin
    if ( RelPos = rpInside ) then
    begin
      DoCheckPoint( Down, X + 0.1, Y, z1, rp );
      DoCheckPoint( Down, X, Y + 0.1, z2, rp );
      v1.X := 0.1;
      v1.Y := 0;
      v1.Z := z1 - Z;
      v2.X := 0;
      v2.Y := 0.1;
      v2.Z := z2 - Z;
      VectTool.VectorialProduct( v1, v2, v3 );
      Normal.Assign( v3 );
    end
    else
    begin
      Normal.X := 0;
      Normal.Y := 0;
      Normal.Z := 1;
    end;
  end;
end;

procedure TSil3dParams.DoCheckPoint( const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos );

  function GetSide( Hypot, Side: double ): double;
  var
    v1, v2: double;
  begin
    v1 := Hypot * Hypot;
    v2 := Side * Side;
    if ( v1 >= v2 ) then
      result := Sqrt( v1 - v2 ) else
      result := 0; 
  end;

const
  kPressRadious = 20;
var
  mustdown: boolean;
  dx, dy, hyp, dx2, dy2, elev2: double;
  midx, midy, elev: integer;
begin
  midx := FWidth div 2;
  midy := FHeight div 2;

  dx := X - midx;
  dy := Y - midy;
  Z := 0;
  RelPos := rpOutside;
  elev := FElevation;
  mustdown := Down;

  case FShape of
  ksEllipse:
    begin
      hyp := Math.Hypot( dx, dy * midx / midy );
      if ( Round( hyp ) = midx ) then
        RelPos := rpBorder
      else if ( hyp < midx ) then
        RelPos := rpInside else
        RelPos := rpOutside;

      if ( RelPos = rpInside ) then
        Z := elev * GetSide( midx, hyp ) / midx;
    end;

  ksPipe:
    begin
      if ( X <= midy ) and ( X <= midx ) or
        ( X > FWidth - midy ) and ( X > midx ) then
      begin
        if ( X <= midy ) and ( X <= midx ) then
          dx2 := X - midy else
          dx2 := X - FWidth + midy;

        hyp := Math.Hypot( dx2, dy );
        if ( hyp < midy ) then
          RelPos := rpInside else
          RelPos := rpOutside;

        if ( RelPos = rpInside ) then
          Z := GetSide( midy, hyp );
      end
      else
      begin
        if ( Abs( dy ) < midy ) then
        begin
          RelPos := rpInside;
          Z := GetSide( midy, dy );
        end;
      end
    end;

  ksRoundRect, ksHorSwitch:
    begin
      if ( elev > midy ) then elev := midy;
      if ( elev > midx ) then elev := midx;

      if ( X <= elev ) then dx2 := elev - X
      else if ( X >= FWidth - elev ) then dx2 := X - FWidth + elev
      else dx2 := 0;
      if ( Y <= elev ) then dy2 := elev - Y
      else if ( Y >= FHeight - elev ) then dy2 := Y - FHeight + elev
      else dy2 := 0;

      hyp := Math.Hypot( dx2, dy2 );
      if ( hyp = 0 ) then
      begin
        RelPos := rpInside;
        Z := elev;
      end
      else if ( hyp <= elev ) then
      begin
        RelPos := rpInside;
        Z := GetSide( elev, hyp );
      end
      else
      begin
        RelPos := rpOutside;
        Z := 0;
      end;

      if ( FShape = ksHorSwitch ) and ( RelPos = rpInside ) then
      begin
        mustdown := false;
        if Down then
          dx2 := X - FWidth * 3 / 4 else
          dx2 := X - FWidth * 1 / 4;

        Z := Z - 20000 / ( 500 + dx2 * dx2 );
      end;
    end;

  ksRhombus:
    begin
      if ( elev > midy ) then elev := midy;
      if ( elev > midx ) then elev := midx;

      dx2 := Abs( X - midx ) + Abs( Y - midy ) * midx / midy;
      if ( dx2 <= midx ) then
      begin
        dx2 := midx - dx2;

        RelPos := rpInside;
        if ( dx2 < elev ) then
          Z := GetSide( elev, ( elev - dx2 ) ) else
          Z := elev;
      end
      else
      begin
        RelPos := rpOutside;
        Z := 0;
      end;
    end;

  ksPool:
    begin
      if ( elev > FWidth div 4 ) then elev := FWidth div 4;
      if ( elev > FHeight div 4 ) then elev := FHeight div 4;
      elev2 := elev / 2;

      { Top }
      if ( Y <= elev ) and ( X >= Y ) and ( X <= FWidth - Y ) then
      begin
        Z := GetSide( elev2, elev2 - Y );
      end
      { Left }
      else if ( X <= elev ) and ( Y <= FHeight - X ) then
      begin
        Z := GetSide( elev2, elev2 - X );
      end
      { Bottom }
      else if ( Y >= FHeight - elev ) and ( X <= FWidth - FHeight + Y ) then
      begin
        Z := GetSide( elev2, FHeight - elev2 - Y );
      end
      { Right }
      else if ( X >= FWidth - elev ) then
      begin
        Z := GetSide( elev2, FWidth - elev2 - X );
      end
      { Center }
      else
      begin
        if Down then
          Z := 0 - 2000 * elev / ( 1000 + elev + dx * dx + dy * dy ) else
          Z := 0;
      end;
      RelPos := rpInside;
      mustdown := false;
    end;
  end;

  if mustdown and ( RelPos = rpInside ) then
    Z := Z - 20000 / ( 1000 + dx * dx + dy * dy );
end;

procedure TSil3dParams.Invalidate;
begin
  if Assigned( FOwner ) then
    FOwner.Invalidate;
end;

end.

