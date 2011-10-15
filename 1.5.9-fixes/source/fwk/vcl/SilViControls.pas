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

unit SilViControls;

interface

uses
  Sil,
  Windows, Controls, Graphics, TypInfo, Classes, Forms;

const
  SM_BASE                 = EV_FIRST + $100;
  SM_GROUPJOIN            = SM_BASE + 1;
  SM_GROUPCHANGED         = SM_BASE + 2;
  SM_GROUPCLIENTSCHANGED  = SM_BASE + 3;

const
  kInitRepeatPause = 400;  { pause before repeat timer (ms) }
  kRepeatPause     = 100;  { pause before hint window displays (ms)}

type
  TSilControlStates = ( acsDefault, acsFocus, acsMouseOver );
  TSilControlState = set of TSilControlStates;

type
  TSilButtonStates = ( absPressed, absRepeating );
  TSilButtonState = set of TSilButtonStates;

type
  TButtonShape = ( bsStandard, bsLowered, bsFlat );

type
  TSilRelativePosition = ( rpFixed,
    rpTopLeft, rpTopCenter, rpTopRight,
    rpCenterLeft, rpCenter, rpCenterRight,
    rpBottomLeft, rpBottomCenter, rpBottomRight );

type
  ISilControlPainter = interface;

  ISilCustomControl = interface;
  ISilControl = interface;
  ISilCustomButton = interface;
  ISilButton = interface;

  ISilControlProperty = interface;
  ISilPositionProperty = interface;
  ISilCaptionProperty = interface;
  ISilImageProperty = interface;
  ISilGroupingProperty = interface;
  ISilHighlightProperty = interface;


  ISilControlPainter = interface
    ['{F98048E3-33F6-4680-B39F-3360D5AEE8DE}']
    procedure Paint(const Control: ISilControl; Canvas: TCanvas);
  end;

  ISilPropertyOwner = interface
    ['{93D84CC0-0689-4199-BED2-F81D72CDE34F}']
    function GetVclControl: TControl;
    function GetSilControl: ISilControl;
    procedure PropertyChanged(const Prop: ISilControlProperty);
    // properties
    property VclControl: TControl read GetVclControl;
    property SilControl: ISilControl read GetSilControl;
  end;

  ISilCustomControl = interface
    ['{DDFDC76F-E8ED-44A1-9D68-14AAB6B9E47C}']
    function GetEnabled: boolean;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetBounds: TRect;
    function GetCaption: TCaption;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetVclControl: TControl;
    procedure Invalidate;
    // properties
    property Enabled: Boolean read GetEnabled;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
    property Bounds: TRect read GetBounds;
    property Caption: TCaption read GetCaption;
    property Color: TColor read GetColor;
    property Font: TFont read GetFont;
    property VclControl: TControl read GetVclControl;
  end;

  ISilControlProperty = interface
    ['{6861BEDB-9AA2-47DB-8059-2C08ECC0320D}']
  end;

  ISilControl = interface ( ISilCustomControl )
    ['{E268FF2E-78F4-4787-A122-E58F7B0A0D58}']
    function GetCaptionProp: ISilCaptionProperty;
    function GetImage: ISilImageProperty;
    function GetGrouping: ISilGroupingProperty;
    function GetHighlight: ISilHighlightProperty;
    function GetFontHandle: HFont;
    function GetInterval: integer;
    function GetState: TSilControlState;
    function GetTick: integer;
    function GetTransparent: boolean;
    function GetTransparentColor: TColor;
    function GetCurrentColor: TColor;
    function GetRunning: boolean;
    function GetHaveFocus: boolean;
    function GetMouseOver: boolean;
    function GetShowing: Boolean;
    function CalcCurrentColor(Color, HighlightColor: TColor): TColor;
    // properties
    property CaptionProp: ISilCaptionProperty read GetCaptionProp;
    property Image: ISilImageProperty read GetImage;
    property Grouping: ISilGroupingProperty read GetGrouping;
    property Highlight: ISilHighlightProperty read GetHighlight;
    property State: TSilControlState read GetState;
    property Tick: integer read GetTick;
    property FontHandle: HFont read GetFontHandle;
    property Interval: integer read GetInterval;
    property Transparent: boolean read GetTransparent;
    property TransparentColor: TColor read GetTransparentColor;
    property CurrentColor: TColor read GetCurrentColor;
    property Running: boolean read GetRunning;
    property HaveFocus: boolean read GetHaveFocus;
    property MouseOver: boolean read GetMouseOver;
    property Showing: boolean read GetShowing;
  end;

  ISilHighlightProperty = interface
    ['{DDEDCC5A-CFF2-4731-8CA4-848EE90E0B2C}']
    function GetEnabled: boolean;
    function GetAutoColor: boolean;
    function GetColor: TColor;
    function GetDelay: Short;
    function GetTime: TDateTime;
    function GetLevel: integer;
    // properties
    property Enabled: boolean read GetEnabled;
    property AutoColor: boolean read GetAutoColor;
    property Color: TColor read GetColor;
    property Time: TDateTime read GetTime;
    property Delay: Short read GetDelay;
    property Level: integer read GetLevel;
  end;
  
  ISilPositionProperty = interface
    ['{C5DA2245-597D-4D0F-82D2-26626BFD9ED3}']
    function GetPosition: TSilRelativePosition;
    function GetLeft: integer;
    function GetTop: integer;
    function GetMargin: integer;
    // properties
    property Position: TSilRelativePosition read GetPosition;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Margin: integer read GetMargin;
  end;

  ISilCaptionProperty = interface
    ['{B447DAB4-37FB-4BC8-9D00-BBE1EDF5B30F}']
    procedure Draw(Canvas: TCanvas; Rect: TRect; ColorShadow, ColorHighlight: TColor);
    function GetCaption: string;
    function GetVertical: boolean;
    function GetPosition: ISilPositionProperty;
    // properties
    property Caption: string read GetCaption;
    property Vertical: Boolean read GetVertical;
    property Position: ISilPositionProperty read GetPosition;
  end;

  ISilImageProperty = interface
    ['{1C462FE1-7FE0-4FE1-8FB5-14C500E4A4CD}']
    function GetPosition: ISilPositionProperty;
    function GetHasBitmap: boolean;
    function GetHasGlyph: boolean;
    function GetGlyph: TBitmap;
    function GetGlyphCount: integer;
    function GetImageList: TImageList;
    function GetImageIndex: integer;
    function GetHeight: integer;
    function GetWidth: integer;
    procedure Draw(Canvas: TCanvas; Rect: TRect; out OutRect: TRect);
    // properties
    property Position: ISilPositionProperty read GetPosition;
    property HasBitmap: boolean read GetHasBitmap;
    property HasGlyph: boolean read GetHasGlyph;
    property Glyph: TBitmap read GetGlyph;
    property GlyphCount: integer read GetGlyphCount;
    property ImageList: TImageList read GetImageList;
    property ImageIndex: integer read GetImageIndex;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

  ISilGroupingProperty = interface
    ['{5AB58935-09F6-4AB9-B4AE-BBC69E0632F4}']
    function GetAllowAllUp: boolean;
    function GetIndex: integer;
    function GetDown: boolean;
    function GetMaxDown: integer;
    function GetMinDown: integer;
    function GetRect: TRect;
    // properties
    property AllowAllUp: boolean read GetAllowAllUp;
    property Index: Integer read GetIndex;
    property Down: Boolean read GetDown;
    property MaxDown: integer read GetMaxDown;
    property MinDown: integer read GetMinDown;
    property Rect: TRect read GetRect;
  end;

  ISilCustomButton = interface ( ISilControl )
    ['{7FE525B4-0AAB-414A-804F-3826B6B70C40}']
    function GetButtState: TSilButtonState;
    function GetCancel: boolean;
    function GetDrawFocus: boolean;
    function GetIsPressed: boolean;
    function GetDefault: boolean;
    function GetAutoRepeat: Boolean;
    function GetModalResult: TModalResult;
    // properties
    property ButtState: TSilButtonState read GetButtState;
    property Cancel: Boolean read GetCancel;
    property DrawFocus: Boolean read GetDrawFocus;
    property IsPressed: boolean read GetIsPressed;
    property Default: boolean read GetDefault;
  end;

  ISilButton = interface ( ISilCustomButton )
    ['{AA965B02-424D-4381-AFD1-585288A1F459}']
    function GetCurrentBrightColor: TColor;
    function GetCurrentShadowColor: TColor;
    function GetShadowColor: TColor;
    function GetBrightColor: TColor;
    function GetShape: TButtonShape;
    // properties
    property CurrentBrightColor: TColor read GetCurrentBrightColor;
    property CurrentShadowColor: TColor read GetCurrentShadowColor;
    property ShadowColor: TColor read GetShadowColor;
    property BrightColor: TColor read GetBrightColor;
    property Shape: TButtonShape read GetShape;
  end;

type
  ISilImageParams = interface
    ['{16268ABE-F62F-4761-9F78-F5ECF97DE6B9}']
    function SameParams(const Source: IUnknown): boolean;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetWidth(const Value: integer);
    procedure SetHeight(const Value: integer);
    // properties
    property Width: integer read Getwidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

  ISilImageSource = interface
    ['{7CF23D95-89F1-4001-BDA6-EB9FBD344FB0}']
    function GetParams: ISilImageParams;
    procedure GetBitmap(Height, Width: integer; Color, BackgroundColor: TColor;
      Down, Transparent: boolean; TransparentColor: TColor; var Bitmap: TBitmap);
    procedure PaintTo(Height, Width: integer; Color, BackgroundColor: TColor;
      Down, Transparent: boolean; TransparentColor: TColor; const Canvas: TCanvas; Rect: TRect);
    // properties
    property Params: ISilImageParams read GetParams;
  end;

type
  TSil3dShape = ( ksNone, ksEllipse, ksPipe, ksRoundRect, ksHorSwitch, ksRhombus, ksPool );

type
  T2dRelPos = ( rpInside, rpBorder, rpOutside );

  RVector = record
    X: double;
    Y: double;
    Z: double;
  end;

type
  ISil3dImageParams = interface ( ISilImageParams )
    ['{38BB5BDF-B202-43D4-8756-B8242BF31FF0}']
    procedure CheckPoint( const View: RVector; const Down: boolean; const X, Y: double; out Z: double; out RelPos: T2dRelPos; out Light: double );
    function GetAlbedo: double;
    function GetShape: TSil3dShape;
    function GetElevation: integer;
    procedure SetShape(const Value: TSil3dShape);
    procedure SetAlbedo(const Value: double);
    procedure SetElevation(const Value: integer);
    procedure ResetLights;
    procedure AddLight( X, Y, Z: double );
    // properties
    property Shape: TSil3dShape read GetShape write SetShape;
    property Elevation: integer read GetElevation write SetElevation;
    property Albedo: double read GetAlbedo write SetAlbedo;
  end;

  ISil3dImageSource = interface ( ISilImageSource )
    ['{E64BB4DB-EBE8-4E51-B54D-8B74D8E9F123}']
    function GetParams: ISil3dImageParams;
    // properties
    property Params: ISil3dImageParams read GetParams;
  end;

implementation

end.

