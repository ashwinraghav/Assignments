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

unit SilVmButton;

interface

{$I Defines.inc}

uses
  Sil, SilVkCustomControl, SilVmCustomButton, SilViControls,
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TButtonExLayout = ( blTop, blBottom, blLeft, blRight, blCenter );

  TSilButton = class
  (
    TSilCustomButton,
    ISilButton
  )
  private
    FShape: TButtonShape;
    FShadowColor: TColor;
    FBrightColor: TColor;
  private
    function GetCentered: Boolean;
    function GetLayout: TButtonExLayout;
    function GetMargin: Integer;
  private
    function GetColorFace: TColor;
    function GetColorHighlight: TColor;
    function GetColorShadow: TColor;
    function IsColorStored: Boolean;
    procedure SetMargin(const Value: Integer);
    procedure SetCentered(const Value: Boolean);
    procedure SetLayout(Value: TButtonExLayout);
    procedure SetShape(const Value: TButtonShape);
    procedure SetColorFace(Color: TColor);
    procedure SetColorShadow(Color: TColor);
    procedure SetColorHighlight(Color: TColor);
  protected // ISilButton
    function GetCurrentBrightColor: TColor;
    function GetCurrentShadowColor: TColor;
    function GetShadowColor: TColor;
    function GetBrightColor: TColor;
    function GetShape: TButtonShape;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Layout: TButtonExLayout read GetLayout write SetLayout default blRight;
    property Glyph;
    property GlyphCount;
    property Centered: Boolean read GetCentered write SetCentered;
    property Spacing: Integer read GetMargin write SetMargin;
    property Shape: TButtonShape read GetShape write SetShape;
    property ColorFace: TColor read GetColorFace write SetColorFace stored IsColorStored;
    property ColorShadow: TColor read GetColorShadow write SetColorShadow stored IsColorStored;
    property ColorHighlight: TColor read GetColorHighlight write SetColorHighlight stored IsColorStored;
  published
    property AutoRepeat;
    property Cancel;
    property TextVertical;
    property DrawFocus;
    property Enabled;
    property ModalResult;
  published
    property AutoHighlight;
    property HighlightDelay;
    property Color;
    property OnMouseEnter;
    property OnMouseLeave;
  published
    property Anchors;
    property Align;
    property Caption;
    property Default;
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentFont;
    property ParentColor;
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

  TSilAnimatedButton = class ( TSilButton ) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};


implementation

{ TSilButton }

constructor TSilButton.Create(AOwner: TComponent);
begin
  inherited;

  Centered := true;
  Layout := blRight;
  //Glyph := TBitmap.Create;
  Spacing := 4;
  Shape := SilViControls.bsLowered;
  Color := clBtnFace;
  ColorHighLight := clBtnHighLight;
  ColorShadow := clBtnShadow;
end;

destructor TSilButton.Destroy;
begin
  Glyph := nil;
  inherited;
end;

procedure TSilButton.SetMargin(const Value: Integer);
begin
  CaptionProp.Position.Margin := Value;
  Image.Position.Margin := Value;
end;

function TSilButton.GetCentered: Boolean;
begin
  result := ( Image.Position.Position = rpCenter );
end;

procedure TSilButton.SetCentered(const Value: Boolean);
begin
  if Value then
    Image.Position.Position := rpCenter else
    Image.Position.Position := rpCenterLeft;
end;

function TSilButton.GetLayout: TButtonExLayout;
begin
  case Image.Position.Position of
  rpTopCenter: result := blBottom;
  rpBottomCenter: result := blTop;
  rpCenterLeft: result := blRight;
  rpCenterRight: result := blLeft;
  rpCenter: result := blCenter;
  else result := blCenter;
  end;
end;

procedure TSilButton.SetLayout(Value: TButtonExLayout);
begin
  case Value of
  blTop: Image.Position.Position := rpBottomCenter;
  blBottom: Image.Position.Position := rpTopCenter;
  blLeft: Image.Position.Position := rpCenterRight;
  blRight: Image.Position.Position := rpCenterLeft;
  blCenter: Image.Position.Position := rpCenter;
  else Image.Position.Position := rpCenterLeft;
  end;
  CaptionProp.Position.Position := rpCenter;
end;

procedure TSilButton.SetShape(const Value: TButtonShape);
begin
  if ( FShape <> Value ) then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

procedure TSilButton.SetColorFace(Color: TColor);
  procedure UpdColor( var Dst: TColor; Prev, New: TColor );
  begin
    if ( Dst = Prev ) then
      Dst := New;
  end;

  procedure UpdBrighter( var Dst: TColor; Prev, New: TColor );
  begin
    UpdColor( Dst, Sil.Rgb.Brighter( Prev, 100 ), Sil.Rgb.Brighter( New, 100 ) );
  end;

  procedure UpdDarker( var Dst: TColor; Prev, New: TColor );
  begin
    UpdColor( Dst, Sil.Rgb.Darker( prev, 60 ), Sil.Rgb.Darker( New, 60 ) );
  end;
var
  prev: TColor;
begin
  if ( self.Color <> Color ) then
  begin
    prev := self.Color;
    self.Color := Color;
    UpdBrighter( FBrightColor, prev, Color );
    UpdDarker( FShadowColor, prev, Color );
    Invalidate;
  end;
end;

procedure TSilButton.SetColorShadow(Color: TColor);
begin
  if ( FShadowColor <> Color ) then
  begin
    FShadowColor := Color;
    Invalidate;
  end;
end;

procedure TSilButton.SetColorHighlight(Color: TColor);
begin
  if ( FBrightColor <> Color ) then
  begin
    FBrightColor := Color;
    Invalidate;
  end;
end;

function TSilButton.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

function TSilButton.GetColorFace: TColor;
begin
  result := Color;
end;

function TSilButton.GetColorHighlight: TColor;
begin
  result := FBrightColor;
end;

function TSilButton.GetColorShadow: TColor;
begin
  result := FShadowColor;
end;

function TSilButton.GetMargin: Integer;
begin
  result := Image.Position.Margin;
end;

function TSilButton.GetShape: TButtonShape;
begin
  result := FShape;
end;

function TSilButton.GetBrightColor: TColor;
begin
  result := FBrightColor;
end;

function TSilButton.GetCurrentBrightColor: TColor;
begin
  result := CalcCurrentColor( FBrightColor, Sil.Rgb.Brighter( HighlightColor, 100 ) );
end;

function TSilButton.GetCurrentShadowColor: TColor;
begin
  result := CalcCurrentColor( FShadowColor, Sil.Rgb.Darker( HighlightColor, 60 ) );
end;

function TSilButton.GetShadowColor: TColor;
begin
  result := FShadowColor;
end;

end.

