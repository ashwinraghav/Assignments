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

unit SilSmGxPen;

interface

uses
  Sil,
  Windows, Graphics, SysUtils,
  SilSiGx;

type
  TGxPen = class
  (
    TSilObject,
    IGxPen
  )
  private
    FUseParentPen: boolean;
    FVclPen: TPen;
  protected
    procedure FirePenChanged; virtual;
  protected // IGxPen
    function GetUseParentPen: boolean;
    function GetVclPen: TPen;
    procedure SetUseParentPen(Value: boolean);
    procedure SetVclPen(const Value: TPen);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TGxBrush = class
  (
    TSilObject,
    IGxBrush
  )
  private
    FUseParentBrush: boolean;
    FVclBrush: TBrush;
  protected
    procedure FireBrushChanged; virtual;
  protected // IGxBrush
    function GetUseParentBrush: boolean;
    function GetVclBrush: TBrush;
    procedure SetUseParentBrush(Value: boolean);
    procedure SetVclBrush(const Value: TBrush);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TGxFont = class
  (
    TSilObject,
    IGxFont
  )
  private
    FUseParentFont: boolean;
    FVclFont: TFont;
  protected
    procedure FireFontChanged;
  protected // IGxFont
    function GetUseParentFont: boolean;
    function GetVclFont: TFont;
    procedure SetUseParentFont(Value: boolean);
    procedure SetVclFont(const Value: TFont);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TGxGraphic = class
  (
    TSilObject,
    IGxGraphic
  )
  private
    FVclGraphic: TGraphic;
    FStretchedBitmap: TBitmap;
  protected
    procedure FireChanged;
  protected // IGxGraphic
    function GetVclGraphic: TGraphic;
    procedure SetVclGraphic(Value: TGraphic);
    function GetVclStretched( Width, Height: integer; out Bitmap: TBitmap ): boolean;
  public
    constructor Create( Graphic: TGraphic ); reintroduce;
    destructor Destroy; override;
  end;


implementation

{ TGxPen }

constructor TGxPen.Create;
begin
  inherited Create;
  FVclPen := TPen.Create;
end;

destructor TGxPen.Destroy;
begin
  FVclPen.Free;
  inherited;
end;

function TGxPen.GetUseParentPen: boolean;
begin
  result := FUseParentPen;
end;

function TGxPen.GetVclPen: TPen;
begin
  result := FVclPen;
end;

procedure TGxPen.SetUseParentPen(Value: boolean);
begin
  if ( FUseParentPen <> Value ) then
  begin
    FUseParentPen := Value;
  end;
end;

procedure TGxPen.SetVclPen(const Value: TPen);
begin
  FVclPen.Assign( Value );
end;

procedure TGxPen.FirePenChanged;
begin

end;

{ TGxBrush }

constructor TGxBrush.Create;
begin
  inherited Create;
  FVclBrush := TBrush.Create;
end;

destructor TGxBrush.Destroy;
begin
  FVclBrush.Free;
  inherited;
end;

function TGxBrush.GetUseParentBrush: boolean;
begin
  result := FUseParentBrush;
end;

function TGxBrush.GetVclBrush: TBrush;
begin
  result := FVclBrush;
end;

procedure TGxBrush.SetUseParentBrush(Value: boolean);
begin
  if ( FUseParentBrush <> Value ) then
  begin
    FUseParentBrush := Value;
    FireBrushChanged;
  end;
end;

procedure TGxBrush.SetVclBrush(const Value: TBrush);
begin
  FVclBrush.Assign( Value );
  FireBrushChanged;
end;

procedure TGxBrush.FireBrushChanged;
begin

end;

{ TGxFont }

constructor TGxFont.Create;
begin
  inherited Create;
  FVclFont := TFont.Create; 
end;

destructor TGxFont.Destroy;
begin
  FVclFont.Create;
  inherited;
end;

function TGxFont.GetUseParentFont: boolean;
begin
  result := FUseParentFont;
end;

function TGxFont.GetVclFont: TFont;
begin
  result := FVclFont;
end;

procedure TGxFont.SetUseParentFont(Value: boolean);
begin
  if ( FUseParentFont <> Value ) then
  begin
    FUseParentFont := Value;
    FireFontChanged;
  end;
end;

procedure TGxFont.SetVclFont(const Value: TFont);
begin
  FVclFont.Assign( Value );
  FireFontChanged;
end;

procedure TGxFont.FireFontChanged;
begin

end;

{ TGxGraphic }

constructor TGxGraphic.Create( Graphic: TGraphic );
begin
  inherited Create;
  FVclGraphic := Graphic;
end;

destructor TGxGraphic.Destroy;
begin
  FVclGraphic := nil;
  FreeAndNil( FStretchedBitmap );
  inherited;
end;

procedure TGxGraphic.FireChanged;
begin

end;

function TGxGraphic.GetVclGraphic: TGraphic;
begin
  result := FVclGraphic;
end;

procedure TGxGraphic.SetVclGraphic(Value: TGraphic);
begin
  FreeAndNil( FStretchedBitmap );
  FVclGraphic := Value;
  FireChanged;
end;

function TGxGraphic.GetVclStretched(Width, Height: integer; out Bitmap: TBitmap): boolean;
var
  rect: TRect;
begin
  if Assigned( FVclGraphic ) and (
    not Assigned( FStretchedBitmap ) or
    ( FStretchedBitmap.Width <> Width ) or
    ( FStretchedBitmap.Height <> Height ) ) then
  begin
    if not Assigned( FStretchedBitmap ) then
      FStretchedBitmap := TBitmap.Create;
      
    FStretchedBitmap.Width := Width;
    FStretchedBitmap.Height := Height;
    rect.Left := 0;
    rect.Top := 0;
    rect.Right := Width;
    rect.Bottom := Height;
    FStretchedBitmap.Canvas.StretchDraw( rect, FVclGraphic );
  end;

  Bitmap := FStretchedBitmap;
  result := Assigned( Bitmap );
end;

end.

