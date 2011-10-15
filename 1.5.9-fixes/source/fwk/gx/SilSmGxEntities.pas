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

unit SilSmGxEntities;

interface

uses
  Sil,
  Windows, Classes, Graphics, Controls,
  SilSiGx, SilSmGxEntity;

type
  TGxDot = class
  (
    TGxEntity,
    IGxDot
  )
  private
    FPen: IGxPen;
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
  protected // IGxDot
    function GetPen: IGxPen;
    // properties
    property Pen: IGxPen read GetPen;
  end;

  TGxLine = class
  (
    TGxEntity,
    IGxLine
  )
  private
    FPen: IGxPen;
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
    function DoGetPointReference(X, Y, NearDelta: double): TPointReference; override;
  protected // IGxLine
    function GetPen: IGxPen;
    // properties
    property Pen: IGxPen read GetPen;
  public
    constructor Create; overload; override; 
    constructor Create(Width, Height: double); reintroduce; overload;
  end;

  TGxRectangle = class
  (
    TGxLine,
    IGxRectangle
  )
  private
    FBrush: IGxBrush;
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
    function DoGetPointReference(X, Y, NearDelta: double): TPointReference; override;
  protected // IGxRectangle
    function GetBrush: IGxBrush;
    // properties
    property Brush: IGxBrush read GetBrush;
  end;

  TGxEllipse = class
  (
    TGxRectangle,
    IGxEllipse
  )
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
  protected // IGxEllipse
  public
    constructor Create( Radious: double ); overload;
  end;

  TGxText = class
  (
    TGxRectangle,
    IGxText
  )
  private
    FText: string;
    FFont: IGxFont;
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
  protected // IGxText
    function GetText: string;
    function GetFont: IGxFont;
    procedure SetText(const Value: string);
    // properties
    property Text: string read GetText write SetText;
    property Font: IGxFont read GetFont;
  public
    constructor Create( Width, Height: double ); overload;
  end;

  TGxImage = class
  (
    TGxRectangle,
    IGxImage
  )
  private
    FGraphic: IGxGraphic;
  protected
    procedure DoPaintAt(const Canvas: IGxCanvas; X, Y: double); override;
  protected // IGxImage
    function GetGraphic: IGxGraphic;
    procedure SetGraphic(const Value: IGxGraphic);
    // properties
    property Graphic: IGxGraphic read GetGraphic write SetGraphic;
  public
    constructor Create( Width, Height: double; VclImage: TGraphic ); overload;
  end;

  TGxEntityBuilder = class
  (
    TGxEntity,
    IGxEntityBuilder
  )
  protected // IGxEntityBuilder
    function NewDot: IGxDot;
    function NewLine( Width, Height: double ): IGxLine;
    function NewRectangle( Width, Height: double ): IGxRectangle;
    function NewEllipse: IGxEllipse;
    function NewCircle( Radious: double ): IGxEllipse;
    function NewImage( Width, Height: double; VclImage: TGraphic ): IGxImage;
    function NewBlock: IGxEntityList;
  end;

implementation

uses
  SilSmGxPen, SilSmGxCanvas;

{ TGxDot }

procedure TGxDot.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  inherited;

  if Assigned( FPen ) and not FPen.UseParentPen then
    Canvas.Pen.Assign( FPen.VclPen );

  Canvas.MoveTo( X, Y );
end;

function TGxDot.GetPen: IGxPen;
begin
  result := DoCheckPen( FPen );
end;

{ TGxLine }

constructor TGxLine.Create;
begin
  inherited Create;
end;

constructor TGxLine.Create(Width, Height: double);
begin
  inherited Create;

  self.Width := Width;
  self.Height := Height;
end;

function TGxLine.DoGetPointReference(X, Y, NearDelta: double): TPointReference;
begin
  (*) FALTA IMPLEMENTAR (*)
  result := prUnknown;
end;

procedure TGxLine.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  inherited;
  
  if Assigned( FPen ) and not FPen.UseParentPen then
    Canvas.Pen.Assign( FPen.VclPen );

  Canvas.MoveTo( X, Y );
  Canvas.LineTo( X + Width, Y + Height );
end;

function TGxLine.GetPen: IGxPen;
begin
  result := DoCheckPen( FPen );
end;

{ TGxRectangle }

function TGxRectangle.DoGetPointReference(X, Y, NearDelta: double): TPointReference;
begin
  if
    ( Left <= X ) and ( X <= Right ) and
    ( Top <= Y ) and ( Y <= Bottom ) then
  begin
    if ( ( X = Left ) or ( X = Right ) ) and ( ( Y = Top ) or ( Y = Bottom ) ) then
      result := prBorder else
      result := prInside;
  end
  else if ( NearDelta > 0 ) and
    ( Left - NearDelta <= X ) and ( X <= Right + NearDelta ) and
    ( Top - NearDelta <= Y ) and ( Y <= Bottom + NearDelta ) then
    result := prNear
  else
    result := prOutside;
end;

procedure TGxRectangle.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  if Assigned( FPen ) and not FPen.UseParentPen then
    Canvas.Pen.Assign( FPen.VclPen ) else
    Canvas.Pen.Style := psClear;

  if Assigned( FBrush ) and not FBrush.UseParentBrush then
    Canvas.Brush.Assign( FBrush.VclBrush );

  Canvas.Rectangle( X, Y, X + Width, Y + Height );
end;

function TGxRectangle.GetBrush: IGxBrush;
begin
  result := DoCheckBrush( FBrush );
end;

{ TGxEllipse }

constructor TGxEllipse.Create(Radious: double);
begin
  inherited Create;
  Width := Radious;
  Height := Radious;
end;

procedure TGxEllipse.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  if Assigned( FPen ) and not FPen.UseParentPen then
    Canvas.Pen.Assign( FPen.VclPen );

  if Assigned( FBrush ) and not FBrush.UseParentBrush then
    Canvas.Brush.Assign( FBrush.VclBrush );

  Canvas.Ellipse( X, Y, X + Width, Y + Height );
end;

{ TGxText }

constructor TGxText.Create(Width, Height: double);
begin
  inherited Create( Width, Height ); 
end;

procedure TGxText.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  if Assigned( FFont ) and not FFont.UseParentFont then
    Canvas.Font.Assign( FFont.VclFont );

  Canvas.TextOut( X, Y, FText );
end;

function TGxText.GetFont: IGxFont;
begin
  result := FFont;
end;

function TGxText.GetText: string;
begin
  result := FText;
end;

procedure TGxText.SetText(const Value: string);
begin
  FText := Value;
end;

{ TGxImage }

constructor TGxImage.Create(Width, Height: double; VclImage: TGraphic);
begin
  inherited Create( Width, Height );
  FGraphic := TGxGraphic.Create( VclImage );
end;

procedure TGxImage.DoPaintAt(const Canvas: IGxCanvas; X, Y: double);
begin
  if Assigned( FGraphic ) then
    Canvas.StretchDraw( X, Y, X + Width{ - 1}, Y + Height {- 1}, FGraphic );
end;

function TGxImage.GetGraphic: IGxGraphic;
begin
  result := FGraphic;
end;

procedure TGxImage.SetGraphic(const Value: IGxGraphic);
begin
  FGraphic := Value;
end;

{ TGxEntityBuilder }

function TGxEntityBuilder.NewBlock: IGxEntityList;
begin
  result := TGxEntity.Create;
  Add( result );
end;

function TGxEntityBuilder.NewCircle(Radious: double): IGxEllipse;
begin
  result := TGxEllipse.Create( Radious );
  Add( result );
end;

function TGxEntityBuilder.NewDot: IGxDot;
begin
  result := TGxDot.Create;
  Add( result );
end;

function TGxEntityBuilder.NewEllipse: IGxEllipse;
begin
  result := TGxEllipse.Create;
  Add( result );
end;

function TGxEntityBuilder.NewImage(Width, Height: double; VclImage: TGraphic): IGxImage;
begin
  result := TGxImage.Create( Width, Height, VclImage );
  Add( result );
end;

function TGxEntityBuilder.NewLine(Width, Height: double): IGxLine;
begin
  result := TGxLine.Create( Width, Height );
  Add( result );
end;

function TGxEntityBuilder.NewRectangle(Width, Height: double): IGxRectangle;
begin
  result := TGxRectangle.Create( Width, Height );
  Add( result );
end;

end.

