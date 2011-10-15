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

unit SilSmGxView;

interface

uses
  Sil,
  SilSiGx;

type
  TSilGxView = class
  (
    TSilObject,
    ISilGxView
  )
  protected
    function DoTranslateXY( X, Y: double; out Left, Top: integer ): boolean; virtual;
  protected // ISilGxView
    function TranslateXY( X, Y: double; out Left, Top: integer ): boolean;
  end;

  TSilGxChainView = class
  (
    TSilGxView
  )
  private
    FView1: ISilGxView;
    FView2: ISilGxView;
  protected
    function DoTranslateXY( X, Y: double; out Left, Top: integer ): boolean; override;
  public
    constructor Create( const View1, View2: ISilGxView ); reintroduce;
    destructor Destroy; override;
  end;

  TSilGxView2D = class
  (
    TSilGxView,
    ISilGxView2D
  )
  private
    FEntities: ISilGxEntityList;
    FScale: double;
    FWidth: double;
    FHeight: double;
    FFocusX: double;
    FFocusY: double;
  protected
    procedure FireViewChanged;
    procedure FireScaleChanged;
  protected
    function DoTranslateXY(X, Y: double; out Left, Top: integer): boolean; override;
  protected // ISilGxView2D
    function GetEntities: ISilGxEntityList;
    function GetScale: double;
    function GetWidth: double;
    function GetHeight: double;
    function GetFocusX: double;
    function GetFocusY: double;
    procedure SetScale(Value: double);
    procedure SetWidth(Value: double);
    procedure SetHeight(Value: double);
    procedure SetFocusX(Value: double);
    procedure SetFocusY(Value: double);
    procedure PaintTo(const Canvas: ISilGxCanvas);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SilSmGxEntities;

{ TSilGxView }

function TSilGxView.TranslateXY(X, Y: double; out Left, Top: integer): boolean;
begin
  result := DoTranslateXY( X, Y, Left, Top );
end;

function TSilGxView.DoTranslateXY(X, Y: double; out Left, Top: integer): boolean;
begin
  result := false;
end;

{ TSilGxChainView }

constructor TSilGxChainView.Create(const View1, View2: ISilGxView);
begin
  FView1 := View1;
  FView2 := View2;
end;

destructor TSilGxChainView.Destroy;
begin
  FView1 := nil;
  FView2 := nil;
  inherited;
end;

function TSilGxChainView.DoTranslateXY(X, Y: double; out Left, Top: integer): boolean;
begin
  result := FView1.TranslateXY( X, Y, Left, Top );
  X := Left;
  Y := Top;
  result := result and FView2.TranslateXY( X, Y, Left, Top );
end;

{ TSilGxView2D }

constructor TSilGxView2D.Create;
begin
  inherited Create;
  FEntities := TSilGxEntityList.Create;
end;

destructor TSilGxView2D.Destroy;
begin
  FEntities := nil;
  inherited;
end;

procedure TSilGxView2D.PaintTo(const Canvas: ISilGxCanvas);
var
  enum: IEnumerator;
  entity: ISilGxEntity;
begin
  while FEntities.Enumerate( enum, entity ) do
    entity.PaintTo( self, Canvas );
end;

function TSilGxView2D.GetEntities: ISilGxEntityList;
begin
  result := FEntities;
end;

function TSilGxView2D.GetHeight: double;
begin
  result := FHeight;
end;

function TSilGxView2D.GetWidth: double;
begin
  result := FWidth;
end;

function TSilGxView2D.GetFocusX: double;
begin
  result := FFocusX;
end;

function TSilGxView2D.GetFocusY: double;
begin
  result := FFocusY;
end;

function TSilGxView2D.GetScale: double;
begin
  result := FScale;
end;

procedure TSilGxView2D.SetHeight(Value: double);
begin
  if ( FHeight <> Value ) then
  begin
    FHeight := Value;
    FireViewChanged;
  end;
end;

procedure TSilGxView2D.SetWidth(Value: double);
begin
  if ( FWidth <> Value ) then
  begin
    FWidth := Value;
    FireViewChanged;
  end;
end;

procedure TSilGxView2D.SetFocusX(Value: double);
begin
  if ( FFocusX <> Value ) then
  begin
    FFocusX := Value;
    FireViewChanged;
  end;
end;

procedure TSilGxView2D.SetFocusY(Value: double);
begin
  if ( FFocusY <> Value ) then
  begin
    FFocusY := Value;
    FireViewChanged;
  end;
end;

procedure TSilGxView2D.SetScale(Value: double);
begin
  if ( FScale <> Value ) then
  begin
    FScale := Value;
    FireScaleChanged;
  end;
end;

function TSilGxView2D.DoTranslateXY(X, Y: double; out Left, Top: integer): boolean;
begin
  Left := Round( FFocusX - FWidth / 2 + FScale * X );
  Top := Round( FFocusY + FHeight / 2 - FScale * Y );
  result := true;
end;

procedure TSilGxView2D.FireViewChanged;
begin

end;

procedure TSilGxView2D.FireScaleChanged;
begin

end;

end.

