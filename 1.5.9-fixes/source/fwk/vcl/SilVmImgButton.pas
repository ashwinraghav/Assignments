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

unit SilVmImgButton;

interface

{$I Defines.inc}

uses
  Sil,
  SilViControls,
  SilVkCustomControl,
  SilVmCustomButton,
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TSilCustomImageButton = class;
  
  TSilCustomImageButton = class
  (
    TSilCustomButton,
    ISilControlPainter
  )
  private
    FImageSource: ISilImageSource;
  private
    procedure SetImageSource(const Value: ISilImageSource);
  protected // ISilControlPainter
    procedure PainterPaint(const Control: ISilControl; Canvas: TCanvas);
    procedure ISilControlPainter.Paint = PainterPaint;
  protected
    function GetPainter(out Painter: ISilControlPainter): boolean; override; 
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  public
    property ImageSource: ISilImageSource read FImageSource write SetImageSource;
  end;

  TSilImageButton = class ( TSilCustomImageButton )
  published
    property AutoBackgroundColor;
    property BackgroundColor;      
    property Transparent;
    property TransparentColor;
  published
    property AutoRepeat;
    property Cancel;
    property TextVertical;
    property DrawFocus;
    property Enabled;
    property ModalResult;
  published
    property Image;
    property Highlight;
    property Grouping;
    property Color;
    property OnMouseEnter;
    property OnMouseLeave;
  published
    property Anchors;
    property Align;
    property Caption;
    property Default;
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

implementation

{ TSilCustomImageButton }

constructor TSilCustomImageButton.Create(AOwner: TComponent);
begin
  inherited;

  Color := clWhite;
end;

destructor TSilCustomImageButton.Destroy;
begin
  FImageSource := nil;

  inherited;
end;

function TSilCustomImageButton.GetPainter(out Painter: ISilControlPainter): boolean;
begin
  Painter := self;
  result := true;
end;

procedure TSilCustomImageButton.PainterPaint(const Control: ISilControl; Canvas: TCanvas);
var
  dwn: integer;
  rect, txtrect: TRect;
begin                                   
  if Assigned( FImageSource ) then
    FImageSource.PaintTo( Height, Width, CurrentColor, BackgroundColor,
      IsPressed, Transparent, TransparentColor, Canvas, ClientRect );

  if IsPressed then dwn := 1 else dwn := 0;
  rect := Classes.Bounds( dwn, dwn, Width - 1, Height - 1 );

  Canvas.Brush.Color := BackgroundColor;
  Canvas.Brush.Style := bsClear;

  txtrect := rect;
  if Image.HasBitmap then
    Image.Draw( Canvas, rect, txtrect );

  CaptionProp.Draw( Canvas, txtrect, clGray, clWhite );
end;

procedure TSilCustomImageButton.SetImageSource(const Value: ISilImageSource);
begin
  FImageSource := Value;
  Invalidate;
end;

end.

