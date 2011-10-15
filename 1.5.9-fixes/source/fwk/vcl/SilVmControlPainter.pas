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

unit SilVmControlPainter;

interface

uses
  Sil,
  SilViControls,
  Classes, Windows, Controls, TypInfo, Graphics;

type
  TSilControlPainter = class
  (
    TSilInterfacedObject,
    ISilControlPainter
  )
  protected // ISilControlPainter
    procedure Paint(const Control: ISilControl; Canvas: TCanvas); virtual; abstract;
  end;

  TSilButtonPainter = class
  (
    TSilControlPainter
  )
  protected
    procedure PaintButton( const Button: ISilButton; Canvas: TCanvas );
  protected
    procedure Paint(const Control: ISilControl; Canvas: TCanvas); override;
  end;

implementation

uses
  SilVtControls;
  
{ TSilButtonPainter }

procedure TSilButtonPainter.Paint(const Control: ISilControl; Canvas: TCanvas);
var
  button: ISilButton;
begin
  inherited;

  if Sil.Ref.GetInterface( Control, ISilButton, button ) then
  begin
    PaintButton( button, Canvas );
  end;
end;

procedure TSilButtonPainter.PaintButton(const Button: ISilButton; Canvas: TCanvas);

  procedure DrwRectangle( Left, Top, Right, Bottom, Thickness: integer );
  begin
    while true do
    begin
      Canvas.Rectangle( Left, Top, Right + 1, Bottom + 1 );
      Dec( Thickness );
      if ( Thickness <= 0 ) then break;
      Inc( Left );
      Inc( Top );
      Dec( Right );
      Dec( Bottom );
    end;
  end;

var
  bPrs: Byte;
  rect, txtrect: TRect;
  cface, cshadow, chighlight: TColor;
  off, shadow: integer;
begin
  Canvas.Font := Button.Font;

  with Canvas, Button do
  begin
    cface := CurrentColor;
    chighlight := CurrentBrightColor;
    cshadow := CurrentShadowColor;

    Brush.Style := bsSolid;
    Pen.Width := 1;
    Brush.Color := cface;
    FillRect( Bounds );
    bPrs := Ord( IsPressed );

    { Dibuja el marco exterior }
    case Shape of
    bsLowered:
      begin
        off := 2;
        shadow := 2;

        Pen.Color := cshadow;
        MoveTo( Width - 1, 0 );
        LineTo( 0, 0 );
        LineTo( 0, Height - 1 );

        Pen.Color := chighlight;
        MoveTo( 0, Height - 1 );
        LineTo( Width - 1, Height - 1 );
        LineTo( Width - 1, -1 );

        Pen.Color := clBlack;
        MoveTo( 1, Height - 3 );
        LineTo( 1, 1 );
        LineTo( Width - 2, 1 );
      end;
    bsStandard:
      begin
        if HaveFocus or Default then
        begin
          off := 1;
          Pen.Color := clBlack;
          DrwRectangle( 0, 0, Width - 1, Height - 1, 1 );
        end
        else
          off := 0;

        shadow := 1;
      end;
    bsFlat:
      begin
        if Default or Assigned( Button.VclControl ) and 
          ( csDesigning in Button.VclControl.ComponentState ) then
        begin
          off := 1;
          Pen.Color := clBlack;
          DrwRectangle( 0, 0, Width - 1, Height - 1, 1 );
        end
        else
          off := 0;

        if MouseOver or ( HaveFocus and not DrawFocus ) then
          shadow := 1 else
          shadow := 0;
      end;
    else
      begin
        shadow := 0;
        off := 0;
      end;
    end;

    { Dibuja la sombra }
    if IsPressed then
    begin
      Pen.Color := clBlack;
      MoveTo( off, Height - off - 1 );
      LineTo( off, off );
      LineTo( Width - off, off );

      Pen.Color := cshadow;
      DrwRectangle( off + 1, off + 1, Width - off - 1, Height - off - 1, shadow );
    end
    else
      case Shape of
      bsLowered:
        begin
          Pen.Color := clBlack;
          MoveTo( off, Height - off - 1 );
          LineTo( Width - off - 1, Height - off - 1 );
          LineTo( Width - off - 1, off - 1 );

          Pen.Color := cshadow;
          MoveTo( off + 1, Height - off - 2 );
          LineTo( Width - off - 2, Height - off - 2 );
          LineTo( Width - off - 2, off );
          MoveTo( off + 2, Height - off - 3 );
          LineTo( Width - off - 3, Height - off - 3 );
          LineTo( Width - off - 3, off + 1 );

          Pen.Color := chighlight;
          MoveTo( off + 2, Height - off - 2 - 2 );
          LineTo( off + 2, off + 2 );
          LineTo( Width - off - 1 - 2, off + 2 );
        end;
      else
        if ( shadow > 0 ) then
        begin
          Pen.Color := clBlack;
          MoveTo( off, Height - off - 1 );
          LineTo( Width - off - 1, Height - off - 1 );
          LineTo( Width - off - 1, off - 1 );

          Pen.Color := cshadow;
          MoveTo( off + 1, Height - off - 2 );
          LineTo( Width - off - 2, Height - off - 2 );
          LineTo( Width - off - 2, off );

          Pen.Color := chighlight;
          MoveTo( off, Height - off - 2 );
          LineTo( off, off );
          LineTo( Width - off - 1, off );
        end;
      end;

    case Shape of
    bsLowered: rect := Classes.Bounds( off + 4 + bPrs, off + 4 + bPrs, Width - off - 10, Height - off - 10 );
    else       rect := Classes.Bounds( 1 + 4 + bPrs, 1 + 4 + bPrs, Width - 1 - 10, Height - 1 - 10 );
    end;

    Brush.Color := cface;

    { Dibuja el bitmap }
    txtrect := rect;
    if Assigned( Button.Image ) then
      with Button.Image do
        if HasBitmap then
          Draw( Canvas, rect, txtrect );

    { Dibuja el foco }
    if HaveFocus and DrawFocus then
    begin
      Brush.Style := bsSolid;
      FrameRect( rect );
      DrawFocusRect( rect );
    end;

    { Dibuja el texto }
    Brush.Style := bsClear;
    Button.CaptionProp.Draw( Canvas, txtrect, cshadow, chighlight );
  end;
end;

end.

