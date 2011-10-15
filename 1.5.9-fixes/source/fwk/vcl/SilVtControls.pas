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

unit SilVtControls;

{$INCLUDE Defines.inc}

interface

uses
  SilViControls,
  Windows, Controls, Graphics, Classes;

const
  kDrawStyle = DT_EXPANDTABS or DT_WORDBREAK or DT_CENTER;

type
  ControlTool = class
    class procedure GetPosition(Rect: TRect; Margin: integer; RelativePosition: TSilRelativePosition; Height, Width: integer; out Left, Top: integer);
    class function ButtonPainter: ISilControlPainter;
    class function ImageProperty( const Owner: ISilPropertyOwner): ISilImageProperty;
    class function CaptionProperty( const Owner: ISilPropertyOwner): ISilCaptionProperty;
    class procedure DoDrawText(const Control: ISilControl; Canvas: TCanvas; var Rect: TRect; Flags: Word;
      ColorShadow, ColorHighlight: TColor; Vertical: boolean);
  end;

implementation

uses
  SilVmControlPainter,
  SilVmControlProperty;

{ ControlTool }

class function ControlTool.ButtonPainter: ISilControlPainter;
begin
  result := TSilButtonPainter.Create;
end;

class function ControlTool.CaptionProperty(const Owner: ISilPropertyOwner): ISilCaptionProperty;
begin
  result := TSilCaptionProperty.Create( Owner );
end;

class function ControlTool.ImageProperty(const Owner: ISilPropertyOwner): ISilImageProperty;
begin
  result := TSilImageProperty.Create( Owner );
end;

class procedure ControlTool.GetPosition(Rect: TRect; Margin: integer;
  RelativePosition: TSilRelativePosition;
  Height, Width: integer; out Left, Top: integer);
begin
  case RelativePosition of
  rpTopLeft, rpTopCenter, rpTopRight: Top := Rect.Top + Margin;
  rpCenterLeft, rpCenter, rpCenterRight: Top := Rect.Top + ( 1 + Rect.Bottom - Rect.Top - Height ) div 2;
  rpBottomLeft, rpBottomCenter, rpBottomRight: Top := Rect.Bottom - Height - Margin;
  end;

  case RelativePosition of
  rpTopLeft, rpCenterLeft, rpBottomLeft: Left := Rect.Left + Margin;
  rpTopCenter, rpCenter, rpBottomCenter: Left := Rect.Left + ( 1 + Rect.Right - Rect.Left - Width ) div 2;
  rpTopRight, rpCenterRight, rpBottomRight: Left := Rect.Right - Width - Margin;
  end;
end;

class procedure ControlTool.DoDrawText(const Control: ISilControl; Canvas: TCanvas; var Rect: TRect; Flags: Word;
  ColorShadow, ColorHighlight: TColor; Vertical: boolean);
var
  OldFont: HFont;
  Hdc: THandle;
  Size: TSize;
  txt: string;
begin
  Hdc := Canvas.Handle;

  txt := Control.CaptionProp.Caption;
  if ( Flags and DT_CALCRECT <> 0 ) and ( ( Length( txt ) = 0 ) or
    ( txt[ 1 ] = '&' ) and ( txt[ 2 ] = #0 ) ) then txt := txt + ' ';

  if Vertical then
  begin
    OldFont := SelectObject( Hdc, Control.FontHandle );
    if Flags and DT_CALCRECT > 0 then
    begin
      GetTextExtentPoint32( Hdc, PChar( txt ), Length( txt ), Size );
      Rect := Classes.Rect( Rect.Left, Rect.Top + Size.cx, Rect.Left + size.cy, Rect.Top );
    end
    else
      TextOut( Hdc, Rect.Left, Rect.Top, PChar( txt ), Length( txt ) );
  end
  else
  begin
    OldFont := 0;
    Canvas.Font := Control.Font;
    if not Control.Enabled then
    begin
      OffsetRect( Rect, 1, 1 );
      Canvas.Font.Color := ColorHighLight;
      Windows.DrawText( Canvas.Handle, PChar( txt ), Length( txt ), Rect, Flags );
      OffsetRect( Rect, -1, -1 );
      Canvas.Font.Color := ColorShadow;
      Windows.DrawText( Canvas.Handle, PChar( txt ), Length( txt ), Rect, Flags );
    end
    else
      Windows.DrawText( Canvas.Handle, PChar( txt ), Length( txt ), Rect, Flags );
  end;

  if Vertical then
    SelectObject( Hdc, OldFont );
end;

end.
