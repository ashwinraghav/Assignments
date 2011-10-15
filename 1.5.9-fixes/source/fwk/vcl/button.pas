{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
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

unit button;

interface

{$I Defines.inc}

uses
  Sil,
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TButtonExLayout = (blTop, blBottom, blLeft, blRight, blCenter);
  TGlyphCount = 1..2;

  TButtonEx = class(TCustomControl)
  private
    FActive: Boolean;
    FLayout: TButtonExLayout;
    FPressed: Boolean;
    FHaveFocus: Boolean;
    FDefault: Boolean;
    FCancel: Boolean;
    FColorFace: TColor;
    FColorShadow: TColor;
    FColorHighlight: TColor;
    FModalResult: TModalResult;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FAutoRepeat: Boolean;
    FRepeatTimer: TTimer;
    FGlyph: TBitmap;
    FBuffer: TBitmap;
    FGlyphCount: TGlyphCount;
    FCentered: Boolean;
    FMargin: Integer;
    FTextVertical: Boolean;
    FDrawFocus: Boolean;
    FFontHandle: HFont;
    FAutoHighlight: Boolean;
    FInside: Boolean;
    FHighlighted: Boolean;
    FColors: array[0..2] of TColor;
    procedure DoDrawText(var Rect: TRect; Flags: Word);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure TimerExpired(Sender: TObject);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphCount(Value: TGlyphCount);
    procedure SetMargin(const Value: Integer);
    function GetGlyphRect(var GlyphMet: TPoint): TRect;
    procedure SetCentered(const Value: Boolean);
    function GetShowing: Boolean;
    function IsColorStored: Boolean;
    procedure SetTextVertical(const Value: Boolean);
    procedure SetDrawFocus(const Value: Boolean);
    procedure DoAutoHighlight;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftSTate); override;
    procedure KeyUp(var Key: Word; Shift: TShiftSTate); override;
    procedure Click; override;
    procedure DblClick; override;
    procedure SetColorFace(c: TColor);
    procedure SetColorShadow(c: TColor);
    procedure SetColorHighlight(c: TColor);
    procedure SetDefault(l: Boolean);
    procedure SetButtonStyle(blnDefecto, blnFoco: Boolean); virtual;
    procedure SetEnabled(l: Boolean); {$IFDEF D40} override; {$ENDIF}
    procedure SetLayout(Value: TButtonExLayout);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsPressed: Boolean read FPressed;
  published
    property AutoHighlight: Boolean read FAutoHighlight write FAutoHighlight;
    property Color;
    property Spacing: Integer read FMargin write SetMargin;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphCount: TGlyphCount read FGlyphCount write SetGlyphCount default 1;
    property Centered: Boolean read FCentered write SetCentered;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat;
    property Anchors;
    property Align;
    property Cancel: Boolean read FCancel write FCancel default false;
    property TextVertical: Boolean read FTextVertical write SetTextVertical;
    property DrawFocus: Boolean read FDrawFocus write SetDrawFocus default true;
    property Caption;
    property ColorFace: TColor read FColorFace write SetColorFace stored IsColorStored;
    property ColorShadow: TColor read FColorShadow write SetColorShadow stored IsColorStored;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight stored IsColorStored;
    property Layout: TButtonExLayout read FLayout write SetLayout default blRight;
    property Default: Boolean read FDefault write SetDefault default false;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property DragCursor;
    property DragMode;
    property Enabled write SetEnabled default true;
    property Font;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
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

procedure Register;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

implementation

constructor TButtonEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable];
  FFontHandle := 0;
  FDrawFocus := true;

  Width := 98;
  Height := 20;

  TabStop := true;
  FCentered := true;

  FMargin := 4;

  FColorFace := clBtnFace;
  FColorShadow := clBtnShadow;
  FColorHighlight := clBtnHighLight;
  FLayout := blRight;

  FActive := FDefault;
  FGlyph := TBitmap.Create;
  FBuffer := TBitmap.Create;
  FGlyphCount := 1;

  FColors[0] := FColorFace;
  FColors[1] := FColorHighlight;
  FColors[2] := FColorShadow;
end;

destructor TButtonEx.Destroy;
begin
  if FRepeatTimer <> nil then FRepeatTimer.Free;
  FGlyph.Free;
  FBuffer.Free;
  if FFontHandle <> 0 then DeleteObject(FFontHandle);
  inherited Destroy;
end;

procedure TButtonEx.DoDrawText(var Rect: TRect; Flags: Word);
var
  Text: String;
  OldFont: HFont;
  Hdc: THandle;
  Size: TSize;
begin
  Text := Caption;
  Hdc := FBuffer.Canvas.Handle;

  if (Flags and DT_CALCRECT <> 0) and ((Length(Text) = 0) or
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';

  if not FTextVertical then
  begin
    OldFont := 0;
    FBuffer.Canvas.Font := Font;
  end else
    OldFont := SelectObject(Hdc, FFontHandle);

  if FTextVertical then
  begin
    GetTextExtentPoint32(Hdc, PChar(Text), Length(Text), Size);
    if Flags and DT_CALCRECT > 0 then
      Rect := Classes.Rect(Rect.Left, Rect.Top + Size.cx, Rect.Left + Size.cy * 2, Rect.Top) else
      TextOut(Hdc, (Width - Size.cy) div 2, Rect.Top, PChar(Text), Length(Text));
  end else
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    FBuffer.Canvas.Font.Color := FColorHighlight;
    DrawText(FBuffer.Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    FBuffer.Canvas.Font.Color := FColorShadow;
    DrawText(FBuffer.Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end else
    DrawText(FBuffer.Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);

  if FTextVertical then
    SelectObject(Hdc, OldFont);
end;

procedure TButtonEx.Paint;
var
  bPrs: Byte;
  CentRect, GlyphRect, TempRect, DrawRect, CalcRect: TRect;
  DrawStyle: Integer;
  GlyphMet: TPoint;
begin
  if Parent = nil then exit;
  FBuffer.Canvas.Font := Font;
  DrawStyle := 0;

  if Width <> FBuffer.Width then FBuffer.Width := Width;
  if Height <> FBuffer.Height then FBuffer.Height := Height;

  with FBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Width := 1;
    Brush.Color := FColorFace;
    FillRect(ClientRect);

    {if FActive then
      Font.Style := Font.Style + [fsBold] else
      Font.Style := Font.Style - [fsBold];}

    if FPressed then
    begin
      Pen.Color := clBlack;
      MoveTo(Width - 3, 2);
      LineTo(2, 2);
      LineTo(2, Height - 3);

      Pen.Color := FColorShadow;
      Rectangle(3, 3, Width - 2, Height - 2);
      Rectangle(4, 4, Width - 3, Height - 3);
    end else
    begin
      Pen.Color := clBlack;
      Rectangle(1, 1, Width - 2, Height - 2);
    end;

    Pen.Color := FColorShadow;//clBtnShadow;
    MoveTo(Width - 1, 0);
    LineTo(0, 0);
    LineTo(0, Height - 1);

    if not FPressed then
    begin
      Pen.Color := FColorShadow;
      MoveTo(3, Height - 4);
      LineTo(Width - 4, Height - 4);
      LineTo(Width - 4, 2);
      MoveTo(4, Height - 5);
      LineTo(Width - 5, Height - 5);
      LineTo(Width - 5, 3);
    end;

    Pen.Color := FColorHighlight;
    MoveTo(0, Height - 1);
    LineTo(0 + Width - 1, Height - 1);
    LineTo(0 + Width - 1, -1);

    bPrs := Ord(FPressed);

    if FPressed then
    begin
      Pen.Color := clBlack;
      MoveTo(1, Height - 3);
      LineTo(1, 1);
      LineTo(Width - 2, 1);
      MoveTo(2, Height - 3);
      LineTo(2, 2);
      LineTo(Width - 2, 2);
    end else
    begin
      MoveTo(4, Height - 6);
      LineTo(4, 4);
      LineTo(Width - 5, 4);
    end;

    DrawRect := Classes.Bounds(6 + bPrs, 6 + bPrs, Width - 12, Height - 12);
    Brush.Color := FColorFace;
    FillRect(DrawRect);

    if not FGlyph.Empty then
    begin
      TempRect := GetGlyphRect(GlyphMet);
      GlyphRect := Sil.Rect.Make(0, 0, GlyphMet.x, GlyphMet.y);

      case FLayout of
        blTop:
        begin
          if not FCentered then OffsetRect(GlyphRect, DrawRect.Left + (DrawRect.Right - DrawRect.Left - GlyphMet.x) div 2, DrawRect.Bottom - GlyphMet.y - FMargin);
          DrawRect.Bottom := DrawRect.Bottom - FGlyph.Height - FMargin * 2;
        end;
        blBottom:
        begin
          if not FCentered then OffsetRect(GlyphRect, DrawRect.Left + (DrawRect.Right - DrawRect.Left - GlyphMet.x) div 2, FMargin + DrawRect.Top);
          DrawRect.Top := DrawRect.Top + FGlyph.Height + FMargin * 2;
        end;
        blLeft:
        begin
          if not FCentered then OffsetRect(GlyphRect, DrawRect.Right - FMargin - GlyphMet.x, DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - GlyphMet.y) div 2);
          DrawRect.Right := DrawRect.Right - FGlyph.Width div FGlyphCount - FMargin * 2;
        end;
        blRight:
        begin
          if not FCentered then OffsetRect(GlyphRect, FMargin + DrawRect.Left, DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - GlyphMet.y) div 2);
          DrawRect.Left := DrawRect.Left + FGlyph.Width div FGlyphCount + FMargin * 2;
        end;
        blCenter:
        begin
          OffsetRect(GlyphRect,
            DrawRect.Left + (DrawRect.Right - DrawRect.Left - GlyphMet.x) div 2,
            DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - GlyphMet.y) div 2);
        end;
      end;
    end;

    Brush.Color := FColorFace;

    if Length(Caption) > 0 then
    begin
      Brush.Style := bsClear;
      DrawStyle := DT_EXPANDTABS or DT_WORDBREAK or DT_CENTER;

      CalcRect := DrawRect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      OffsetRect(DrawRect, 0, (DrawRect.Bottom - DrawRect.Top) div 2 - (CalcRect.Bottom - CalcRect.Top) div 2);
    end;

    if not FCentered then
    begin
      if not FGlyph.Empty then BrushCopy(GlyphRect, FGlyph, TempRect, FGlyph.TransparentColor);
      if Length(Caption) > 0 then DoDrawText(DrawRect, DrawStyle);
    end else
    begin
      if not FGlyph.Empty then
      begin
        OffsetRect(CalcRect, -CalcRect.Left, -CalcRect.Top);

        case FLayout of
          blLeft, blRight:
          begin
            CentRect := Classes.Rect(0, 0, GlyphRect.Right + CalcRect.Right + FMargin, Int.Max(GlyphRect.Bottom, CalcRect.Bottom));
            OffsetRect(CentRect, (Width - CentRect.Right) div 2 + bPrs + 1, (Height - CentRect.Bottom) div 2 + bPrs);
            if FLayout = blLeft then
              OffsetRect(GlyphRect, CentRect.Right - GlyphMet.x, (CentRect.Top + CentRect.Bottom - GlyphRect.Bottom) div 2) else
              OffsetRect(GlyphRect, CentRect.Left, (CentRect.Top + CentRect.Bottom - GlyphRect.Bottom) div 2);
          end;
          blTop, blBottom:
          begin
            CentRect := Classes.Rect(0, 0, Int.Max(GlyphRect.Right, CalcRect.Right), GlyphRect.Bottom + CalcRect.Bottom + FMargin);
            OffsetRect(CentRect, (Width - CentRect.Right) div 2 + bPrs, (Height - CentRect.Bottom) div 2 + bPrs + 1);
            if FLayout = blTop then
              OffsetRect(GlyphRect, (CentRect.Left + CentRect.Right - GlyphRect.Right) div 2, CentRect.Bottom - GlyphMet.y) else
              OffsetRect(GlyphRect, (CentRect.Left + CentRect.Right - GlyphRect.Right) div 2, CentRect.Top);
          end;
        end;

        BrushCopy(GlyphRect, FGlyph, TempRect, FGlyph.TransparentColor);
      end;

      if Length(Caption) > 0 then DoDrawText(DrawRect, DrawStyle);
    end;

    if FHaveFocus and FDrawFocus then
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColorFace;

      DrawRect := Classes.Bounds(6 + bPrs, 6 + bPrs, Width - 12, Height - 12);
      FrameRect(DrawRect);
      DrawFocusRect(DrawRect);
    end;
  end;

  Canvas.Draw(0, 0, FBuffer);
  inherited Paint;
end;

function TButtonEx.GetGlyphRect(var GlyphMet: TPoint): TRect;
begin
  if not FGlyph.Empty then
  begin
    GlyphMet.x := FGlyph.Width div FGlyphCount;
    GlyphMet.y := FGlyph.Height;
    if Enabled or (FGlyphCount = 1) then
      Result := Classes.Rect(0, 0, GlyphMet.x, GlyphMet.y) else
      Result := Classes.Rect(GlyphMet.x, 0, GlyphMet.x * 2, GlyphMet.y);
  end else
    Result := Classes.Rect(0, 0, 0, 0);
end;

procedure TButtonEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
    FPressed := true;

    if IsWindow(WindowHandle) and
       IsWindowVisible(WindowHandle) and
       IsWindowEnabled(WindowHandle) then
      SetFocus;

    FHaveFocus := true;

    MouseCapture := true;
    Invalidate;
    inherited MouseDown(Button, Shift, X, Y);

    if FAutoRepeat then
    begin
      inherited Click;
      if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Interval := InitRepeatPause;
      FRepeatTimer.Enabled  := True;
    end;
  end;
end;

procedure TButtonEx.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if FPressed and MouseCapture then
  begin
    try
      inherited Click;
    except
      FRepeatTimer.Enabled := false;
      raise;
    end;
  end;
end;

procedure TButtonEx.Click;
var
  Form: TForm;
begin
  Form := TForm(GetParentForm(Self));
  if Form <> nil then Form.ModalResult := FModalResult;

  if FPressed then
  begin
    MouseCapture := false;
    FPressed := false;
    Invalidate;
  end;

  inherited Click;
end;

procedure TButtonEx.DblClick;
begin
  Click;
end;

procedure TButtonEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FRepeatTimer <> nil then FRepeatTimer.Enabled := false;

  if FPressed then
  begin
    MouseCapture := false;
    FPressed := false;
    Invalidate;
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TButtonEx.MouseMove(Shift: TShiftState;X, Y: Integer);
var
  b: Boolean;
begin
  if MouseCapture then
  begin
    b := FPressed;
    FPressed := (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height);
    if b <> FPressed then Invalidate;

    inherited MouseMove(Shift, X, Y);
  end;
end;

procedure TButtonEx.KeyDown(var Key: Word; Shift: TShiftSTate);
var
  b: Boolean;
begin
  if Key = vk_Space then
  begin
    b := FPressed;
    FPressed := true;
    if b <> FPressed then Invalidate;

    inherited KeyDown(Key, Shift);
  end;
end;

procedure TButtonEx.KeyUp(var Key: Word; Shift: TShiftSTate);
var
  b: Boolean;
begin
  if Key = vk_Space then
  begin
    b := FPressed;
    FPressed := false;
    if b <> FPressed then Invalidate;

    Click;
    //inherited Click;
  end;
end;

procedure TButtonEx.SetColorFace(c: TColor);
begin
  FColorFace := c;
  FColors[0] := FColorFace;
  Invalidate;
end;

procedure TButtonEx.SetColorShadow(c: TColor);
begin
  FColorShadow := c;
  FColors[2] := FColorShadow;
  Invalidate;
end;

procedure TButtonEx.SetColorHighlight(c: TColor);
begin
  FColorHighlight := c;
  FColors[1] := FColorHighlight;
  Invalidate;
end;

procedure TButtonEx.SetEnabled(l: Boolean);
begin
  if l = Enabled then Exit;
  if FDefault and (not l) then Default := false;

  if not l then
  begin
    FPressed := false;
    FHaveFocus := false;

    if FAutoHighlight and FHighlighted then
    begin
      FHighlighted := false;
      FColorFace := FColors[0];
      FColorHighlight := FColors[1];
      FColorShadow := FColors[2];
    end;
  end;

  {$IFDEF D40}
  inherited SetEnabled(l);
  {$ELSE}
  inherited Enabled := l;
  {$ENDIF}
  Invalidate;
end;

procedure TButtonEx.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

function TButtonEx.GetShowing: Boolean;
var
  Control: TControl;
begin
  Control := Self;
  while (Control.Parent <> nil) do
  begin
    Control := Control.Parent;
    if not Control.Visible  or not Control.Enabled then Break;
  end;
  Result := Control.Visible and Control.Enabled;
end;

procedure TButtonEx.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if Enabled and Visible and GetShowing and (((CharCode = VK_RETURN) and (FDefault or FHaveFocus)) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TButtonEx.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if Enabled and Visible and GetShowing and IsAccel(CharCode, Caption) then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TButtonEx.CMFocusChanged(var Message: TCMFocusChanged);
var
  blnBold, blnFoco: Boolean;
begin
  with Message do
    if Sender is TButtonEx then
    begin
      blnBold := (Sender = Self);
      blnFoco := FHaveFocus <> blnBold;
      FHaveFocus := blnBold;
    end else
    begin
      blnBold := FDefault;
      blnFoco := FHaveFocus <> false;
      FHaveFocus := false;
    end;

  SetButtonStyle(blnBold, blnFoco);
  inherited;
end;

procedure TButtonEx.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Click;
end;

procedure TButtonEx.DoAutoHighlight;
begin
  if FAutoHighlight and FInside then
  begin
    FHighlighted := true;
    FColors[0] := FColorFace;
    FColors[1] := FColorHighlight;
    FColors[2] := FColorShadow;

    FColorFace := Sil.Rgb.Brighter(FColorFace, 30);
    FColorHighlight := Sil.Rgb.Brighter(FColorFace, 30);
    FColorShadow := Sil.Rgb.Darker(FColorFace, 30);
    Invalidate;
  end;
end;

procedure TButtonEx.CMMouseEnter(var Message: TMessage);
begin
  if csDesigning in ComponentState then Exit;

  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
  FInside := true;
  DoAutoHighlight;
end;

procedure TButtonEx.CMMouseLeave(var Message: TMessage);
begin
  if csDesigning in ComponentState then Exit;

  FInside := false;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  if FAutoHighlight then
  begin
    FHighlighted := false;
    FColorFace := FColors[0];
    FColorHighlight := FColors[1];
    FColorShadow := FColors[2];
    Invalidate;
  end;
end;

procedure TButtonEx.SetDefault(l: Boolean);
var
  intPos, i: integer;
  ControlForm : TControl;
begin
  intPos := -1;

  ControlForm := Self;
  while ControlForm.Parent <> nil do ControlForm := ControlForm.Parent;
  if not ( ControlForm is TCustomForm ) then ControlForm := nil;

  if l and ( ControlForm<>nil ) then
    with ControlForm as TForm do

    begin
      if not (ActiveControl is TButtonEx) then
        for i := 0 to ControlCount - 1 do
          if (Controls[i] is TButtonEx) then
            if (Controls[i] as TButtonEx).Default then
              intPos := i;

      if (intPos > -1) then
      begin
        (Controls[intPos] as TButtonEx).Default := false;
        (Controls[intPos] as TButtonEx).Invalidate;
      end;
    end;

  FDefault := l;
  FActive := l;
  Invalidate;
end;

procedure TButtonEx.SetButtonStyle(blnDefecto, blnFoco: Boolean);
begin
  if HandleAllocated then
  begin
    if (FActive <> blnDefecto) or blnFoco then
    begin
      FActive := blnDefecto;
      Invalidate;
    end;
  end;
end;

procedure TButtonEx.SetLayout(Value: TButtonExLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TButtonEx.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := LRESULT(false);
end;

procedure TButtonEx.SetGlyph(const Value: TBitmap);
var
  iGlyphs: Integer;
begin
  Invalidate;
  FGlyph.Assign(Value);

  if (Value <> nil) and (Value.Height > 0) then
  begin
    FGlyph.TransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      iGlyphs := Value.Width div Value.Height;
      if iGlyphs > 4 then iGlyphs := 1;
      SetGlyphCount(iGlyphs);
    end;
  end;
end;

procedure TButtonEx.SetGlyphCount(Value: TGlyphCount);
begin
  if (Value <> FGlyphCount) and (Value > 0) then
  begin
    Invalidate;
    FGlyphCount := Value;
    Invalidate;
  end;
end;

procedure TButtonEx.SetMargin(const Value: Integer);
begin
  FMargin := Value;
  Invalidate;
end;

procedure TButtonEx.SetCentered(const Value: Boolean);
begin
  FCentered := Value;
  Invalidate;
end;

procedure TButtonEx.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FColorFace := inherited Color;
  FColorHighlight := Sil.Rgb.Brighter(FColorFace, 30);
  FColorShadow := Sil.Rgb.Darker(FColorFace, 30);

  DoAutoHighlight;
end;

function TButtonEx.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

procedure TButtonEx.SetTextVertical(const Value: Boolean);
var
  LogRec: TLogFont;
begin
  if FTextVertical = Value then Exit;
  FTextVertical := Value;

  if FTextVertical then
  begin
    GetObject(Font.Handle, SizeOf(TLogFont), @LogRec);
    LogRec.lfEscapement := 900;
    FFontHandle := CreateFontIndirect(LogRec);
  end else
  if FFontHandle <> 0 then
  begin
    DeleteObject(FFontHandle);
    FFontHandle := 0;
  end;

  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Mariano', [TButtonEx]);
end;

procedure TButtonEx.SetDrawFocus(const Value: Boolean);
begin
  FDrawFocus := Value;
end;

end.
