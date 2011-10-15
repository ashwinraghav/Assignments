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

unit SilVkControl;

{$I Defines.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls,

  Sil,
  SilViControls,
  SilVtControls,
  SilVmControlProperty,
  SilVkCustomControl;

type
  TSilControl = class
  (
    TSilCustomControl,
    ITimerEvents,
    ISilPropertyOwner,
    ISilControl,       
    ISilControlPainter
  )
  private
    FDrawBuff: TBitmap;
  private
    FCaption: TSilCaptionProperty;
    FImage: TSilImageProperty;
    FGrouping: TSilGroupingProperty;
    FHighlight: TSilHighlightProperty;
    FAutoBackgroundColor: boolean;
    FBackgroundColor: TColor;
  private
    FState: TSilControlState;
  private
    FTimer: ITimer;
    FTick: integer;
    FEvents: IInterfaceList;
    FInterval: integer;
  private
    FFontHandle: HFont;
    FTransparent: boolean;
    FTransparentColor: TColor;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  private
    function GetGroupingEnabled: boolean;
    function GetHighlightProperty: TSilHighlightProperty;
    procedure SetHighlightProperty(const Value: TSilHighlightProperty);
    function GetGlyphCount: integer;
    procedure SetGlyphCount(const Value: integer);
    function StoreImageProperty: Boolean;
    function StoreHighlightProperty: boolean;
    function GetGroupingProperty: TSilGroupingProperty;
    function GetImageProperty: TSilImageProperty;
    function GetCaptionProperty: TSilCaptionProperty;
    procedure SetCaptionProp(const Value: TSilCaptionProperty);
    procedure SetGroupingProperty(const Value: TSilGroupingProperty);
    procedure SetAutoHighlight(const Value: boolean);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlightDelay(const Value: Short);
    procedure SetImageProperty(const Value: TSilImageProperty);
    procedure SetInterval(const Value: integer);
    procedure SetRunning(const Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure SetTransparentColor(const Value: TColor);
    procedure DoCreateFontHandle;
    procedure DoDestroyFontHandle;
    procedure CheckCaption;
    procedure CheckFontHandle;
    procedure CheckGrouping;
    procedure CheckHighlight;
    procedure CheckImage;
    function GetDown: boolean;
    procedure SetDown(const Value: boolean);
    function GetAllowAllUp: Boolean;
    function GetGroupIndex: Integer;
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    procedure DoDrawBitmap(Canvas: TCanvas; const Rect: TRect; const Bitmap: TBitmap);
    procedure SetTextVertical(const Value: Boolean);
    function GetTextVertical: boolean;
    function GetAutoHighlight: boolean;
    function GetHighlightColor: TColor;
    function GetHighlightDelay: Short;
    function GetHighlighted: boolean;
    function GetHighlightLevel: integer;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetAutoBackgroundColor(const Value: boolean);
    function _StoreBackgroundColor: Boolean;
    procedure DoSetBackgroundColor(const Value: TColor);
  private
    procedure DoFireEvents;
    function IndexOfEvent(Method: TNotifyEvent): integer;
    procedure CheckTimer;
    procedure DoRunStop(const Sender: IUnknown; Param: Pointer);
    procedure EvtCheckMouseOver(Sender: TObject);
  private
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  private // Grouping messages
    procedure SMGroupJoin(var Message: TMessage); message SM_GROUPJOIN;
    procedure SMGroupInvalidate(var Message: TMessage); message SM_GROUPCHANGED;
    procedure SMGroupClientsChanged(var Message: TMessage); message SM_GROUPCLIENTSCHANGED;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
  protected
    procedure SetState(const Value: TSilControlState);
  protected // ISilControlPainter
    procedure DoPaint(const Control: ISilControl; Canvas: TCanvas); virtual;
    procedure ISilControlPainter.Paint = DoPaint;
  protected // ITimerEvents
    procedure OnTickEvent(const Event: RTimerEvent);
    procedure ITimerEvents.OnTick = OnTickEvent;
  protected // ISilPropertyOwner
    function GetControl: TControl;
    function GetSilControl: ISilControl;
    procedure PropertyChanged(const Prop: ISilControlProperty); virtual;
  protected // ISilControl
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
  protected
    procedure Paint; override;
  protected
    function DoToggle: boolean;
    function DoGetCurrentColor: TColor; virtual;
    function GetPainter(out Painter: ISilControlPainter): boolean; virtual; 
    procedure AspectChanged; virtual;
    procedure StateChanged(Previous, New: TSilControlState); virtual;
    procedure Stopped; virtual;
    procedure GroupIndexChanged; virtual;
    procedure GroupChanged; virtual;
  protected
    property HighlightLevel: integer read GetHighlightLevel;
    property Tick: integer read GetTick;
    property Running: boolean read GetRunning write SetRunning;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure CopyTo(Canvas: TCanvas);
    procedure AddEvent(Method: TNotifyEvent; Interval, StartInterval: integer);
    procedure RemoveEvent(Method: TNotifyEvent);
  protected
    property HaveFocus: boolean read GetHaveFocus;
    property Color;
    property Interval: integer read GetInterval write SetInterval;
    property Transparent: boolean read GetTransparent write SetTransparent;
    property TransparentColor: TColor read GetTransparentColor write SetTransparentColor;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Highlighted: boolean read GetHighlighted;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor stored _StoreBackgroundColor;
    property AutoBackgroundColor: boolean read FAutoBackgroundColor write SetAutoBackgroundColor;
  protected // Caption properties
    property CaptionProp: TSilCaptionProperty read GetCaptionProperty write SetCaptionProp;
    property TextVertical: Boolean read GetTextVertical write SetTextVertical;
  protected // Highlight properties
    property Highlight: TSilHighlightProperty read GetHighlightProperty write SetHighlightProperty stored StoreHighlightProperty;
    property AutoHighlight: boolean read GetAutoHighlight write SetAutoHighlight default false;
    property HighlightDelay: Short read GetHighlightDelay write SetHighlightDelay stored StoreHighlightProperty;
    property HighlightColor: TColor read GetHighlightColor write SetHighlightColor stored StoreHighlightProperty;
  protected // Grouping properties
    property GroupingEnabled: boolean read GetGroupingEnabled;
    property Grouping: TSilGroupingProperty read GetGroupingProperty write SetGroupingProperty stored GetGroupingEnabled;
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read GetDown write SetDown default False;
  protected // Image properties
    property Image: TSilImageProperty read GetImageProperty write SetImageProperty stored StoreImageProperty;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored StoreImageProperty;
    property GlyphCount: integer read GetGlyphCount write SetGlyphCount stored StoreImageProperty;
  protected
    property CurrentColor: TColor read GetCurrentColor;
    property State: TSilControlState read GetState;
  end;

implementation

uses
  SysUtils,
  SilOiTimer,
  SilLtList, SilBtInt, SilBtColor;

type
  IACEvent = interface
    ['{E1961447-B5D5-4AB4-A23A-564CC84546E1}']
    function GetLastCall: Integer;
    procedure SetLastCall(Value: Integer);
    function GetInterval: Integer;
    function GetMethod: TNotifyEvent;
    property LastCall: Integer read GetLastCall write SetLastCall;
    property Interval: Integer read GetInterval;
    property Method: TNotifyEvent read GetMethod;
  end;

  TSilACEvent = class (TSilObject, IACEvent)
  private
    FLastCall: integer;
    FInterval: integer;
    FMethod: TNotifyEvent;
  protected
    function GetLastCall: Integer;
    procedure SetLastCall(Value: Integer);
    function GetInterval: Integer;
    function GetMethod: TNotifyEvent;
  public
    constructor Create(Interval: integer; Method: TNotifyEvent; LastCall: integer);
  end;

type
  TWinControlFriend = class (TWinControl)
  end;

{ TSilControl }

constructor TSilControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTransparentColor := clFuchsia;
  FInterval := 25;
  FDrawBuff := TBitmap.Create();

  Width := 40;
  Height := 40;
  ParentColor := false;

  FAutoBackgroundColor := true;
end;

destructor TSilControl.Destroy;
begin
  FTimer := nil;
  FEvents := nil;
  
  FreeAndNil(FCaption);
  FreeAndNil(FImage);
  FreeAndNil(FGrouping);
  FreeAndNil(FHighlight);

  FDrawBuff.Free;

  inherited;
end;

procedure TSilControl.SetState(const Value: TSilControlState);
var
  prev: TSilControlState;
begin
  if (FState <> Value) then
  begin
    if not (acsMouseOver in State) and (acsMouseOver in Value) then
    begin
      if Assigned(FHighlight) then
        FHighlight.TargetLevel := 255;

      if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
      AddEvent(EvtCheckMouseOver, 100, 100);
    end;

    if (acsMouseOver in State) and not (acsMouseOver in Value) then
    begin
      if Assigned(FHighlight) then
        FHighlight.TargetLevel := 0;

      if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    end;

    prev := FState;
    FState := Value;
    StateChanged(prev, FState);
  end;
end;

procedure TSilControl.StateChanged(Previous, New: TSilControlState);
begin

end;

procedure TSilControl.OnTickEvent(const Event: RTimerEvent);
begin
  try
    if Sil.Ref.SameObject(Event.Sender, FTimer) then
    begin
      Inc(FTick, FInterval);
      DoFireEvents;
    end;
  except
  end;
end;

procedure TSilControl.DoFireEvents;
var
  enum: IEnumerator;
  evt: IACEvent;
begin
  if Assigned(FEvents) then
    while FEvents.Enumerate(enum, evt) do
      if (FTick - evt.LastCall >= evt.Interval) then
      begin
        evt.LastCall := evt.LastCall + evt.Interval;
        evt.Method(self);

        if (evt.Interval = 0) then
          FEvents.Remove(evt);
      end;
end;

procedure TSilControl.AddEvent(Method: TNotifyEvent; Interval, StartInterval: integer);
var
  ev: IACEvent;
begin
  if (Interval <= 0) then Interval := 1;

  if not Assigned(FEvents) then
    FEvents := Sil.List.InterfaceList;

  RemoveEvent(Method);
  ev := TSilACEvent.Create(Interval, Method, FTick - Interval + StartInterval);
  FEvents.Add(ev);
  CheckTimer;
end;

procedure TSilControl.RemoveEvent(Method: TNotifyEvent);
var
  idx: integer;
begin
  idx := IndexOfEvent(Method);
  if (idx >= 0) then
  begin
    FEvents.Delete(idx);
    CheckTimer;
  end;
end;

procedure TSilControl.CheckTimer;
begin
  Running := Assigned(FEvents) and (FEvents.Count > 0);
end;

procedure TSilControl.SetInterval(const Value: integer);
begin
  if (FInterval <> Value) then
  begin
    FInterval := Value;
    if Assigned(FTimer) then
      FTimer.Interval := FInterval;
  end;
end;

procedure TSilControl.SetRunning(const Value: boolean);
begin
  if (Running <> Value) then
    Sil.Os.Thread.SyncCall( DoRunStop, Pointer( Value ) );
end;

function TSilControl.GetRunning: boolean;
begin
  result := Assigned(FTimer) and FTimer.Enabled;
end;

procedure TSilControl.DoRunStop(const Sender: IUnknown; Param: Pointer);
var
  enum: IEnumerator;
  evt: IACEvent;
begin
  if boolean(Param) then
  begin
    if not Assigned(FTimer) then
      FTimer := Sil.Os.Timer.Create(Cardinal(self), FInterval, self);

    FTimer.Enabled := true;
  end
  else
  begin
    if Assigned(FTimer) then
      FTimer.Enabled := false;

    FTick := 0;
    if Assigned(FEvents) then
      while FEvents.Enumerate(enum, evt) do
        evt.LastCall := 0;

    Stopped;
  end;
end;

procedure TSilControl.Stopped;
begin
end;

function TSilControl.IndexOfEvent(Method: TNotifyEvent): integer;
var
  evt: IACEvent;
begin
  if Assigned(FEvents) then
  begin
    for result := 0 to FEvents.Count - 1 do
    begin
      evt := IACEvent(FEvents[result]);
      if (@evt.Method = @Method) then
        exit;
    end;
  end;

  result := -1;
end;

procedure TSilControl.EvtCheckMouseOver(Sender: TObject);
var
  pnt, ptr: TPoint;
begin
  pnt.X := 0;
  pnt.Y := 0;
  pnt := ClientToScreen(pnt);
  ptr := Mouse.CursorPos;
  if (ptr.X < pnt.X) or (pnt.X + Width < ptr.X) or
    (ptr.Y < pnt.Y) or (pnt.Y + Height < ptr.Y) then
  begin
    RemoveEvent(EvtCheckMouseOver);
    SetState(State - [acsMouseOver]);
  end;
end;

procedure TSilControl.Paint;
var
  painter: ISilControlPainter;
begin
  if Assigned(Parent) and Assigned(FDrawBuff) and Assigned(FDrawBuff.Canvas) then
  begin
    GetPainter(painter);
    if Assigned(painter) then
      try
        if (FDrawBuff.Width <> Width) or
          (FDrawBuff.Height <> Height) then
        begin
          FDrawBuff.Width := Width;
          FDrawBuff.Height := Height;

          AspectChanged;
        end;

        FDrawBuff.Transparent := FTransparent;
        if FTransparent then
        begin
          FDrawBuff.TransparentMode := tmFixed;
          FDrawBuff.TransparentColor := FTransparentColor;
        end;

        painter.Paint(self, FDrawBuff.Canvas);

        DoDrawBitmap(Canvas, ClientRect, FDrawBuff);

      except on ex: Exception do
        begin
          Canvas.Pen.Color := clRed;
          Canvas.Rectangle(0, 0, 2, 2);
        end;
      end;
  end;
end;

procedure TSilControl.DoDrawBitmap(Canvas: TCanvas; const Rect: TRect; const Bitmap: TBitmap);
var
  tmp: TColor;
begin
  if Bitmap.Transparent then
  begin
    tmp := Canvas.Brush.Color;
    Canvas.Brush.Style := bsClear;
    Canvas.Draw(0, 0, Bitmap);
    Canvas.Brush.Color := tmp;
  end
  else
    Canvas.CopyRect(Rect, Bitmap.Canvas, Rect);
end;

procedure TSilControl.CopyTo(Canvas: TCanvas);
begin
  if Assigned(Canvas) and Assigned(FDrawBuff.Canvas) then
    Canvas.CopyRect(Canvas.ClipRect, FDrawBuff.Canvas, FDrawBuff.Canvas.ClipRect);
end;

procedure TSilControl.CMFocusChanged(var Message: TCMFocusChanged);
var
  focus: Boolean;
begin
  with Message do
    focus := (Sender = self) and (Sender is TSilControl);

  if focus then
    SetState(State + [acsFocus]) else
    SetState(State - [acsFocus]);

  inherited;
end;

procedure TSilControl.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FHighlight) then
    FHighlight.ColorChanged(self.Color);
end;

procedure TSilControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  CheckFontHandle;
  Invalidate;
end;

procedure TSilControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSilControl.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSilControl.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Click;
end;

procedure TSilControl.CMMouseEnter(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    SetState(State + [acsMouseOver]);
end;

procedure TSilControl.CMMouseLeave(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    SetState(State - [acsMouseOver]);
end;

procedure TSilControl.WMMouseMove(var Message: TWMMouseMove);
var
  inside: boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
    inside := (0 <= Message.XPos) and (Message.XPos < Width) and
      (0 <= Message.YPos) and (Message.YPos < Height);

    if not (acsMouseOver in State) and inside then
      SetState(State + [acsMouseOver])
    else if (acsMouseOver in State) and not inside then
      SetState(State - [acsMouseOver]);

    inherited;
  end;
end;

procedure TSilControl.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := integer( FTransparent );
end;

function TSilControl.GetShowing: Boolean;
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

procedure TSilControl.SetTransparentColor(const Value: TColor);
begin
  if (FTransparentColor <> Value) then
  begin
    FTransparentColor := Value;
    Invalidate;
  end;
end;

procedure TSilControl.SetTransparent(const Value: boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TSilControl.SetHighlightColor(const Value: TColor);
begin
  CheckHighlight;
  FHighlight.Color := Value;
end;

function TSilControl.GetHighlighted: boolean;
begin
  result := Enabled and AutoHighlight and (acsMouseOver in State);
end;

function TSilControl.GetAutoHighlight: boolean;
begin
  result := Assigned(FHighlight) and FHighlight.Enabled;
end;

procedure TSilControl.SetAutoHighlight(const Value: boolean);
begin
  if Value then
    CheckHighlight;

  if Assigned(FHighlight) then
    FHighlight.Enabled := Value;
end;

procedure TSilControl.DoCreateFontHandle;
var
  LogRec: TLogFont;
begin
  DoDestroyFontHandle;

  GetObject(Font.Handle, SizeOf(TLogFont), @LogRec);
  LogRec.lfEscapement := 900;
  FFontHandle := CreateFontIndirect(LogRec);
end;

procedure TSilControl.DoDestroyFontHandle;
begin
  if (FFontHandle <> 0) then
  begin
    DeleteObject(FFontHandle);
    FFontHandle := 0;
  end;
end;

function TSilControl.GetMouseOver: boolean;
begin
  result := Enabled and (acsMouseOver in State);
end;

function TSilControl.GetCurrentColor: TColor;
begin
  result := DoGetCurrentColor;
end;

function TSilControl.DoGetCurrentColor: TColor;
begin
  result := CalcCurrentColor(Color, HighlightColor);
end;

function TSilControl.GetHighlightDelay: Short;
begin
  if Assigned(FHighlight) then
    result := FHighlight.Delay else
    result := 0;
end;

procedure TSilControl.SetHighlightDelay(const Value: Short);
begin
  CheckHighlight;
  FHighlight.Delay := Value;
end;

function TSilControl.StoreImageProperty: Boolean;
begin
  result := Assigned(FImage) and FImage.MustStore;
end;

function TSilControl.GetGroupingEnabled: boolean;
begin
  result := Assigned(FGrouping) and FGrouping.MustStore;
end;

function TSilControl.StoreHighlightProperty: boolean;
begin
  result := Assigned(FHighlight) and FHighlight.MustStore;
end;

function TSilControl.GetTransparent: boolean;
begin
  result := FTransparent;
end;

function TSilControl.GetTransparentColor: TColor;
begin
  result := FTransparentColor;
end;

function TSilControl.GetInterval: integer;
begin
  result := FInterval;
end;

function TSilControl.GetTick: integer;
begin
  result := FTick; 
end;

function TSilControl.GetState: TSilControlState;
begin
  result := FState;
end;

function TSilControl.GetHighlightColor: TColor;
begin
  if Assigned(FHighlight) then
    result := FHighlight.Color else
    result := clBlack;
end;

function TSilControl.CalcCurrentColor(Color, HighlightColor: TColor): TColor;
begin
  if Assigned(FHighlight) then
    result := FHighlight.CalcCurrentColor(Color, HighlightColor) else
    result := Color;

  if GetDown then
  begin
    if Assigned( Grouping ) and ( Grouping.DownColor > 0 ) then
      result := Grouping.DownColor else
      result := Sil.Rgb.Darker(result, 40);
  end;
end;

function TSilControl.GetHighlightLevel: integer;
begin
  if Assigned(FHighlight) then
    result := FHighlight.Level else
    result := 0;
end;

function TSilControl.GetCaptionProp: ISilCaptionProperty;
begin
  result := FCaption;
end;

function TSilControl.GetCaptionProperty: TSilCaptionProperty;
begin
  CheckCaption;
  result := FCaption;
end;

procedure TSilControl.SetCaptionProp(const Value: TSilCaptionProperty);
begin
  CheckCaption;
  FCaption.Assign(Value);
end;

procedure TSilControl.CheckCaption;
begin
  if not Assigned(FCaption) then
  begin
    FCaption := TSilCaptionProperty.Create(self);
    FCaption.Position.Position := rpCenter;
    FCaption.Position.Margin := 2;
  end;
end;

procedure TSilControl.CheckImage;
begin
  if not Assigned(FImage) then
  begin
    FImage := TSilImageProperty.Create(self);
    FImage.Position.Position := rpCenterLeft;
    FImage.Position.Margin := 5;
  end;
end;

function TSilControl.GetFontHandle: HFont;
begin
  result := FFontHandle;
end;

function TSilControl.GetHaveFocus: boolean;
begin
  result := Enabled and (acsFocus in State);
end;

function TSilControl.GetImageProperty: TSilImageProperty;
begin
  CheckImage;
  result := FImage;
end;

procedure TSilControl.SetImageProperty(const Value: TSilImageProperty);
begin
  CheckImage;
  FImage.Assign(Value);
  Invalidate;
end;

function TSilControl.GetGroupingProperty: TSilGroupingProperty;
begin
  CheckGrouping;
  result := FGrouping;
end;

procedure TSilControl.SetGroupingProperty(const Value: TSilGroupingProperty);
begin
  CheckGrouping;
  FGrouping.Assign(Value);
end;

function TSilControl.GetHighlightProperty: TSilHighlightProperty;
begin
  CheckHighlight;
  result := FHighlight;
end;

procedure TSilControl.SetHighlightProperty(const Value: TSilHighlightProperty);
begin
  CheckHighlight;
  FHighlight.Assign(Value);
end;

procedure TSilControl.CheckGrouping;
begin
  if not Assigned(FGrouping) then
    FGrouping := TSilGroupingProperty.Create(self);
end;

procedure TSilControl.CheckHighlight;
begin
  if not Assigned(FHighlight) then
  begin
    FHighlight := TSilHighlightProperty.Create(self);
    FHighlight.ColorChanged(self.Color);
  end;
end;

procedure TSilControl.PropertyChanged(const Prop: ISilControlProperty);
begin
  if Sil.Ref.SameObject(Prop, FCaption) then
    CheckFontHandle;

  Invalidate;
end;

procedure TSilControl.CheckFontHandle;
begin
  if Assigned(FCaption) and FCaption.Vertical then
    DoCreateFontHandle else
    DoDestroyFontHandle;
end;

procedure TSilControl.SMGroupJoin(var Message: TMessage);
begin
  if Assigned(FGrouping) then
    FGrouping.CheckJoin(Message);
end;

procedure TSilControl.SMGroupInvalidate(var Message: TMessage);
begin
  Invalidate;
end;

procedure TSilControl.SMGroupClientsChanged(var Message: TMessage);
begin
  GroupChanged;
end;

function TSilControl.GetControl: TControl;
begin
  result := self;
end;

function TSilControl.GetSilControl: ISilControl;
begin
  result := self;
end;

function TSilControl.GetGrouping: ISilGroupingProperty;
begin
  result := FGrouping;
end;

function TSilControl.GetHighlight: ISilHighlightProperty;
begin
  result := FHighlight;
end;

function TSilControl.GetImage: ISilImageProperty;
begin
  result := FImage;
end;

function TSilControl.GetDown: boolean;
begin
  if Assigned( FGrouping ) then
    result := Grouping.Down else
    result := false;
end;

function TSilControl.GetAllowAllUp: Boolean;
begin
  if Assigned( FGrouping ) then
    result := Grouping.AllowAllUp else
    result := false;
end;

function TSilControl.GetGroupIndex: Integer;
begin
  if Assigned( FGrouping ) then
    result := Grouping.Index else
    result := 0;
end;

procedure TSilControl.SetDown(const Value: boolean);
begin
  Grouping.Down := Value;
end;

procedure TSilControl.SetAllowAllUp(const Value: Boolean);
begin
  Grouping.AllowAllUp := Value;
end;

procedure TSilControl.SetGroupIndex(const Value: Integer);
begin
  Grouping.Index := Value;
  GroupIndexChanged;
end;

function TSilControl.GetGlyph: TBitmap;
begin
  result := Image.Glyph;
end;

function TSilControl.GetGlyphCount: integer;
begin
  result := Image.GlyphCount;
end;

procedure TSilControl.SetGlyph(const Value: TBitmap);
begin
  Image.Glyph := Value;
end;

procedure TSilControl.SetGlyphCount(const Value: integer);
begin
  Image.GlyphCount := Value;
end;

procedure TSilControl.AspectChanged;
begin

end;

procedure TSilControl.SetTextVertical(const Value: Boolean);
begin
  CaptionProp.Vertical := Value;
end;

function TSilControl.GetTextVertical: boolean;
begin
  result := CaptionProp.Vertical;
end;

procedure TSilControl.CMParentColorChanged(var Message: TMessage);
begin
  inherited;

  if FAutoBackgroundColor then
  begin
    if (Message.wParam <> 0) then
      DoSetBackgroundColor(TColor(Message.lParam)) else
      DoSetBackgroundColor(TWinControlFriend(Parent).Color);
  end;
end;

procedure TSilControl.CMInvalidate(var Message: TMessage);
begin
  inherited;

  if Assigned( FGrouping ) then
    FGrouping.InvalidateRect;
end;

function TSilControl._StoreBackgroundColor: Boolean;
begin
  result := not FAutoBackgroundColor;
end;

procedure TSilControl.SetAutoBackgroundColor(const Value: boolean);
begin
  if (FAutoBackgroundColor <> Value) then
  begin
    FAutoBackgroundColor := Value;
    if FAutoBackgroundColor and Assigned(Parent) then
      DoSetBackgroundColor(TWinControlFriend(Parent).Color);
  end;
end;

procedure TSilControl.SetBackgroundColor(const Value: TColor);
begin
  DoSetBackgroundColor(Value);
  FAutoBackgroundColor := false;
end;

procedure TSilControl.DoSetBackgroundColor(const Value: TColor);
begin
  if (FBackgroundColor <> Value) then
  begin
    FBackgroundColor := Value;
    Invalidate;
  end;
end;


function TSilControl.DoToggle: boolean;
var
  prev: boolean;
begin
  if GroupingEnabled then
  begin
    prev := Down;
    Down := not prev;
    result := ( Down = not prev );
  end
  else
    result := false;
end;

procedure TSilControl.GroupIndexChanged;
begin

end;

procedure TSilControl.GroupChanged;
begin

end;

function TSilControl.GetPainter(out Painter: ISilControlPainter): boolean;
begin
  Painter := self;
  result := true;
end;

procedure TSilControl.DoPaint(const Control: ISilControl; Canvas: TCanvas);
begin

end;

{ TSilACEvent }

constructor TSilACEvent.Create(Interval: integer; Method: TNotifyEvent; LastCall: integer);
begin
  inherited Create;

  FInterval := Interval;
  FMethod := Method;
  FLastCall := LastCall;
end;

function TSilACEvent.GetInterval: Integer;
begin
  Result := FInterval;
end;

function TSilACEvent.GetLastCall: Integer;
begin
  Result := FLastCall;
end;

function TSilACEvent.GetMethod: TNotifyEvent;
begin
  Result := FMethod;
end;

procedure TSilACEvent.SetLastCall(Value: Integer);
begin
  FLastCall := Value;
end;

end.

