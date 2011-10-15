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

unit SilVmControlProperty;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilViControls,
  Windows, Classes, Controls, Graphics, Messages;

const
  kDefaultHighlightDelay = 150;
  kHighlightTimerInterval = 25;
  
type
  TSilControlProperty = class;
  TSilPositionProperty = class;
  TSilCaptionProperty = class;
  TSilImageProperty = class;
  TSilGroupingProperty = class;
  TSilHighlightProperty = class;

  TSilGroup = class;

  TSilControlProperty = class
  (
    TPersistent,
    IUnknown,
    ISilControlProperty,
    ISilPropertyOwner
  )
  private
    FOwner: ISilPropertyOwner;
  private
    procedure Changed;
  protected // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected // ISilPropertyOwner
    function GetVclControl: TControl;
    function GetSilControl: ISilControl;
    procedure PropertyChanged(const Prop: ISilControlProperty);
  protected // ISilControlProperty
  protected
    property VclControl: TControl read GetVclControl;
    property SilControl: ISilControl read GetSilControl;
  public
    constructor Create(const Owner: ISilPropertyOwner); reintroduce; virtual;
  end;

  TSilPositionProperty = class
  (
    TSilControlProperty,
    ISilPositionProperty
  )
  private
    FPosition: TSilRelativePosition;
    FLeft: integer;
    FTop: integer;
    FMargin: integer;
  private
    FClientRect: TRect;
    FWidth: integer;
    FHeight: integer;
  private
    procedure SetPosition(const Value: TSilRelativePosition);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetMargin(const Value: integer);
    procedure AdjustPosition(Rect: TRect; const Height, Width: integer); overload;
    procedure AdjustPosition; overload;
    function _StorePosition: boolean;
  protected // ISilPositionProperty
    function GetPosition: TSilRelativePosition;
    function GetLeft: integer;
    function GetTop: integer;
    function GetMargin: integer;
  public
    constructor Create(const Owner: ISilPropertyOwner); override;
  published
    property Position: TSilRelativePosition read FPosition write SetPosition;
    property Left: integer read FLeft write SetLeft stored _StorePosition;
    property Top: integer read FTop write SetTop stored _StorePosition;
    property Margin: integer read FMargin write SetMargin;
  end;

  TSilCaptionProperty = class
  (
    TSilControlProperty,
    ISilCaptionProperty
  )
  private
    FPosition: TSilPositionProperty;
    FVertical: boolean;
    FWidth: integer;
    FHeight: integer;
  private
    procedure SetCaption(const Value: string);
    procedure SetVertical(const Value: boolean);
    procedure SetPosition(const Value: TSilPositionProperty);
    procedure AdjustPosition(Rect: TRect; Canvas: TCanvas);
  public // ISilCaptionProperty
    procedure Draw(Canvas: TCanvas; Rect: TRect; ColorShadow, ColorHighlight: TColor);
    function GetCaption: string;
    function GetVertical: boolean;
    function GetPosition: ISilPositionProperty;
  public
    constructor Create(const Owner: ISilPropertyOwner); reintroduce;
    destructor Destroy; override;
  published
    property Caption: string read GetCaption write SetCaption;
    property Vertical: boolean read FVertical write SetVertical;
    property Position: TSilPositionProperty read FPosition write SetPosition;
  end;

  TSilImageProperty = class
  (
    TSilControlProperty,
    ISilImageProperty
  )
  private
    FVisible: boolean;
    FPosition: TSilPositionProperty;
    FGlyph: TBitmap;
    FGlyphCount: integer;
    FImageList: TImageList;
    FImageIndex: integer;
  private
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphCount(const Value: integer);
    procedure SetImageIndex(const Value: integer);
    procedure SetImageList(const Value: TImageList);
    procedure SetPosition(const Value: TSilPositionProperty);
    procedure SetVisible(const Value: boolean);
    procedure AdjustPosition(Rect: TRect);
    function _HasImageList: boolean;
    function _HasGlyph: boolean;
  protected // ISilImageProperty
    function GetPosition: ISilPositionProperty;
    function GetHasBitmap: boolean;
    function GetHasGlyph: boolean;
    function GetGlyph: TBitmap;
    function GetGlyphCount: integer;
    function GetImageList: TImageList;
    function GetImageIndex: integer;
    function GetHeight: integer;
    function GetWidth: integer;
  public
    procedure Draw(Canvas: TCanvas; Rect: TRect; out OutRect: TRect);
  public
    constructor Create(const Owner: ISilPropertyOwner); reintroduce;
    destructor Destroy; override;
  public
    property MustStore: boolean read GetHasBitmap;
    property HasBitmap: boolean read GetHasBitmap;
    property HasGlyph: boolean read GetHasGlyph;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  published
    property Visible: boolean read FVisible write SetVisible;
    property Position: TSilPositionProperty read FPosition write SetPosition;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored _HasGlyph;
    property GlyphCount: integer read GetGlyphCount write SetGlyphCount stored _HasGlyph;
    property ImageList: TImageList read FImageList write SetImageList stored _HasImageList;
    property ImageIndex: integer read FImageIndex write SetImageIndex stored _HasImageList;
  end;

  TSilGroupingProperty = class
  (
    TSilControlProperty,
    ISilGroupingProperty
  )
  private
    FGroup: TSilGroup;
  private
    procedure SetDownColor(const Value: TColor);
    procedure SetIndex(const Value: Integer);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure DoGetGroup(GroupIndex: integer);
    procedure DoReleaseGroup;
    procedure SetMaxDown(const Value: integer);
    procedure SetMinDown(const Value: integer);
    function GetMustStore: boolean;
    function GetDownCount: integer;
    function GetDownColor: TColor;
  protected // ISilGroupingProperty
    function GetAllowAllUp: boolean;
    function GetIndex: integer;
    function GetDown: boolean;
    function GetMaxDown: integer;
    function GetMinDown: integer;
    function GetRect: TRect;
  public
    procedure CheckJoin(Message: TMessage);
    procedure InvalidateRect;
    procedure SetAllUp;
  public
    destructor Destroy; override;
  public
    property MustStore: boolean read GetMustStore;
    property Rect: TRect read GetRect;
  published
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property Index: Integer read GetIndex write SetIndex default 0;
    property Down: Boolean read GetDown write SetDown default False;
    property MaxDown: integer read GetMaxDown write SetMaxDown;
    property MinDown: integer read GetMinDown write SetMinDown;
    property DownCount: integer read GetDownCount;
    property DownColor: TColor read GetDownColor write SetDownColor;
  end;

  TSilGroup = class
  (
    TSilControlProperty
  )
  private
    FIndex: integer;
    FMinDown: integer;
    FMaxDown: integer;
    FClients: IPointerList;
    FDownList: IPointerList;
    FDownColor: TColor;
  private
    FRectValid: boolean;
    FRect: TRect;
  private
    procedure SetDownColor(const Value: TColor);
    procedure SetMaxDown(const Value: integer);
    procedure SetMinDown(const Value: integer);
    procedure DoApplyRules;
    function GetEmpty: boolean;
    procedure NotifyGroupClientsChanged;
    procedure CheckRect;
    function GetRect: TRect;
    procedure Notify(Dest: TComponent; Code: Cardinal);
    function GetDownCount: integer;
    procedure DoSetUpCount(Count: integer);
  public
    constructor Create(const Owner: ISilPropertyOwner; Index: integer); reintroduce;
  public
    procedure AddClient(Client: TComponent);
    procedure RemoveClient(Client: TComponent);
    procedure SetDown(Client: TComponent; Down: boolean);
    function IsDown(Client: TComponent): boolean;
    procedure InvalidateRect;
    procedure SetAllUp;
  public
    property Index: integer read FIndex;
    property MaxDown: integer read FMaxDown write SetMaxDown;
    property MinDown: integer read FMinDown write SetMinDown;
    property Empty: boolean read GetEmpty;
    property DownCount: integer read GetDownCount;
    property DownColor: TColor read FDownColor write SetDownColor;
  public
    property Rect: TRect read GetRect;
  end;

  TSilHighlightProperty = class
  (
    TSilControlProperty,
    ITimerEvents,
    ISilHighlightProperty
  )
  private
    FEnabled: boolean;
    FAutoColor: boolean;
    FColor: TColor;
    FLevel: integer;
    FTime: TDateTime;
    FDelay: Short;
    FParentColor: TColor;
  private
    FTimer: ITimer;
    FTargetLevel: integer;
  private
    function GetMustStore: boolean;
    function StoreColor: boolean;
    procedure CalcColor;
    procedure SetAutoColor(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetDelay(const Value: Short);
    procedure SetEnabled(const Value: boolean);
    procedure SetTargetLevel(Value: integer);
    procedure CheckTimer;
    procedure CalcLevel;
  public // ISilHighlightProperty
    function GetEnabled: boolean;
    function GetAutoColor: boolean;
    function GetColor: TColor;
    function GetDelay: Short;
    function GetTime: TDateTime;
    function GetLevel: integer;
  protected // ITimerEvents
    procedure OnTick(const Event: RTimerEvent);
  public
    constructor Create(const Owner: ISilPropertyOwner); reintroduce;
    destructor Destroy; override;
  public
    procedure ColorChanged(Color: TColor);
    function CalcCurrentColor(Color, HighlightColor: TColor): TColor;
  public
    property MustStore: boolean read GetMustStore;
    property Level: integer read FLevel;
    property TargetLevel: integer read FTargetLevel write SetTargetLevel;
  published
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property AutoColor: boolean read FAutoColor write SetAutoColor default true;
    property Color: TColor read FColor write SetColor stored StoreColor;
    property Delay: Short read FDelay write SetDelay default kDefaultHighlightDelay;
  end;

implementation

uses
  SysUtils,
  SilVtControls;

type
  TControlFriend = class (TControl)
  end;

{ TSilControlProperty }

constructor TSilControlProperty.Create(const Owner: ISilPropertyOwner);
begin
  inherited Create;
  FOwner := Owner;
end;

procedure TSilControlProperty.Changed;
begin
  if Assigned(FOwner) then
    FOwner.PropertyChanged(self);
end;

procedure TSilControlProperty.PropertyChanged(const Prop: ISilControlProperty);
begin
  Changed;
end;

function TSilControlProperty.GetVclControl: TControl;
begin
  result := FOwner.VclControl;
end;

function TSilControlProperty.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    result := S_OK else
    result := E_NOINTERFACE;
end;

function TSilControlProperty._AddRef: Integer;
begin
  result := -1;
end;

function TSilControlProperty._Release: Integer;
begin
  result := -1;
end;

function TSilControlProperty.GetSilControl: ISilControl;
begin
  result := FOwner.SilControl;
end;

{ TSilPositionProperty }

constructor TSilPositionProperty.Create(const Owner: ISilPropertyOwner);
begin
  inherited;
  FPosition := rpCenter;
end;

function TSilPositionProperty.GetLeft: integer;
begin
  result := FLeft;
end;

function TSilPositionProperty.GetMargin: integer;
begin
  result := FMargin;
end;

function TSilPositionProperty.GetPosition: TSilRelativePosition;
begin
  result := FPosition;
end;

function TSilPositionProperty.GetTop: integer;
begin
  result := FTop;
end;

procedure TSilPositionProperty.SetLeft(const Value: integer);
begin
  FPosition := rpFixed;
  if (FLeft <> Value) then
  begin
    FLeft := Value;
    AdjustPosition;
    Changed;
  end;
end;

procedure TSilPositionProperty.SetTop(const Value: integer);
begin
  FPosition := rpFixed;
  if (FTop <> Value) then
  begin
    FTop := Value;
    AdjustPosition;
    Changed;
  end;
end;

procedure TSilPositionProperty.SetMargin(const Value: integer);
begin
  if (FMargin <> Value) then
  begin
    FMargin := Value;
    AdjustPosition;
    Changed;
  end;
end;

procedure TSilPositionProperty.SetPosition(const Value: TSilRelativePosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    AdjustPosition;
    Changed;
  end;
end;

procedure TSilPositionProperty.AdjustPosition(Rect: TRect; const Height, Width: integer);
begin
  FClientRect := Rect;
  FWidth := Width;
  FHeight := Height;
  AdjustPosition;
end;

procedure TSilPositionProperty.AdjustPosition;
begin
  ControlTool.GetPosition(FClientRect, FMargin, FPosition, FHeight, FWidth, FLeft, FTop);
end;

function TSilPositionProperty._StorePosition: boolean;
begin
  result := (FPosition = rpFixed);
end;

{ TSilCaptionProperty }

constructor TSilCaptionProperty.Create(const Owner: ISilPropertyOwner);
begin
  inherited Create(Owner);
  FPosition := TSilPositionProperty.Create(self);
end;

destructor TSilCaptionProperty.Destroy;
begin
  FPosition.Free;
  inherited;
end;

procedure TSilCaptionProperty.Draw(Canvas: TCanvas; Rect: TRect; ColorShadow, ColorHighlight: TColor);
begin
  with Canvas do
    if (Length(Caption) > 0) then
    begin
      AdjustPosition(Rect, Canvas);

      Rect.Left := FPosition.Left;
      Rect.Top := FPosition.Top;
      Rect.Right := Rect.Left + FWidth - 1;
      Rect.Bottom := Rect.Top + FHeight - 1;

      ControlTool.DoDrawText(SilControl, Canvas, Rect, kDrawStyle, ColorShadow, ColorHighlight, FVertical);
    end;
end;

procedure TSilCaptionProperty.AdjustPosition(Rect: TRect; Canvas: TCanvas);
var
  txtrect: TRect;
begin
  txtrect := Rect;
  ControlTool.DoDrawText(SilControl, Canvas, txtrect, kDrawStyle or DT_CALCRECT, clGray, clWhite, FVertical);

  FWidth := 1 + txtrect.Right - txtrect.Left;
  FHeight := 1 + txtrect.Bottom - txtrect.Top;
  FPosition.AdjustPosition(Rect, FHeight, FWidth);
end;

function TSilCaptionProperty.GetCaption: string;
begin
  result := TControlFriend(VclControl).Caption;
end;

function TSilCaptionProperty.GetPosition: ISilPositionProperty;
begin
  result := FPosition;
end;

function TSilCaptionProperty.GetVertical: boolean;
begin
  result := FVertical;
end;

procedure TSilCaptionProperty.SetCaption(const Value: string);
begin
  if (TControlFriend(VclControl).Caption <> Value) then
  begin
    TControlFriend(VclControl).Caption := Value;
    FPosition.AdjustPosition;
    Changed;
  end;
end;

procedure TSilCaptionProperty.SetPosition(const Value: TSilPositionProperty);
begin
  if (FPosition <> Value) then
    FPosition.Assign(Value);
end;

procedure TSilCaptionProperty.SetVertical(const Value: boolean);
begin
  if (FVertical <> Value) then
  begin
    FVertical := Value;
    FPosition.AdjustPosition;
    Changed;
  end;
end;

{ TSilImageProperty }

constructor TSilImageProperty.Create(const Owner: ISilPropertyOwner);
begin
  inherited Create(Owner);
  FPosition := TSilPositionProperty.Create(self);
  FImageIndex := -1;
  FVisible := true;
end;

destructor TSilImageProperty.Destroy;
begin
  SetGlyph(nil);
  FPosition.Free;
  inherited;
end;

procedure TSilImageProperty.Draw(Canvas: TCanvas; Rect: TRect; out OutRect: TRect);
var
  imgrect: TRect;
  wd: integer;
begin
  OutRect := Rect;
  AdjustPosition(Rect);

  if FVisible and GetHasGlyph then
  begin
    if (FGlyphCount > 1) then
    begin
      wd := FGlyph.Width div FGlyphCount;
      imgrect.Left := wd * (1 - integer(VclControl.Enabled));
    end
    else
    begin
      wd := FGlyph.Width;
      imgrect.Left := 0;
    end;
    imgrect.Top := 0;
    imgrect.Bottom := FGlyph.Height;
    imgrect.Right := imgrect.Left + wd;

    Rect.Left := FPosition.Left;
    Rect.Top := FPosition.Top;
    Rect.Right := Rect.Left + imgrect.Right - imgrect.Left;
    Rect.Bottom := Rect.Top + FGlyph.Height;

    {Canvas.CopyRect(Rect, FGlyph.Canvas, imgrect);}
    Canvas.BrushCopy(Rect, FGlyph, imgrect, FGlyph.TransparentColor);
  end
  else if FVisible and Assigned(FImageList) then
  begin
    FImageList.Draw(Canvas, FPosition.Left, FPosition.Top, FImageIndex, VclControl.Enabled);
    wd := FImageList.Width;
  end
  else
    wd := 0;

  // Obtiene el espacio residual 
  case FPosition.Position of
  rpTopLeft, rpCenterLeft, rpBottomLeft: OutRect.Left := FPosition.Left + wd;
  rpTopRight, rpCenterRight, rpBottomRight: OutRect.Right := FPosition.Left - 1;
  end;

  case FPosition.Position of
  rpTopLeft, rpTopCenter, rpTopRight: OutRect.Top := FPosition.Top + Height;
  rpBottomLeft, rpBottomCenter, rpBottomRight: OutRect.Bottom := FPosition.Top - 1;
  end;
end;

function TSilImageProperty.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then
    FGlyph := TBitmap.Create;

  result := FGlyph;
end;

function TSilImageProperty.GetHasBitmap: boolean;
begin
  result := GetHasGlyph or Assigned(FImageList) and (FImageList.Count > 0);
end;

function TSilImageProperty.GetHasGlyph: boolean;
begin
  result := Assigned(FGlyph) and not FGlyph.Empty and (FGlyphCount > 0);
end;

function TSilImageProperty.GetHeight: integer;
begin
  if GetHasGlyph then
    result := FGlyph.Height
  else if Assigned(FImageList) then
    result := FImageList.Height else
  result := 0;
end;

function TSilImageProperty.GetWidth: integer;
begin
  if GetHasGlyph then
    result := FGlyph.Width div FGlyphCount
  else if Assigned(FImageList) then
    result := FImageList.Width
  else
    result := 0;
end;

function TSilImageProperty.GetImageIndex: integer;
begin
  result := FImageIndex;
end;

function TSilImageProperty.GetImageList: TImageList;
begin
  result := FImageList;
end;

function TSilImageProperty.GetPosition: ISilPositionProperty;
begin
  result := FPosition;
end;

function TSilImageProperty.GetGlyphCount: integer;
begin
  result := FGlyphCount;
end;

procedure TSilImageProperty.SetGlyph(const Value: TBitmap);
begin
  if Assigned(FGlyph) or Assigned(Value) then
  begin
    if not Assigned(FGlyph) then
      FGlyph := TBitmap.Create;

    if not Assigned(Value) or (Value.Height = 0) then
    begin
      FGlyph.Free;
      FGlyph := nil;
      FGlyphCount := 0;
    end
    else
    begin
      FGlyph.Assign(Value);
      FGlyph.TransparentColor := Value.TransparentColor;

      if (Value.Width > Value.Height) and (Value.Width mod Value.Height = 0) then
        FGlyphCount := Value.Width div Value.Height else
        FGlyphCount := 1;
    end;

    FPosition.AdjustPosition;
    Changed;
  end;
end;

procedure TSilImageProperty.SetGlyphCount(const Value: integer);
begin
  if (FGlyphCount <> Value) then
  begin
    FGlyphCount := Value;
    FPosition.AdjustPosition;
    Changed;
  end;
end;

procedure TSilImageProperty.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    FPosition.AdjustPosition;
    Changed;
  end;
end;

procedure TSilImageProperty.SetImageList(const Value: TImageList);
begin
  if (FImageList <> Value) then
  begin
    FImageList := Value;
    FPosition.AdjustPosition;
    Changed;
  end;
end;

procedure TSilImageProperty.SetPosition(const Value: TSilPositionProperty);
begin
  if (FPosition <> Value) then
    FPosition.Assign(Value);
end;

procedure TSilImageProperty.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TSilImageProperty.AdjustPosition(Rect: TRect);
begin
  FPosition.AdjustPosition(Rect, GetHeight, GetWidth);
end;

function TSilImageProperty._HasGlyph: Boolean;
begin
  result := Assigned(FGlyph);
end;

function TSilImageProperty._HasImageList: Boolean;
begin
  result := Assigned(FImageList);
end;

{ TSilGroupingProperty }

destructor TSilGroupingProperty.Destroy;
begin
  DoReleaseGroup;

  inherited;
end;

function TSilGroupingProperty.GetIndex: Integer;
begin
  if Assigned(FGroup) then
    result := FGroup.Index else
    result := 0;
end;

procedure TSilGroupingProperty.SetIndex(const Value: Integer);
begin
  if ( Value = 0 ) then
    DoReleaseGroup
  else if (Index <> Value) then
    DoGetGroup(Value);
end;

procedure TSilGroupingProperty.DoGetGroup(GroupIndex: integer);

  function GetRootParent(const Control: TControl; var Parent: TWinControl): boolean;
  begin
    if Assigned(Control) then
    begin
      Parent := Control.Parent;
      while Assigned(Parent.Parent) do
        Parent := Parent.Parent;
    end
    else
      Parent := nil;

    result := Assigned(Parent);
  end;
var
  Msg: TMessage;
  parent: TWinControl;
begin
  DoReleaseGroup;

  Msg.Msg := SM_GROUPJOIN;
  Msg.WParam := GroupIndex;
  Msg.LParam := integer(Pointer(@FGroup));
  Msg.Result := 0;
  //if GetRootParent(VclControl, parent) then
  parent := VclControl.Parent;
  parent.Broadcast(Msg);

  if not Assigned(FGroup) then
    FGroup := TSilGroup.Create(self, GroupIndex);

  FGroup.AddClient(VclControl);
end;

procedure TSilGroupingProperty.DoReleaseGroup;
begin
  if Assigned(FGroup) then
  begin
    FGroup.RemoveClient(VclControl);
    if FGroup.Empty then
      FGroup.Free;
    FGroup := nil;
  end;
end;

procedure TSilGroupingProperty.SetDown(Value: Boolean);
begin
  if Assigned(FGroup) then
    FGroup.SetDown(VclControl, Value);
end;

function TSilGroupingProperty.GetDown: Boolean;
begin
  result := Assigned(FGroup) and FGroup.IsDown(VclControl);
end;

procedure TSilGroupingProperty.SetAllowAllUp(const Value: Boolean);
begin
  if Assigned(FGroup) then
  begin
    if Value then
      FGroup.MinDown := 0 else
      FGroup.MinDown := 1;
  end;
end;

function TSilGroupingProperty.GetAllowAllUp: Boolean;
begin
  result := Assigned(FGroup) and (FGroup.MinDown = 0);
end;

function TSilGroupingProperty.GetMaxDown: integer;
begin
  if Assigned(FGroup) then
    result := FGroup.MaxDown else
    result := 0;
end;

function TSilGroupingProperty.GetMinDown: integer;
begin
  if Assigned(FGroup) then
    result := FGroup.MinDown else
    result := 0;
end;

procedure TSilGroupingProperty.SetMaxDown(const Value: integer);
begin
  if Assigned(FGroup) then
    FGroup.MaxDown := Value;
end;

procedure TSilGroupingProperty.SetMinDown(const Value: integer);
begin
  if Assigned(FGroup) then
    FGroup.MinDown := Value;
end;

procedure TSilGroupingProperty.CheckJoin(Message: TMessage);
begin
  if Assigned(FGroup) and (Message.WParam = FGroup.Index) then
    PPointer(Message.LParam)^ := FGroup;
end;

function TSilGroupingProperty.GetMustStore: boolean;
begin
  result := Assigned(FGroup);
end;

procedure TSilGroupingProperty.InvalidateRect;
begin
  if Assigned(FGroup) then
    FGroup.InvalidateRect;
end;

procedure TSilGroupingProperty.SetAllUp;
begin
  if Assigned(FGroup) then
    FGroup.SetAllUp;
end;

function TSilGroupingProperty.GetRect: TRect;
begin
  if not Assigned(FGroup) then
  begin
    result.Left := 0;
    result.Right := 0;
    result.Top := 0;
    result.Bottom := 0;
  end
  else
    result := FGroup.Rect;
end;

function TSilGroupingProperty.GetDownCount: integer;
begin
  if Assigned(FGroup) then
    result := FGroup.DownCount else
    result := 0;
end;

function TSilGroupingProperty.GetDownColor: TColor;
begin
  if Assigned(FGroup) then
    result := FGroup.DownColor else
    result := 0;
end;

procedure TSilGroupingProperty.SetDownColor(const Value: TColor);
begin
  if Assigned(FGroup) then
    FGroup.DownColor := Value;
end;

{ TSilGroup }

constructor TSilGroup.Create(const Owner: ISilPropertyOwner; Index: integer);
begin
  inherited Create(Owner);
  FIndex := Index;
  FMinDown := 1;
  FMaxDown := 1;
end;

procedure TSilGroup.AddClient(Client: TComponent);
begin
  if not Assigned(FClients) then
    FClients := Sil.List.PointerList;

  FClients.Add(Client);
  FRectValid := false;
  NotifyGroupClientsChanged;
end;

procedure TSilGroup.RemoveClient(Client: TComponent);
begin
  if Assigned(FClients) and ( FClients.IndexOf(Client) >= 0 ) then
  begin
    FRectValid := false;
    FClients.Remove(Client);
    NotifyGroupClientsChanged;
  end;

  if Assigned(FDownList) and ( FDownList.IndexOf(Client) >= 0 ) then
    FDownList.Remove(Client);
end;

procedure TSilGroup.SetMaxDown(const Value: integer);
begin
  if (FMaxDown <> Value) then
  begin
    FMaxDown := Value;
    DoApplyRules;
  end;
end;

procedure TSilGroup.SetMinDown(const Value: integer);
begin
  if (FMinDown <> Value) then
  begin
    FMinDown := Value;
  end;
end;

procedure TSilGroup.SetDown(Client: TComponent; Down: boolean);
begin
  if not Assigned(FDownList) then
    FDownList := Sil.List.PointerList;

  if Down or (FDownList.Count > FMinDown) then
  begin
    FDownList.Remove(Client);
    if Down then
      FDownList.Add(Client);

    Notify(Client, SM_GROUPCHANGED);
  end;

  DoApplyRules;
end;

procedure TSilGroup.DoApplyRules;
begin
  if (FMaxDown > 0) then
    DoSetUpCount(FMaxDown);
end;

procedure TSilGroup.NotifyGroupClientsChanged;
var
  enum: IEnumerator;
  comp: TComponent;
begin
  while FClients.Enumerate(enum, comp) do
    Notify(comp, SM_GROUPCLIENTSCHANGED);
end;

procedure TSilGroup.Notify(Dest: TComponent; Code: Cardinal);
var
  Msg: TMessage;
begin
  Msg.Msg := Code;
  Msg.WParam := 0;
  Msg.LParam := 0;
  Msg.Result := 0;
  Dest.Dispatch(Msg);
end;

function TSilGroup.GetEmpty: boolean;
begin
  result := not Assigned(FClients) or (FClients.Count = 0);
end;

function TSilGroup.IsDown(Client: TComponent): boolean;
begin
  result := Assigned(FDownList) and (FDownList.IndexOf(Client) >= 0);
end;

function TSilGroup.GetRect: TRect;
begin
  CheckRect;
  result := FRect;
end;

procedure TSilGroup.CheckRect;
var
  enum: IEnumerator;
  comp: TComponent;
  lf, rg, tp, bt: integer;
begin
  if not FRectValid then
  begin
    while FClients.Enumerate(enum, comp) do
      if comp is TControl then
      begin
        lf := TControl(comp).Left;
        rg := TControl(comp).Left + TControl(comp).Width - 1;
        tp := TControl(comp).Top;
        bt := TControl(comp).Top + TControl(comp).Height - 1;
        if not FRectValid then
        begin
          FRect.Left := lf;
          FRect.Right := rg;
          FRect.Top := tp;
          FRect.Bottom := bt;
        end
        else
        begin
          if lf < FRect.Left then FRect.Left := lf;
          if rg > FRect.Right then FRect.Right := rg;
          if tp < FRect.Top then FRect.Top := tp;
          if bt > FRect.Bottom then FRect.Bottom := bt;
        end;
        
        FRectValid := true;
      end;
  end;
end;

procedure TSilGroup.InvalidateRect;
begin
  FRectValid := false;
end;

procedure TSilGroup.SetAllUp;
begin
  DoSetUpCount(0);
end;

function TSilGroup.GetDownCount: integer;
begin
  if Assigned(FDownList) then
    result := FDownList.Count else
    result := 0;
end;

procedure TSilGroup.DoSetUpCount(Count: integer);
var
  client: TComponent;
begin
  if Assigned(FDownList) then
    while (FDownList.Count > Count) do
    begin
      client := FDownList.First;
      FDownList.Delete(0);
      Notify(client, SM_GROUPCHANGED);
    end;
end;

procedure TSilGroup.SetDownColor(const Value: TColor);
begin
  if ( FDownColor <> Value ) then
  begin
    FDownColor := Value;
    NotifyGroupClientsChanged;
  end;
end;

{ TSilHighlightProperty }

constructor TSilHighlightProperty.Create(const Owner: ISilPropertyOwner);
begin
  inherited Create(Owner);
  FEnabled := false;
  FAutoColor := true;
  FDelay := kDefaultHighlightDelay;
end;

destructor TSilHighlightProperty.Destroy;
begin

  inherited;
end;

function TSilHighlightProperty.GetMustStore: boolean;
begin
  result := FEnabled or (FDelay <> kDefaultHighlightDelay) or
    not FAutoColor;
end;

function TSilHighlightProperty.GetAutoColor: boolean;
begin
  result := FAutoColor;
end;

function TSilHighlightProperty.GetColor: TColor;
begin
  result := FColor;
end;

function TSilHighlightProperty.GetDelay: Short;
begin
  result := FDelay;
end;

function TSilHighlightProperty.GetEnabled: boolean;
begin
  result := FEnabled;
end;

function TSilHighlightProperty.GetLevel: integer;
begin
  result := FLevel;
end;

function TSilHighlightProperty.GetTime: TDateTime;
begin
  result := FTime;
end;

procedure TSilHighlightProperty.SetTargetLevel(Value: integer);
begin
  Value := Sil.Int.Range(Value, 0, 255);
  if (FTargetLevel <> Value) then
  begin
    FTargetLevel := Value;
    FTime := Now();
    CheckTimer;
  end;
end;

procedure TSilHighlightProperty.CheckTimer;
begin
  if (FLevel = FTargetLevel) then
  begin
    FTimer := nil;
  end
  else if not Assigned(FTimer) then
    FTimer := Sil.Os.Timer.Create(Cardinal(self), kHighlightTimerInterval, self);
end;

procedure TSilHighlightProperty.OnTick(const Event: RTimerEvent);
begin
  CalcLevel;
  CheckTimer;
end;

procedure TSilHighlightProperty.CalcLevel;
var
  nw: TDateTime;
  level, dif: integer;
begin
  if (FDelay <= 0) then
  begin
    level := 0;
  end
  else if (FLevel <> FTargetLevel) and (FDelay > 0) and (FTime > 0) then
  begin
    nw := Now();
    dif := Round(255 * (nw - FTime) * 86400000 / FDelay);
    if (dif > 0) then
    begin
      if (FTargetLevel > FLevel) then
        level := Sil.Int.Min(255, FLevel + dif) else
        level := Sil.Int.Max(0, FLevel - dif);

      FTime := nw;
    end
    else
      level := FLevel;
  end
  else
    level := FTargetLevel;

  if (FLevel <> level) then
  begin
    FLevel := level;
    Changed;
  end;
end;

procedure TSilHighlightProperty.ColorChanged(Color: TColor);
begin
  FParentColor := Color;
  CalcColor;
end;

procedure TSilHighlightProperty.SetAutoColor(const Value: boolean);
begin
  FAutoColor := Value;
  CalcColor;
end;

procedure TSilHighlightProperty.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    FAutoColor := false;
  end;
end;

procedure TSilHighlightProperty.SetDelay(const Value: Short);
begin
  if (FDelay <> Value) then
  begin
    FDelay := Value;
    Changed;
  end;
end;

procedure TSilHighlightProperty.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TSilHighlightProperty.StoreColor: boolean;
begin
  result := not FAutoColor;
end;

procedure TSilHighlightProperty.CalcColor;
var
  color: TColor;
begin
  if FAutoColor then
  begin
    color := Sil.Rgb.Brighter(FParentColor, 100);
    if (FColor <> color) then
    begin
      FColor := color;
      Changed;
    end;
  end;
end;

function TSilHighlightProperty.CalcCurrentColor(Color, HighlightColor: TColor): TColor;
begin
  if (FLevel = 0) then
    result := Color
  else if (FLevel >= 255) then
    result := HighlightColor
  else
    result := Sil.Rgb.MorphRel(Color, HighlightColor, FLevel);
end;

end.

