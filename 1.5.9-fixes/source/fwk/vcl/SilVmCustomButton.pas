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

unit SilVmCustomButton;

interface

{$I Defines.inc}

uses
  Sil, SilVkControl, SilViControls,
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TSilCustomButton = class
  (
    TSilControl,
    ISilCustomButton
  )
  private
    FPainter: ISilControlPainter;
  private
    FAutoRepeat: Boolean;
    FModalResult: TModalResult;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FCancel: Boolean;
    FDrawFocus: Boolean;
    FButtState: TSilButtonState;
    FRepeatStep: integer;
  private
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  private
    procedure SetDrawFocus(const Value: Boolean);
    procedure RepeatEvent(Sender: TObject);
    procedure DoStartRepeatEvent;
    procedure DoStopRepeatEvent;
    procedure DoClick;
  private
    procedure SetDefault(const Value: boolean);
    procedure SetIsPressed(const Value: boolean);
    procedure SetButState(const Value: TSilButtonState);
  protected // ISilCustomButton
    function GetButtState: TSilButtonState;
    function GetCancel: boolean;
    function GetDrawFocus: boolean;
    function GetIsPressed: boolean;
    function GetDefault: boolean;
    function GetAutoRepeat: Boolean;
    function GetModalResult: TModalResult;
  protected
    property Default: boolean read GetDefault write SetDefault default false;
  protected
    function DoGetCurrentColor: TColor; override;
    procedure StateChanged(Previous, New: TSilControlState); override;
    function GetPainter(out Painter: ISilControlPainter): boolean; override;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftSTate); override;
    procedure KeyUp(var Key: Word; Shift: TShiftSTate); override;
    procedure Click; override;
    procedure DblClick; override;
    procedure DoDefaultChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property TextVertical;
  public
    property IsPressed: boolean read GetIsPressed write SetIsPressed;
    property ButState: TSilButtonState read FButtState;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat;
    property Cancel: Boolean read FCancel write FCancel default false;
    property DrawFocus: Boolean read FDrawFocus write SetDrawFocus default true;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
  public
    property Anchors;
    property Align;
    property Caption;
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

const
  kRepeatSteps = 4;
  kRepeatScale: double = 100 / kRepeatSteps;

implementation

uses
  SilVmControlPainter;
  
constructor TSilCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse];
  FDrawFocus := true;

  Width := 75;
  Height := 25;

  TabStop := true;
end;

destructor TSilCustomButton.Destroy;
begin
  inherited Destroy;
end;

procedure TSilCustomButton.RepeatEvent(Sender: TObject);
begin
  if IsPressed then
  begin
    if (FRepeatStep = 0) then
      try
        if DoToggle then DoClick;
      except
        DoStopRepeatEvent;
        raise;
      end;

    FRepeatStep := (FRepeatStep + 1) mod kRepeatSteps;
  end
  else
    DoStopRepeatEvent;

  Invalidate;
end;

function TSilCustomButton.DoGetCurrentColor: TColor;
begin
  result := inherited DoGetCurrentColor;

  { Realiza el parpadeo en repeticiones }
  if (absRepeating in ButState) then
    result := Sil.Rgb.MorphRel(Color, result, Round(kRepeatScale * FRepeatStep));
end;

procedure TSilCustomButton.DblClick;
begin
  Click;
end;

procedure TSilCustomButton.Click;
var
  Form: TForm;
begin
  if not GroupingEnabled or DoToggle then
  begin
    if FModalResult > 0 then
    begin
      Form := TForm(GetParentForm(Self));
      if (Form <> nil) then
        Form.ModalResult := FModalResult;
    end;

    (*)if IsPressed then
    begin
      inherited MouseCapture := false;
      IsPressed := false;
    end;(*)

    DoClick;
  end;
end;

procedure TSilCustomButton.DoClick;
begin
  inherited Click;
end;

procedure TSilCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
    IsPressed := true;
    
    if IsWindow(WindowHandle)
      and IsWindowVisible(WindowHandle)
      and IsWindowEnabled(WindowHandle)
        then SetFocus;

    inherited MouseDown(Button, Shift, X, Y);

    if FAutoRepeat then
      DoStartRepeatEvent;

    Invalidate;

  end else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSilCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoStopRepeatEvent;
  
  if IsPressed then
  try
    inherited MouseUp(Button, Shift, X, Y);
  finally
    IsPressed := false;
  end;
end;

procedure TSilCustomButton.MouseMove(Shift: TShiftState;X, Y: Integer);
begin
  (*)
  if MouseCapture then
    IsPressed := (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height);(*)
  inherited MouseMove(Shift, X, Y);
end;

procedure TSilCustomButton.KeyDown(var Key: Word; Shift: TShiftSTate);
begin
  if (Key = vk_Space) then
    IsPressed := true;

  inherited KeyDown(Key, Shift);
end;

procedure TSilCustomButton.KeyUp(var Key: Word; Shift: TShiftSTate);
begin
  if (Key = vk_Space) then
  begin
    IsPressed := false;

    Click;
  end;
end;

procedure TSilCustomButton.DoStartRepeatEvent;
begin
  if not (absRepeating in ButState) then
  begin
    FRepeatStep := 0;
    AddEvent(RepeatEvent, kRepeatPause div kRepeatSteps, kInitRepeatPause);
    SetButState(ButState + [absRepeating]);
  end;
end;

procedure TSilCustomButton.DoStopRepeatEvent;
begin
  if (absRepeating in ButState) then
  begin
    RemoveEvent(RepeatEvent);
    SetButState(ButState - [absRepeating]);
  end;
end;

procedure TSilCustomButton.CMEnabledChanged(var Message: TMessage);
begin
  if absPressed in ButState then
    SetIsPressed(false) else
    Invalidate;

  inherited;
end;

procedure TSilCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if Enabled and Visible and Showing and (((CharCode = VK_RETURN) and (Default or HaveFocus)) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TSilCustomButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if Enabled and Visible and Showing and IsAccel(CharCode, Caption) then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TSilCustomButton.StateChanged(Previous, New: TSilControlState);
begin
  inherited;

  if (acsFocus in New) and not (acsFocus in Previous) or
    not (acsFocus in New) and (acsFocus in Previous) then
  begin
    Invalidate;
  end;

  if (acsDefault in New) and not (acsDefault in Previous) or
    not (acsDefault in New) and (acsDefault in Previous) then
  begin
    DoDefaultChanged;
  end;
end;

procedure TSilCustomButton.DoDefaultChanged;
var
  i: integer;
  container: TControl;
  ctrl: TSilCustomButton;
begin
  if (acsDefault in State) then
  begin
    container := Self;
    while Assigned(container.Parent) do
      container := container.Parent;
    if not (container is TCustomForm) then
      container := nil;

    if Assigned(container) and (container is TCustomForm) then
      with container as TCustomForm do
      begin
        if not (ActiveControl is TSilCustomButton) then
          for i := 0 to ControlCount - 1 do
            if (Controls[i] is TSilCustomButton) then
            begin
              ctrl := Controls[i] as TSilCustomButton;
              if ctrl.Default and (ctrl <> self) then
              begin
                ctrl.Default := false;
                ctrl.Invalidate;
              end;
            end;
      end;
  end;

  Invalidate;
end;

procedure TSilCustomButton.SetDrawFocus(const Value: Boolean);
begin
  FDrawFocus := Value;
end;

function TSilCustomButton.GetIsPressed: boolean;
begin
  result := Enabled and ((absPressed in ButState) or Down);
end;

procedure TSilCustomButton.SetIsPressed(const Value: boolean);
begin
  if (IsPressed <> Value) then
  begin
    if Value then
      SetButState(ButState + [absPressed]) else
      SetButState(ButState - [absPressed]);

    Invalidate;
  end;
end;

procedure TSilCustomButton.SetButState(const Value: TSilButtonState);
begin
  if (FButtState <> Value) then
  begin
    FButtState := Value;
  end;
end;

function TSilCustomButton.GetDefault: boolean;
begin
  result := Enabled and (acsDefault in State);
end;

procedure TSilCustomButton.SetDefault(const Value: boolean);
begin
  if Value then
    SetState(State + [acsDefault]) else
    SetState(State - [acsDefault]);
end;

function TSilCustomButton.GetPainter(out Painter: ISilControlPainter): boolean;
begin
  if not Assigned(FPainter) then
    FPainter := TSilButtonPainter.Create;
  Painter := FPainter;
  result := true;
end;

function TSilCustomButton.GetAutoRepeat: Boolean;
begin
  result := FAutoRepeat;
end;

function TSilCustomButton.GetButtState: TSilButtonState;
begin
  result := FButtState;
end;

function TSilCustomButton.GetCancel: boolean;
begin
  result := FCancel;
end;

function TSilCustomButton.GetDrawFocus: boolean;
begin
  result := FDrawFocus;
end;

function TSilCustomButton.GetModalResult: TModalResult;
begin
  result := FModalResult;
end;

end.

