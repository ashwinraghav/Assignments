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

unit SilVm3dButton;

interface

{$I Defines.inc}

uses
  Sil,
  SilViControls,
  SilVkCustomControl,
  SilVmCustomButton,
  SilVmImgButton,
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TSilCustom3dButton = class ( TSilCustomImageButton )
  private
    function GetAlbedo: double;
    function GetElevation: integer;
    function GetShape: TSil3dShape;
    procedure SetAlbedo(const Value: double);
    procedure SetElevation(const Value: integer);
    procedure SetShape(const Value: TSil3dShape);
    function GetImageSource3d: ISil3dImageSource;
    procedure DoApplyLights;
  public
    constructor Create( AOwner: TComponent ); override;
  public
    property Shape: TSil3dShape read GetShape write SetShape;
    property Elevation: integer read GetElevation write SetElevation;
    property Albedo: double read GetAlbedo write SetAlbedo;
    property ImageSource3d: ISil3dImageSource read GetImageSource3d;
  end;

  TSil3dButton = class ( TSilCustom3dButton )
  published
    property Elevation;
    property Shape;
    {property Albedo;}
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

uses
  SilBtColor,
  {$IFDEF D60} Types, {$ENDIF}
  SilOtGuid,
  SilVm3dShape;

{ TSilCustom3dButton }

constructor TSilCustom3dButton.Create(AOwner: TComponent);
begin
  inherited;

  ImageSource := ImageTool.Create3dShape;
  DoApplyLights;
  Shape := ksRoundRect;
  Elevation := 15;
  {Transparent := true;{}
end;

procedure TSilCustom3dButton.DoApplyLights;
begin
  if Assigned( ImageSource3d ) then
    with ImageSource3d.Params do
    begin
      ResetLights;
      AddLight( -30, -30, 30 );
      AddLight( 0, 0, 60 );
    end;
end;

function TSilCustom3dButton.GetAlbedo: double;
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    result := imgsrc.Params.Albedo else
    result := 0;
end;

function TSilCustom3dButton.GetElevation: integer;
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    result := imgsrc.Params.Elevation else
    result := 0;
end;

function TSilCustom3dButton.GetImageSource3d: ISil3dImageSource;
begin
  Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, result );
end;

function TSilCustom3dButton.GetShape: TSil3dShape;
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    result := imgsrc.Params.Shape else
    result := ksNone; 
end;

procedure TSilCustom3dButton.SetAlbedo(const Value: double);
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    with imgsrc.Params do
      if ( Albedo <> Value ) then
      begin
        Albedo := Value;
        Invalidate;
      end;
end;

procedure TSilCustom3dButton.SetElevation(const Value: integer);
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    with imgsrc.Params do
      if ( Elevation <> Value ) then
      begin
        Elevation := Value;
        Invalidate;
      end;
end;

procedure TSilCustom3dButton.SetShape(const Value: TSil3dShape);
var
  imgsrc: ISil3dImageSource;
begin
  if Sil.Ref.GetInterface( ImageSource, ISil3dImageSource, imgsrc ) then
    with imgsrc.Params do
      if ( Shape <> Value ) and ( Ord( Value ) >= 0 ) then
      begin
        Shape := Value;
        Invalidate;
      end;
end;

end.

