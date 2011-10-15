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

unit SilVkCustomControl;

{$I Defines.inc}

interface

uses
  Sil,
  SilViControls,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls;

type
  TSilCustomControl = class
  (
    TCustomControl,
    ISilCustomControl
  )
  private
    procedure SetColor(const Value: TColor);
  protected // ISilCustomControl
    function GetIsEnabled: boolean;
    function ISilCustomControl.GetEnabled = GetIsEnabled;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetBounds: TRect;
    function GetCaption: TCaption;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetVclControl: TControl;
  protected
    property Color: TColor read GetColor write SetColor;
  end;

implementation

type
  TControlFriend = class ( TControl )
  end;

{ TSilCustomControl }

function TSilCustomControl.GetBounds: TRect;
begin
  result := ClientRect;
end;

function TSilCustomControl.GetColor: TColor;
begin
  result := TControlFriend( self ).Color;
end;

function TSilCustomControl.GetCaption: TCaption;
begin
  result := TControlFriend( self ).Caption;
end;

function TSilCustomControl.GetIsEnabled: boolean;
begin
  result := TControlFriend( self ).Enabled;
end;

function TSilCustomControl.GetFont: TFont;
begin
  result := TControlFriend( self ).Font;
end;

function TSilCustomControl.GetHeight: integer;
begin
  result := TControlFriend( self ).Height;
end;

function TSilCustomControl.GetWidth: integer;
begin
  result := TControlFriend( self ).Width;
end;

procedure TSilCustomControl.SetColor(const Value: TColor);
begin
  if ( Color <> Value ) then
    TControlFriend( self ).Color := Value;
end;

function TSilCustomControl.GetVclControl: TControl;
begin
  result := self;
end;

end.

