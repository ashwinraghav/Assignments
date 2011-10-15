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

unit SilOiMouse;

{$I Defines.inc}

interface

uses
  SilOeTypes;

type
  (*IMouseImage = interface
    ['{F292CBD3-30C2-11D4-9882-00104B0FA1EF}']
    property HotSpot: TPointe read GetHotSpot write SetHotSpot;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;*)

  IMousePointer = interface
    ['{F292CBD2-30C2-11D4-9882-00104B0FA1EF}']
    function GetClipRegion: TRect;
    procedure SetClipRegion(const Rect: TRect);
    function GetPosition: TPoint;
    procedure SetPosition(const Value: TPoint);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    {function GetImage: IMouseImage;
    procedure SetImage(const Value: IMouseImage);}

    property ClipRegion: TRect read GetClipRegion write SetClipRegion;
    property Position: TPoint read GetPosition write SetPosition;
    property Visible: Boolean read GetVisible write SetVisible;
    //property Image: IMouseImage read GetImage write SetImage;
  end;

implementation

end.
 