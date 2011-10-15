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

unit SilBeBounds;

interface

{$INCLUDE Defines.inc}

uses
  SilOeTypes;

type
  TBounds = {packed} object
  private
    function GetOrigin: TPoint;
    procedure SetOrigin(const AValue: TPoint);
    function GetSize: TPoint;
    procedure SetSize(const AValue: TPoint);
    function GetRect: TRect;
    procedure SetRect(const AValue: TRect);
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    property Origin: TPoint read GetOrigin write SetOrigin;
    property Size: TPoint read GetSize write SetSize;
    property AsRect: TRect read GetRect write SetRect;
    procedure MoveBy(const ADelta: TPoint);
    procedure GrowBy(const ADelta: TPoint);
    procedure GrowRel(const ANum, ADen: TPoint);
  end;

implementation

uses
  SilBtPoint,
  SilBtRect;

/////////////////////////////////////////////////////////////////////////////////////////////

procedure TBounds.MoveBy(const ADelta: TPoint);
begin
  Origin := Point.Add(Origin, ADelta);
end;

procedure TBounds.GrowBy(const ADelta: TPoint);
begin
  Size := Point.Add(Size, ADelta);
end;

procedure TBounds.GrowRel(const ANum, ADen: TPoint);
begin
  Size := Point.Make(Size.X * ANum.X div ADen.X, Size.Y * ANum.Y div ADen.Y);
end;

function TBounds.GetOrigin: TPoint;
begin
  Result := Point.Make(Left, Top);
end;

procedure TBounds.SetOrigin(const AValue: TPoint);
begin
  Left := AValue.X; 
  Top  := AValue.Y; 
end;

function TBounds.GetSize: TPoint;
begin
  Result := Point.Make(Width, Height);
end;

procedure TBounds.SetSize(const AValue: TPoint);
begin
  Width  := AValue.X;
  Height := AValue.Y;
end;

function TBounds.GetRect: TRect;
begin
  Result := Rect.Make(Left, Top, Left + Width, Top + Height);
end;

procedure TBounds.SetRect(const AValue: TRect);
begin
  Left   := AValue.Left;
  Top    := AValue.Top;
  Width  := AValue.Right  - Left;
  Height := AValue.Bottom - Top;
end;


end.
