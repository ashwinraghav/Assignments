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

unit SilBtRect;

{$I Defines.inc}

interface

uses
  SilOeTypes,
  SilBkTool;

type
  Rect = class(Tool)
    class function Make(Left, Top, Right, Bottom: Integer): TRect;
    class function Normalize(const Rect: TRect): TRect;
    class function Overlaps(const R1, R2: TRect): Boolean;
    class function MoveTo(var Rect: TRect; Left, Top: Integer): TRect;
  end;

(*
  TRectangle = packed object
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    function Bottom: Integer;
    function Right: Integer;
    function Rect: TRect;
    procedure Assign( ALeft, ATop, ARight, ABottom: Integer );
    function BiggerThan( SizeX, SizeY: Integer ): Boolean;
    function Contains( X, Y: Integer ): Boolean;
    procedure MoveBy( dX, dY: Integer );
    procedure MoveTo( LocX, LocY: Integer );
    procedure CenterIn( CenterX, CenterY: Integer );
    procedure Copy( const Other: TRectangle );
    procedure Intersect( const Other: TRectangle );
    procedure Union( const Other: TRectangle );
    function Empty: Boolean;
    procedure Grow( dX, dY: Integer );
    procedure FactorGrow( FactorX, FactorY: Double );
    function Overlays( const Other: TRectangle ): Boolean;
    procedure Resize( dLeft, dTop, dRight, dBottom: Integer);
    procedure Normalize;
    procedure MakeEmpty;
    procedure SetLeft( ALeft: Integer );
    procedure SetBottom( ABottom: Integer );
    procedure SetEnd( ARight, ABottom: Integer );
    procedure SetLeftBottom( ALeft, ABottom: Integer );
    procedure SetLeftTop ( ALeft, ATop: Integer );
    procedure SetOrg ( X, Y: Integer );
    procedure SetRight ( ARight: Integer );
    procedure SetRightBottom ( ARight, ABottom: Integer );
    procedure SetRightTop ( ARight, ATop: Integer );
    procedure SetSize ( SizeX, SizeY: Integer );
    procedure SetTop ( ATop: Integer );
  end;*)


implementation

uses
  SilOfRect, Types;

{ Rect }

class function Rect.Make(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Right  := Right;
  Result.Bottom := Bottom;
end;

class function Rect.MoveTo(var Rect: TRect; Left, Top: Integer): TRect;
var
  Diff: TPoint;
begin
  Diff.X := Left - Rect.Left;
  Diff.Y := Top - Rect.Top;

  Rect.Left := Left;
  Rect.Top := Top;
  Inc(Rect.Right, Diff.X);
  Inc(Rect.Bottom, Diff.Y);
end;

class function Rect.Normalize(const Rect: TRect): TRect;
begin
  if Rect.Left > Rect.Right then
  begin
    Result.Left := Rect.Right;
    Result.Right := Rect.Left;
  end else
  begin
    Result.Left := Rect.Left;
    Result.Right := Rect.Right;
  end;
  if Rect.Top > Rect.Bottom then
  begin
    Result.Top := Rect.Bottom;
    Result.Bottom := Rect.Top;
  end else
  begin
    Result.Top := Rect.Top;
    Result.Bottom := Rect.Bottom;
  end;
end;

class function Rect.Overlaps(const R1, R2: TRect): Boolean;
begin
  Result := SilOfRect.Overlaps(R1, R2);
end;

(*
function TRectangle.Bottom: Integer;
begin
  Result := Top + Height;
end;

function TRectangle.Right: Integer;
begin
  Result := Left + Width;
end;

function TRectangle.Rect: TRect;
begin
  Result := TRect(Self);
end;

procedure TRectangle.Assign( ALeft, ATop, ARight, ABottom: Integer );
begin
  Left := ALeft;
  Top := ATop;
  Width := ARight - ALeft;
  Height := ABottom - ATop;
end;


function TRectangle.BiggerThan( SizeX, SizeY: Integer ): Boolean;
begin
  Result := ( Abs(Width) >= SizeX ) and ( Abs(Height) >= SizeY );
end;


procedure TRectangle.Copy( const Other: TRectangle );
begin
  System.Move( Other, Self, SizeOf(Self) );
end;


function TRectangle.Empty: Boolean;
begin
  Result := (Width = 0 ) or (Height = 0);
end;


procedure TRectangle.Grow( dX, dY: Integer );
begin
  Dec( Left,   dX );
  Dec( Top ,   dY );
  Inc( Width,  dX * 2 );
  Inc( Height, dY * 2 );
end;


procedure TRectangle.FactorGrow( FactorX, FactorY: Double );
begin
  Width  := Round( Width  * FactorX );
  Height := Round( Height * FactorY );
  Normalize;
end;


function TRectangle.Contains( X, Y: Integer ): Boolean;
begin
  Result := (X >= Left) and (X <= (Left + Width)) and
            (Y >= Top)  and (Y <= (Top  + Height));
end;

procedure TRectangle.Intersect( const Other: TRectangle );
  var TheLeft, TheRight, TheTop, TheBottom: Integer;
begin
  MakeEmpty;
  if not Overlays( Other ) then Exit;
  if Other.Left   > Left   then TheLeft   := Other.Left   else TheLeft   := Left;
  if Other.Top    > Top    then TheTop    := Other.Top    else TheTop    := Top;
  if Other.Right  < Right  then TheRight  := Other.Right  else TheRight  := Right;
  if Other.Bottom < Bottom then TheBottom := Other.Bottom else TheBottom := Bottom;
  Assign( TheLeft, TheTop, TheRight, TheBottom );
end;

procedure TRectangle.Union( const Other: TRectangle );
begin
  if Left   > Other.Left   then SetLeft  ( Other.Left   );
  if Top    > Other.Top    then SetTop   ( Other.Top    );
  if Right  < Other.Right  then SetRight ( Other.Right  );
  if Bottom < Other.Bottom then SetBottom( Other.Bottom );
end;


procedure TRectangle.MakeEmpty;
begin
  Width := 0;
  Height := 0;
end;


procedure TRectangle.MoveBy( dX, dY: Integer );
begin
  Inc( Left, dX );
  Inc( Top,  dY );
end;


procedure TRectangle.CenterIn( CenterX, CenterY: Integer );
begin
  Left := CenterX - Width div 2;
  Top  := CenterY - Height div 2;
end;


procedure TRectangle.SetBottom( ABottom: Integer );
begin
  Height := ABottom - Top;
end;


procedure TRectangle.SetEnd( ARight, ABottom: Integer );
begin
  Width := ARight - Left;
  Height := ABottom - Top;
end;

procedure TRectangle.SetLeft( ALeft: Integer );
begin
  Dec( Width, ALeft - Left );
  Left := ALeft;
end;


procedure TRectangle.SetLeftBottom( ALeft, ABottom: Integer );
begin
   SetLeft( ALeft );  SetBottom( ABottom );
end;

procedure TRectangle.SetLeftTop( ALeft, ATop: Integer );
begin
   SetLeft( ALeft );
   SetTop( ATop );
end;

procedure TRectangle.SetOrg ( X, Y: Integer );
begin
  Left := X;
  Top := Y;
end;

procedure TRectangle.SetRight ( ARight: Integer );
begin
  Width := ARight - Left;
end;

procedure TRectangle.SetRightBottom ( ARight, ABottom: Integer );
begin
   SetRight(  ARight );
   SetBottom( ABottom );
end;

procedure TRectangle.SetRightTop ( ARight, ATop: Integer );
begin
  SetRight( ARight );
  SetTop( ATop );
end;

procedure TRectangle.SetSize( SizeX, SizeY: Integer );
begin
  Width := SizeX;
  Height := SizeY;
end;



procedure TRectangle.SetTop ( ATop: Integer );
begin
  Dec( Height, ATop - Top );
  Top := ATop;
end;


procedure TRectangle.Normalize;
begin
  if Width < 0 Then
  begin
    Inc( Left, Width );
    Width := -Width;
  end;
  if Height < 0 Then
  begin
    Inc( Top, Height );
    Height := -Height;
  end;
end;

procedure TRectangle.MoveTo( LocX, LocY: Integer );
begin
  MoveBy( LocX - Left, LocY - Top );
end;


function TRectangle.Overlays( const Other: TRectangle ): Boolean;
  var X, Y: Integer;
begin
  Result := True;
  X := Left + Width;
  Y := Top + Height;
  if Other.Contains(Left, Top) then Exit;
  if Other.Contains(Left, Y  ) then Exit;
  if Other.Contains(X   , Top) then Exit;
  if Other.Contains(X   , Y  ) then Exit;
  with Other do
  begin
    X := Left + Width;
    Y := Top + Height;
    if Self.Contains(Left, Top) then Exit;
    if Self.Contains(Left, Y  ) then Exit;
    if Self.Contains(X   , Top) then Exit;
    if Self.Contains(X   , Y  ) then Exit;
  end;
  Result := False;
end;


procedure TRectangle.Resize( dLeft, dTop, dRight, dBottom: Integer);
begin
  Inc( Left  , dLeft );
  Inc( Top   , dTop  );
  Inc( Width , dLeft+dRight );
  Inc( Height, dTop +dBottom );
end; *)


end.
