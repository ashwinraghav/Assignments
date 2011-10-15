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

unit ScrollBox;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TWMMouseWheel = record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TScroller = class(TScrollBox)
  private
    procedure DoCalcScrollBarRange;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    function IndexOf(Control: TControl): Integer;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mariano', [TScroller]);
end;

{ TScroller }

// deberia ser automatico, pero en delphi 3 falla y esto lo hace andar
procedure TScroller.DoCalcScrollBarRange;
var
  i, iSize: Integer;
begin
  iSize := 0;
  for i := 0 to ControlCount - 1 do
    with Controls[i] do
      if Align = alTop then
        Inc(iSize, Height);
  if iSize > 0 then
    VertScrollBar.Range := iSize;
end;

function TScroller.IndexOf(Control: TControl): Integer;
var
	i: Integer;
begin
    for i := 0 to ControlCount - 1 do
		if Controls[i] = Control then
		begin
			Result := i;
			Exit;
		end;
	Result := -1;
end;

procedure TScroller.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;
  DoCalcScrollBarRange;
end;

procedure TScroller.WMMouseWheel(var Msg: TWMMouseWheel);
var
	i, iInc: Integer;
	Pos: TPoint;
begin
	if Msg.WheelDelta <> 0 then
	begin
		iInc := Msg.WheelDelta div Abs(Msg.WheelDelta) * 3;
		i := 1;
		while i < VertScrollBar.Increment * 3 do
		begin
			GetCursorPos(Pos);
			Pos := ScreenToClient(Pos);
			if (Pos.x < 0) or (Pos.x > Width - 1) or (Pos.y < 0) or (Pos.y > Height - 1) then
				Break;

			VertScrollBar.Position := VertScrollBar.Position - iInc;
			Update;
			Inc(i, 1);
		end;
		Msg.Result := 1;
	end;
end;

end.
 