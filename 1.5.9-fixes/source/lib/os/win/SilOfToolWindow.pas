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

unit SilOfToolWindow;

{$I Defines.inc}

interface

uses
  Windows,
  SilOiWindow;

function Allocate(AMethod: TWindowCallback): HWND;

implementation

uses
  SysUtils,
  Messages;

var 
	UtilWindowClass: TWndClass = (
		style: 0;
		lpfnWndProc: @DefWindowProc;
		cbClsExtra: 0;
		cbWndExtra: 0;
		hInstance: 0;
		hIcon: 0;
		hCursor: 0;
		hbrBackground: 0;
		lpszMenuName: nil;
		lpszClassName: 'TSilUtilityWindow');

{ Window }

function WindowProc(Hnd: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Longint; stdcall;
var
	Method: TMethod;
	Message: TMessage;
begin
	Method.Code := Pointer(GetWindowLong(Hnd, 0));
	Method.Data := Pointer(GetWindowLong(Hnd, 4));
	Message.Msg := Msg;
	Message.LParam := lParam;
	Message.WParam := wParam;
	TWindowCallback(Method)(Message);
	Result := Message.Result;
end;

function Allocate(AMethod: TWindowCallback): HWND;
var
	TempClass: TWndClass;
	ClassRegistered: Boolean;
begin
  Result := 0;

	if Assigned(AMethod) then
  begin
    UtilWindowClass.hInstance := HInstance;
    UtilWindowClass.cbWndExtra := SizeOf(TMethod);

    ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName, TempClass);

    if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
    begin
      if ClassRegistered then
        Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
      Windows.RegisterClass(UtilWindowClass);
    end;

    Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
      '', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);

    SetWindowLong(Result, 0, Longint(TMethod(AMethod).Code));
    SetWindowLong(Result, 4, Longint(TMethod(AMethod).Data));
    SetWindowLong(Result, GWL_WNDPROC, Longint(@WindowProc));
  end;
end;

end.
 