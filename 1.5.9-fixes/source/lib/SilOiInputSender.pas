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

unit SilOiInputSender;

{$I Defines.inc}

interface

type
  TInputMouseButton = (imNone, imLeft, imRight, imMiddle);

{ IInputSender }

  IInputSender = interface
    ['{FACA6601-0584-11D4-9879-00104B0FA1EF}']
    procedure MouseMove(X, Y: Integer);
    procedure MouseDown(Button: TInputMouseButton; Release: Boolean = false); overload;
    procedure MouseDown(Buttons: array of Word; Release: Boolean = false); overload;
    procedure MouseUp(Button: TInputMouseButton);
    procedure KeyboardDown(VKey: Word; Release: Boolean = false); overload;
    procedure KeyboardDown(VKeys: array of Word; Release: Boolean = false); overload;
    procedure KeyboardUp(VKey: Word);
    procedure KeyboardSendText(const Text: string; AppendReturn: boolean = FALSE);
    procedure ReleaseKeyboard;
    procedure ReleaseMouse;
    function Execute: Integer;
  end;

implementation

end.
 