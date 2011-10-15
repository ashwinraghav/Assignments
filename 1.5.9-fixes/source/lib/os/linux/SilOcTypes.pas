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

unit SilOcTypes;

{$I Defines.inc}

interface

uses
  Libc,
  SilOeTypes;

const
  ERROR_SUCCESS         = OsCode(0);
  INFINITE              = LongWord(Libc.RLIM_INFINITY);
  NULL_HANDLE_VALUE     = THandle(0);
  INVALID_HANDLE_VALUE  = NULL_HANDLE_VALUE;

const
  S_OK                  = HRESULT(0);
  S_FALSE               = HRESULT(1);

  NOERROR               = HRESULT(0);
  E_UNEXPECTED          = HRESULT(-1);
  E_NOTIMPL             = HRESULT(-1);
  E_OUTOFMEMORY         = HRESULT(-1);
  E_INVALIDARG          = HRESULT(-1);
  E_NOINTERFACE         = HRESULT(-1);
  E_POINTER             = HRESULT(-1);
  E_HANDLE              = HRESULT(-1);
  E_ABORT               = HRESULT(-1);
  E_FAIL                = HRESULT(-1);
  E_ACCESSDENIED        = HRESULT(-1);
  E_PENDING             = HRESULT(-1);

const
  CPathSeparator        = Char('/');
  CPathListSeparator    = Char(':');

const
  SwitchChars           = ['-'];

{$IFNDEF D60}
const
  sLineBreak            = #13;
{$ENDIF}

type
  PColor                                = ^TColor;
  TColor                                = -$7FFFFFFF-1..$7FFFFFFF;

const
  clScrollBar                           = TColor(0 or $80000000);
  clBackground                          = TColor(1 or $80000000);
  clActiveCaption                       = TColor(1 or $80000000);
  clInactiveCaption                     = TColor(1 or $80000000);
  clMenu                                = TColor(1 or $80000000);
  clWindow                              = TColor(1 or $80000000);
  clWindowFrame                         = TColor(1 or $80000000);
  clMenuText                            = TColor(1 or $80000000);
  clWindowText                          = TColor(1 or $80000000);
  clCaptionText                         = TColor(1 or $80000000);
  clActiveBorder                        = TColor(1 or $80000000);
  clInactiveBorder                      = TColor(1 or $80000000);
  clAppWorkSpace                        = TColor(1 or $80000000);
  clHighlight                           = TColor(1 or $80000000);
  clHighlightText                       = TColor(1 or $80000000);
  clBtnFace                             = TColor(1 or $80000000);
  clBtnShadow                           = TColor(1 or $80000000);
  clGrayText                            = TColor(1 or $80000000);
  clBtnText                             = TColor(1 or $80000000);
  clInactiveCaptionText                 = TColor(1 or $80000000);
  clBtnHighlight                        = TColor(1 or $80000000);
  cl3DDkShadow                          = TColor(1 or $80000000);
  cl3DLight                             = TColor(1 or $80000000);
  clInfoText                            = TColor(1 or $80000000);
  clInfoBk                              = TColor(1 or $80000000);
  clGradientActiveCaption               = TColor(1 or $80000000);
  clGradientInactiveCaption             = TColor(1 or $80000000);

const
  clBlack                               = TColor($000000);
  clMaroon                              = TColor($000080);
  clGreen                               = TColor($008000);
  clOlive                               = TColor($008080);
  clNavy                                = TColor($800000);
  clPurple                              = TColor($800080);
  clTeal                                = TColor($808000);
  clGray                                = TColor($808080);
  clSilver                              = TColor($C0C0C0);
  clRed                                 = TColor($0000FF);
  clLime                                = TColor($00FF00);
  clYellow                              = TColor($00FFFF);
  clBlue                                = TColor($FF0000);
  clFuchsia                             = TColor($FF00FF);
  clAqua                                = TColor($FFFF00);
  clLtGray                              = TColor($C0C0C0);
  clDkGray                              = TColor($808080);
  clWhite                               = TColor($FFFFFF);
  StandardColorsCount                   = 16;

  clMoneyGreen                          = TColor($C0DCC0);
  clSkyBlue                             = TColor($F0CAA6);
  clCream                               = TColor($F0FBFF);
  clMedGray                             = TColor($A4A0A0);
  ExtendedColorsCount                   = 4;

  clNone                                = TColor($1FFFFFFF);
  clDefault                             = TColor($20000000);

implementation
end.
