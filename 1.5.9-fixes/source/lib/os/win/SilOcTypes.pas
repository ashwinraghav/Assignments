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
  SilOeTypes,
  Windows;

const
  ERROR_SUCCESS         = Windows.ERROR_SUCCESS;
  INFINITE              = Windows.INFINITE;
  NULL_HANDLE_VALUE     = THandle(0);
  INVALID_HANDLE_VALUE  = THandle(Windows.INVALID_HANDLE_VALUE);

const
  S_OK                  = HRESULT(Windows.S_OK          );
  S_FALSE               = HRESULT(Windows.S_FALSE       );
  NOERROR               = HRESULT(Windows.NOERROR       );
  E_UNEXPECTED          = HRESULT(Windows.E_UNEXPECTED  );
  E_NOTIMPL             = HRESULT(Windows.E_NOTIMPL     );
  E_OUTOFMEMORY         = HRESULT(Windows.E_OUTOFMEMORY );
  E_INVALIDARG          = HRESULT(Windows.E_INVALIDARG  );
  E_NOINTERFACE         = HRESULT(Windows.E_NOINTERFACE );
  E_POINTER             = HRESULT(Windows.E_POINTER     );
  E_HANDLE              = HRESULT(Windows.E_HANDLE      );
  E_ABORT               = HRESULT(Windows.E_ABORT       );
  E_FAIL                = HRESULT(Windows.E_FAIL        );
  E_ACCESSDENIED        = HRESULT(Windows.E_ACCESSDENIED);
  E_PENDING             = HRESULT(Windows.E_PENDING     );
  
const
  REGDB_E_CLASSNOTREG   = HRESULT(Windows.REGDB_E_CLASSNOTREG);

const
  CPathSeparator        = Char('\');
  CPathListSeparator    = Char(';');

const
  SwitchChars           = ['/', '-'];

{$IFNDEF D60}
const
  sLineBreak            = #13#10;
{$ENDIF}

const
  clScrollBar                           = COLOR_SCROLLBAR               or $80000000;
  clBackground                          = COLOR_BACKGROUND              or $80000000;
  clActiveCaption                       = COLOR_ACTIVECAPTION           or $80000000;
  clInactiveCaption                     = COLOR_INACTIVECAPTION         or $80000000;
  clMenu                                = COLOR_MENU                    or $80000000;
  clWindow                              = COLOR_WINDOW                  or $80000000;
  clWindowFrame                         = COLOR_WINDOWFRAME             or $80000000;
  clMenuText                            = COLOR_MENUTEXT                or $80000000;
  clWindowText                          = COLOR_WINDOWTEXT              or $80000000;
  clCaptionText                         = COLOR_CAPTIONTEXT             or $80000000;
  clActiveBorder                        = COLOR_ACTIVEBORDER            or $80000000;
  clInactiveBorder                      = COLOR_INACTIVEBORDER          or $80000000;
  clAppWorkSpace                        = COLOR_APPWORKSPACE            or $80000000;
  clHighlight                           = COLOR_HIGHLIGHT               or $80000000;
  clHighlightText                       = COLOR_HIGHLIGHTTEXT           or $80000000;
  clBtnFace                             = COLOR_BTNFACE                 or $80000000;
  clBtnShadow                           = COLOR_BTNSHADOW               or $80000000;
  clGrayText                            = COLOR_GRAYTEXT                or $80000000;
  clBtnText                             = COLOR_BTNTEXT                 or $80000000;
  clInactiveCaptionText                 = COLOR_INACTIVECAPTIONTEXT     or $80000000;
  clBtnHighlight                        = COLOR_BTNHIGHLIGHT            or $80000000;
  cl3DDkShadow                          = COLOR_3DDKSHADOW              or $80000000;
  cl3DLight                             = COLOR_3DLIGHT                 or $80000000;
  clInfoText                            = COLOR_INFOTEXT                or $80000000;
  clInfoBk                              = COLOR_INFOBK                  or $80000000;
  clGradientActiveCaption               = COLOR_GRADIENTACTIVECAPTION   or $80000000;
  clGradientInactiveCaption             = COLOR_GRADIENTINACTIVECAPTION or $80000000;

  clBlack                               = $000000;
  clMaroon                              = $000080;
  clGreen                               = $008000;
  clOlive                               = $008080;
  clNavy                                = $800000;
  clPurple                              = $800080;
  clTeal                                = $808000;
  clGray                                = $808080;
  clSilver                              = $C0C0C0;
  clRed                                 = $0000FF;
  clLime                                = $00FF00;
  clYellow                              = $00FFFF;
  clBlue                                = $FF0000;
  clFuchsia                             = $FF00FF;
  clAqua                                = $FFFF00;
  clLtGray                              = $C0C0C0;
  clDkGray                              = $808080;
  clWhite                               = $FFFFFF;
  StandardColorsCount                   = 16;

  clMoneyGreen                          = $C0DCC0;
  clSkyBlue                             = $F0CAA6;
  clCream                               = $F0FBFF;
  clMedGray                             = $A4A0A0;
  ExtendedColorsCount                   = 4;

  clNone                                = $1FFFFFFF;
  clDefault                             = $20000000;

implementation
end.
