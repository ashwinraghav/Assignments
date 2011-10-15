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

unit SilOsTypes;

{$I Defines.inc}

interface

uses
  SilOcTypes,
  SilOeTypes;

const
  ERROR_SUCCESS                               = SilOcTypes.ERROR_SUCCESS;
  INFINITE                                    = SilOcTypes.INFINITE;
  INVALID_HANDLE_VALUE                        = SilOcTypes.INVALID_HANDLE_VALUE;
  NULL_HANDLE_VALUE                           = SilOcTypes.NULL_HANDLE_VALUE;

const
  S_OK                                        = HRESULT(SilOcTypes.S_OK          );
  S_FALSE                                     = HRESULT(SilOcTypes.S_FALSE       );
  NOERROR                                     = HRESULT(SilOcTypes.NOERROR       );
  E_UNEXPECTED                                = HRESULT(SilOcTypes.E_UNEXPECTED  );
  E_NOTIMPL                                   = HRESULT(SilOcTypes.E_NOTIMPL     );
  E_INVALIDARG                                = HRESULT(SilOcTypes.E_INVALIDARG  );
  E_NOINTERFACE                               = HRESULT(SilOcTypes.E_NOINTERFACE );
  E_POINTER                                   = HRESULT(SilOcTypes.E_POINTER     );
  E_HANDLE                                    = HRESULT(SilOcTypes.E_HANDLE      );
  E_FAIL                                      = HRESULT(SilOcTypes.E_FAIL        );
  E_ACCESSDENIED                              = HRESULT(SilOcTypes.E_ACCESSDENIED);

const
  E_OUTOFMEMORY                               = HRESULT(SilOcTypes.E_OUTOFMEMORY );
  E_ABORT                                     = HRESULT(SilOcTypes.E_ABORT       );
  E_PENDING                                   = HRESULT(SilOcTypes.E_PENDING     );

//const
//  REGDB_E_CLASSNOTREG                         = HRESULT(SilOcTypes.REGDB_E_CLASSNOTREG);

type
  THandle                                     = SilOeTypes.THandle;
  THandleArray                                = SilOeTypes.THandleArray;

type
  OsWord                                      = SilOeTypes.OsWord;
  OsBool                                      = SilOeTypes.OsBool;
  OsByte                                      = SilOeTypes.OsByte;
  OsChar                                      = SilOeTypes.OsChar;
  OsBytePtr                                   = SilOeTypes.OsBytePtr;
  OsCharPtr                                   = SilOeTypes.OsCharPtr;

type
  OsCode                                      = SilOeTypes.OsWord;
  OsState                                     = SilOeTypes.OsBool;

const
  CPathSeparator                              = SilOcTypes.CPathSeparator;
  CPathListSeparator                          = SilOcTypes.CPathListSeparator;

const
  SwitchChars                                 = SilOcTypes.SwitchChars;

{$IFNDEF D60}
const
  sLineBreak                                  = SilOcTypes.sLineBreak;
{$ENDIF}


type
  OsColorPtr                                  = SilOeTypes.OsColorPtr;
  OsColor                                     = SilOeTypes.OsColor;

const
  clScrollBar                                 = OsColor(SilOcTypes.clScrollBar              );
  clBackground                                = OsColor(SilOcTypes.clBackground             );
  clActiveCaption                             = OsColor(SilOcTypes.clActiveCaption          );
  clInactiveCaption                           = OsColor(SilOcTypes.clInactiveCaption        );
  clMenu                                      = OsColor(SilOcTypes.clMenu                   );
  clWindow                                    = OsColor(SilOcTypes.clWindow                 );
  clWindowFrame                               = OsColor(SilOcTypes.clWindowFrame            );
  clMenuText                                  = OsColor(SilOcTypes.clMenuText               );
  clWindowText                                = OsColor(SilOcTypes.clWindowText             );
  clCaptionText                               = OsColor(SilOcTypes.clCaptionText            );
  clActiveBorder                              = OsColor(SilOcTypes.clActiveBorder           );
  clInactiveBorder                            = OsColor(SilOcTypes.clInactiveBorder         );
  clAppWorkSpace                              = OsColor(SilOcTypes.clAppWorkSpace           );
  clHighlight                                 = OsColor(SilOcTypes.clHighlight              );
  clHighlightText                             = OsColor(SilOcTypes.clHighlightText          );
  clBtnFace                                   = OsColor(SilOcTypes.clBtnFace                );
  clBtnShadow                                 = OsColor(SilOcTypes.clBtnShadow              );
  clGrayText                                  = OsColor(SilOcTypes.clGrayText               );
  clBtnText                                   = OsColor(SilOcTypes.clBtnText                );
  clInactiveCaptionText                       = OsColor(SilOcTypes.clInactiveCaptionText    );
  clBtnHighlight                              = OsColor(SilOcTypes.clBtnHighlight           );
  cl3DDkShadow                                = OsColor(SilOcTypes.cl3DDkShadow             );
  cl3DLight                                   = OsColor(SilOcTypes.cl3DLight                );
  clInfoText                                  = OsColor(SilOcTypes.clInfoText               );
  clInfoBk                                    = OsColor(SilOcTypes.clInfoBk                 );
  clGradientActiveCaption                     = OsColor(SilOcTypes.clGradientActiveCaption  );
  clGradientInactiveCaption                   = OsColor(SilOcTypes.clGradientInactiveCaption);

  clBlack                                     = OsColor(SilOcTypes.clBlack                  );
  clMaroon                                    = OsColor(SilOcTypes.clMaroon                 );
  clGreen                                     = OsColor(SilOcTypes.clGreen                  );
  clOlive                                     = OsColor(SilOcTypes.clOlive                  );
  clNavy                                      = OsColor(SilOcTypes.clNavy                   );
  clPurple                                    = OsColor(SilOcTypes.clPurple                 );
  clTeal                                      = OsColor(SilOcTypes.clTeal                   );
  clGray                                      = OsColor(SilOcTypes.clGray                   );
  clSilver                                    = OsColor(SilOcTypes.clSilver                 );
  clRed                                       = OsColor(SilOcTypes.clRed                    );
  clLime                                      = OsColor(SilOcTypes.clLime                   );
  clYellow                                    = OsColor(SilOcTypes.clYellow                 );
  clBlue                                      = OsColor(SilOcTypes.clBlue                   );
  clFuchsia                                   = OsColor(SilOcTypes.clFuchsia                );
  clAqua                                      = OsColor(SilOcTypes.clAqua                   );
  clLtGray                                    = OsColor(SilOcTypes.clLtGray                 );
  clDkGray                                    = OsColor(SilOcTypes.clDkGray                 );
  clWhite                                     = OsColor(SilOcTypes.clWhite                  );

  clMoneyGreen                                = OsColor(SilOcTypes.clMoneyGreen             );
  clSkyBlue                                   = OsColor(SilOcTypes.clSkyBlue                );
  clCream                                     = OsColor(SilOcTypes.clCream                  );
  clMedGray                                   = OsColor(SilOcTypes.clMedGray                );

  clNone                                      = OsColor(SilOcTypes.clNone                   );
  clDefault                                   = OsColor(SilOcTypes.clDefault                );

implementation
end.
