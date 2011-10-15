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

unit SilBeTypeInfo;

interface

{$INCLUDE Defines.inc}

uses
  SilBeError,
  TypInfo;

type
  TTypeKind                         = TypInfo.TTypeKind;

const
  tkUnknown                         = TTypeKind(TypInfo.tkUnknown);
  tkInteger                         = TTypeKind(TypInfo.tkInteger);
  tkChar                            = TTypeKind(TypInfo.tkChar);
  tkEnumeration                     = TTypeKind(TypInfo.tkEnumeration);
  tkFloat                           = TTypeKind(TypInfo.tkFloat);
  tkString                          = TTypeKind(TypInfo.tkString);
  tkSet                             = TTypeKind(TypInfo.tkSet);
  tkClass                           = TTypeKind(TypInfo.tkClass);
  tkMethod                          = TTypeKind(TypInfo.tkMethod);
  tkWChar                           = TTypeKind(TypInfo.tkWChar);
  tkLString                         = TTypeKind(TypInfo.tkLString);
  tkWString                         = TTypeKind(TypInfo.tkWString);
  tkVariant                         = TTypeKind(TypInfo.tkVariant);
  tkArray                           = TTypeKind(TypInfo.tkArray);
  tkRecord                          = TTypeKind(TypInfo.tkRecord);
  tkInterface                       = TTypeKind(TypInfo.tkInterface);
  tkInt64                           = TTypeKind(TypInfo.tkInt64);
  tkDynArray                        = TTypeKind(TypInfo.tkDynArray);

type
  TTypeKinds                        = TypInfo.TTypeKinds;

type
  TOrdType                          = TypInfo.TOrdType;

const
  otSByte                           = TOrdType(TypInfo.otSByte);
  otUByte                           = TOrdType(TypInfo.otUByte);
  otSWord                           = TOrdType(TypInfo.otSWord);
  otUWord                           = TOrdType(TypInfo.otUWord);
  otSLong                           = TOrdType(TypInfo.otSLong);
  otULong                           = TOrdType(TypInfo.otULong);

type
  TFloatType                        = TypInfo.TFloatType;

const
  ftSingle                          = TFloatType(TypInfo.ftSingle);
  ftDouble                          = TFloatType(TypInfo.ftDouble);
  ftExtended                        = TFloatType(TypInfo.ftExtended);
  ftComp                            = TFloatType(TypInfo.ftComp);
  ftCurr                            = TFloatType(TypInfo.ftCurr);

type
  TMethodKind                       = TypInfo.TMethodKind;

const
  mkProcedure                       = TMethodKind(TypInfo.mkProcedure);
  mkFunction                        = TMethodKind(TypInfo.mkFunction);
  mkConstructor                     = TMethodKind(TypInfo.mkConstructor);
  mkDestructor                      = TMethodKind(TypInfo.mkDestructor);
  mkClassProcedure                  = TMethodKind(TypInfo.mkClassProcedure);
  mkClassFunction                   = TMethodKind(TypInfo.mkClassFunction);
  mkSafeProcedure                   = TMethodKind(TypInfo.mkSafeProcedure);
  mkSafeFunction                    = TMethodKind(TypInfo.mkSafeFunction);

type
  TParamAttribute                   = (
      paVar,
      paConst,
      paArray,
      paAddress,
      paReference,
      paOut,
      paResult
    );

type
  TParamAttributes                  = set of TParamAttribute;
  TParamAttributesBase              = set of TParamAttribute;

type
  TIntfFlag                         = TypInfo.TIntfFlag;

type
  TIntfFlags                        = TypInfo.TIntfFlags;
  TIntfFlagsBase                    = TypInfo.TIntfFlagsBase;

type
  TCallingKind = (
      ckRegister,
      ckCdecl,
      ckPascal,
      ckStdCall,
      ckSafeCall
    );

type
  ShortStringBase                   = TypInfo.ShortStringBase;

type
  PPTypeInfo                        = TypInfo.PPTypeInfo;
  PTypeInfo                         = TypInfo.PTypeInfo;

type
  PTypeData                         = TypInfo.PTypeData;
  PPropInfo                         = TypInfo.PPropInfo;

type
  TPropInfoProc                     = TypInfo.TPropInfoProc;
  PPropList                         = TypInfo.PPropList;

{$IFDEF D50}
type
  EPropertyError        = TypInfo.EPropertyError;
  EPropertyConvertError = TypInfo.EPropertyConvertError;
{$ELSE}
  EPropertyError        = class(EAbort);
  EPropertyConvertError = class(EAbort);
{$ENDIF}

type
  EEnumIdentNotFound    = class(EAbort);
  EEnumValueOutOfRange  = class(EAbort);
  EEnumDataNotFound     = class(EAbort);

const
  tkAny           = TypInfo.tkAny;
  tkMethods       = TypInfo.tkMethod;
  tkProperties    = TypInfo.tkProperties;

implementation
end.
