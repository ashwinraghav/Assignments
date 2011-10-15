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

unit SilBeError;

interface

uses
  SysUtils;

{$INCLUDE Defines.inc}

type
  ExceptionType                  = SysUtils.ExceptClass;
  Exception                      = SysUtils.Exception;
  EAbort                         = SysUtils.EAbort;
  EHeapException                 = SysUtils.EHeapException;
  EOutOfMemory                   = SysUtils.EOutOfMemory;
  EInOutError                    = SysUtils.EInOutError;
  EExternal                      = SysUtils.EExternal;
  EExternalException             = SysUtils.EExternalException;
  EIntError                      = SysUtils.EIntError;
  EDivByZero                     = SysUtils.EDivByZero;
  ERangeError                    = SysUtils.ERangeError;
  EIntOverflow                   = SysUtils.EIntOverflow;
  EMathError                     = SysUtils.EMathError;
  EInvalidOp                     = SysUtils.EInvalidOp;
  EZeroDivide                    = SysUtils.EZeroDivide;
  EOverflow                      = SysUtils.EOverflow;
  EUnderflow                     = SysUtils.EUnderflow;
  EInvalidPointer                = SysUtils.EInvalidPointer;
  EInvalidCast                   = SysUtils.EInvalidCast;
  EConvertError                  = SysUtils.EConvertError;
  EAccessViolation               = SysUtils.EAccessViolation;
  EPrivilege                     = SysUtils.EPrivilege;
  EStackOverflow                 = SysUtils.EStackOverflow;
  EControlC                      = SysUtils.EControlC;
  EVariantError                  = SysUtils.EVariantError;
  EPropReadOnly                  = SysUtils.EPropReadOnly;
  EPropWriteOnly                 = SysUtils.EPropWriteOnly;
  EAssertionFailed               = SysUtils.EAssertionFailed;
  EIntfCastError                 = SysUtils.EIntfCastError;
  EInvalidContainer              = SysUtils.EInvalidContainer;
  EInvalidInsert                 = SysUtils.EInvalidInsert;
  EPackageError                  = SysUtils.EPackageError;
  ESafecallException             = SysUtils.ESafecallException;

{$IFDEF D60}
  EOSError                       = SysUtils.EOSError;
{$ELSE}
  EOSError                       = SysUtils.EWin32Error;
{$ENDIF}

{$IFNDEF PC_MAPPED_EXCEPTIONS}  // linux
  EAbstractError                 = SysUtils.EAbstractError;
{$ENDIF}

type
  ESilException                  = class(Exception);

implementation
end.
 