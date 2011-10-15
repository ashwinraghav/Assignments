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

unit SilBeTypes;

{$I Defines.inc}

interface

uses
  SilBeDelphiTypes,
  SilBcDelphi;

type
  Int8                                  = System.ShortInt;
  Int16                                 = System.SmallInt;
  Int32                                 = System.LongInt;
  
type
  Uint8                                 = System.Byte;
  Uint16                                = System.Word;
  Uint32                                = System.Cardinal;

type
  Float32                               = System.Single;
  Float64                               = System.Double;
  Float80                               = System.Extended;

type
  Int64                                 = SilBeDelphiTypes.Int64;

type
  LargeInt                              = Int64;
  LongWord                              = Uint32;

type
  LongBool                              = SilBeDelphiTypes.LongBool;

type
  TMethod                               = SilBeDelphiTypes.TMethod;

type
  PByte                                 = SilBeDelphiTypes.PByte;
  PWord                                 = SilBeDelphiTypes.PWord;
  PLongint                              = SilBeDelphiTypes.PLongint;
  PInteger                              = SilBeDelphiTypes.PInteger;
  PLongWord                             = SilBeDelphiTypes.PLongWord;
  PSmallInt                             = SilBeDelphiTypes.PSmallint;
  PDouble                               = SilBeDelphiTypes.PDouble;
  PShortInt                             = SilBeDelphiTypes.PShortInt;
  PLargeInt                             = ^LargeInt;

type
  PInt64                                = SilBeDelphiTypes.PInt64;
  PExtended                             = SilBeDelphiTypes.PExtended;
  PComp                                 = SilBeDelphiTypes.PComp;
  PCurrency                             = SilBeDelphiTypes.PCurrency;
  PVariant                              = SilBeDelphiTypes.PVariant;
  POleVariant                           = SilBeDelphiTypes.POleVariant;
  PPointer                              = SilBeDelphiTypes.PPointer;
  PBoolean                              = SilBeDelphiTypes.PBoolean;
  PCardinal                             = SilBeDelphiTypes.PCardinal;
  PSingle                               = SilBeDelphiTypes.PSingle;
  PDate                                 = SilBeDelphiTypes.PDate;
  PDispatch                             = SilBeDelphiTypes.PDispatch;
  PPDispatch                            = SilBeDelphiTypes.PPDispatch;
  PError                                = SilBeDelphiTypes.PError;
  PWordBool                             = SilBeDelphiTypes.PWordBool;
  PLongBool                             = ^LongBool;


type
  PPUnknown                             = SilBeDelphiTypes.PPUnknown;
  PPWideChar                            = SilBeDelphiTypes.PPWideChar;
  PPChar                                = SilBeDelphiTypes.PPChar;
  PPAnsiChar                            = SilBeDelphiTypes.PPAnsiChar;

type
  TClass                                = SilBeDelphiTypes.TClass;
  TObject                               = SilBeDelphiTypes.TObject;
  IInterface                            = SilBeDelphiTypes.IInterface;
  IInvokable                            = SilBeDelphiTypes.IInvokable;

type
  PUnknown                              = SilBeDelphiTypes.PUnknown;
  PInterface                            = SilBeDelphiTypes.PInterface;
  PClass                                = SilBeDelphiTypes.PClass;
  PObject                               = SilBeDelphiTypes.PObject;

type
  LongDouble                            = Float80;

type
  WordRec = packed record
    case Integer of
      0: (Lo, Hi: Byte);
      1: (Bytes: array [0..1] of Byte);
  end;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  Int64Rec = packed record
    case Integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;

type
  TIntegerRange                         = 0 .. SizeOf(Integer) * 8 - 1;
  TAsciiRange                           = #32 .. #127;
  TCharRange                            = #0  .. #255;
  TByteRange                            = 0   .. 255;
  TCtrlRange                            = #0  .. #31;

type
  TIntegerSet                           = set of TIntegerRange;
  TAsciiSet                             = set of TAsciiRange;
  TCharSet                              = set of TCharRange;
  TByteSet                              = set of TByteRange;
  TCtrlSet                              = set of TCtrlRange;

const
  ccCtrlChars                           = [Low(TCtrlRange) .. High(TCtrlRange)];
  ccSpaceChars                          = ccCtrlChars + [#32];

{$IFNDEF D60}
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: Pointer;
  end;
{$ENDIF}

type                                    
{$IFDEF USE_DYNARRAYS}                  
  TByteArray                            = array of Byte;
  TWordArray                            = array of Word;
  TLongArray                            = array of LongWord;
  TCardinalArray                        = TLongArray;
  TLongWordArray                        = TLongArray;
  TFloatArray                           = array of Double;
  TIntegerArray                         = array of Integer;
  TStringArray                          = array of String;
  TVarRecArray                          = array of TVarRec;
  TGuidArray                            = array of TGuid;
  TVariantArray                         = array of Variant;
  TDateTimeArray                        = array of TDateTime;
{$ELSE}                                 
  TCardinalArray                        = array [0..MaxInt div SizeOf(Cardinal) - 1] of Cardinal;
  TIntegerArray                         = array [0..MaxInt div SizeOf(Integer) - 1] of Integer;
  TStringArray                          = array [0..MaxInt div SizeOf(Integer) - 1] of String;
{$ENDIF}                                

type
  PByteArray                            = ^TByteArray;
  PWordArray                            = ^TWordArray;
  PLongArray                            = ^TLongArray;
  PCardinalArray                        = ^TCardinalArray;
  PLongWordArray                        = ^TLongWordArray;
  PIntegerArray                         = ^TIntegerArray;
  PStringArray                          = ^TStringArray;
  PVarRecArray                          = ^TVarRecArray;
  PGuidArray                            = ^TGuidArray;
  PVariantArray                         = ^TVariantArray;

const                                   
  varEmpty                              = SilBcDelphi.varEmpty;
  varNull                               = SilBcDelphi.varNull;
  varSmallint                           = SilBcDelphi.varSmallint;
  varInteger                            = SilBcDelphi.varInteger;
  varSingle                             = SilBcDelphi.varSingle;
  varDouble                             = SilBcDelphi.varDouble;
  varCurrency                           = SilBcDelphi.varCurrency;
  varDate                               = SilBcDelphi.varDate;
  varOleStr                             = SilBcDelphi.varOleStr;
  varDispatch                           = SilBcDelphi.varDispatch;
  varError                              = SilBcDelphi.varError;
  varBoolean                            = SilBcDelphi.varBoolean;
  varVariant                            = SilBcDelphi.varVariant;
  varUnknown                            = SilBcDelphi.varUnknown;
  varByte                               = SilBcDelphi.varByte;
  varStrArg                             = SilBcDelphi.varStrArg;
  varString                             = SilBcDelphi.varString;
  varAny                                = SilBcDelphi.varAny;
  varTypeMask                           = SilBcDelphi.varTypeMask;
  varArray                              = SilBcDelphi.varArray;
  varByRef                              = SilBcDelphi.varByRef;
                                        
const                                   
  varDecimal                            = SilBcDelphi.varDecimal ; 
  varShortInt                           = SilBcDelphi.varShortInt; 
  varWord                               = SilBcDelphi.varWord    ; 
  varLongWord                           = SilBcDelphi.varLongWord; 
  varInt64                              = SilBcDelphi.varInt64   ; 

type
  TTimeParts = record
    Hour: Word;
    Minutes: Word;
    Seconds: Word;
    mSeconds: Word;
  end;

type
  TDateParts = record
    Year: Word;
    Month: Word;
    Day: Word;
  end;

type
  TDateTimeParts = record
    Date: TDateParts;
    Time: TTimeParts;
  end;

type
  TDayOfWeek = (dwUnknown, dwSunday, dwMonday, dwTuesday, dwWednesday, dwThursday, dwFriday, dwSaturday);
  TDaysOfWeek = set of dwSunday..dwSaturday;

type
  TTimeElement = (teYear, teMonth, teDay, teHour, teMinute, teSecond, teMSecond);
  TTimeElements = set of TTimeElement;

type
  TDayPosition = 0..5;
  TDayPositions = set of TDayPosition;

type
  TDateTimeStamp = packed record
    Time: Integer;
    Date: Integer;
  end;

implementation
end.                                                
