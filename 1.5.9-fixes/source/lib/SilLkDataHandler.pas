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

unit SilLkDataHandler;
     
{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLiContainerTypes,
  SilLhDataType;

type
  PConvertFrame = ^RConvertFrame;
  RConvertFrame = record
    Source, Destination: PChar;
    SourceSize, DestinationSize: LongWord;
    Param: Pointer;
    ReturnedSize: PLongWord;
    Status: TDataTypecastStatus;
  end;

type
  TSilAbstractDataHandlerClass = class of TSilAbstractDataHandler;

  TSilAbstractDataHandler = class(
    TSilDataHandlerType,
    ITypeComparator,
    IDataHandler,
    IDataConvert,
    IDataAssign )
  private
    FList: Pointer;
    FData: IDataType;
    FHandler: ITypeHandler;
  private
    function DoCheckConvertTo(Target: TDataType): Boolean;
    function DoCheckConvertFrom(Target: TDataType): Boolean;
    function DoCheckVariant(const Value: Variant): Variant;
    procedure DoVariantError(const Value: PVarData);
    function DoConvertTo(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus; register;
    function DoConvertFrom(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus; register; 
    procedure DoInvoke; register;
    function DoCreateHandler(const Data: IDataType): ITypeHandler;
    function DoGetDataType: IDataType;
    function DoGetDataList: IDataTypeList;
  protected
    function DoInitialize(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil): TDataTypecastStatus;     
    function DoFinalize(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil): TDataTypecastStatus;     
  protected // ITypeComparator
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer; virtual;
  protected // IDataHandler
    function GetDataType: IDataType;
    function GetHandler: ITypeHandler;
    function GetCast: IDataConvert;
    function GetFrom: IDataAssign;
    function CanConvert(const Target: IDataType): Boolean; overload;
    function CanConvert(const Target: IDataHandler): Boolean; overload;
    function CanConvert(const Target: RDataType): Boolean; overload;
    function CanConvert(const Target: TDataType; Flags: TDataFlags = []): Boolean; overload;
    function Convert(const Target: RDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Convert(const Target: TDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Convert(const Target: IDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Convert(const Target: IDataHandler; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function CanAssign(const Target: IDataType): Boolean; overload;
    function CanAssign(const Target: IDataHandler): Boolean; overload;
    function CanAssign(const Target: RDataType): Boolean; overload;
    function CanAssign(const Target: TDataType; Flags: TDataFlags = []): Boolean; overload;
    function Assign(const Target: RDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Assign(const Target: TDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Assign(const Target: IDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
    function Assign(const Target: IDataHandler; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; overload; stdcall;
  protected
    function NotSupported(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus; virtual; stdcall;
    function Initialize(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil): TDataTypecastStatus; virtual; stdcall;
    function Finalize(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil): TDataTypecastStatus; virtual; stdcall;     
  protected // IDataConvert
    function IDataConvert.Null          = Null;
    function IDataConvert.Smallint       = DoToSmallint;
    function IDataConvert.LongInt        = DoToLongInt;
    function IDataConvert.Single         = DoToSingle;
    function IDataConvert.Double         = DoToDouble;
    function IDataConvert.Currency       = DoToCurrency;
    function IDataConvert.DateTime       = DoToDateTime;
    function IDataConvert.WideString     = DoToWideString;
    function IDataConvert.Dispatch       = DoToDispatch;
    function IDataConvert.Error          = DoToError;
    function IDataConvert.WordBool       = DoToWordBool;
    function IDataConvert.Variant        = DoToVariant;
    function IDataConvert.IInterface     = DoToInterface;
    function IDataConvert.Decimal        = DoToDecimal;
    function IDataConvert.Extended       = DoToExtended;
    function IDataConvert.ShortInt       = DoToShortInt;
    function IDataConvert.Byte           = DoToByte;
    function IDataConvert.Word           = DoToWord;
    function IDataConvert.LongWord       = DoToLongWord;
    function IDataConvert.LargeInt       = DoToLargeInt;
    function IDataConvert.LargeWord      = DoToLargeWord;
    function IDataConvert.Integer        = DoToInteger;
    function IDataConvert.Cardinal       = DoToCardinal;
    function IDataConvert.Void           = DoToVoid;
    function IDataConvert.HResult        = DoToHResult;
    function IDataConvert.Pointer        = DoToPointer;
    function IDataConvert.Safearray      = DoToSafearray;
    function IDataConvert.Dynarray       = DoToDynarray;
    function IDataConvert.Userdefined    = DoToUserdefined;
    function IDataConvert.PAnsiChar      = DoToPAnsiChar;
    function IDataConvert.PWideChar      = DoToPWideChar;
    function IDataConvert.GUID           = DoToGUID;
    function IDataConvert.TClass         = DoToClass;
    function IDataConvert.TObject        = DoToObject;
    function IDataConvert.Boolean        = DoToBoolean;
    function IDataConvert.LongBool       = DoToLongBool;
    function IDataConvert.AnsiChar       = DoToAnsiChar;
    function IDataConvert.WideChar       = DoToWideChar;
    function IDataConvert.AnsiString     = DoToAnsiString;
  protected
    function DoToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToDynarray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
  protected // IDataAssign
    function IDataAssign.Clear          = Clear;
    function IDataAssign.Smallint       = DoFromSmallint;
    function IDataAssign.LongInt        = DoFromLongInt;
    function IDataAssign.Single         = DoFromSingle;
    function IDataAssign.Double         = DoFromDouble;
    function IDataAssign.Currency       = DoFromCurrency;
    function IDataAssign.DateTime       = DoFromDateTime;
    function IDataAssign.WideString     = DoFromWideString;
    function IDataAssign.Dispatch       = DoFromDispatch;
    function IDataAssign.Error          = DoFromError;
    function IDataAssign.WordBool       = DoFromWordBool;
    function IDataAssign.Variant        = DoFromVariant;
    function IDataAssign.IInterface     = DoFromInterface;
    function IDataAssign.Decimal        = DoFromDecimal;
    function IDataAssign.Extended       = DoFromExtended;
    function IDataAssign.ShortInt       = DoFromShortInt;
    function IDataAssign.Byte           = DoFromByte;
    function IDataAssign.Word           = DoFromWord;
    function IDataAssign.LongWord       = DoFromLongWord;
    function IDataAssign.LargeInt       = DoFromLargeInt;
    function IDataAssign.LargeWord      = DoFromLargeWord;
    function IDataAssign.Integer        = DoFromInteger;
    function IDataAssign.Cardinal       = DoFromCardinal;
    function IDataAssign.Void           = DoFromVoid;
    function IDataAssign.HResult        = DoFromHResult;
    function IDataAssign.Pointer        = DoFromPointer;
    function IDataAssign.Safearray      = DoFromSafearray;
    function IDataAssign.Dynarray       = DoFromDynarray;
    function IDataAssign.Userdefined    = DoFromUserdefined;
    function IDataAssign.PAnsiChar      = DoFromPAnsiChar;
    function IDataAssign.PWideChar      = DoFromPWideChar;
    function IDataAssign.GUID           = DoFromGUID;
    function IDataAssign.TClass         = DoFromClass;
    function IDataAssign.TObject        = DoFromObject;
    function IDataAssign.Boolean        = DoFromBoolean;
    function IDataAssign.LongBool       = DoFromLongBool;
    function IDataAssign.AnsiChar       = DoFromAnsiChar;
    function IDataAssign.WideChar       = DoFromWideChar;
    function IDataAssign.AnsiString     = DoFromAnsiString;
  protected
    function DoFromEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromDynarray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DoFromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
  protected // IDataConvert
    function Null(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSIsNull): TDataTypecastStatus; virtual; stdcall;
    function ToEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; 
    function ToNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; 
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract; 
    function ToHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToArray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
  protected // IDataAssign
    function Clear(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSIsNull): TDataTypecastStatus; virtual; stdcall;
    function FromEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall;
    function FromNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall;
    function FromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromArray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
    function FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; virtual; stdcall; abstract;
  public
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; override;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; override;
    class function TypeSize(Def: PDataTypeDef): LongWord; override;
  public
    property Data: IDataType read DoGetDataType;
    property List: IDataTypeList read DoGetDataList;
    property Handler: ITypeHandler read FHandler;
  public
    constructor CreateNew(const List: IDataTypeList; Def: PDataTypeDef; Parameter: Pointer = nil); reintroduce; virtual;
    destructor Destroy; override;
  public
    class function Create(const List: IDataTypeList; Def: PDataTypeDef; const IID: TGUID; Ref: PUnknown; Parameter: Pointer = nil): Boolean; overload; override;
    class function Create(Def: PDataTypeDef; Parameter: Pointer = nil): IDataHandler; overload;
  end;

implementation

uses
  SilAfStackUtils,
  SilBtError,
  SilBtMem,
  SilBtVart,
  SilLdDataHandler,
  SilLtTypeInfo,
  SilLmDataType,
  SilLtContainer;

{$O+,W-,R-,V-,Q-}

{ TSilAbstractDataHandler }

class function TSilAbstractDataHandler.Create(Def: PDataTypeDef; Parameter: Pointer): IDataHandler;
begin
  Create(Typ.List, Def, IDataHandler, @Result, Parameter);
end;

class function TSilAbstractDataHandler.Create(const List: IDataTypeList; Def: PDataTypeDef; const IID: TGUID; Ref: PUnknown; Parameter: Pointer): Boolean;  
var
  Handler: TSilDataHandlerClass;
  Instance: IUnknown;
begin
  if Assigned(Def) then
    Handler := Def.Handler else
    Handler := nil;

  if not Assigned(Handler) then Handler := Self;

  Instance := TSilAbstractDataHandlerClass(Handler).CreateNew(List, Def, Parameter);
  try
    Result := Instance.QueryInterface(IID, Ref^) <> 0;
  finally
    Instance := nil;
  end;
end;

constructor TSilAbstractDataHandler.CreateNew(const List: IDataTypeList; Def: PDataTypeDef; Parameter: Pointer);
begin
  inherited Create;
  FList := Pointer(List);
  FData := TSilDataType.Create(Self, Def);
end;

destructor TSilAbstractDataHandler.Destroy;
begin
  FHandler := nil;
  FData := nil;
  FList := nil;
  inherited;
end;

class function TSilAbstractDataHandler.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := nil;
end;

class function TSilAbstractDataHandler.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := Typ.Data.ToFieldType(Def.DataType);
end;

class function TSilAbstractDataHandler.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := 0;
end;

function TSilAbstractDataHandler.DoInitialize(Destination: PChar; DestinationSize: LongWord; Param: Pointer): TDataTypecastStatus; 
begin
  Mem.Clear(Destination^, DestinationSize);
  if Assigned(FHandler) then Handler.Initialize(Destination);
  Result := Initialize(Destination, DestinationSize, Param);
end;

function TSilAbstractDataHandler.DoFinalize(Destination: PChar; DestinationSize: LongWord; Param: Pointer): TDataTypecastStatus; 
begin
  Result := Finalize(Destination, DestinationSize, Param);
  if Assigned(FHandler) then Handler.Finalize(Destination);
  Mem.Clear(Destination^, DestinationSize)
end;

function TSilAbstractDataHandler.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Integer(Data1) - Integer(Data2);
end;

function TSilAbstractDataHandler.GetDataType: IDataType;
begin
  Result := FData;
end;

function TSilAbstractDataHandler.GetHandler: ITypeHandler;
begin
  if FHandler = nil then FHandler := DoCreateHandler(Data);
  Result := FHandler;
end;

function TSilAbstractDataHandler.GetCast: IDataConvert;
begin
  Result := IDataConvert(Self);
end;

function TSilAbstractDataHandler.GetFrom: IDataAssign;
begin
  Result := IDataAssign(Self);
end;

function TSilAbstractDataHandler.CanConvert(const Target: IDataType): Boolean;
begin
  Result := CanConvert(Target.Kind.Value);
end;

function TSilAbstractDataHandler.CanConvert(const Target: IDataHandler): Boolean;
begin
  Result := CanConvert(Target.DataType);
end;

function TSilAbstractDataHandler.CanConvert(const Target: RDataType): Boolean;
begin
  Result := CanConvert(Target.Value, Target.Flags);
end;

function TSilAbstractDataHandler.CanConvert(const Target: TDataType; Flags: TDataFlags): Boolean;
begin
  Result := DoCheckConvertTo(Target);
end;

function TSilAbstractDataHandler.Convert(const Target: RDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertTo(
      Data.Kind.Value,
      Target.Value,
      @Source);
end;

function TSilAbstractDataHandler.Convert(const Target: TDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertTo(
      Data.Kind.Value,
      Target,
      @Source);
end;

function TSilAbstractDataHandler.Convert(const Target: IDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertTo(
      Data.Kind.Value,
      Target.Kind.Value,
      @Source);
end;

function TSilAbstractDataHandler.Convert(const Target: IDataHandler; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertTo(
      Data.Kind.Value,
      Target.DataType.Kind.Value,
      @Source);
end;

function TSilAbstractDataHandler.CanAssign(const Target: IDataType): Boolean;
begin
  Result := CanAssign(Target.Kind.Value);
end;

function TSilAbstractDataHandler.CanAssign(const Target: IDataHandler): Boolean;
begin
  Result := CanAssign(Target.DataType);
end;

function TSilAbstractDataHandler.CanAssign(const Target: RDataType): Boolean;
begin
  Result := CanAssign(Target.Value, Target.Flags);
end;

function TSilAbstractDataHandler.CanAssign(const Target: TDataType; Flags: TDataFlags): Boolean;
begin
  Result := DoCheckConvertFrom(Target);
end;

function TSilAbstractDataHandler.Assign(const Target: RDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertFrom(
      Data.Kind.Value,
      Target.Value,
      @Source);
end;

function TSilAbstractDataHandler.Assign(const Target: TDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertFrom(
      Data.Kind.Value,
      Target,
      @Source);
end;

function TSilAbstractDataHandler.Assign(const Target: IDataType; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertFrom(
      Data.Kind.Value,
      Target.Kind.Value,
      @Source);
end;

function TSilAbstractDataHandler.Assign(const Target: IDataHandler; Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result :=
    DoConvertFrom(
      Data.Kind.Value,
      Target.DataType.Kind.Value,
      @Source);
end;

function TSilAbstractDataHandler.NotSupported(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus;
begin
  if Assigned(Params.ReturnedSize) then Params.ReturnedSize^ := 0;
  Result := tcECantConvert;
end;

function TSilAbstractDataHandler.Initialize(Destination: PChar; DestinationSize: LongWord; Param: Pointer): TDataTypecastStatus; 
begin
  Result := tcSDefault;
end;

function TSilAbstractDataHandler.Finalize(Destination: PChar; DestinationSize: LongWord; Param: Pointer): TDataTypecastStatus; 
begin
  Result := tcSUnassigned;
end;

function TSilAbstractDataHandler.DoToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtSmallint) then
    Result := ToSmallint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSmallint, @Source);
end;

function TSilAbstractDataHandler.DoToLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtLongint) then
    Result := ToLongint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongint, @Source);
end;

function TSilAbstractDataHandler.DoToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtSingle) then
    Result := ToSingle(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSingle, @Source);
end;

function TSilAbstractDataHandler.DoToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtDouble) then
    Result := ToDouble(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDouble, @Source);
end;

function TSilAbstractDataHandler.DoToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtCurrency) then
    Result := ToCurrency(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtCurrency, @Source);
end;

function TSilAbstractDataHandler.DoToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtDate) then
    Result := ToDateTime(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDate, @Source);
end;

function TSilAbstractDataHandler.DoToWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtWideString) then
    Result := ToWideString(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWideString, @Source);
end;

function TSilAbstractDataHandler.DoToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtDispatch) then
    Result := ToDispatch(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDispatch, @Source);
end;

function TSilAbstractDataHandler.DoToError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtError) then
    Result := ToError(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtError, @Source);
end;

function TSilAbstractDataHandler.DoToWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtWordBool) then
    Result := ToWordBool(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWordBool, @Source);
end;

function TSilAbstractDataHandler.DoToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtVariant) then
    Result := ToVariant(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtVariant, @Source);
end;

function TSilAbstractDataHandler.DoToInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtInterface) then
    Result := ToInterface(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtInterface, @Source);
end;

function TSilAbstractDataHandler.DoToDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtDecimal) then
    Result := ToDecimal(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDecimal, @Source);
end;

function TSilAbstractDataHandler.DoToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtExtended) then
    Result := ToExtended(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtExtended, @Source);
end;

function TSilAbstractDataHandler.DoToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtShortint) then
    Result := ToShortint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtShortint, @Source);
end;

function TSilAbstractDataHandler.DoToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtByte) then
    Result := ToByte(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtByte, @Source);
end;

function TSilAbstractDataHandler.DoToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtWord) then
    Result := ToWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWord, @Source);
end;

function TSilAbstractDataHandler.DoToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtLongWord) then
    Result := ToLongWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongWord, @Source);
end;

function TSilAbstractDataHandler.DoToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtLargeInt) then
    Result := ToLargeInt(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLargeInt, @Source);
end;

function TSilAbstractDataHandler.DoToLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtLargeWord) then
    Result := ToLargeWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLargeWord, @Source);
end;

function TSilAbstractDataHandler.DoToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtInteger) then
    Result := ToInteger(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtInteger, @Source);
end;

function TSilAbstractDataHandler.DoToCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtCardinal) then
    Result := ToCardinal(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtCardinal, @Source);
end;

function TSilAbstractDataHandler.DoToVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtVoid) then
    Result := ToVoid(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtVoid, @Source);
end;

function TSilAbstractDataHandler.DoToHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtHResult) then
    Result := ToHResult(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtHResult, @Source);
end;

function TSilAbstractDataHandler.DoToPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtPointer) then
    Result := ToPointer(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPointer, @Source);
end;

function TSilAbstractDataHandler.DoToSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtSafeArray) then
    Result := ToSafeArray(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSafeArray, @Source);
end;

function TSilAbstractDataHandler.DoToDynarray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtDynarray) then
    Result := ToArray(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDynarray, @Source);
end;

function TSilAbstractDataHandler.DoToUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtUserdefined) then
    Result := ToUserdefined(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtUserdefined, @Source);
end;

function TSilAbstractDataHandler.DoToPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtPAnsiChar) then
    Result := ToPAnsiChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPAnsiChar, @Source);
end;

function TSilAbstractDataHandler.DoToPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtPWideChar) then
    Result := ToPWideChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPWideChar, @Source);
end;

function TSilAbstractDataHandler.DoToGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtGUID) then
    Result := ToGUID(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtGUID, @Source);
end;

function TSilAbstractDataHandler.DoToClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtClass) then
    Result := ToClass(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtClass, @Source);
end;

function TSilAbstractDataHandler.DoToObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtObject) then
    Result := ToObject(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtObject, @Source);
end;

function TSilAbstractDataHandler.DoToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtBoolean) then
    Result := ToBoolean(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtBoolean, @Source);
end;

function TSilAbstractDataHandler.DoToLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtLongBool) then
    Result := ToLongBool(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongBool, @Source);
end;

function TSilAbstractDataHandler.DoToAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtAnsiChar) then
    Result := ToAnsiChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtAnsiChar, @Source);
end;

function TSilAbstractDataHandler.DoToWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtWideChar) then
    Result := ToWideChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWideChar, @Source);
end;

function TSilAbstractDataHandler.DoToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertTo(dtAnsiString) then
    Result := ToAnsiString(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtAnsiString, @Source);
end;

function TSilAbstractDataHandler.DoFromEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtEmpty) then
    Result := FromEmpty(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtEmpty, @Source);
end;

function TSilAbstractDataHandler.DoFromNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtNull) then
    Result := FromNull(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtNull, @Source);
end;

function TSilAbstractDataHandler.DoFromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtSmallint) then
    Result := FromSmallint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSmallint, @Source);
end;

function TSilAbstractDataHandler.DoFromLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtLongint) then
    Result := FromLongint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongint, @Source);
end;

function TSilAbstractDataHandler.DoFromSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtSingle) then
    Result := FromSingle(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSingle, @Source);
end;

function TSilAbstractDataHandler.DoFromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtDouble) then
    Result := FromDouble(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDouble, @Source);
end;

function TSilAbstractDataHandler.DoFromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtCurrency) then
    Result := FromCurrency(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtCurrency, @Source);
end;

function TSilAbstractDataHandler.DoFromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtDate) then
    Result := FromDateTime(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDate, @Source);
end;

function TSilAbstractDataHandler.DoFromWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtWideString) then
    Result := FromWideString(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWideString, @Source);
end;

function TSilAbstractDataHandler.DoFromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtDispatch) then
    Result := FromDispatch(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDispatch, @Source);
end;

function TSilAbstractDataHandler.DoFromError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtError) then
    Result := FromError(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtError, @Source);
end;

function TSilAbstractDataHandler.DoFromWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtWordBool) then
    Result := FromWordBool(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWordBool, @Source);
end;

function TSilAbstractDataHandler.DoFromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PVariant absolute Source;
  TargetSize: LongWord;
  Target: IDataHandler;
  Value: Variant;
  Buffer: Pointer;
begin
  if DoCheckConvertFrom(dtVariant) then
  begin
    Value := DoCheckVariant(Origin^);
    Target := List.Get(Data.Kind);
    TargetSize := Target.DataType.Size;

    Buffer := StackAlloc(TargetSize);
    
    Target.Handler.Initialize(Buffer);
    try
      Result := FromVariant(@Value, Buffer, SizeOf(Value), TargetSize, Param, ReturnedSize, Status);
      Result := Assign(Target.DataType.Kind, Buffer, Destination, TargetSize, DestinationSize, Param, ReturnedSize, Result);
    finally
      Target.Handler.Finalize(Buffer);
    end;
  end else
    Result := NotSupported(Data.Kind.Value, dtVariant, @Source);
end;

function TSilAbstractDataHandler.DoFromInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtInterface) then
    Result := FromInterface(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtInterface, @Source);
end;

function TSilAbstractDataHandler.DoFromDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtDecimal) then
    Result := FromDecimal(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDecimal, @Source);
end;

function TSilAbstractDataHandler.DoFromExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtExtended) then
    Result := FromExtended(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtExtended, @Source);
end;

function TSilAbstractDataHandler.DoFromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtShortint) then
    Result := FromShortint(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtShortint, @Source);
end;

function TSilAbstractDataHandler.DoFromByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtByte) then
    Result := FromByte(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtByte, @Source);
end;

function TSilAbstractDataHandler.DoFromWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtWord) then
    Result := FromWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWord, @Source);
end;

function TSilAbstractDataHandler.DoFromLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtLongWord) then
    Result := FromLongWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongWord, @Source);
end;

function TSilAbstractDataHandler.DoFromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtLargeInt) then
    Result := FromLargeInt(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLargeInt, @Source);
end;

function TSilAbstractDataHandler.DoFromLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtLargeWord) then
    Result := FromLargeWord(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLargeWord, @Source);
end;

function TSilAbstractDataHandler.DoFromInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtInteger) then
    Result := FromInteger(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtInteger, @Source);
end;

function TSilAbstractDataHandler.DoFromCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtCardinal) then
    Result := FromCardinal(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtCardinal, @Source);
end;

function TSilAbstractDataHandler.DoFromVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtVoid) then
    Result := FromVoid(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtVoid, @Source);
end;

function TSilAbstractDataHandler.DoFromHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtHResult) then
    Result := FromHResult(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtHResult, @Source);
end;

function TSilAbstractDataHandler.DoFromPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtPointer) then
    Result := FromPointer(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPointer, @Source);
end;

function TSilAbstractDataHandler.DoFromSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtSafeArray) then
    Result := FromSafeArray(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtSafeArray, @Source);
end;

function TSilAbstractDataHandler.DoFromDynarray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtDynarray) then
    Result := FromArray(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtDynarray, @Source);
end;

function TSilAbstractDataHandler.DoFromUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtUserdefined) then
    Result := FromUserdefined(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtUserdefined, @Source);
end;

function TSilAbstractDataHandler.DoFromPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtPAnsiChar) then
    Result := FromPAnsiChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPAnsiChar, @Source);
end;

function TSilAbstractDataHandler.DoFromPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtPWideChar) then
    Result := FromPWideChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtPWideChar, @Source);
end;

function TSilAbstractDataHandler.DoFromGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtGUID) then
    Result := FromGUID(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtGUID, @Source);
end;

function TSilAbstractDataHandler.DoFromClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtClass) then
    Result := FromClass(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtClass, @Source);
end;

function TSilAbstractDataHandler.DoFromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtObject) then
    Result := FromObject(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtObject, @Source);
end;

function TSilAbstractDataHandler.DoFromBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtBoolean) then
    Result := FromBoolean(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtBoolean, @Source);
end;

function TSilAbstractDataHandler.DoFromLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtLongBool) then
    Result := FromLongBool(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtLongBool, @Source);
end;

function TSilAbstractDataHandler.DoFromAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtAnsiChar) then
    Result := FromAnsiChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtAnsiChar, @Source);
end;

function TSilAbstractDataHandler.DoFromWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtWideChar) then
    Result := FromWideChar(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtWideChar, @Source);
end;

function TSilAbstractDataHandler.DoFromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if DoCheckConvertFrom(dtAnsiString) then
    Result := FromAnsiString(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status) else
    Result := NotSupported(Data.Kind.Value, dtAnsiString, @Source);
end;

function TSilAbstractDataHandler.Null(Destination: PChar; DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if Assigned(ReturnedSize) then ReturnedSize^ := 0;
  Result := DoFinalize(Destination, DestinationSize, Param);
end;

function TSilAbstractDataHandler.ToEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Null(Destination, DestinationSize, Param, ReturnedSize, tcSUnassigned);
end;

function TSilAbstractDataHandler.ToNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Null(Destination, DestinationSize, Param, ReturnedSize, tcSIsNull);
end;

function TSilAbstractDataHandler.Clear(Destination: PChar; DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  if Assigned(ReturnedSize) then ReturnedSize^ := 0;
  Result := DoFinalize(Destination, DestinationSize, Param);
end;

function TSilAbstractDataHandler.FromEmpty(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Clear(Destination, DestinationSize, Param, ReturnedSize, tcSUnassigned);
end;

function TSilAbstractDataHandler.FromNull(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Clear(Destination, DestinationSize, Param, ReturnedSize, tcSIsNull);
end;

function TSilAbstractDataHandler.DoCheckConvertTo(Target: TDataType): Boolean;
asm
          MOV       EAX, [EAX]
          MOVZX     ECX, DX
          CMP       [EAX + 4*ECX + VMTOFFSET ToEmpty], OFFSET System.@AbstractError
          MOV       AL, 1
          JNE       @@Return
          DEC       AL
@@Return:
end;

function TSilAbstractDataHandler.DoCheckConvertFrom(Target: TDataType): Boolean;
asm
          MOV       EAX, [EAX]
          MOVZX     ECX, DX
          CMP       [EAX + 4*ECX + VMTOFFSET FromEmpty], OFFSET System.@AbstractError
          MOV       AL, 1
          JNE       @@Return
          DEC       AL
@@Return:
end;

function TSilAbstractDataHandler.DoCheckVariant(const Value: Variant): Variant;
begin
  if not (TVarData(Value).VType in [Ord(Low(TDataType)) .. Ord(High(TDataType))]) then
    case TVarData(Value).VType of
      varString:  Result := Vart.ToType(Value, '', varOleStr);
      else        DoVariantError(@Value);
    end
  else
    Result := Value;
end;

procedure TSilAbstractDataHandler.DoVariantError(const Value: PVarData);
begin
  raise Error.Create(SUnsupportedVariantTye, [Value.VType]);
end;

function TSilAbstractDataHandler.DoConvertTo(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus;
asm
          PUSH    ESI
          PUSH    EDI
          PUSH    EBX
          MOV     EBX, Params
          MOVZX   EDI, Target
          MOVZX   ESI, Origin
          MOV     EDX, VMTOFFSET ToEmpty
          CALL    DoInvoke
          POP     EBX
          POP     EDI
          POP     ESI
end;

function TSilAbstractDataHandler.DoConvertFrom(Origin, Target: TDataType; Params: PConvertFrame): TDataTypecastStatus;
asm
          PUSH    ESI
          PUSH    EDI
          PUSH    EBX
          MOV     EBX, Params
          MOVZX   EDI, Target
          MOVZX   ESI, Origin
          MOV     EDX, VMTOFFSET FromEmpty
          CALL    DoInvoke
          POP     EBX
          POP     EDI
          POP     ESI
end;

procedure TSilAbstractDataHandler.DoInvoke;
//  EAX:  Self
//  EBX:  Caller params
//  EDX:  Offset inicio de la tabla de conversion
//  EDI:  Tipo destino -> Metodo a ejecutar
//  ESI:  Tipo origen
asm
          MOV       ECX, [EAX]                            // tomo el VMT link
          ADD       EDX, ECX                              // sumo el offset de la base
          LEA       EDX, [EDX+4*EDI]                      // tomo la dirección el método a llamar
          CMP       [EDX], OFFSET System.@AbstractError
          JNE       @@Convert
@@Error:
          LEA       EDX, [ECX + VMTOFFSET NotSupported]
          PUSH      EBX
          PUSH      EDI
          PUSH      ESI
          JMP       @@Invoke
@@Convert:
          PUSH      RConvertFrame[EBX].Status.DWORD
          PUSH      RConvertFrame[EBX].ReturnedSize
          PUSH      RConvertFrame[EBX].Param
          PUSH      RConvertFrame[EBX].DestinationSize
          PUSH      RConvertFrame[EBX].SourceSize
          PUSH      RConvertFrame[EBX].Destination
          PUSH      RConvertFrame[EBX].Source
@@Invoke:
          PUSH      EAX
          CALL      [EDX]
end;

function TSilAbstractDataHandler.DoCreateHandler(const Data: IDataType): ITypeHandler;
var
  Info: PTypeInfo;
begin
  Info := Data.Info;
  if Assigned(Info) then
    Result := SilLtContainer.Handler.Create(Info, Self) else
    Result := SilLtContainer.Handler.Create(Data.Size, Self);
end;

function TSilAbstractDataHandler.DoGetDataType: IDataType;
begin
  Result := FData;
end;

function TSilAbstractDataHandler.DoGetDataList: IDataTypeList;
begin
  Result := IDataTypeList(FList);
end;

end.
