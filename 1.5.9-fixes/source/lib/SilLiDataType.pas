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

unit SilLiDataType;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilBeTypes,
  SilBeError,
  SilBeTypeInfo,
  SilLiTypeInfo,
  SilLiContainerTypes;

type
  TDataTypecastStatus = (
      tcSOk,
      tcSUnassigned,
      tcSDefault,
      tcSIgnore,
      tcSIsNull,
      tcSTruncated,
      tcSRoundOff,
      tcECantConvert,
      tcESignMismatch,
      tcEDataOverflow,
      tcECantCreate,
      tcEUnavailable,
      tcEBadStatus,
      tcEUnespecified
    );

const
  tcSucceeded   = [
      tcSOk,
      tcSUnassigned,
      tcSIsNull,
      tcSTruncated,
      tcSRoundOff,
      tcSDefault,
      tcSIgnore
    ];

const
  tcFailed      = [
      tcEUnespecified,
      tcECantConvert,
      tcESignMismatch,
      tcEDataOverflow,
      tcECantCreate,
      tcEUnavailable,
      tcEBadStatus
    ];

type
  IDataType = interface;
  IDataTypeDef = interface;
  IDataHandler = interface;
  IDataTypeList = interface;
  IDataTypecast = interface;
  IDataBuffer = interface;

  IDataType = interface
    ['{427301FC-44B8-4B25-A57A-5EAA08EE4EF4}']
    function GetKind: RDataType;
    function GetInfo: PTypeInfo;
    function GetSize: LongWord;
    function GetName: PChar;
    function GetHandler: IDataHandler;
    property Name: PChar read GetName;
    property Kind: RDataType read GetKind;
    property Info: PTypeInfo read GetInfo;
    property Size: LongWord read GetSize;
    property Handler: IDataHandler read GetHandler;
  end;

  IDataTypeDef = interface (IDataType)
    ['{8CF978C7-FD3B-4BF5-BBCD-7B437F6D2AAD}']
    procedure SetKind(const Value: RDataType);
    property Kind: RDataType read GetKind write SetKind;
  end;

  IDataTypeList = interface
    ['{AB84F854-0839-4739-970E-BC389A00FF12}']
    function Get(VType: Word): IDataHandler; overload;
    function Get(DataType: TDataType): IDataHandler; overload;
    function Get(const DataType: RDataType): IDataHandler; overload;
    function Get(TypeInfo: PTypeInfo): IDataHandler; overload;
    function Get(const TypeInfo: ITypeInfo): IDataHandler; overload;
    function GetTypeSmallint: IDataHandler;
    function GetTypeLongInt: IDataHandler;
    function GetTypeSingle: IDataHandler;
    function GetTypeDouble: IDataHandler;
    function GetTypeCurrency: IDataHandler;
    function GetTypeDateTime: IDataHandler;
    function GetTypeWideString: IDataHandler;
    function GetTypeDispatch: IDataHandler;
    function GetTypeError: IDataHandler;
    function GetTypeWordBool: IDataHandler;
    function GetTypeVariant: IDataHandler;
    function GetTypeInterface: IDataHandler;
    function GetTypeDecimal: IDataHandler;
    function GetTypeExtended: IDataHandler;
    function GetTypeShortInt: IDataHandler;
    function GetTypeByte: IDataHandler;
    function GetTypeWord: IDataHandler;
    function GetTypeLongWord: IDataHandler;
    function GetTypeLargeInt: IDataHandler;
    function GetTypeLargeWord: IDataHandler;
    function GetTypeInteger: IDataHandler;
    function GetTypeCardinal: IDataHandler;
    function GetTypeVoid: IDataHandler;
    function GetTypeHRESULT: IDataHandler;
    function GetTypePointer: IDataHandler;
    function GetTypeSafearray: IDataHandler;
    function GetTypeArray: IDataHandler;
    function GetTypeUserdefined: IDataHandler;
    function GetTypePAnsiChar: IDataHandler;
    function GetTypePWideChar: IDataHandler;
    function GetTypeGUID: IDataHandler;
    function GetTypeClass: IDataHandler;
    function GetTypeObject: IDataHandler;
    function GetTypeBoolean: IDataHandler;
    function GetTypeLongBool: IDataHandler;
    function GetTypeAnsiChar: IDataHandler;
    function GetTypeWideChar: IDataHandler;
    function GetTypeAnsiString: IDataHandler;
    //function Describe(const DataType: RDataType): string;
    property Smallint: IDataHandler read GetTypeSmallint;
    property LongInt: IDataHandler read GetTypeLongInt;
    property Single: IDataHandler read GetTypeSingle;
    property Double: IDataHandler read GetTypeDouble;
    property Currency: IDataHandler read GetTypeCurrency;
    property DateTime: IDataHandler read GetTypeDateTime;
    property WideString: IDataHandler read GetTypeWideString;
    property Dispatch: IDataHandler read GetTypeDispatch;
    property Error: IDataHandler read GetTypeError;
    property WordBool: IDataHandler read GetTypeWordBool;
    property Variant: IDataHandler read GetTypeVariant;
    property IInterface: IDataHandler read GetTypeInterface;
    property Decimal: IDataHandler read GetTypeDecimal;
    property Extended: IDataHandler read GetTypeExtended;
    property ShortInt: IDataHandler read GetTypeShortInt;
    property Byte: IDataHandler read GetTypeByte;
    property Word: IDataHandler read GetTypeWord;
    property LongWord: IDataHandler read GetTypeLongWord;
    property LargeInt: IDataHandler read GetTypeLargeInt;
    property LargeWord: IDataHandler read GetTypeLargeWord;
    property Integer: IDataHandler read GetTypeInteger;
    property Cardinal: IDataHandler read GetTypeCardinal;
    property Void: IDataHandler read GetTypeVoid;
    property HResult: IDataHandler read GetTypeHRESULT;
    property Pointer: IDataHandler read GetTypePointer;
    property Safearray: IDataHandler read GetTypeSafearray;
    property Dynarray: IDataHandler read GetTypeArray;
    property Userdefined: IDataHandler read GetTypeUserdefined;
    property PAnsiChar: IDataHandler read GetTypePAnsiChar;
    property PWideChar: IDataHandler read GetTypePWideChar;
    property GUID: IDataHandler read GetTypeGUID;
    property TClass: IDataHandler read GetTypeClass;
    property TObject: IDataHandler read GetTypeObject;
    property Boolean: IDataHandler read GetTypeBoolean;
    property LongBool: IDataHandler read GetTypeLongBool;
    property AnsiChar: IDataHandler read GetTypeAnsiChar;
    property WideChar: IDataHandler read GetTypeWideChar;
    property AnsiString: IDataHandler read GetTypeAnsiString;
  end;

  ESilConversionFailed = class(ESilException)
    Error: TDataTypecastStatus;
    Origin, Target: TDataType; 
  end;

  IDataConvert = interface;
  IDataAssign = interface;

  IDataHandler = interface
    ['{498B16AF-2C24-4D00-A83C-382145C4A65B}']
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
    property DataType: IDataType read GetDataType;
    property Handler: ITypeHandler read GetHandler;
    property Cast: IDataConvert read GetCast;
    property From: IDataAssign read GetFrom;
  end;

  IDataTypecast = interface (IDataHandler)
    ['{470D1F8F-986B-4268-8669-C3BD7E3EDD0B}']
    function Smallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function LongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Single(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Double(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Currency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function DateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function WideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Dispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Error(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function WordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Variant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function IInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Decimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Extended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function ShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Byte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Word(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function LongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function LargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function LargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Integer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Cardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Void(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function HResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Pointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Safearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Dynarray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Userdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function PAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function PWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function GUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function TClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function TObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function Boolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function LongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function AnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function WideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;
    function AnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; stdcall;          
  end;

  IDataConvert = interface (IDataTypecast)
    ['{92A32199-4AAE-43B9-A20C-3B68839E4BEA}']
    function Null(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSIsNull): TDataTypecastStatus; stdcall;
  end;

  IDataAssign = interface (IDataTypecast)
    ['{891760DE-8AC0-45D8-B6EC-B79AE443B8EE}']
    function Clear(Destination: PChar; DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSIsNull): TDataTypecastStatus; stdcall;
  end;

  IDataBuffer = interface
    ['{ED19962A-3F50-45C7-828B-46EB11B9BCCD}']
    function GetHandler: IDataHandler;
    function GetSize: LongWord;
    function GetMemory: PChar;
    property Handler: IDataHandler read GetHandler;
    property Size: LongWord read GetSize;
    property Memory: PChar read GetMemory;
  end;

implementation
end.
 