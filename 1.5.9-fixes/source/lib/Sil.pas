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

unit Sil;
        
interface

// changelog.txt

{$INCLUDE Defines.inc}

uses
  SilBcDebug,
  SilBcChr,

  SilBeTypes,
  SilBeDebug,
  SilBeBounds,
  SilBeVariant,
  SilBeArg,
  SilBeTypeInfo,
  SilBeMemMgr,
  SilBeError,
  SilBeDataType,

  SilBkTool,
  SilBkPtr,
  SilBkMemMgr,

  SilBtColor,
  SilBtMem,
  SilBtDate,
  SilBtTime,
  SilBtDateTime,
  SilBtInt,
  SilBtChr,
  SilBtStr,
  SilBtText,
  SilBtFloat,
  SilBtVart,
  SilBtVArray,
  SilBtBin,
  SilBtHex,
  SilBtPoint,
  SilBtBounds,
  SilBtRect,
  SilBtLarge,
  SilBtArg,
  SilBtTypeInfo,
  SilBtError,
  SilBtVoidPtr,
  SilBtInterfacePtr,
  SilBtStringPtr,
  SilBtVariantPtr,

  SilLeTrace,

  SilLiLinkedList,
  SilLiObject,
  SilLiFactory,
  SilLiCrossReference,
  SilLiValue,
  SilLiCompare,
  SilLiAction,
  SilLiEnumerator,
  SilLiLock,
  SilLiList,
  SilLiFiler,
  SilLiField,
  SilLiInterfaceList,
  SilLiPointerList,
  SilLiRandomPointerList,
  SilLiReference,
  SilLiStream,
  SilLiSerializer,
  SilLiStringList,
  SilLiWideStringList,
  SilLiValueList,
  SilLiConnection,
  SilLiTrace,
  SilLiTraits,
  SilLiKey,
  SilLiSort,
  SilLiSearch,
  SilLiClone,
  SilLiEventList,
  SilLiReadWriteLock,
  SilLiQueue,
  SilLiInterfaceQueue,
  SilLiPointerQueue,
  SilLiTypeInfo,
  SilLiDataType,

  SilLiPacket,
  SilLiParameters,
  SilLiMemberList,

  SilLiLoggers,
  SilLiGlobalServices,

  SilLjFactory,

  SilLkLogger,
  SilLkObject,
  SilLkFactory,
  SilLkInterfaced,
  SilLkSingleton,
  SilLkNonCounted,
  SilLkAggregated,
  SilLkAggregable,
  SilLkAggregation,
  SilLkExtensible,

  SilLtObjectFactory,
  SilLtAggregatedFactory,
  SilLtAggregableFactory,
  SilLtAction,
  SilLtLock,
  SilLtReference,
  SilLtStream,
  SilLtSerializer,
  SilLtConnection,
  SilLtTrace,
  SilLtSort,
  SilLtSearch,
  SilLtClone,
  SilLtTool,
  SilLtList,
  SilLtFields,
  SilLtIntegerEnumerator,
  SilLtTypeInfo,

  SilLtGlobal,

  SilOsTypes,

  SilOeWait,
  SilOeTypes,
  SilOeLang,
  SilOeProcess,
  SilOeFilesystemNotifier,
  SilOeMessenger,

  SilOiHandle,
  SilOiWait,

  SilOiWindow,

  SilOiIpc,
  SilOiFilesystemNotifier,

  SilOiEnvironment,
  SilOiFile,
  SilOiTextFile,
  SilOiTimer,
  SilOiProcess,
  SilOiThread,
  SilOiTask,
  SilOiMessenger,
  SilOiSharedMemory,
  SilOiSocket,
  SilOiPipe,
  SilOiSharedLibrary,
  SilOiLibraryBinding,

  SilOiEventQueue,
  SilOiSerialPort,
  SilOiVersion,
  SilOiModule,
  SilOiPerformance,
  SilOiComputer,

  SilOtHandle,
  SilOtWindow,
  SilOtIpc,
  SilOtFile,
  SilOtTimer,
  SilOtProcess,
  SilOtThread,
  SilOtSharedMemory,
  SilOtSocket,
  SilOtWait,
  SilOtEventQueue,
  SilOtSerialPort,
  SilOtEnvironment,
  SilOtVersion,
  SilOtLocked,
  SilOtLang,
  SilOtModule,
  SilOtTool,

  SilOsError,
  SilOsTool;

const
  SilVersionNumber              = $01050906;

const
  SilBaseMajorVersion           = Char(Ord('0') + (SilVersionNumber shr 24  and 255));
  SilBaseMinorVersion           = Char(Ord('0') + (SilVersionNumber shr 16  and 255));
  SilBaseRelease                = Char(Ord('0') + (SilVersionNumber shr 8  and 255));
  SilBaseBuildNumber            = Char(Ord('0') + (SilVersionNumber shr 0  and 255));

const
  SilBaseBranch                 = 'stable';
  SilBaseVersion                = SilBaseMajorVersion + '.' + SilBaseMinorVersion;

const
  SilVersionString              = SilBaseBranch + ':' + SilBaseVersion + '.' + SilBaseRelease + '-' + SilBaseBuildNumber;

const
  CSilRootKey: PChar = '$SYSTEM\SOFTWARE\SIL';

const
  ccNUL                         = SilBcChr.ccNUL;
  ccSOH                         = SilBcChr.ccSOH;
  ccSTX                         = SilBcChr.ccSTX;
  ccETX                         = SilBcChr.ccETX;
  ccEOT                         = SilBcChr.ccEOT;
  ccENQ                         = SilBcChr.ccENQ;
  ccACK                         = SilBcChr.ccACK;
  ccBEL                         = SilBcChr.ccBEL;
  ccBS                          = SilBcChr.ccBS;
  ccHT                          = SilBcChr.ccHT;
  ccLF                          = SilBcChr.ccLF;
  ccVT                          = SilBcChr.ccVT;
  ccFF                          = SilBcChr.ccFF;
  ccCR                          = SilBcChr.ccCR;
  ccSO                          = SilBcChr.ccSO;
  ccSI                          = SilBcChr.ccSI;
  ccDLE                         = SilBcChr.ccDLE;
  ccDC1                         = SilBcChr.ccDC1;
  ccDC2                         = SilBcChr.ccDC2;
  ccDC3                         = SilBcChr.ccDC3;
  ccDC4                         = SilBcChr.ccDC4;
  ccNAK                         = SilBcChr.ccNAK;
  ccSYN                         = SilBcChr.ccSYN;
  ccETB                         = SilBcChr.ccETB;
  ccCAN                         = SilBcChr.ccCAN;
  ccEM                          = SilBcChr.ccEM;
  ccSUB                         = SilBcChr.ccSUB;
  ccESC                         = SilBcChr.ccESC;
  ccFS                          = SilBcChr.ccFS;
  ccGS                          = SilBcChr.ccGS;
  ccRS                          = SilBcChr.ccRS;
  ccUS                          = SilBcChr.ccUS;
  ccSPC                         = SilBcChr.ccSPC;
  ccCRLF                        = SilBcChr.ccCRLF;

type
  Int8                          = SilBeTypes.Int8;
  Int16                         = SilBeTypes.Int16;
  Int32                         = SilBeTypes.Int32;
  Int64                         = SilBeTypes.Int64;
  Pint8                         = ^Int8;
  Pint16                        = ^Int16;
  Pint32                        = ^Int32;
  Pint64                        = ^Int64;
                                
type                            
  Uint8                         = SilBeTypes.Uint8;
  Uint16                        = SilBeTypes.Uint16;
  Uint32                        = SilBeTypes.Uint32;
  Puint8                        = ^Uint8;
  Puin16                        = ^Uint16;
  Puint32                       = ^Uint32;
                                
type                            
  Float32                       = SilBeTypes.Float32;
  Float64                       = SilBeTypes.Float64;
  Float80                       = SilBeTypes.Float80;
  PPointer                      = SilBeTypes.PPointer;
  Pfloat32                      = ^Float32;
  Pfloat64                      = ^Float64;
  Pfloat80                      = ^Float80;

type                            
  LongWord                      = SilBeTypes.LongWord;
  LargeInt                      = SilBeTypes.LargeInt;
  LongDouble                    = SilBeTypes.Float80;

type
  TIntegerRange                 = SilBeTypes.TIntegerRange;
  TAsciiRange                   = SilBeTypes.TAsciiRange;  
  TCharRange                    = SilBeTypes.TCharRange;           
  TByteRange                    = SilBeTypes.TByteRange;           
  TCtrlRange                    = SilBeTypes.TCtrlRange;           

type
  TIntegerSet                   = SilBeTypes.TIntegerSet;
  TAsciiSet                     = SilBeTypes.TAsciiSet;        
  TCharSet                      = SilBeTypes.TCharSet;        
  TByteSet                      = SilBeTypes.TByteSet;        
  TCtrlSet                      = SilBeTypes.TCtrlSet;        

type
  TMethod                       = SilBeTypes.TMethod;

type
  TFloatArray                   = SilBeTypes.TFloatArray;
  TIntegerArray                 = SilBeTypes.TIntegerArray;
  TStringArray                  = SilBeTypes.TStringArray;
  TVarRecArray                  = SilBeTypes.TVarRecArray;
  TByteArray                    = SilBeTypes.TByteArray;
  TWordArray                    = SilBeTypes.TWordArray;
  TCardinalArray                = SilBeTypes.TCardinalArray;
  TLongArray                    = SilBeTypes.TLongArray;
  TVariantArray                 = SilBeTypes.TVariantArray;
  TDateTimeArray                = SilBeTypes.TDateTimeArray;
  TGuidArray                    = SilBeTypes.TGuidArray;
  PCardinalArray                = SilBeTypes.PCardinalArray;
  PStringArray                  = SilBeTypes.PStringArray;
  PIntegerArray                 = SilBeTypes.PIntegerArray;
  PLongWordArray                = SilBeTypes.PLongWordArray;
  PByteArray                    = SilBeTypes.PByteArray;
  PVariantArray                 = SilBeTypes.PVariantArray;
  PGuidArray                    = SilBeTypes.PGuidArray;

type
  IInterface                    = SilBeTypes.IInterface;
  IInvokable                    = SilBeTypes.IInvokable;
  TClass                        = SilBeTypes.TClass;
  TObject                       = SilBeTypes.TObject;

type
  PInterface                    = SilBeTypes.PInterface;
  PUnknown                      = SilBeTypes.PUnknown;
  PClass                        = SilBeTypes.PClass;
  PObject                       = SilBeTypes.PObject;
                                
type                            
  TPoint                        = SilOeTypes.TPoint;
                                
type                            
  TRect                         = SilOeTypes.TRect;
                                
type
  TBounds                       = SilBeBounds.TBounds;
                                
type                            
  TVariant                      = SilBeVariant.TVariant;
                                
type                            
  PArgument                     = SilBeArg.PArgument;
  TArgument                     = SilBeArg.TArgument;
                                
type                            
  PArgumentArray                = SilBeArg.PArgumentArray;
  TArgumentArray                = SilBeArg.TArgumentArray;

type
  TTypeKind                     = SilBeTypeInfo.TTypeKind;

const
  tkUndefined                   = TTypeKind(SilBeTypeInfo.tkUnknown);
  tkInteger                     = TTypeKind(SilBeTypeInfo.tkInteger);
  tkChar                        = TTypeKind(SilBeTypeInfo.tkChar);
  tkEnumeration                 = TTypeKind(SilBeTypeInfo.tkEnumeration);
  tkFloat                       = TTypeKind(SilBeTypeInfo.tkFloat);
  tkString                      = TTypeKind(SilBeTypeInfo.tkString);
  tkSet                         = TTypeKind(SilBeTypeInfo.tkSet);
  tkClass                       = TTypeKind(SilBeTypeInfo.tkClass);
  tkMethod                      = TTypeKind(SilBeTypeInfo.tkMethod);
  tkWChar                       = TTypeKind(SilBeTypeInfo.tkWChar);
  tkLString                     = TTypeKind(SilBeTypeInfo.tkLString);
  tkWString                     = TTypeKind(SilBeTypeInfo.tkWString);
  tkVariant                     = TTypeKind(SilBeTypeInfo.tkVariant);
  tkArray                       = TTypeKind(SilBeTypeInfo.tkArray);
  tkRecord                      = TTypeKind(SilBeTypeInfo.tkRecord);
  tkInterface                   = TTypeKind(SilBeTypeInfo.tkInterface);
  tkInt64                       = TTypeKind(SilBeTypeInfo.tkInt64);
  tkDynArray                    = TTypeKind(SilBeTypeInfo.tkDynArray);

type
  TTypeKinds                    = SilBeTypeInfo.TTypeKinds;

type
  TOrdType                      = SilBeTypeInfo.TOrdType;

type
  TFloatType                    = SilBeTypeInfo.TFloatType;

const
  ftSingle                      = TFloatType(SilBeTypeInfo.ftSingle);
  ftDouble                      = TFloatType(SilBeTypeInfo.ftDouble);
  ftExtended                    = TFloatType(SilBeTypeInfo.ftExtended);
  ftComp                        = TFloatType(SilBeTypeInfo.ftComp);
  ftCurr                        = TFloatType(SilBeTypeInfo.ftCurr);

type
  TMethodKind                   = SilBeTypeInfo.TMethodKind;

const
  mkProcedure                   = TMethodKind(SilBeTypeInfo.mkProcedure);
  mkFunction                    = TMethodKind(SilBeTypeInfo.mkFunction);
  mkConstructor                 = TMethodKind(SilBeTypeInfo.mkConstructor);
  mkDestructor                  = TMethodKind(SilBeTypeInfo.mkDestructor);
  mkClassProcedure              = TMethodKind(SilBeTypeInfo.mkClassProcedure);
  mkClassFunction               = TMethodKind(SilBeTypeInfo.mkClassFunction);
  mkSafeProcedure               = TMethodKind(SilBeTypeInfo.mkSafeProcedure);
  mkSafeFunction                = TMethodKind(SilBeTypeInfo.mkSafeFunction);

type
  TParamAttribute               = SilBeTypeInfo.TParamAttribute;

const
  paVar                         = TParamAttribute(SilBeTypeInfo.paVar      );
  paOut                         = TParamAttribute(SilBeTypeInfo.paOut      );
  paConst                       = TParamAttribute(SilBeTypeInfo.paConst    );
  paAddress                     = TParamAttribute(SilBeTypeInfo.paAddress  );
  paReference                   = TParamAttribute(SilBeTypeInfo.paReference);
  paArray                       = TParamAttribute(SilBeTypeInfo.paArray    );
  paResult                      = TParamAttribute(SilBeTypeInfo.paResult   );

type
  TParamAttributes              = SilBeTypeInfo.TParamAttributes;
  TParamAttributesBase          = SilBeTypeInfo.TParamAttributesBase;

type
  TIntfFlag                     = SilBeTypeInfo.TIntfFlag;

type
  TIntfFlags                    = SilBeTypeInfo.TIntfFlags;
  TIntfFlagsBase                = SilBeTypeInfo.TIntfFlagsBase;

type
  TCallingKind                  = SilBeTypeInfo.TCallingKind;

const
  ckRegister                    = TCallingKind(SilBeTypeInfo.ckRegister);
  ckCdecl                       = TCallingKind(SilBeTypeInfo.ckCdecl   );
  ckPascal                      = TCallingKind(SilBeTypeInfo.ckPascal  );
  ckStdCall                     = TCallingKind(SilBeTypeInfo.ckStdCall );
  ckSafeCall                    = TCallingKind(SilBeTypeInfo.ckSafeCall);

type
  ShortStringBase               = SilBeTypeInfo.ShortStringBase;

type
  PPTypeInfo                    = SilBeTypeInfo.PPTypeInfo;
  PTypeInfo                     = SilBeTypeInfo.PTypeInfo;

type
  PTypeData                     = SilBeTypeInfo.PTypeData;
  PPropInfo                     = SilBeTypeInfo.PPropInfo;

type
  TPropInfoProc                 = SilBeTypeInfo.TPropInfoProc;
  PPropList                     = SilBeTypeInfo.PPropList;

type
  EPropertyError                = SilBeTypeInfo.EPropertyError;
  EPropertyConvertError         = SilBeTypeInfo.EPropertyConvertError;
                                
type                            
  EEnumIdentNotFound            = class(EAbort);
  EEnumValueOutOfRange          = class(EAbort);
  EEnumDataNotFound             = class(EAbort);

const
  tkAny                         = SilBeTypeInfo.tkAny;
  tkMethods                     = SilBeTypeInfo.tkMethod;
  tkProperties                  = SilBeTypeInfo.tkProperties;

type
  RMemoryInfo                   = SilBeMemMgr.RMemoryInfo;

type
  ExceptionType                 = SilBeError.ExceptionType;
  ExcepClass                    = ExceptionType;
  Exception                     = SilBeError.Exception;
  EAbort                        = SilBeError.EAbort;
  EHeapException                = SilBeError.EHeapException;
  EOutOfMemory                  = SilBeError.EOutOfMemory;
  EInOutError                   = SilBeError.EInOutError;
  EExternal                     = SilBeError.EExternal;
  EExternalException            = SilBeError.EExternalException;
  EIntError                     = SilBeError.EIntError;
  EDivByZero                    = SilBeError.EDivByZero;
  ERangeError                   = SilBeError.ERangeError;
  EIntOverflow                  = SilBeError.EIntOverflow;
  EMathError                    = SilBeError.EMathError;
  EInvalidOp                    = SilBeError.EInvalidOp;
  EZeroDivide                   = SilBeError.EZeroDivide;
  EOverflow                     = SilBeError.EOverflow;
  EUnderflow                    = SilBeError.EUnderflow;
  EInvalidPointer               = SilBeError.EInvalidPointer;
  EInvalidCast                  = SilBeError.EInvalidCast;
  EConvertError                 = SilBeError.EConvertError;
  EAccessViolation              = SilBeError.EAccessViolation;
  EPrivilege                    = SilBeError.EPrivilege;
  EStackOverflow                = SilBeError.EStackOverflow;
  EControlC                     = SilBeError.EControlC;
  EVariantError                 = SilBeError.EVariantError;
  EPropReadOnly                 = SilBeError.EPropReadOnly;
  EPropWriteOnly                = SilBeError.EPropWriteOnly;
  EAssertionFailed              = SilBeError.EAssertionFailed;
  EIntfCastError                = SilBeError.EIntfCastError;
  EInvalidContainer             = SilBeError.EInvalidContainer;
  EInvalidInsert                = SilBeError.EInvalidInsert;
  EPackageError                 = SilBeError.EPackageError;
  EOSError                      = SilBeError.EOSError;
  ESafecallException            = SilBeError.ESafecallException;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
type
  EAbstractError                = SilBeError.EAbstractError;
{$ENDIF}

type                            
  DebugFlag                     = SilBcDebug.TDebugFlag;
  DebugFlags                    = SilBcDebug.TDebugFlags;
  DebugLevel                    = SilBcDebug.TDebugLevel;
  DebugRange                    = SilBcDebug.TDebugUserRange;

const
  NDebugDb                      = SilBcDebug.NDebugDb;
  NDebugMemory                  = SilBcDebug.NDebugMemory;
  NDebugTracing                 = SilBcDebug.NDebugTracing;
  NDebugLibrary                 = SilBcDebug.NDebugLibrary;
  NDebugCalls                   = SilBcDebug.NDebugCalls;
  NDebugOnErrorForceEnter       = SilBcDebug.NDebugOnErrorForce;
  NDebugOnTraceForceEnter       = SilBcDebug.NDebugOnTraceForce;
  NDebugFirst                   = SilBcDebug.NDebugFirst;
  NDebugLast                    = SilBcDebug.NDebugLast;
  NDebugError                   = SilBcDebug.NDebugError;

const
  DebugNone                     = SilBcDebug.CDebugNone;
  DebugAll                      = SilBcDebug.CDebugAll;
  DebugMemory                   = SilBcDebug.CDebugMemory;
  DebugDb                       = SilBcDebug.CDebugDb;
  DebugTracing                  = SilBcDebug.CDebugTracing;
  DebugLibrary                  = SilBcDebug.CDebugLibrary;
  DebugCalls                    = SilBcDebug.CDebugCalls;
  DebugOnErrorForceEnter        = SilBcDebug.CDebugOnErrorForce;
  DebugOnTraceForceEnter        = SilBcDebug.CDebugOnTraceForce;
  DebugError                    = SilBcDebug.CDebugError;
  DebugReserved                 = SilBcDebug.CDebugReserved;

type                            
  Mem                           = SilBtMem.Mem;
                                
type                            
  Date                          = SilBtDate.Date;
  TDateParts                    = SilBeTypes.TDateParts;
                                
type                            
  Time                          = SilBtTime.Time;
  TTimeParts                    = SilBeTypes.TTimeParts;

type
  TDayOfWeek                    = SilBeTypes.TDayOfWeek;
  TDaysOfWeek                   = SilBeTypes.TDaysOfWeek; 
  TDayPosition                  = SilBeTypes.TDayPosition;
  TDayPositions                 = SilBeTypes.TDayPositions;
  TTimeElement                  = SilBeTypes.TTimeElement;
  TTimeElements                 = SilBeTypes.TTimeElements;

const
  dwUnknown                     = SilBeTypes.dwUnknown; 
  dwSunday                      = SilBeTypes.dwSunday; 
  dwMonday                      = SilBeTypes.dwMonday; 
  dwTuesday                     = SilBeTypes.dwTuesday; 
  dwWednesday                   = SilBeTypes.dwWednesday; 
  dwThursday                    = SilBeTypes.dwThursday; 
  dwFriday                      = SilBeTypes.dwFriday; 
  dwSaturday                    = SilBeTypes.dwSaturday;

const
  teYear                        = SilBeTypes.teYear;
  teMonth                       = SilBeTypes.teMonth;
  teDay                         = SilBeTypes.teDay;
  teHour                        = SilBeTypes.teHour;
  teMinute                      = SilBeTypes.teMinute;
  teSecond                      = SilBeTypes.teSecond;
  teMSecond                     = SilBeTypes.teMSecond;

type                            
  DateTime                      = SilBtDateTime.DateTime;
  TDateTimeParts                = SilBeTypes.TDateTimeParts;
                                
type                            
  Int                           = SilBtInt.Int;
                                
type                            
  Chr                           = SilBtChr.Chr;
                                
type                            
  Str                           = SilBtStr.Str;

type
  Text                          = SilBtText.Text;

type
  WStr                          = SilOsTool.OsWStr;
  Wide                          = SilOsTool.OsWStr;
                                
type
  Float                         = SilBtFloat.Float;
                                
type                            
  Vart                          = SilBtVart.Vart;
                                
type                            
  VArray                        = SilBtVArray.VArray;

type                            
  Bin                           = SilBtBin.Bin;
                                
type                            
  Hex                           = SilBtHex.Hex;
                                
type
  Rgb                           = SilBtColor.Rgb;

type
  RColorHsl                     = SilBtColor.RColorHsl;

type
  Point                         = SilBtPoint.Point;
                                
type                            
  Rect                          = SilBtRect.Rect;
                                
type
  Bounds                        = SilBtBounds.Bounds;
                                
type                            
  Arg                           = SilBtArg.ArgTool;

type                            
  Large                         = SilBtLarge.Large;
                                
type                            
  Enum                          = SilBtTypeInfo.EnumTool;

type
  Typ                           = SilLtTypeInfo.Typ;

type                            
  Error                         = SilBtError.Error;
                                
type                            
  HandlerType                   = SilBkPtr.HandlerType;
  BaseHandler                   = SilBkPtr.BaseHandler;
  PointerHandler                = SilBkPtr.PointerHandler;
  DataHandler                   = SilBkPtr.DataHandler;

type                            
  VoidHandler                   = SilBtVoidPtr.VoidHandler;
  DynamicHandler                = SilBtVoidPtr.DynamicHandler;
  ObjectHandler                 = SilBtVoidPtr.ObjectHandler;
                                
type                            
  InterfaceHandler              = SilBtInterfacePtr.InterfaceHandler;

type
  StringHandler                 = SilBtStringPtr.StringHandler;

type
  VariantHandler                = SilBtVariantPtr.VariantHandler;

type
  MemoryManagerType             = SilBkMemMgr.MemoryManagerType;
  MemoryManager                 = SilBkMemMgr.MemoryManager;

type
  PLongint                      = SilBeTypes.PLongint;
  PInteger                      = SilBeTypes.PInteger;
  PLongWord                     = SilBeTypes.PLongWord;
  PSmallInt                     = SilBeTypes.PSmallInt;
  PDouble                       = SilBeTypes.PDouble;
  PShortInt                     = SilBeTypes.PShortInt;
  PLargeInt                     = SilBeTypes.PLargeInt;

const
  varEmpty                      = SilBeTypes.varEmpty;
  varNull                       = SilBeTypes.varNull;
  varSmallint                   = SilBeTypes.varSmallint;
  varInteger                    = SilBeTypes.varInteger;
  varSingle                     = SilBeTypes.varSingle;
  varDouble                     = SilBeTypes.varDouble;
  varCurrency                   = SilBeTypes.varCurrency;
  varDate                       = SilBeTypes.varDate;
  varOleStr                     = SilBeTypes.varOleStr;
  varDispatch                   = SilBeTypes.varDispatch;
  varError                      = SilBeTypes.varError;
  varBoolean                    = SilBeTypes.varBoolean;
  varVariant                    = SilBeTypes.varVariant;
  varUnknown                    = SilBeTypes.varUnknown;
//varDecimal                    = SilBeTypes.varDecimal;

  varShortInt                   = SilBeTypes.varShortInt;
  varByte                       = SilBeTypes.varByte;
  varWord                       = SilBeTypes.varWord;
  varLongWord                   = SilBeTypes.varLongWord;
  varInt64                      = SilBeTypes.varInt64;
//varWord64                     = SilBeTypes.varWord64;

  varStrArg                     = SilBeTypes.varStrArg;
  varString                     = SilBeTypes.varString;
  varAny                        = SilBeTypes.varAny;
  varTypeMask                   = SilBeTypes.varTypeMask;
  varArray                      = SilBeTypes.varArray;
  varByRef                      = SilBeTypes.varByRef;

type
  IConstant                     = SilLiValue.IConstant;
  IVariant                      = SilLiValue.IVariant;
  IDataType                     = SilLiValue.IDataType1;
  IDataTypeDef                  = SilLiValue.IDataTypeDef1;
  IValue                        = SilLiValue.IValue;
  IVariable                     = SilLiValue.IVariable;
  IValueChar                    = SilLiValue.IValueChar;
  IValueByte                    = SilLiValue.IValueByte;
  IValueAnsiString              = SilLiValue.IValueAnsiString;
  IValueWideString              = SilLiValue.IValueWideString;
  IValueInteger                 = SilLiValue.IValueInteger;
  IValueLargeInt                = SilLiValue.IValueLargeInt;
  IValueLongWord                = SilLiValue.IValueLongWord;
  IValueBoolean                 = SilLiValue.IValueBoolean;
  IValueDouble                  = SilLiValue.IValueDouble;
  IValueCurrency                = SilLiValue.IValueCurrency;
  IValueDateTime                = SilLiValue.IValueDateTime;
  IValueGuid                    = SilLiValue.IValueGuid;
  IValueInterface               = SilLiValue.IValueInterface;
  IVariableChar                 = SilLiValue.IVariableChar;
  IVariableByte                 = SilLiValue.IVariableByte;
  IVariableAnsiString           = SilLiValue.IVariableAnsiString;
  IVariableWideString           = SilLiValue.IVariableWideString;
  IVariableInteger              = SilLiValue.IVariableInteger;
  IVariableLargeInt             = SilLiValue.IVariableLargeInt;
  IVariableLongWord             = SilLiValue.IVariableLongWord;
  IVariableBoolean              = SilLiValue.IVariableBoolean;
  IVariableDouble               = SilLiValue.IVariableDouble;
  IVariableCurrency             = SilLiValue.IVariableCurrency;
  IVariableDateTime             = SilLiValue.IVariableDateTime;
  IVariableGuid                 = SilLiValue.IVariableGuid;
  IVariableInterface            = SilLiValue.IVariableInterface;

type
  IComparable                   = SilLiCompare.IComparable;
  IComparator                   = SilLiCompare.IComparator;
                                
type                            
  ITester                       = SilLiAction.ITester;
  IAction                       = SilLiAction.IAction;
                                
type                            
  IEnumerator                   = SilLiEnumerator.IEnumerator;
  IEnumeration                  = SilLiEnumerator.IEnumeration;
  IEnumerable                   = SilLiEnumerator.IEnumerable;
  IItemization                  = SilLiEnumerator.IItemization;
                                
type                            
  ILockable                     = SilLiLock.ILockable;
  ILock                         = SilLiLock.ILock;
  ILocker                       = SilLiLock.ILocker;
  IExtendedLocker               = SilLiLock.IExtendedLocker;
  ILockingSet                   = SilLiLock.ILockingSet;
  ISynchronizable               = SilLiLock.ISynchronizable;

type                            
  IList                         = SilLiList.IList;
  IListEvents                   = SilLiList.IListEvents;
                                
type                            
  IReader                       = SilLiFiler.IReader;
  IWriter                       = SilLiFiler.IWriter;
  IPbPacketReader               = SilLiFiler.IPbPacketReader;
  IValueReader                  = SilLiFiler.IValueReader;
  IValueAccess                  = SilLiFiler.IValueAccess;
  IValueDef                     = SilLiFiler.IValueDef;

type
  TDataFieldType                = SilBeDataType.TDataFieldType;

const
  ftUnknown                     = TDataFieldType(SilBeDataType.ftUnknown);
  ftChar                        = TDataFieldType(SilBeDataType.ftChar);
  ftString                      = TDataFieldType(SilBeDataType.ftString);
  ftWideString                  = TDataFieldType(SilBeDataType.ftWideString);
  ftSmallInt                    = TDataFieldType(SilBeDataType.ftSmallInt);
  ftInteger                     = TDataFieldType(SilBeDataType.ftInteger);
  ftLargeInt                    = TDataFieldType(SilBeDataType.ftLargeInt);
  ftByte                        = TDataFieldType(SilBeDataType.ftByte);
  ftWord                        = TDataFieldType(SilBeDataType.ftWord);
  ftLongWord                    = TDataFieldType(SilBeDataType.ftLongWord);
  ftBoolean                     = TDataFieldType(SilBeDataType.ftBoolean);
  ftFloat                       = TDataFieldType(SilBeDataType.ftFloat);
  ftCurrency                    = TDataFieldType(SilBeDataType.ftCurrency);
  ftDate                        = TDataFieldType(SilBeDataType.ftDate);
  ftTime                        = TDataFieldType(SilBeDataType.ftTime);
  ftDateTime                    = TDataFieldType(SilBeDataType.ftDateTime);
  ftBytes                       = TDataFieldType(SilBeDataType.ftBytes);
  ftVarBytes                    = TDataFieldType(SilBeDataType.ftVarBytes);
  ftAutoInc                     = TDataFieldType(SilBeDataType.ftAutoInc);
  ftBlob                        = TDataFieldType(SilBeDataType.ftBlob);
  ftMemo                        = TDataFieldType(SilBeDataType.ftMemo);
  ftDataSet                     = TDataFieldType(SilBeDataType.ftDataSet);
  ftVariant                     = TDataFieldType(SilBeDataType.ftVariant);
  ftGuid                        = TDataFieldType(SilBeDataType.ftGuid);
  ftInterface                   = TDataFieldType(SilBeDataType.ftInterface);
  ftPointer                     = TDataFieldType(SilBeDataType.ftPointer);

type
  TDataTypeID                   = SilBeDataType.TDataTypeID; 

type
  TDataType                     = SilBeDataType.TDataType; 

const
  dtEmpty                       = TDataType(SilBeDataType.dtEmpty       );
  dtNull                        = TDataType(SilBeDataType.dtNull        ); 
  dtSmallint                    = TDataType(SilBeDataType.dtSmallint    ); 
  dtLongInt                     = TDataType(SilBeDataType.dtLongInt     ); 
  dtSingle                      = TDataType(SilBeDataType.dtSingle      ); 
  dtDouble                      = TDataType(SilBeDataType.dtDouble      ); 
  dtCurrency                    = TDataType(SilBeDataType.dtCurrency    ); 
  dtDate                        = TDataType(SilBeDataType.dtDate        ); 
  dtWideString                  = TDataType(SilBeDataType.dtWideString  ); 
  dtDispatch                    = TDataType(SilBeDataType.dtDispatch    ); 
  dtError                       = TDataType(SilBeDataType.dtError       ); 
  dtWordBool                    = TDataType(SilBeDataType.dtWordBool    ); 
  dtVariant                     = TDataType(SilBeDataType.dtVariant     ); 
  dtInterface                   = TDataType(SilBeDataType.dtInterface   ); 
  dtDecimal                     = TDataType(SilBeDataType.dtDecimal     ); 
  dtExtended                    = TDataType(SilBeDataType.dtExtended    ); 
  dtShortInt                    = TDataType(SilBeDataType.dtShortInt    ); 
  dtByte                        = TDataType(SilBeDataType.dtByte        ); 
  dtWord                        = TDataType(SilBeDataType.dtWord        ); 
  dtLongWord                    = TDataType(SilBeDataType.dtLongWord    ); 
  dtLargeInt                    = TDataType(SilBeDataType.dtLargeInt    ); 
  dtLargeWord                   = TDataType(SilBeDataType.dtLargeWord   ); 
  dtInteger                     = TDataType(SilBeDataType.dtInteger     ); 
  dtCardinal                    = TDataType(SilBeDataType.dtCardinal    ); 
  dtVoid                        = TDataType(SilBeDataType.dtVoid        ); 
  dtHRESULT                     = TDataType(SilBeDataType.dtHRESULT     ); 
  dtPointer                     = TDataType(SilBeDataType.dtPointer     ); 
  dtSafearray                   = TDataType(SilBeDataType.dtSafearray   ); 
  dtDynarray                    = TDataType(SilBeDataType.dtDynarray    ); 
  dtUserdefined                 = TDataType(SilBeDataType.dtUserdefined ); 
  dtPAnsiChar                   = TDataType(SilBeDataType.dtPAnsiChar   ); 
  dtPWideChar                   = TDataType(SilBeDataType.dtPWideChar   ); 
  dtGUID                        = TDataType(SilBeDataType.dtGUID        ); 
  dtClass                       = TDataType(SilBeDataType.dtClass       ); 
  dtObject                      = TDataType(SilBeDataType.dtObject      ); 
  dtBoolean                     = TDataType(SilBeDataType.dtBoolean     ); 
  dtLongBool                    = TDataType(SilBeDataType.dtLongBool    ); 
  dtAnsiChar                    = TDataType(SilBeDataType.dtAnsiChar    ); 
  dtWideChar                    = TDataType(SilBeDataType.dtWideChar    ); 
  dtAnsiString                  = TDataType(SilBeDataType.dtAnsiString  ); 

const
  dtUnknown                     = TDataType(SilBeDataType.dtUnknown     );

const
  dtU1                          = TDataType(SilBeDataType.dtU1          );
  dtI1                          = TDataType(SilBeDataType.dtI1          ); 
  dtU2                          = TDataType(SilBeDataType.dtU2          ); 
  dtI2                          = TDataType(SilBeDataType.dtI2          ); 
  dtU4                          = TDataType(SilBeDataType.dtU4          ); 
  dtI4                          = TDataType(SilBeDataType.dtI4          ); 
  dtU8                          = TDataType(SilBeDataType.dtU8          ); 
  dtI8                          = TDataType(SilBeDataType.dtI8          ); 
  dtR4                          = TDataType(SilBeDataType.dtR4          ); 
  dtR8                          = TDataType(SilBeDataType.dtR8          ); 
  dtR10                         = TDataType(SilBeDataType.dtR10         ); 

type
  TDataFlag                     = SilBeDataType.TDataFlag; 

const
  dfPointer                     = TDataFlag(SilBeDataType.dfPointer     );
  dfArray                       = TDataFlag(SilBeDataType.dfArray       ); 
  dfByRef                       = TDataFlag(SilBeDataType.dfByRef       ); 

type
  TDataFlags                    = SilBeDataType.TDataFlags; 

type
  RDataType                     = SilBeDataType.RDataType; 

type                            
  IFieldAccess                  = SilLiField.IFieldAccess;
  IFieldDef                     = SilLiField.IFieldDef;
  IFieldStore                   = SilLiField.IFieldStore;
                                
type                            
  IInterfaceList                = SilLiInterfaceList.IInterfaceList;
                                
type                            
  IPointerList                  = SilLiPointerList.IPointerList;
  IRandomPointerList            = SilLiRandomPointerList.IRandomPointerList;

type
  TFreeMethod                   = SilLiPointerList.TFreeMethod;

const
  fmNone                        = TFreeMethod(SilLiPointerList.fmNone);
  fmFreeMem                     = TFreeMethod(SilLiPointerList.fmFreeMem);
  fmFreeObject                  = TFreeMethod(SilLiPointerList.fmFreeObject);
  fmRelease                     = TFreeMethod(SilLiPointerList.fmRelease);
                                
type
  IDispatchable                 = SilLiReference.IDispatchable;
  IReferenceable                = SilLiReference.IReferenceable;
                                
type                            
  IStream                       = SilLiStream.IStream;
  IRandomStream                 = SilLiStream.IRandomStream;
  IBufferStream                 = SilLiStream.IBufferStream;
  IMemoryStream                 = SilLiStream.IMemoryStream;
  ITextStream                   = SilLiStream.ITextStream;
  IWriteOnlyStream              = SilLiStream.IWriteOnlyStream;

type
  TStreamBufferEvent            = SilLiStream.TStreamBufferEvent;
  IStreamEvents                 = SilLiStream.IStreamEvents;
  TStreamPositionEvent          = SilLiStream.TStreamPositionEvent;
  IRandomStreamEvents           = SilLiStream.IRandomStreamEvents;

type
  TSeekOrigin                   = SilLiStream.TSeekOrigin;

const
  soFromBeginning               = TSeekOrigin(SilLiStream.soFromBeginning);
  soFromCurrent                 = TSeekOrigin(SilLiStream.soFromCurrent);
  soFromEnd                     = TSeekOrigin(SilLiStream.soFromEnd);
                                
type                            
  ISerializable                 = SilLiSerializer.ISerializable;

type
  REvOnTransfer                 = SilLiSerializer.REvOnTransfer;
  RTransferEvents               = SilLiSerializer.RTransferEvents;

type
  IEvOnTransferInit             = SilLiSerializer.IEvOnTransferInit;
  IEvOnTransferProgress         = SilLiSerializer.IEvOnTransferProgress;
  IEvOnTransferFinished         = SilLiSerializer.IEvOnTransferFinished;
                                
type
  IStrings                      = SilLiStringList.IStrings;
  IStringList                   = SilLiStringList.IStringList;
  PStringItem                   = SilLiStringList.PStringItem;
  RStringItem                   = SilLiStringList.RStringItem;

type
  IWideStringList               = SilLiWideStringList.IWideStringList;

type
  IEvents                       = SilLiEventList.IEvents;
  IEventList                    = SilLiEventList.IEventList;
  IConnections                  = SilLiEventList.IConnections;
                                
type                            
  IValueList                    = SilLiValueList.IValueList;
                                
type                            
  ISortable                     = SilLiSort.ISortable;
                                
type                            
  ISearchable                   = SilLiSearch.ISearchable;
                                
type                            
  ICloneable                    = SilLiClone.ICloneable;

type
  TNamedKeyDataType             = SilLiKey.TNamedKeyDataType;
  TNamedKeyPermision            = SilLiKey.TNamedKeyPermision;

const
  kdUnknown                     = TNamedKeyDataType(SilLiKey.kdUnknown);
  kdString                      = TNamedKeyDataType(SilLiKey.kdString);
  kdInteger                     = TNamedKeyDataType(SilLiKey.kdInteger);
  kdBinary                      = TNamedKeyDataType(SilLiKey.kdBinary);
  kdStrings                     = TNamedKeyDataType(SilLiKey.kdStrings);
  kdLargeInt                    = TNamedKeyDataType(SilLiKey.kdLargeInt);

const
  kpRead                        = TNamedKeyPermision(SilLiKey.kpRead);
  kpWrite                       = TNamedKeyPermision(SilLiKey.kpWrite);
  kpReadWrite                   = TNamedKeyPermision(SilLiKey.kpReadWrite);

type
  INamedItems                   = SilLiKey.INamedItems;
  INamedKeys                    = SilLiKey.INamedKeys;
  INamedValues                  = SilLiKey.INamedValues;
  INamedItem                    = SilLiKey.INamedItem;
  IEvNamedKeyChanged            = SilLiKey.IEvNamedKeyChanged;
  INamedKey                     = SilLiKey.INamedKey;
  IValueKeys                    = SilLiKey.IValueKeys;
  IValueKey                     = SilLiKey.IValueKey;

type
  Tool                          = SilBkTool.Tool;
  Tools                         = SilBkTool.Tools;

type
  IConnectable                  = SilLiConnection.IConnectable;

type
  FactoryType                   = SilLjFactory.FactoryType;
  FactoryTool                   = SilLjFactory.FactoryTool;

type
  TSilObjectClass               = SilLkObject.TSilObjectClass;
  TSilObject                    = SilLkObject.TSilObject;

type
  TSilFactory                   = SilLkFactory.TSilFactory;  

type
  ILinkedList                   = SilLiLinkedList.ILinkedList;

type
  IObject                       = SilLiObject.IObject;

type
  IFactory                      = SilLiFactory.IFactory;

type
  ICrossReference               = SilLiCrossReference.ICrossReference;

type
  TSilInterfacedObject          = SilLkInterfaced.TSilInterfacedObject;

type
  TSingletonObject              = SilLkSingleton.TSingletonObject;

type
  TNonCountedObject             = SilLkNonCounted.TNonCountedObject;

type
  TSilAggregatedObject          = SilLkAggregated.TSilAggregatedObject;
  TSilAggregatedClass           = SilLkAggregated.TSilAggregatedClass;

type // deprecated symbols: do not use!!!!!
  TAggregatedObject             = class(SilLkAggregated.TAggregatedObject) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};
  TInterfacedObject             = class(SilLkInterfaced.TInterfacedObject) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

type
  TSilAggregableObject          = SilLkAggregable.TSilAggregableObject;
  TSilAggregableClass           = SilLkAggregable.TSilAggregableClass;

type
  TSilAggregationObject         = SilLkAggregation.TSilAggregationObject;

type
  TSilExtensibleObject          = SilLkExtensible.TSilExtensibleObject;

type
  ObjectFactory                 = SilLtObjectFactory.ObjectFactory;

type
  AggregatedFactory             = SilLtAggregatedFactory.AggregatedFactory;

type
  AggregableFactory             = SilLtAggregableFactory.AggregableFactory;

type
  Sort                          = SilLtSort.SortTool;
                                
type
  Search                        = SilLtSearch.Search;
                                
type                            
  Clone                         = SilLtClone.CloneTool;
                                
type                            
  NestedAction                  = SilLtAction.NestedAction;
                                
type                            
  NestedTester                  = SilLtAction.NestedTester;
                                
type
  Lock                          = SilLtLock.Lock;
                                
type                            
  Reference                     = SilLtReference.Reference;
  Ref                           = SilLtReference.Reference;
                                
type
  Stream                        = SilLtStream.Stream;
  MemoryStream                  = SilLtStream.MemoryStream;

type
  ILogginManager                = SilLiLoggers.ILogginManager;

type
  Logger                        = SilLkLogger.Logger;
  LoggerType                    = SilLkLogger.LoggerType;
                                
type                            
  Formatter                     = SilLkLogger.Formatter;
  FormatterType                 = SilLkLogger.FormatterType;
                                
type
  TTraceKind                    = SilLeTrace.TTraceKind;
                                
const                           
  tkLog                         = TTraceKind(SilLeTrace.tkLog);
  tkEnter                       = TTraceKind(SilLeTrace.tkEnter);
  tkLeave                       = TTraceKind(SilLeTrace.tkLeave);
  tkError                       = TTraceKind(SilLeTrace.tkError);

type
  ITrace                        = SilLiTrace.ITrace;
  ITraceData                    = SilLiTrace.ITraceData;
  IThreadData                   = SilLiTrace.IThreadData;

type
  IAdoptionTrait                = SilLiTraits.IAdoptionTrait;
  RAdoptedRef                   = SilLiTraits.RAdoptedRef;

type
  Trace                         = SilLtTrace.Trace;

type
  Sink                          = SilLtConnection.Sink;

type
  Serializer                    = SilLtSerializer.Serializer;

type
  Tk                            = SilLtTool.Tk;
  OS                            = SilOtTool.OS;
  Fld                           = SilLtFields.Fld;

type
  List                          = SilLtList.ListTool;
  
type
  IntegerEnumerator             = SilLtIntegerEnumerator.IntegerEnumerator;

type
  PServiceRef                   = SilLiGlobalServices.PServiceRef;
  RServiceRef                   = SilLiGlobalServices.RServiceRef;

type
  GlobalService                 = SilLiGlobalServices.GlobalService;
  IGlobalServicesV1             = SilLiGlobalServices.IGlobalServicesV1;
  GlobalServiceType             = SilLiGlobalServices.GlobalServiceType;
  PServiceRefV1                 = SilLiGlobalServices.PServiceRefV1;
  RServiceRefV1                 = SilLiGlobalServices.RServiceRefV1;

type
  PGlobalService                = SilLiGlobalServices.PGlobalService;
  RGlobalService                = SilLiGlobalServices.RGlobalService;
  IGlobalServicesV2             = SilLiGlobalServices.IGlobalServicesV2;
  IGlobalServiceListV2          = SilLiGlobalServices.IGlobalServiceListV2;
  IGlobalListV2                 = SilLiGlobalServices.IGlobalListV2;
  IGlobalServiceV2              = SilLiGlobalServices.IGlobalServiceV2;
  IGlobalReferencesV2           = SilLiGlobalServices.IGlobalReferencesV2;
  IGlobalLinksV2                = SilLiGlobalServices.IGlobalLinksV2;
  IGlobalServicesHookV2         = SilLiGlobalServices.IGlobalServicesHookV2;
  PGlobalRef                    = SilLiGlobalServices.PGlobalRef;
  RGlobalRef                    = SilLiGlobalServices.RGlobalRef;
  TGlobalServiceKind            = SilLiGlobalServices.TGlobalServiceKind;

type
  IGlobalServices               = SilLiGlobalServices.IGlobalServices;
  
type
  IGlobalServiceList            = SilLiGlobalServices.IGlobalServiceList;
  IGlobalList                   = SilLiGlobalServices.IGlobalList;
  IGlobalService                = SilLiGlobalServices.IGlobalService;
  IGlobalReferences             = SilLiGlobalServices.IGlobalReferences;
  IGlobalServicesHook           = SilLiGlobalServices.IGlobalServicesHook;
  IGlobalLinks                  = SilLiGlobalServices.IGlobalLinks;

const
  skGlobal                      = TGlobalServiceKind(SilLiGlobalServices.skGlobal);
  skLocal                       = TGlobalServiceKind(SilLiGlobalServices.skLocal);

type
  Global                        = SilLtGlobal.Global;
  Local                         = SilLtGlobal.Local;

type
  IReadWriteLock                = SilLiReadWriteLock.IReadWriteLock;

type
  IQueue                        = SilLiQueue.IQueue;
  IItemQueue                    = SilLiQueue.IItemQueue;

type
  IInterfaceQueue               = SilLiInterfaceQueue.IInterfaceQueue;

type
  IPointerQueue                 = SilLiPointerQueue.IPointerQueue;

type
  ITypeInfo                     = SilLiTypeInfo.ITypeInfo;
  ITypeData                     = SilLiTypeInfo.ITypeData;
  ITypeItems                    = SilLiTypeInfo.ITypeItems;
  ITypeProperties               = SilLiTypeInfo.ITypeProperties;
  ITypeProperty                 = SilLiTypeInfo.ITypeProperty;
  ITypeParams                   = SilLiTypeInfo.ITypeParams;
  ITypeBaseParam                = SilLiTypeInfo.ITypeBaseParam;
  ITypeParam                    = SilLiTypeInfo.ITypeParam;
  ITypeMethods                  = SilLiTypeInfo.ITypeMethods;
  ITypeOrdinal                  = SilLiTypeInfo.ITypeOrdinal;
  ITypeEnum                     = SilLiTypeInfo.ITypeEnum;
  ITypeSet                      = SilLiTypeInfo.ITypeSet;
  ITypeFloat                    = SilLiTypeInfo.ITypeFloat;
  ITypeString                   = SilLiTypeInfo.ITypeString;
  ITypeLarge                    = SilLiTypeInfo.ITypeLarge;
  ITypeClass                    = SilLiTypeInfo.ITypeClass;
  ITypeMethod                   = SilLiTypeInfo.ITypeMethod;
  ITypeInterface                = SilLiTypeInfo.ITypeInterface;
  ITypeInterfaceMethod          = SilLiTypeInfo.ITypeInterfaceMethod;
  ITypeArray                    = SilLiTypeInfo.ITypeArray;
  ITypeStaticArray              = SilLiTypeInfo.ITypeStaticArray;
  ITypeDynarray                 = SilLiTypeInfo.ITypeDynarray;

type
  TDataTypecastStatus           = SilLiDataType.TDataTypecastStatus;

const
  tcSOk                         = TDataTypecastStatus(SilLiDataType.tcSOk           );
  tcSUnassigned                 = TDataTypecastStatus(SilLiDataType.tcSUnassigned   );
  tcSDefault                    = TDataTypecastStatus(SilLiDataType.tcSDefault      );
  tcSIgnore                     = TDataTypecastStatus(SilLiDataType.tcSIgnore       );
  tcSIsNull                     = TDataTypecastStatus(SilLiDataType.tcSIsNull       );
  tcSTruncated                  = TDataTypecastStatus(SilLiDataType.tcSTruncated    );
  tcSRoundOff                   = TDataTypecastStatus(SilLiDataType.tcSRoundOff     );
  tcECantConvert                = TDataTypecastStatus(SilLiDataType.tcECantConvert  );
  tcESignMismatch               = TDataTypecastStatus(SilLiDataType.tcESignMismatch );
  tcEDataOverflow               = TDataTypecastStatus(SilLiDataType.tcEDataOverflow );
  tcECantCreate                 = TDataTypecastStatus(SilLiDataType.tcECantCreate   );
  tcEUnavailable                = TDataTypecastStatus(SilLiDataType.tcEUnavailable  );
  tcEBadStatus                  = TDataTypecastStatus(SilLiDataType.tcEBadStatus    );
  tcEUnespecified               = TDataTypecastStatus(SilLiDataType.tcEUnespecified );

const
  tcSucceeded                   = SilLiDataType.tcSucceeded;

const
  tcFailed                      = SilLiDataType.tcFailed;

type
  IDataType2                    = SilLiDataType.IDataType                     ;
  IDataTypeDef2                 = SilLiDataType.IDataTypeDef                  ;
  IDataHandler                  = SilLiDataType.IDataHandler                  ;
  IDataConvert                  = SilLiDataType.IDataConvert                  ;
  IDataAssign                   = SilLiDataType.IDataAssign                   ;
  IDataTypeList                 = SilLiDataType.IDataTypeList                 ;
  IDataTypecast                 = SilLiDataType.IDataTypecast                 ;
  IDataBuffer                   = SilLiDataType.IDataBuffer                   ;

type
  ESilConversionFailed          = SilLiDataType.ESilConversionFailed;

type
  IPacket                       = SilLiPacket.IPacket;
  IPacketDelivery               = SilLiPacket.IPacketDelivery; 
                                
type                                                      
  RParameter                    = SilLiParameters.RParameter;
  RParameters                   = SilLiParameters.RParameters; 
  IArguments                    = SilLiParameters.IArguments;
  IParameters                   = SilLiParameters.IParameters;
  IArgumentList                 = SilLiParameters.IArgumentList;
  IParameterList                = SilLiParameters.IParameterList;
  REvParametersItemEvent        = SilLiParameters.REvParametersItemEvent;
  IParameterListEvents          = SilLiParameters.IParameterListEvents;
  TMergePrecedence              = SilLiParameters.TMergePrecedence;

const
  mkSource                      = SilLiParameters.mkSource;
  mkDestination                 = SilLiParameters.mkDestination;

type                                                      
  PMemberHeader                 = SilLiMemberList.PMemberHeader;
  PPMemberHeader                = SilLiMemberList.PPMemberHeader;
  RMemberHeader                 = SilLiMemberList.RMemberHeader;
  PMember                       = SilLiMemberList.PMember;
  RMember                       = SilLiMemberList.RMember;
  IMemberList                   = SilLiMemberList.IMemberList;
   
const
  ERROR_SUCCESS                 = SilOsTypes.ERROR_SUCCESS;
  INFINITE                      = SilOsTypes.INFINITE;
  INVALID_HANDLE_VALUE          = SilOsTypes.INVALID_HANDLE_VALUE;
  NULL_HANDLE_VALUE             = SilOsTypes.NULL_HANDLE_VALUE;

const
  S_OK                          = HRESULT(SilOsTypes.S_OK          );
  S_FALSE                       = HRESULT(SilOsTypes.S_FALSE       );
  NOERROR                       = HRESULT(SilOsTypes.NOERROR       );
  E_UNEXPECTED                  = HRESULT(SilOsTypes.E_UNEXPECTED  );
  E_NOTIMPL                     = HRESULT(SilOsTypes.E_NOTIMPL     );
  E_INVALIDARG                  = HRESULT(SilOsTypes.E_INVALIDARG  );
  E_NOINTERFACE                 = HRESULT(SilOsTypes.E_NOINTERFACE );
  E_POINTER                     = HRESULT(SilOsTypes.E_POINTER     );
  E_HANDLE                      = HRESULT(SilOsTypes.E_HANDLE      );
  E_FAIL                        = HRESULT(SilOsTypes.E_FAIL        );
  E_ACCESSDENIED                = HRESULT(SilOsTypes.E_ACCESSDENIED);
  E_OUTOFMEMORY                 = HRESULT(SilOsTypes.E_OUTOFMEMORY );
  E_ABORT                       = HRESULT(SilOsTypes.E_ABORT       );
  E_PENDING                     = HRESULT(SilOsTypes.E_PENDING     );

(*)const
  REGDB_E_CLASSNOTREG           = HRESULT(SilOsTypes.REGDB_E_CLASSNOTREG);(*)

type
  OsColorPtr                    = SilOsTypes.OsColorPtr;
  OsColor                       = SilOsTypes.OsColor;

const
  clScrollBar                   = SilOsTypes.clScrollBar              ;
  clBackground                  = SilOsTypes.clBackground             ;
  clActiveCaption               = SilOsTypes.clActiveCaption          ;
  clInactiveCaption             = SilOsTypes.clInactiveCaption        ;
  clMenu                        = SilOsTypes.clMenu                   ;
  clWindow                      = SilOsTypes.clWindow                 ;
  clWindowFrame                 = SilOsTypes.clWindowFrame            ;
  clMenuText                    = SilOsTypes.clMenuText               ;
  clWindowText                  = SilOsTypes.clWindowText             ;
  clCaptionText                 = SilOsTypes.clCaptionText            ;
  clActiveBorder                = SilOsTypes.clActiveBorder           ;
  clInactiveBorder              = SilOsTypes.clInactiveBorder         ;
  clAppWorkSpace                = SilOsTypes.clAppWorkSpace           ;
  clHighlight                   = SilOsTypes.clHighlight              ;
  clHighlightText               = SilOsTypes.clHighlightText          ;
  clBtnFace                     = SilOsTypes.clBtnFace                ;
  clBtnShadow                   = SilOsTypes.clBtnShadow              ;
  clGrayText                    = SilOsTypes.clGrayText               ;
  clBtnText                     = SilOsTypes.clBtnText                ;
  clInactiveCaptionText         = SilOsTypes.clInactiveCaptionText    ;
  clBtnHighlight                = SilOsTypes.clBtnHighlight           ;
  cl3DDkShadow                  = SilOsTypes.cl3DDkShadow             ;
  cl3DLight                     = SilOsTypes.cl3DLight                ;
  clInfoText                    = SilOsTypes.clInfoText               ;
  clInfoBk                      = SilOsTypes.clInfoBk                 ;
  clGradientActiveCaption       = SilOsTypes.clGradientActiveCaption  ;
  clGradientInactiveCaption     = SilOsTypes.clGradientInactiveCaption;

const
  clBlack                       = SilOsTypes.clBlack                  ;
  clMaroon                      = SilOsTypes.clMaroon                 ;
  clGreen                       = SilOsTypes.clGreen                  ;
  clOlive                       = SilOsTypes.clOlive                  ;
  clNavy                        = SilOsTypes.clNavy                   ;
  clPurple                      = SilOsTypes.clPurple                 ;
  clTeal                        = SilOsTypes.clTeal                   ;
  clGray                        = SilOsTypes.clGray                   ;
  clSilver                      = SilOsTypes.clSilver                 ;
  clRed                         = SilOsTypes.clRed                    ;
  clLime                        = SilOsTypes.clLime                   ;
  clYellow                      = SilOsTypes.clYellow                 ;
  clBlue                        = SilOsTypes.clBlue                   ;
  clFuchsia                     = SilOsTypes.clFuchsia                ;
  clAqua                        = SilOsTypes.clAqua                   ;
  clLtGray                      = SilOsTypes.clLtGray                 ;
  clDkGray                      = SilOsTypes.clDkGray                 ;
  clWhite                       = SilOsTypes.clWhite                  ;

const
  clMoneyGreen                  = SilOsTypes.clMoneyGreen             ;
  clSkyBlue                     = SilOsTypes.clSkyBlue                ;
  clCream                       = SilOsTypes.clCream                  ;
  clMedGray                     = SilOsTypes.clMedGray                ;

const
  clNone                        = SilOsTypes.clNone                   ;
  clDefault                     = SilOsTypes.clDefault                ;

type
  OsCode                        = SilOsError.OsCode;
  OsState                       = SilOsError.OsState;
  EOsException                  = SilOsError.OsException;

const
  SUCCESS                       = OsCode(SilOsError.SUCCESS);

type
  OsWord                        = SilOsTypes.OsWord;
  OsBool                        = SilOsTypes.OsBool;
  OsByte                        = SilOsTypes.OsByte;
  OsChar                        = SilOsTypes.OsChar;
  OsBytePtr                     = SilOsTypes.OsBytePtr;
  OsCharPtr                     = SilOsTypes.OsCharPtr;

const
  CPathSeparator                = SilOsTypes.CPathSeparator;
  CPathListSeparator            = SilOsTypes.CPathListSeparator;

const
  SwitchChars                   = SilOsTypes.SwitchChars;

{$IFNDEF D60}
const
  sLineBreak                    = SilOsTypes.sLineBreak;
{$ENDIF}                               

type
  GUID                          = SilOsTool.OsGuid;

type
  TLocaleLevel                  = SilOeLang.TLocaleLevel;
                                
const                           
  DefaultLevel                  = TLocaleLevel(SilOeLang.DefaultLevel);
  SystemLevel                   = TLocaleLevel(SilOeLang.SystemLevel);
  UserLevel                     = TLocaleLevel(SilOeLang.UserLevel);
                                
type                            
  TLocaleCheck                  = SilOeLang.TLocaleCheck;
                                
const                           
  InstalledCheck                = TLocaleCheck(SilOeLang.InstalledCheck);
  SupportedCheck                = TLocaleCheck(SilOeLang.SupportedCheck);
                                
type                            
  LangID                        = SilOeLang.LangID;
  Locale                        = SilOeLang.Locale;
                                
type                            
  IHandle                       = SilOiHandle.IHandle;
  IHandleList                   = SilOiHandle.IHandleList;
  IHandledObject                = SilOiHandle.IHandledObject;

type
  IEnvironmentList              = SilOiEnvironment.IEnvironmentList;

type
  TFileAccessMode               = SilOiFile.TFileAccessMode;
  TFileInfoKind                 = SilOiFile.TFileInfoKind;
  TFileShareMode                = SilOiFile.TFileShareMode;
                                
const // TFileAccessMode        
  fmAccessRead                  = SilOiFile.fmAccessRead;
  fmAccessWrite                 = SilOiFile.fmAccessWrite;
  fmAccessReadWrite             = SilOiFile.fmAccessReadWrite;
                                
const // TFileInfoKind          
  ikName                        = SilOiFile.ikName;
  ikDate                        = SilOiFile.ikDate;
  ikSize                        = SilOiFile.ikSize;
  ikPath                        = SilOiFile.ikPath;
  ikAttributes                  = SilOiFile.ikAttributes;
                                
const // TFileShareMode         
  fmShareNone                   = SilOiFile.fmShareNone;
  fmShareRead                   = SilOiFile.fmShareRead;
  fmShareWrite                  = SilOiFile.fmShareWrite;
  fmShareReadWrite              = SilOiFile.fmShareReadWrite;
                                
type                            
  IWindow                       = SilOiWindow.IWindow;
                                
type                            
  TFileAttribute                = SilOiFile.TFileAttribute;
  TFileAttributes               = SilOiFile.TFileAttributes;
                                
const // TFileAttributes        
  faArchive                     = SilOiFile.faArchive;
  faDirectory                   = SilOiFile.faDirectory;
  faHidden                      = SilOiFile.faHidden;
  faReadOnly                    = SilOiFile.faReadOnly;
  faSysFile                     = SilOiFile.faSysFile;
                                
type                            
  IFile                         = SilOiFile.IFile;
  IFileList                     = SilOiFile.IFileList;
  IFileInfo                     = SilOiFile.IFileInfo;
  IFileInfoDef                  = SilOiFile.IFileInfoDef;
  IFileInfoList                 = SilOiFile.IFileInfoList;
  IDirectoryReader              = SilOiFile.IDirectoryReader;
                                
type                            
  ITextFile                     = SilOiTextFile.ITextFile;
  IFileStream                   = ITextStream;
                                
type                            
  RTimerEvent                   = SilOiTimer.RTimerEvent;
  ITimer                        = SilOiTimer.ITimer;
  ITimerEvents                  = SilOiTimer.ITimerEvents;
                                
type                            
  IThread                       = SilOiThread.IThread;
  IThreadHook                   = SilOiThread.IThreadHook;
  RThreadRunMessage             = SilOiThread.RThreadRunMessage;
  IRunnable                     = SilOiThread.IRunnable;
  IThreadManager                = SilOiThread.IThreadManager;
  IThreadEvents                 = SilOiThread.IThreadEvents;
  IThreadList                   = SilOiThread.IThreadList;
  ICachedThread                 = SilOiThread.ICachedThread;
  IThreads                      = SilOiThread.IThreads;
  TThreadMethod                 = SilOiThread.TThreadMethod;
  TThreadCallMethod             = SilOiThread.TThreadCallMethod;

type
  TThreadPriority               = SilOiThread.TThreadPriority;

const
  tpIdle                        = TThreadPriority(SilOiThread.tpIdle        );
  tpLowest                      = TThreadPriority(SilOiThread.tpLowest      );
  tpLower                       = TThreadPriority(SilOiThread.tpLower       );
  tpNormal                      = TThreadPriority(SilOiThread.tpNormal      );
  tpHigher                      = TThreadPriority(SilOiThread.tpHigher      );
  tpHighest                     = TThreadPriority(SilOiThread.tpHighest     );
  tpTimeCritical                = TThreadPriority(SilOiThread.tpTimeCritical);
                                
type                            
  TThreadCallMode               = SilOiThread.TThreadCallMode;
                                
const                           
  tcSync                        = TThreadCallMode(SilOiThread.tcSync );
  tcAsync                       = TThreadCallMode(SilOiThread.tcAsync);
                                
type
  IMessenger                    = SilOiMessenger.IMessenger;

type
  ITask                         = SilOiTask.ITask;
  ITaskHook                     = SilOiTask.ITaskHook;

type
  TProcessPriority              = SilOeProcess.TProcessPriority;
  TProcessVisibility            = SilOeProcess.TProcessVisibility;

type
  RProcessParameters            = SilOeProcess.RProcessParameters;

const // TProcessPriority
  ppIdle                        = SilOeProcess.ppIdle;
  ppNormal                      = SilOeProcess.ppNormal;
  ppHigh                        = SilOeProcess.ppHigh;
  ppRealtime                    = SilOeProcess.ppRealtime;

const // TProcessVisibility
  pvHidden                      = TProcessVisibility(SilOeProcess.pvHidden);
  pvNormal                      = TProcessVisibility(SilOeProcess.pvNormal);
  pvMinimized                   = TProcessVisibility(SilOeProcess.pvMinimized);
  pvMaximized                   = TProcessVisibility(SilOeProcess.pvMaximized);
  pvNoActivate                  = TProcessVisibility(SilOeProcess.pvNoActivate);
                                
type                            
  IProcess                      = SilOiProcess.IProcess;
  IProcessList                  = SilOiProcess.IProcessList;

type
  TSyncWaitResult               = SilOeWait.TSyncWaitResult;

type
  IWaitable                     = SilOiWait.IWaitable;
//  TWaitableArray                = SilOiIpc.TWaitableArray;

const         
  wrSignaled                    = TSyncWaitResult(SilOeWait.wrSignaled);
  wrTimeout                     = TSyncWaitResult(SilOeWait.wrTimeout);
  wrAbandoned                   = TSyncWaitResult(SilOeWait.wrAbandoned);
  wrMessage                     = TSyncWaitResult(SilOeWait.wrMessage);
  wrError                       = TSyncWaitResult(SilOeWait.wrError);

type                            
  ICriticalSection              = SilOiIpc.ICriticalSection;
  IIpcObject                    = SilOiIpc.IIpcObject;
  IEvent                        = SilOiIpc.IEvent;
  IMutex                        = SilOiIpc.IMutex;
  ISemaphore                    = SilOiIpc.ISemaphore;

type
  TFilesystemChangeFilter       = SilOeFilesystemNotifier.TFilesystemChangeFilter;
  TFilesystemChangeFilters      = SilOeFilesystemNotifier.TFilesystemChangeFilters;
  TChangeFilter                 = TFilesystemChangeFilter;
  TChangeFilters                = TFilesystemChangeFilters;

const // TChangeFilter
  cfFileName                    = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfFileName);
  cfDirName                     = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfDirName);
  cfAttrib                      = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfAttrib);
  cfSize                        = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfSize);
  cfLastWrite                   = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfLastWrite);
  cfLastAccess                  = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfLastAccess);
  cfSecurity                    = TFilesystemChangeFilter(SilOeFilesystemNotifier.cfSecurity);

type
  IFilesystemChangeNotification = SilOiFilesystemNotifier.IFilesystemChangeNotification;

//  IChangeNotification

const
  EV_INITIALIZE                 = SilOeMessenger.EV_INITIALIZE;
  EV_FINALIZE                   = SilOeMessenger.EV_FINALIZE;
  EV_STARTUP                    = SilOeMessenger.EV_STARTUP;
  EV_SHUTDOWN                   = SilOeMessenger.EV_SHUTDOWN;
  EV_ACTIVATE                   = SilOeMessenger.EV_ACTIVATE;
  EV_DEACTIVATE                 = SilOeMessenger.EV_DEACTIVATE;
  EV_DISPATCH                   = SilOeMessenger.EV_DISPATCH;
  EV_CREATE                     = SilOeMessenger.EV_CREATE;
  EV_DESTROY                    = SilOeMessenger.EV_DESTROY;
  EV_FIRST                      = SilOeMessenger.EV_FIRST;
  
type
  ISharedMemory                 = SilOiSharedMemory.ISharedMemory;

type
  ISocket                       = SilOiSocket.ISocket;
  ISocketPeer                   = SilOiSocket.ISocketPeer;
  ISocketStream                 = SilOiSocket.ISocketStream;
  ISocketParameters             = SilOiSocket.ISocketParameters;
  ISocketInfo                   = SilOiSocket.ISocketInfo;
  ISocketPeerInfo               = SilOiSocket.ISocketPeerInfo;
  ISocketAddress                = SilOiSocket.ISocketAddress;
  ISocketAddresses              = SilOiSocket.ISocketAddresses;
  ISocketAddressList            = SilOiSocket.ISocketAddressList;
  ISocketAddressDef             = SilOiSocket.ISocketAddressDef;
  ISocketClient                 = SilOiSocket.ISocketClient;
  ISocketServer                 = SilOiSocket.ISocketServer;
  ISocketProtocol               = SilOiSocket.ISocketProtocol;
  ISocketService                = SilOiSocket.ISocketService;
  ISockets                      = SilOiSocket.ISockets;
  ISocketList                   = SilOiSocket.ISocketList;

type
  TSocketState                  = SilOiSocket.TSocketState;
  TSocketStates                 = SilOiSocket.TSocketStates;
  TSocketShutdown               = SilOiSocket.TSocketShutdown;
  TSocketType                   = SilOiSocket.TSocketType;
  TSocketProtocol               = SilOiSocket.TSocketProtocol;
  TSocketNetworkClass           = SilOiSocket.TSocketNetworkClass;
  TSocketFlag                   = SilOiSocket.TSocketFlag;
  TSocketFlags                  = SilOiSocket.TSocketFlags;
  TSocketStreamFlag             = SilOiSocket.TSocketStreamFlag;
  TSocketStreamFlags            = SilOiSocket.TSocketStreamFlags;
  TSocketAddressFlag            = SilOiSocket.TSocketAddressFlag;
  TSocketAddressFlags           = SilOiSocket.TSocketAddressFlags;

const
  // TSocketState
  ssRead                        = TSocketState(SilOiSocket.ssRead);
  ssWrite                       = TSocketState(SilOiSocket.ssWrite);
  ssError                       = TSocketState(SilOiSocket.ssError);

  // TSocketShutdown
  shRead                        = TSocketShutdown(SilOiSocket.shRead);
  shWrite                       = TSocketShutdown(SilOiSocket.shWrite);
  shBoth                        = TSocketShutdown(SilOiSocket.shBoth);

  // TSocketType
  stUnknown                     = TSocketType(SilOiSocket.stUnknown);
  stStream                      = TSocketType(SilOiSocket.stStream);
  stDatagram                    = TSocketType(SilOiSocket.stDatagram);
  stRaw                         = TSocketType(SilOiSocket.stRaw);
  stRdm                         = TSocketType(SilOiSocket.stRdm);
  stSeqPacket                   = TSocketType(SilOiSocket.stSeqPacket);

  // TSocketProtocol
  spUnknown                     = TSocketProtocol(SilOiSocket.spUnknown);
  spIP                          = TSocketProtocol(SilOiSocket.spIP);
  spICMP                        = TSocketProtocol(SilOiSocket.spICMP);
  spTCP                         = TSocketProtocol(SilOiSocket.spTCP);
  spUDP                         = TSocketProtocol(SilOiSocket.spUDP);
  spRaw                         = TSocketProtocol(SilOiSocket.spRaw);

  // TSocketNetworkClass
  snUnknown                     = TSocketNetworkClass(SilOiSocket.snUnknown);
  snInternal                    = TSocketNetworkClass(SilOiSocket.snInternal);
  snClassA                      = TSocketNetworkClass(SilOiSocket.snClassA);
  snClassB                      = TSocketNetworkClass(SilOiSocket.snClassB);
  snClassC                      = TSocketNetworkClass(SilOiSocket.snClassC);

  // TSocketFlag
  sfBroadcast                   = TSocketFlag(SilOiSocket.sfBroadcast);
  sfDebugging                   = TSocketFlag(SilOiSocket.sfDebugging);
  sfDontLinger                  = TSocketFlag(SilOiSocket.sfDontLinger);
  sfDontRouting                 = TSocketFlag(SilOiSocket.sfDontRouting);
  sfKeepAlive                   = TSocketFlag(SilOiSocket.sfKeepAlive);
  sfOutOfBandReception          = TSocketFlag(SilOiSocket.sfOutOfBandReception);
  sfReuseAddress                = TSocketFlag(SilOiSocket.sfReuseAddress);
  sfReceiveTimeout              = TSocketFlag(SilOiSocket.sfReceiveTimeout);
  sfSendTimeout                 = TSocketFlag(SilOiSocket.sfSendTimeout);
  sfNoDelay                     = TSocketFlag(SilOiSocket.sfNoDelay);

  // TSocketStreamFlag
  rfOutOfBand                   = TSocketStreamFlag(SilOiSocket.rfOutOfBand);
  rfPeek                        = TSocketStreamFlag(SilOiSocket.rfPeek);
  wfOutOfBand                   = TSocketStreamFlag(SilOiSocket.wfOutOfBand);


  // TSocketAddressFlag
  afHost                        = TSocketAddressFlag(SilOiSocket.afHost);
  afAddress                     = TSocketAddressFlag(SilOiSocket.afAddress);
  afPort                        = TSocketAddressFlag(SilOiSocket.afPort);
  afTypeSpec                    = TSocketAddressFlag(SilOiSocket.afTypeSpec);
  afProtocol                    = TSocketAddressFlag(SilOiSocket.afProtocol);
  afSubnetMask                  = TSocketAddressFlag(SilOiSocket.afSubnetMask);
  afNetworkClass                = TSocketAddressFlag(SilOiSocket.afNetworkClass);
  afBroadcast                   = TSocketAddressFlag(SilOiSocket.afBroadcast);
  afNetwork                     = TSocketAddressFlag(SilOiSocket.afNetwork);

  CAddressShort                 = SilOiSocket.CAddressShort;
  CAddressLong                  = SilOiSocket.CAddressLong;
  CAddressAndMask               = SilOiSocket.CAddressAndMask;

  CAddress                      = SilOiSocket.CAddress;
  CAddressPort                  = SilOiSocket.CAddressPort;
  CAddressMask                  = SilOiSocket.CAddressMask;
  CAddressBroadcast             = SilOiSocket.CAddressBroadcast;
  CAddressNetwork               = SilOiSocket.CAddressNetwork;
  CAddressNetBits               = SilOiSocket.CAddressNetBits;

type  
  TPipeAccess                   = SilOiPipe.TPipeAccess;

const
  paDuplex                      = TPipeAccess(SilOiPipe.paDuplex);
  paInbound                     = TPipeAccess(SilOiPipe.paInbound);
  paOutbound                    = TPipeAccess(SilOiPipe.paOutbound);

type
  TPipeMode                     = SilOiPipe.TPipeMode;

const
  pmStream                      = TPipeMode(SilOiPipe.pmStream);
  pmMessage                     = TPipeMode(SilOiPipe.pmMessage);

type  
  TPipeEndpoint                 = SilOiPipe.TPipeEndpoint;

const
  peClient                      = TPipeEndpoint(SilOiPipe.peClient);
  peServer                      = TPipeEndpoint(SilOiPipe.peServer);

type  
  INamedPipeParameters          = SilOiPipe.INamedPipeParameters;
  INamedPipeServer              = SilOiPipe.INamedPipeServer;
  IPipe                         = SilOiPipe.IPipe;
  INamedPipeClient              = SilOiPipe.INamedPipeClient;
  INamedPipeServerClient        = SilOiPipe.INamedPipeServerClient;

type
  ISharedLibrary                = SilOiSharedLibrary.ISharedLibrary;

type
  ILibraryBinding               = SilOiLibraryBinding.ILibraryBinding;

type
  IPendingEvent                 = SilOiEventQueue.IPendingEvent;
  IEventQueue                   = SilOiEventQueue.IEventQueue;
  IEventSink                    = SilOiEventQueue.IEventSink;
  TEvent                        = SilOiEventQueue.TEvent;
  TEventParams                  = SilOiEventQueue.TEventParams;
  TEventQueueMessage            = SilOiEventQueue.TEventQueueMessage;

type
  ISerialPort                   = SilOiSerialPort.ISerialPort;
  ISerialPortParameters         = SilOiSerialPort.ISerialPortParameters;
  ISerialPortTimeouts           = SilOiSerialPort.ISerialPortTimeouts;
  ISerialPortControlBlock       = SilOiSerialPort.ISerialPortControlBlock;
  TSerialPortEvent              = SilOiSerialPort.TSerialPortEvent;
  ISerialPortEvents             = SilOiSerialPort.ISerialPortEvents;
                                
type                            
  TCommParity                   = SilOiSerialPort.TCommParity;
                                
const // TCommParity            
  paNone                        = TCommParity(SilOiSerialPort.paNone);
  paOdd                         = TCommParity(SilOiSerialPort.paOdd);
  paEven                        = TCommParity(SilOiSerialPort.paEven);
  paMark                        = TCommParity(SilOiSerialPort.paMark);
  paSpace                       = TCommParity(SilOiSerialPort.paSpace);
                                
type                            
  TCommStopBits                 = SilOiSerialPort.TCommStopBits;
                                
const // TCommStopBits          
  sbOne                         = TCommStopBits(SilOiSerialPort.sbOne);
  sbOneHalf                     = TCommStopBits(SilOiSerialPort.sbOneHalf);
  sbTwo                         = TCommStopBits(SilOiSerialPort.sbTwo);

type
  TCommEvent                    = SilOiSerialPort.TCommEvent;
  TCommEvents                   = SilOiSerialPort.TCommEvents;

const // TCommEvent
  evBreak                       = TCommEvent(SilOiSerialPort.evBreak);
  evCTS                         = TCommEvent(SilOiSerialPort.evCTS);
  evDSR                         = TCommEvent(SilOiSerialPort.evDSR);
  evError                       = TCommEvent(SilOiSerialPort.evError);
  evRing                        = TCommEvent(SilOiSerialPort.evRing);
  evRlsd                        = TCommEvent(SilOiSerialPort.evRlsd);
  evRxChar                      = TCommEvent(SilOiSerialPort.evRxChar);
  evRxFlag                      = TCommEvent(SilOiSerialPort.evRxFlag);
  evTxEmpty                     = TCommEvent(SilOiSerialPort.evTxEmpty);

type
  TCommEscapeCode               = SilOiSerialPort.TCommEscapeCode;

const
  ecSetXOff                     = TCommEscapeCode(SilOiSerialPort.ecSetXOff    );
  ecSetXOn                      = TCommEscapeCode(SilOiSerialPort.ecSetXOn     );
  ecSetRTS                      = TCommEscapeCode(SilOiSerialPort.ecSetRTS     );
  ecClearRTS                    = TCommEscapeCode(SilOiSerialPort.ecClearRTS   );
  ecSetDTR                      = TCommEscapeCode(SilOiSerialPort.ecSetDTR     );
  ecClearDTR                    = TCommEscapeCode(SilOiSerialPort.ecClearDTR   );
  ecResetDev                    = TCommEscapeCode(SilOiSerialPort.ecResetDev   );
  ecSetBreak                    = TCommEscapeCode(SilOiSerialPort.ecSetBreak   );
  ecClearBreak                  = TCommEscapeCode(SilOiSerialPort.ecClearBreak );
                                
type                            
  TCommPurge                    = SilOiSerialPort.TCommPurge;
                                
const                           
  puTxAbort                     = TCommPurge(SilOiSerialPort.puTxAbort);
  puRxAbort                     = TCommPurge(SilOiSerialPort.puRxAbort);
  puTxClear                     = TCommPurge(SilOiSerialPort.puTxClear);
  puRxClear                     = TCommPurge(SilOiSerialPort.puRxClear);
                                
type                            
  TSerialDtrControl             = SilOiSerialPort.TSerialDtrControl;
                                
const                           
  dcEnabled                     = TSerialDtrControl(SilOiSerialPort.dcEnabled  );
  dcDisabled                    = TSerialDtrControl(SilOiSerialPort.dcDisabled );
  dcHandShake                   = TSerialDtrControl(SilOiSerialPort.dcHandShake);
                                
type                            
  TSerialRtsControl             = SilOiSerialPort.TSerialRtsControl;
                                
const                           
  rcEnabled                     = TSerialRtsControl(SilOiSerialPort.rcEnabled   );
  rcDisabled                    = TSerialRtsControl(SilOiSerialPort.rcDisabled  );
  rcHandShake                   = TSerialRtsControl(SilOiSerialPort.rcHandShake );
  rcToggle                      = TSerialRtsControl(SilOiSerialPort.rcToggle    );
                                
type
  TVersionItem                  = SilOiVersion.TVersionItem;
  TVersionItems                 = SilOiVersion.TVersionItems;
                                
type                            
  IModule                       = SilOiModule.IModule;
  IModule2                      = SilOiModule.IModule2;
  IModules                      = SilOiModule.IModules;
  IModuleList                   = SilOiModule.IModuleList;

const // TVersionItem           
  viMajor                       = TVersionItem(SilOiVersion.viMajor);
  viMinor                       = TVersionItem(SilOiVersion.viMinor);
  viRevision                    = TVersionItem(SilOiVersion.viRevision);
  viBuild                       = TVersionItem(SilOiVersion.viBuild);
                                
type
  IVersionInfo                  = SilOiVersion.IVersionInfo;
  IVersionNumber                = SilOiVersion.IVersionNumber;
  IVersionTags                  = SilOiVersion.IVersionTags;
  IStandardTags                 = SilOiVersion.IStandardTags;

const
  CShortVersion                 = SilOiVersion.CShortVersion;
  CLongVersion                  = SilOiVersion.CLongVersion;
  CRelease                      = SilOiVersion.CRelease;

type
  IPerformanceCounter           = SilOiPerformance.IPerformanceCounter;

type
  IComputer                     = SilOiComputer.IComputer;
  IComputerDomain               = SilOiComputer.IComputerDomain;
  IComputerNetworkCard          = SilOiComputer.IComputerNetworkCard;

type
  TShutdownFlag                 = SilOiComputer.TShutdownFlag;
  TShutdownFlags                = SilOiComputer.TShutdownFlags;               

const
  sfLogOff                      = SilOiComputer.sfLogOff;
  sfShutdown                    = SilOiComputer.sfShutdown;
  sfReboot                      = SilOiComputer.sfReboot;
  sfForce                       = SilOiComputer.sfForce;
  sfPowerOff                    = SilOiComputer.sfPowerOff;

function Debug: PDebug;

implementation

uses
  SilBgDebug;

function Debug: PDebug;
begin
  Result := @SilBgDebug.GDebug;
end;

end.

