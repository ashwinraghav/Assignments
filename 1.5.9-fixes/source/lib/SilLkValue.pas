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

unit SilLkValue;

{$INCLUDE Defines.inc}

interface

uses
  SilLkAggregable,
  SilLkExtensible,
  SilBeError,
  SilBeDataType,
  SilLiDataType,
  SilLiValue,
  SilLiTraits,
  SilLhValue;


type
  TSilValueItems = class(
    TSilExtensibleObject,
    IDataBuffer )
  private
    FBuffer: RAdoptedRef;
    FClasses: PValueClasses;
    FItems: array of Pointer;
  private
    procedure DoGet(ID: LongWord; const IID: TGUID; Result: PUnknown);
    function DoGetBuffer: IDataBuffer;
  protected // IDataBuffer
    property DataBuffer: IDataBuffer read DoGetBuffer implements IDataBuffer;
  protected
    constructor Create(const Buffer: IDataBuffer; Classes: PValueClasses; const Controller: IUnknown = nil); reintroduce;
  public
    destructor Destroy; override;
  end;

  TSilValue = class(
    TSilValueItems,
    IValue )
  protected // IValue
    function GetValueVariant: IConstant;
    function GetValueChar: IValueChar;
    function GetValueByte: IValueByte;
    function GetValueShortint: IValueShortint;
    function GetValueWord: IValueWord;
    function GetValueSmallint: IValueSmallint;
    function GetValueString: IValueAnsiString;
    function GetValueWideString: IValueWideString;
    function GetValueInteger: IValueInteger;
    function GetValueLargeInt: IValueLargeInt;
    function GetValueLongWord: IValueLongWord;
    function GetValueBoolean: IValueBoolean;
    function GetValueDouble: IValueDouble;
    function GetValueSingle: IValueSingle;
    function GetValueExtended: IValueExtended;
    function GetValueCurrency: IValueCurrency;
    function GetValueDateTime: IValueDateTime;
    function GetValueGuid: IValueGuid;
    function GetValuePointer: IValuePointer;
    function GetValueInterface: IValueInterface;
  protected
    property AsVariant: IConstant read GetValueVariant;
    property AsChar: IValueChar read GetValueChar;
    property AsByte: IValueByte read GetValueByte;
    property AsShortint: IValueShortint read GetValueShortint;
    property AsWord: IValueWord read GetValueWord;
    property AsSmallint: IValueSmallint read GetValueSmallint;
    property AsString: IValueAnsiString read GetValueString;
    property AsWideString: IValueWideString read GetValueWideString;
    property AsInteger: IValueInteger read GetValueInteger;
    property AsLargeInt: IValueLargeInt read GetValueLargeInt;
    property AsLongWord: IValueLongWord read GetValueLongWord;
    property AsBoolean: IValueBoolean read GetValueBoolean;
    property AsDouble: IValueDouble read GetValueDouble;
    property AsSingle: IValueSingle read GetValueSingle;
    property AsExtended: IValueExtended read GetValueExtended;
    property AsCurrency: IValueCurrency read GetValueCurrency;
    property AsDateTime: IValueDateTime read GetValueDateTime;
    property AsGuid: IValueGuid read GetValueGuid;
    property AsPointer: IValuePointer read GetValuePointer;
    property AsInterface: IValueInterface read GetValueInterface;
  public
    constructor Create(const Buffer: IDataBuffer; const Controller: IUnknown = nil); overload;  
  end;    

  TSilVariable = class(
    TSilValue,
    IVariable )
  protected 
    procedure Null(Param: Pointer = nil); overload; 
    procedure Clear(Param: Pointer = nil); overload;
  protected // IVariable
    function GetVariableVariant: IVariant;
    function GetVariableChar: IVariableChar;
    function GetVariableByte: IVariableByte;
    function GetVariableShortint: IVariableShortint;
    function GetVariableWord: IVariableWord;
    function GetVariableSmallint: IVariableSmallint;
    function GetVariableString: IVariableAnsiString;
    function GetVariableWideString: IVariableWideString;
    function GetVariableInteger: IVariableInteger;
    function GetVariableLargeInt: IVariableLargeInt;
    function GetVariableLongWord: IVariableLongWord;
    function GetVariableBoolean: IVariableBoolean;
    function GetVariableDouble: IVariableDouble;
    function GetVariableSingle: IVariableSingle;
    function GetVariableExtended: IVariableExtended;
    function GetVariableCurrency: IVariableCurrency;
    function GetVariableDateTime: IVariableDateTime;
    function GetVariableGuid: IVariableGuid;
    function GetVariablePointer: IVariablePointer;
    function GetVariableInterface: IVariableInterface;
  protected
    property AsVariant: IVariant read GetVariableVariant;
    property AsChar: IVariableChar read GetVariableChar;
    property AsByte: IVariableByte read GetVariableByte;
    property AsShortint: IVariableShortint read GetVariableShortint;
    property AsWord: IVariableWord read GetVariableWord;
    property AsSmallint: IVariableSmallint read GetVariableSmallint;
    property AsString: IVariableAnsiString read GetVariableString;
    property AsWideString: IVariableWideString read GetVariableWideString;
    property AsInteger: IVariableInteger read GetVariableInteger;
    property AsLargeInt: IVariableLargeInt read GetVariableLargeInt;
    property AsLongWord: IVariableLongWord read GetVariableLongWord;
    property AsBoolean: IVariableBoolean read GetVariableBoolean;
    property AsDouble: IVariableDouble read GetVariableDouble;
    property AsSingle: IVariableSingle read GetVariableSingle;
    property AsExtended: IVariableExtended read GetVariableExtended;
    property AsCurrency: IVariableCurrency read GetVariableCurrency;
    property AsDateTime: IVariableDateTime read GetVariableDateTime;
    property AsGuid: IVariableGuid read GetVariableGuid;
    property AsPointer: IVariablePointer read GetVariablePointer;
    property AsInterface: IVariableInterface read GetVariableInterface;
  public
    constructor Create(const Buffer: IDataBuffer; const Controller: IUnknown = nil); 
  end;

  TSilValueItem = class(TSilValueType)
  private
    FBuffer: Pointer;
    FTarget: Pointer;
    FOrigin: Pointer;
  private
    function DoGetBuffer: IDataBuffer;
    function DoGetOrigin: IDataHandler;
    function DoGetTarget: IDataHandler;
  protected
    function DoCreateError(Error: TDataTypecastStatus): Exception;
    function DoGetAsString: string;
  protected
    property Buffer: IDataBuffer read DoGetBuffer;
    property Origin: IDataHandler read DoGetOrigin;
    property Target: IDataHandler read DoGetTarget;
  public
    constructor Create(const Controller: IUnknown; const Target: IDataHandler); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLfTraits,
  SilLhDataType,
  SilLtTypeInfo,
  SilLtReference,
  SilLgValueClasses;

const
  idVariant       = $0000 + Ord(dtVariant)      shl 16;
  idChar          = $0001 + Ord(dtAnsiChar)     shl 16;
  idByte          = $0002 + Ord(dtByte)         shl 16;
  idShortint      = $0003 + Ord(dtShortint)     shl 16;
  idWord          = $0004 + Ord(dtWord)         shl 16;
  idSmallint      = $0005 + Ord(dtSmallint)     shl 16;
  idString        = $0006 + Ord(dtAnsiString)   shl 16;
  idWideString    = $0007 + Ord(dtWideString)   shl 16;
  idInteger       = $0008 + Ord(dtInteger)      shl 16;
  idLargeInt      = $0009 + Ord(dtLargeInt)     shl 16;
  idLongWord      = $000A + Ord(dtLongWord)     shl 16;
  idBoolean       = $000B + Ord(dtBoolean)      shl 16;
  idDouble        = $000C + Ord(dtDouble)       shl 16;
  idSingle        = $000D + Ord(dtSingle)       shl 16;
  idExtended      = $000E + Ord(dtExtended)     shl 16;
  idCurrency      = $000F + Ord(dtCurrency)     shl 16;
  idDateTime      = $0010 + Ord(dtDate)         shl 16;
  idGuid          = $0011 + Ord(dtGuid)         shl 16;
  idPointer       = $0012 + Ord(dtPointer)      shl 16;
  idInterface     = $0013 + Ord(dtInterface)    shl 16;
                    
{ TSilValueItems }

constructor TSilValueItems.Create(const Buffer: IDataBuffer; Classes: PValueClasses; const Controller: IUnknown);
begin
  inherited Create(Controller);
  Adopt(FBuffer, Buffer);
  FClasses := Classes;
end;

destructor TSilValueItems.Destroy;
begin
  SetLength(FItems, 0);
  Adopt(FBuffer, nil);
  inherited;
end;

procedure TSilValueItems.DoGet(ID: LongWord; const IID: TGUID; Result: PUnknown);
var
  Index: Word;
  Kind: TDataType;
  Item: PPointer;
begin
  Index := ID and $FFFF;
  Kind := TDataType(ID shr 16);

  if Index >= Length(FItems) then
    SetLength(FItems, Succ(Index));

  Item := @FItems[Index];  
  if Item^ = nil then
    Extension.Add(FClasses[Kind].Create(Self, Typ.List.Get(Kind)), IID, Item);

  Ref.Result(Result, PUnknown(Item));
//  Result^ := IUnknown(Item^);
end;

function TSilValueItems.DoGetBuffer: IDataBuffer;
begin
  Result := IDataBuffer(FBuffer.Instance);
end;

{ TSilValue }

constructor TSilValue.Create(const Buffer: IDataBuffer; const Controller: IInterface);
begin
  inherited Create(Buffer, @GValueClasses, Controller);
end;

function TSilValue.GetValueVariant: IConstant;
begin
  DoGet(idVariant, IConstant, @Result);
end;

function TSilValue.GetValueChar: IValueChar;
begin
  DoGet(idChar, IValueChar, @Result);
end;

function TSilValue.GetValueByte: IValueByte;
begin
  DoGet(idByte, IValueByte, @Result);
end;

function TSilValue.GetValueShortint: IValueShortint;
begin
  DoGet(idShortint, IValueShortint, @Result);
end;

function TSilValue.GetValueWord: IValueWord;
begin
  DoGet(idWord, IValueWord, @Result);
end;

function TSilValue.GetValueSmallint: IValueSmallint;
begin
  DoGet(idSmallint, IValueSmallint, @Result);
end;

function TSilValue.GetValueString: IValueAnsiString;
begin
  DoGet(idString, IValueAnsiString, @Result);
end;

function TSilValue.GetValueWideString: IValueWideString;
begin
  DoGet(idWideString, IValueWideString, @Result);
end;

function TSilValue.GetValueInteger: IValueInteger;
begin
  DoGet(idInteger, IValueInteger, @Result);
end;

function TSilValue.GetValueLargeInt: IValueLargeInt;
begin
  DoGet(idLargeInt, IValueLargeInt, @Result);
end;

function TSilValue.GetValueLongWord: IValueLongWord;
begin
  DoGet(idLongWord, IValueLongWord, @Result);
end;

function TSilValue.GetValueBoolean: IValueBoolean;
begin
  DoGet(idBoolean, IValueBoolean, @Result);
end;

function TSilValue.GetValueDouble: IValueDouble;
begin
  DoGet(idDouble, IValueDouble, @Result);
end;

function TSilValue.GetValueSingle: IValueSingle;
begin
  DoGet(idSingle, IValueSingle, @Result);
end;

function TSilValue.GetValueExtended: IValueExtended;
begin
  DoGet(idExtended, IValueExtended, @Result);
end;

function TSilValue.GetValueCurrency: IValueCurrency;
begin
  DoGet(idCurrency, IValueCurrency, @Result);
end;

function TSilValue.GetValueDateTime: IValueDateTime;
begin
  DoGet(idDateTime, IValueDateTime, @Result);
end;

function TSilValue.GetValueGuid: IValueGuid;
begin
  DoGet(idGUID, IValueGUID, @Result);
end;

function TSilValue.GetValuePointer: IValuePointer;
begin
  DoGet(idPointer, IValuePointer, @Result);
end;

function TSilValue.GetValueInterface: IValueInterface;
begin
  DoGet(idInterface, IValueInterface, @Result);
end;

{ TSilVariable }

constructor TSilVariable.Create(const Buffer: IDataBuffer; const Controller: IInterface);
begin
  inherited Create(Buffer, @GVariableClasses, Controller);
end;

procedure TSilVariable.Clear(Param: Pointer);
var
  Buffer: IDataBuffer;
begin
  Buffer := DataBuffer;
  with Buffer.Handler.From do Clear(Buffer.Memory, Buffer.Size, Param, nil, tcSUnassigned);
end;

procedure TSilVariable.Null(Param: Pointer);
var
  Buffer: IDataBuffer;
begin
  Buffer := DataBuffer;
  with Buffer.Handler.From do Clear(Buffer.Memory, Buffer.Size, Param, nil, tcSIsNull);
end;

function TSilVariable.GetVariableVariant: IVariant;
begin
  DoGet(idVariant, IVariant, @Result);
end;

function TSilVariable.GetVariableChar: IVariableChar;
begin
  DoGet(idChar, IVariableChar, @Result);
end;

function TSilVariable.GetVariableByte: IVariableByte;
begin
  DoGet(idByte, IVariableByte, @Result);
end;

function TSilVariable.GetVariableShortint: IVariableShortint;
begin
  DoGet(idShortint, IVariableShortint, @Result);
end;

function TSilVariable.GetVariableWord: IVariableWord;
begin
  DoGet(idWord, IVariableWord, @Result);
end;

function TSilVariable.GetVariableSmallint: IVariableSmallint;
begin
  DoGet(idSmallint, IVariableSmallint, @Result);
end;

function TSilVariable.GetVariableString: IVariableAnsiString;
begin
  DoGet(idString, IVariableAnsiString, @Result);
end;

function TSilVariable.GetVariableWideString: IVariableWideString;
begin
  DoGet(idWideString, IVariableWideString, @Result);
end;

function TSilVariable.GetVariableInteger: IVariableInteger;
begin
  DoGet(idInteger, IVariableInteger, @Result);
end;

function TSilVariable.GetVariableLargeInt: IVariableLargeInt;
begin
  DoGet(idLargeInt, IVariableLargeInt, @Result);
end;

function TSilVariable.GetVariableLongWord: IVariableLongWord;
begin
  DoGet(idLongWord, IVariableLongWord, @Result);
end;

function TSilVariable.GetVariableBoolean: IVariableBoolean;
begin
  DoGet(idBoolean, IVariableBoolean, @Result);
end;

function TSilVariable.GetVariableDouble: IVariableDouble;
begin
  DoGet(idDouble, IVariableDouble, @Result);
end;

function TSilVariable.GetVariableSingle: IVariableSingle;
begin
  DoGet(idSingle, IVariableSingle, @Result);
end;

function TSilVariable.GetVariableExtended: IVariableExtended;
begin
  DoGet(idExtended, IVariableExtended, @Result);
end;

function TSilVariable.GetVariableCurrency: IVariableCurrency;
begin
  DoGet(idCurrency, IVariableCurrency, @Result);
end;

function TSilVariable.GetVariableDateTime: IVariableDateTime;
begin
  DoGet(idDateTime, IVariableDateTime, @Result);
end;

function TSilVariable.GetVariableGuid: IVariableGuid;
begin
  DoGet(idGUID, IVariableGUID, @Result);
end;

function TSilVariable.GetVariablePointer: IVariablePointer;
begin
  DoGet(idPointer, IVariablePointer, @Result);
end;

function TSilVariable.GetVariableInterface: IVariableInterface;
begin
  DoGet(idInterface, IVariableInterface, @Result);
end;

{ TSilValueItem }

constructor TSilValueItem.Create(const Controller: IInterface; const Target: IDataHandler);
begin
  inherited Create(Controller);
  FBuffer := Pointer(Controller as IDataBuffer);
  FOrigin := Pointer(IDataBuffer(FBuffer).Handler);
  FTarget := Pointer(Target);
end;

destructor TSilValueItem.Destroy;
begin
  FTarget := nil;
  FOrigin := nil;
  FBuffer := nil;
  inherited;
end;

function TSilValueItem.DoCreateError(Error: TDataTypecastStatus): Exception;
begin
  Result := ESilConversionFailed.Create(Error, Origin.DataType.Kind.Value, Target.DataType.Kind.Value, DoGetAsString());
end;

function TSilValueItem.DoGetAsString: string;
begin
  try
    Origin.Cast.AnsiString(Buffer.Memory, @Result, Buffer.Size, SizeOf(Result));
  except
    Result := '';
  end;
end;

function TSilValueItem.DoGetBuffer: IDataBuffer;
begin
  Result := IDataBuffer(FBuffer);
end;

function TSilValueItem.DoGetOrigin: IDataHandler;
begin
  Result := IDataHandler(FOrigin);
end;

function TSilValueItem.DoGetTarget: IDataHandler;
begin
  Result := IDataHandler(FTarget);
end;

end.
