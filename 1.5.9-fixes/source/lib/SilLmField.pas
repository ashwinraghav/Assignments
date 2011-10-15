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

unit SilLmField;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDataType,
  SilLiValue,
  SilLiValueChar,
  SilLiValueAnsiString,
  SilLiValueBoolean,
  SilLiValueByte,
  SilLiValueCurrency,
  SilLiValueDateTime,
  SilLiValueDouble,
  SilLiValueInteger,
  SilLiValueInterface,
  SilLiValueLargeInt,
  SilLiValueLongWord,
  SilLiValueWideString,
  SilLiValueGuid,
  SilLiValueVariant,
  SilLkObject;

type
  TSilField = class (
    // extends
    TSilObject,
    // implements
    IConstant,
    IVariant,
    IDataType1,
    IDataTypeDef1,
    IValue,
    IVariable,
    IValueChar,
    IValueByte,
    IValueAnsiString,
    IValueWideString,
    IValueInteger,
    IValueLargeInt,
    IValueLongWord,
    IValueBoolean,
    IValueDouble,
    IValueCurrency,
    IValueDateTime,
    IValueGuid,
    IValueInterface,
    IVariableChar,
    IVariableByte,
    IVariableAnsiString,
    IVariableWideString,
    IVariableInteger,
    IVariableLargeInt,
    IVariableLongWord,
    IVariableBoolean,
    IVariableDouble,
    IVariableCurrency,
    IVariableDateTime,
    IVariableGuid,
    IVariableInterface)
  private
    FDataType: TDataFieldType;
    FSize: LongWord;
  protected
    procedure DoSetDefaultSize;
    procedure DoCheckDataType(const DataType: TDataFieldType);
    property DataType: TDataFieldType read FDataType;
    property Size: LongWord read FSize;
  protected
    procedure DoWrite(const Buffer; Size: LongWord); virtual; abstract;
    procedure DoRead(var Buffer; Size: LongWord); virtual; abstract;
    function DoIsAssigned: Boolean; virtual; abstract;
  protected // IConstant
    function GetValue: Variant;
    function Read(const Default: Variant): Variant;
    function Get(out Value: Variant): Boolean;
  protected // IVariant
    procedure SetValue(const Value: Variant);
    procedure IVariant.Write = SetValue;
  protected // IDataType
    function GetDataType: TDataFieldType;
    function GetSize: LongWord;
  protected // IDataTypeDef
    procedure SetDataType(Value: TDataFieldType);
    procedure SetSize(Value: LongWord);
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
  protected // IValueChar
    function IValueChar.GetValue = ValueCharGetValue;
    function IValueChar.Read = ValueCharRead;
    function IValueChar.Get = ValueCharGet;
    function ValueCharGetValue: Char;
    function ValueCharRead(const Default: Char): Char;
    function ValueCharGet(out Value: Char): Boolean;
  protected // IValueByte
    function IValueByte.GetValue = ValueByteGetValue;
    function IValueByte.Read = ValueByteRead;
    function IValueByte.Get = ValueByteGet;
    function ValueByteGetValue: Byte;
    function ValueByteRead(const Default: Byte): Byte;
    function ValueByteGet(out Value: Byte): Boolean;
  protected // IValueAnsiString
    function IValueAnsiString.GetValue = ValueAnsiStringGetValue;
    function IValueAnsiString.Read = ValueAnsiStringRead;
    function IValueAnsiString.Get = ValueAnsiStringGet;
    function ValueAnsiStringGetValue: AnsiString;
    function ValueAnsiStringRead(const Default: AnsiString): AnsiString;
    function ValueAnsiStringGet(out Value: AnsiString): Boolean;
  protected // IValueWideString
    function IValueWideString.GetValue = ValueWideStringGetValue;
    function IValueWideString.Read = ValueWideStringRead;
    function IValueWideString.Get = ValueWideStringGet;
    function ValueWideStringGetValue: WideString;
    function ValueWideStringRead(const Default: WideString): WideString;
    function ValueWideStringGet(out Value: WideString): Boolean;
  protected // IValueInteger
    function IValueInteger.GetValue = ValueIntegerGetValue;
    function IValueInteger.Read = ValueIntegerRead;
    function IValueInteger.Get = ValueIntegerGet;
    function ValueIntegerGetValue: Integer;
    function ValueIntegerRead(const Default: Integer): Integer;
    function ValueIntegerGet(out Value: Integer): Boolean;
  protected // IValueLargeInt
    function IValueLargeInt.GetValue = ValueLargeIntGetValue;
    function IValueLargeInt.Read = ValueLargeIntRead;
    function IValueLargeInt.Get = ValueLargeIntGet;
    function ValueLargeIntGetValue: LargeInt;
    function ValueLargeIntRead(const Default: LargeInt): LargeInt;
    function ValueLargeIntGet(out Value: LargeInt): Boolean;
  protected // IValueLongWord
    function IValueLongWord.GetValue = ValueLongWordGetValue;
    function IValueLongWord.Read = ValueLongWordRead;
    function IValueLongWord.Get = ValueLongWordGet;
    function ValueLongWordGetValue: LongWord;
    function ValueLongWordRead(const Default: LongWord): LongWord;
    function ValueLongWordGet(out Value: LongWord): Boolean;
  protected // IValueBoolean
    function IValueBoolean.GetValue = ValueBooleanGetValue;
    function IValueBoolean.Read = ValueBooleanRead;
    function IValueBoolean.Get = ValueBooleanGet;
    function ValueBooleanGetValue: Boolean;
    function ValueBooleanRead(const Default: Boolean): Boolean;
    function ValueBooleanGet(out Value: Boolean): Boolean;
  protected // IValueDouble
    function IValueDouble.GetValue = ValueDoubleGetValue;
    function IValueDouble.Read = ValueDoubleRead;
    function IValueDouble.Get = ValueDoubleGet;
    function ValueDoubleGetValue: Double;
    function ValueDoubleRead(const Default: Double): Double;
    function ValueDoubleGet(out Value: Double): Boolean;
  protected // IValueCurrency
    function IValueCurrency.GetValue = ValueCurrencyGetValue;
    function IValueCurrency.Read = ValueCurrencyRead;
    function IValueCurrency.Get = ValueCurrencyGet;
    function ValueCurrencyGetValue: Currency;
    function ValueCurrencyRead(const Default: Currency): Currency;
    function ValueCurrencyGet(out Value: Currency): Boolean;
  protected // IValueDateTime
    function IValueDateTime.GetValue = ValueDateTimeGetValue;
    function IValueDateTime.Read = ValueDateTimeRead;
    function IValueDateTime.Get = ValueDateTimeGet;
    function ValueDateTimeGetValue: TDateTime;
    function ValueDateTimeRead(const Default: TDateTime): TDateTime;
    function ValueDateTimeGet(out Value: TDateTime): Boolean;
  protected // IValueGuid
    function IValueGuid.GetValue = ValueGuidGetValue;
    function IValueGuid.Read = ValueGuidRead;
    function IValueGuid.Get = ValueGuidGet;
    function ValueGuidGetValue: TGuid;
    function ValueGuidRead(const Default: TGuid): TGuid;
    function ValueGuidGet(out Value: TGuid): Boolean;
  protected // IValueInterface
    function IValueInterface.GetValue = ValueInterfaceGetValue;
    function IValueInterface.Read = ValueInterfaceRead;
    function IValueInterface.Get = ValueInterfaceGet;
    function ValueInterfaceGetValue: IInterface;
    function ValueInterfaceRead(const Default: IInterface): IInterface;
    function ValueInterfaceGet(out Value: IInterface): Boolean;
  protected // IVariableChar
    function IVariableChar.GetValue = ValueCharGetValue;
    function IVariableChar.Read = ValueCharRead;
    function IVariableChar.Get = ValueCharGet;
    procedure IVariableChar.SetValue = VariableCharSetValue;
    procedure IVariableChar.Write = VariableCharSetValue;
    procedure VariableCharSetValue(const Value: Char);
  protected // IVariableByte
    function IVariableByte.GetValue = ValueByteGetValue;
    function IVariableByte.Read = ValueByteRead;
    function IVariableByte.Get = ValueByteGet;
    procedure IVariableByte.SetValue = VariableByteSetValue;
    procedure IVariableByte.Write = VariableByteSetValue;
    procedure VariableByteSetValue(const Value: Byte);
  protected // IVariableAnsiString
    function IVariableAnsiString.GetValue = ValueAnsiStringGetValue;
    function IVariableAnsiString.Read = ValueAnsiStringRead;
    function IVariableAnsiString.Get = ValueAnsiStringGet;
    procedure IVariableAnsiString.SetValue = VariableAnsiStringSetValue;
    procedure IVariableAnsiString.Write = VariableAnsiStringSetValue;
    procedure VariableAnsiStringSetValue(const Value: AnsiString);
  protected // IVariableWideString
    function IVariableWideString.GetValue = ValueWideStringGetValue;
    function IVariableWideString.Read = ValueWideStringRead;
    function IVariableWideString.Get = ValueWideStringGet;
    procedure IVariableWideString.SetValue = VariableWideStringSetValue;
    procedure IVariableWideString.Write = VariableWideStringSetValue;
    procedure VariableWideStringSetValue(const Value: WideString);
  protected // IVariableInteger
    function IVariableInteger.GetValue = ValueIntegerGetValue;
    function IVariableInteger.Read = ValueIntegerRead;
    function IVariableInteger.Get = ValueIntegerGet;
    procedure IVariableInteger.SetValue = VariableIntegerSetValue;
    procedure IVariableInteger.Write = VariableIntegerSetValue;
    procedure VariableIntegerSetValue(const Value: Integer);
  protected // IVariableLargeInt
    function IVariableLargeInt.GetValue = ValueLargeIntGetValue;
    function IVariableLargeInt.Read = ValueLargeIntRead;
    function IVariableLargeInt.Get = ValueLargeIntGet;
    procedure IVariableLargeInt.SetValue = VariableLargeIntSetValue;
    procedure IVariableLargeInt.Write = VariableLargeIntSetValue;
    procedure VariableLargeIntSetValue(const Value: LargeInt);
  protected // IVariableLongWord
    function IVariableLongWord.GetValue = ValueLongWordGetValue;
    function IVariableLongWord.Read = ValueLongWordRead;
    function IVariableLongWord.Get = ValueLongWordGet;
    procedure IVariableLongWord.SetValue = VariableLongWordSetValue;
    procedure IVariableLongWord.Write = VariableLongWordSetValue;
    procedure VariableLongWordSetValue(const Value: LongWord);
  protected // IVariableBoolean
    function IVariableBoolean.GetValue = ValueBooleanGetValue;
    function IVariableBoolean.Read = ValueBooleanRead;
    function IVariableBoolean.Get = ValueBooleanGet;
    procedure IVariableBoolean.SetValue = VariableBooleanSetValue;
    procedure IVariableBoolean.Write = VariableBooleanSetValue;
    procedure VariableBooleanSetValue(const Value: Boolean);
  protected // IVariableDouble
    function IVariableDouble.GetValue = ValueDoubleGetValue;
    function IVariableDouble.Read = ValueDoubleRead;
    function IVariableDouble.Get = ValueDoubleGet;
    procedure IVariableDouble.SetValue = VariableDoubleSetValue;
    procedure IVariableDouble.Write = VariableDoubleSetValue;
    procedure VariableDoubleSetValue(const Value: Double);
  protected // IVariableCurrency
    function IVariableCurrency.GetValue = ValueCurrencyGetValue;
    function IVariableCurrency.Read = ValueCurrencyRead;
    function IVariableCurrency.Get = ValueCurrencyGet;
    procedure IVariableCurrency.SetValue = VariableCurrencySetValue;
    procedure IVariableCurrency.Write = VariableCurrencySetValue;
    procedure VariableCurrencySetValue(const Value: Currency);
  protected // IVariableDateTime
    function IVariableDateTime.GetValue = ValueDateTimeGetValue;
    function IVariableDateTime.Read = ValueDateTimeRead;
    function IVariableDateTime.Get = ValueDateTimeGet;
    procedure IVariableDateTime.SetValue = VariableDateTimeSetValue;
    procedure IVariableDateTime.Write = VariableDateTimeSetValue;
    procedure VariableDateTimeSetValue(const Value: TDateTime);
  protected // IVariableGuid
    function IVariableGuid.GetValue = ValueGuidGetValue;
    function IVariableGuid.Read = ValueGuidRead;
    function IVariableGuid.Get = ValueGuidGet;
    procedure IVariableGuid.SetValue = VariableGuidSetValue;
    procedure IVariableGuid.Write = VariableGuidSetValue;
    procedure VariableGuidSetValue(const Value: TGuid);
  protected // IVariableInterface
    function IVariableInterface.GetValue = ValueInterfaceGetValue;
    function IVariableInterface.Read = ValueInterfaceRead;
    function IVariableInterface.Get = ValueInterfaceGet;
    procedure IVariableInterface.SetValue = VariableInterfaceSetValue;
    procedure IVariableInterface.Write = VariableInterfaceSetValue;
    procedure VariableInterfaceSetValue(const Value: IInterface);
  public
    constructor Create(DataType: TDataFieldType = ftUnknown; Size: LongWord = 0); overload;
    constructor Create(DataType: TDataFieldType; Size: LongWord; const Value); overload;
  end;

  TSilFieldValue = class (
    // extends
    TSilField)
  private
    FValue: String;
  protected
    procedure DoWrite(const Buffer; Size: LongWord = 0); override;
    procedure DoRead(var Buffer; Size: LongWord = 0); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Value: Variant);
  end;

  TSilFieldDynamic = class (
    // extends
    TSilField)
  private
    FField: Pointer;
    FStorage: IValueStorage;
  protected
    procedure DoWrite(const Buffer; Size: LongWord = 0); override;
    procedure DoRead(var Buffer; Size: LongWord = 0); override;
    function DoIsAssigned: Boolean; override;
  public
    constructor Create(const Storage: IValueStorage; DataType: TDataFieldType = ftUnknown; Size: LongWord = 0);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBtVart,
  SilOsTool,
  SilBtInt,
  SilBtError,
  SilBtLarge,
  SilBtFloat,
  SilBtDateTime;

{ TSilField }

constructor TSilField.Create(DataType: TDataFieldType; Size: LongWord);
begin
  inherited Create;
  FDataType := DataType;

  if Size = 0 then
    DoSetDefaultSize else
    FSize := Size;
end;

constructor TSilField.Create(DataType: TDataFieldType; Size: LongWord; const Value);
begin
  Create(DataType, Size);
  DoWrite(Value, FSize);
end;

function TSilField.GetValue: Variant;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := ValueCharGetValue;
    ftString,
    ftMemo:       Result := ValueAnsiStringGetValue;
    ftWideString: Result := ValueWideStringGetValue;
    ftSmallInt,
    ftInteger:    Result := ValueIntegerGetValue;
    ftLargeInt:   Result := Integer(ValueLargeIntGetValue);
    ftWord,
    ftLongWord:   Result := Integer(ValueLongWordGetValue);
    ftBoolean:    Result := ValueBooleanGetValue;
    ftFloat:      Result := ValueDoubleGetValue;
    ftDateTime:   Result := ValueDateTimeGetValue;
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TSilField.GetDataType: TDataFieldType;
begin
  Result := FDataType;
end;

function TSilField.GetSize: LongWord;
begin
  Result := FSize;
end;

procedure TSilField.SetSize(Value: LongWord);
begin
  FSize := Value;
end;

procedure TSilField.SetValue(const Value: Variant);
begin
  if FDataType = ftUnknown then
    DoCheckDataType(Vart.VTypeToDataType(Vart.VType(Value)));

  case FDataType of
    ftChar:       VariableAnsiStringSetValue(#0);
    ftByte:       VariableByteSetValue(Value);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Value);
    ftWideString: VariableWideStringSetValue(Value);
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Value);
    ftLargeInt:   VariableLargeIntSetValue(Integer(Value));
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Value);
    ftBoolean:    VariableBooleanSetValue(Value);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Value);
    //ftGuid:
  end;
end;

procedure TSilField.SetDataType(Value: TDataFieldType);
begin
  FDataType := Value;
end;

function TSilField.Get(out Value: Variant): Boolean;
begin
  Result := not DoIsAssigned;
  if Result then Value := GetValue;
end;

function TSilField.Read(const Default: Variant): Variant;
begin
  if DoIsAssigned then
    Result := GetValue else
    Result := Default;
end;

function TSilField.GetValueBoolean: IValueBoolean;
begin
  Result := Self;
end;

function TSilField.GetValueByte: IValueByte;
begin
  Result := Self;
end;

function TSilField.GetValueChar: IValueChar;
begin
  Result := Self;
end;

function TSilField.GetValueCurrency: IValueCurrency;
begin
  Result := Self;
end;

function TSilField.GetValueDateTime: IValueDateTime;
begin
  Result := Self;
end;

function TSilField.GetValueDouble: IValueDouble;
begin
  Result := Self;
end;

function TSilField.GetValueGuid: IValueGuid;
begin
  Result := Self;
end;

function TSilField.GetValueInteger: IValueInteger;
begin
  Result := Self;
end;

function TSilField.GetValueInterface: IValueInterface;
begin
  Result := Self;
end;

function TSilField.GetValueLargeInt: IValueLargeInt;
begin
  Result := Self;
end;

function TSilField.GetValueLongWord: IValueLongWord;
begin
  Result := Self;
end;

function TSilField.GetValueString: IValueAnsiString;
begin
  Result := Self;
end;

function TSilField.GetValueVariant: IConstant;
begin
  Result := Self;
end;

function TSilField.GetValueWideString: IValueWideString;
begin
  Result := Self;
end;

function TSilField.GetVariableBoolean: IVariableBoolean;
begin
  Result := Self;
end;

function TSilField.GetVariableByte: IVariableByte;
begin
  Result := Self;
end;

function TSilField.GetVariableChar: IVariableChar;
begin
  Result := Self;
end;

function TSilField.GetVariableCurrency: IVariableCurrency;
begin
  Result := Self;
end;

function TSilField.GetVariableDateTime: IVariableDateTime;
begin
  Result := Self;
end;

function TSilField.GetVariableDouble: IVariableDouble;
begin
  Result := Self;
end;

function TSilField.GetVariableGuid: IVariableGuid;
begin
  Result := Self;
end;

function TSilField.GetVariableInteger: IVariableInteger;
begin
  Result := Self;
end;

function TSilField.GetVariableInterface: IVariableInterface;
begin
  Result := Self;
end;

function TSilField.GetVariableLargeInt: IVariableLargeInt;
begin
  Result := Self;
end;

function TSilField.GetVariableLongWord: IVariableLongWord;
begin
  Result := Self;
end;

function TSilField.GetVariableString: IVariableAnsiString;
begin
  Result := Self;
end;

function TSilField.GetVariableVariant: IVariant;
begin
  Result := Self;
end;

function TSilField.GetVariableWideString: IVariableWideString;
begin
  Result := Self;
end;

function TSilField.ValueAnsiStringGet(out Value: AnsiString): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueAnsiStringGetValue;
end;

function TSilField.ValueAnsiStringGetValue: AnsiString;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := ValueCharGetValue;
    ftString,
    ftMemo:
    begin
      SetLength(Result, FSize);
      DoRead(Result[1], Length(Result));
      Result := PChar(Result);
    end;
    ftWideString: Result := ValueWideStringGetValue;
    ftSmallInt,
    ftInteger:    Result := Int.ToStr(ValueIntegerGetValue);
    ftLargeInt:   Result := Large.ToStr(ValueLargeIntGetValue);
    ftWord,
    ftLongWord:   Result := Int.ToStr(ValueLongWordGetValue);
    ftBoolean:    Result := Str.IIf(ValueBooleanGetValue, 'true', 'false');
    ftFloat:      Result := Float.ToStr(ValueDoubleGetValue);
    ftDateTime:   Result := DateTime.ToStr(ValueDateTimeGetValue);
    ftGuid:       Result := OsGuid.ToStr(ValueGuidGetValue);
    else          Result := '';
  end;
end;

function TSilField.ValueAnsiStringRead(const Default: AnsiString): AnsiString;
begin
  if not DoIsAssigned then
  begin
    VariableAnsiStringSetValue(Default);
    Result := Default;
  end else
    Result := ValueAnsiStringGetValue;
end;

function TSilField.ValueBooleanGet(out Value: Boolean): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueBooleanGetValue;
end;

function TSilField.ValueBooleanGetValue: Boolean;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := ValueCharGetValue <> #0;
    ftString,
    ftMemo:       Result := Str.ToLower(Str.ToChr(ValueAnsiStringGetValue, 'f')) = 't';
    ftWideString: Result := Str.ToLower(Str.ToChr(ValueWideStringGetValue, 'f')) = 't';
    ftSmallInt,
    ftInteger:    Result := ValueIntegerGetValue > 0;
    ftLargeInt:   Result := ValueLargeIntGetValue > 0;
    ftWord,
    ftLongWord:   Result := ValueLongWordGetValue > 0;
    ftBoolean:    DoRead(Result, FSize);
    ftFloat,
    ftDateTime:   Result := ValueDoubleGetValue > 0;
    ftGuid:       Result := false;
    else          Result := false;
  end;
end;

function TSilField.ValueBooleanRead(const Default: Boolean): Boolean;
begin
  if not DoIsAssigned then
  begin
    VariableBooleanSetValue(Default);
    Result := Default;
  end else
    Result := ValueBooleanGetValue;
end;

function TSilField.ValueByteGet(out Value: Byte): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueByteGetValue;
end;

function TSilField.ValueByteGetValue: Byte;
begin
  Result := Byte(Str.ToChr(ValueAnsiStringGetValue, #0));
end;

function TSilField.ValueByteRead(const Default: Byte): Byte;
begin
  if not DoIsAssigned then
  begin
    VariableByteSetValue(Default);
    Result := Default;
  end else
    Result := ValueByteGetValue;
end;

function TSilField.ValueCharGet(out Value: Char): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueCharGetValue;
end;

function TSilField.ValueCharGetValue: Char;
begin
  Result := Str.ToChr(ValueAnsiStringGetValue, #0);
end;

function TSilField.ValueCharRead(const Default: Char): Char;
begin
  if not DoIsAssigned then
  begin
    VariableCharSetValue(Default);
    Result := Default;
  end else
    Result := ValueCharGetValue;
end;

function TSilField.ValueCurrencyGet(out Value: Currency): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueCurrencyGetValue;
end;

function TSilField.ValueCurrencyGetValue: Currency;
begin
  Result := ValueDoubleGetValue;
end;

function TSilField.ValueCurrencyRead(const Default: Currency): Currency;
begin
  if not DoIsAssigned then
  begin
    VariableCurrencySetValue(Default);
    Result := Default;
  end else
    Result := ValueCurrencyGetValue;
end;

function TSilField.ValueDateTimeGet(out Value: TDateTime): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueDateTimeGetValue;
end;

function TSilField.ValueDateTimeGetValue: TDateTime;
begin
  Result := ValueDoubleGetValue;
end;

function TSilField.ValueDateTimeRead(const Default: TDateTime): TDateTime;
begin
  if not DoIsAssigned then
  begin
    VariableDateTimeSetValue(Default);
    Result := Default;
  end else
    Result := ValueDateTimeGetValue;
end;

function TSilField.ValueDoubleGet(out Value: Double): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueDoubleGetValue;
end;

function TSilField.ValueDoubleGetValue: Double;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(ValueCharGetValue);
    ftString,
    ftMemo:       Result := Str.ToFloat(ValueAnsiStringGetValue);
    ftWideString: Result := Str.ToFloat(ValueWideStringGetValue);
    ftSmallInt,
    ftInteger:    Result := ValueIntegerGetValue;
    ftLargeInt:   Result := ValueLargeIntGetValue;
    ftWord,
    ftLongWord:   Result := ValueLongWordGetValue;
    ftBoolean:    Result := Ord(ValueBooleanGetValue);
    ftFloat,
    ftDateTime:   DoRead(Result, FSize);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TSilField.ValueDoubleRead(const Default: Double): Double;
begin
  if not DoIsAssigned then
  begin
    VariableDoubleSetValue(Default);
    Result := Default;
  end else
    Result := ValueDoubleGetValue;
end;

function TSilField.ValueGuidGet(out Value: TGuid): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueGuidGetValue;
end;

function TSilField.ValueGuidGetValue: TGuid;

  procedure DoFill(var Buf: TGUID);
  begin
    FillChar(Buf, SizeOf(TGUID), 0);
  end;

begin
  case FDataType of
    ftChar,
    ftByte:       DoFill(Result);
    ftString,
    ftMemo:       Result := OsGuid.FromStr(ValueAnsiStringGetValue);
    ftWideString: Result := OsGuid.FromStr(ValueWideStringGetValue);
    ftSmallInt,
    ftInteger,
    ftLargeInt,
    ftWord,
    ftLongWord,
    ftBoolean,
    ftFloat,
    ftDateTime:   DoFill(Result);
    ftGuid:       DoRead(Result, FSize);
    else          DoFill(Result);
  end;
end;

function TSilField.ValueGuidRead(const Default: TGuid): TGuid;
begin
  if not DoIsAssigned then
  begin
    VariableGuidSetValue(Default);
    Result := Default;
  end else
    Result := ValueGuidGetValue;
end;

function TSilField.ValueIntegerGet(out Value: Integer): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueIntegerGetValue;
end;

function TSilField.ValueIntegerGetValue: Integer;
var
  Data: record
    case Integer of
      0: (B: Byte);
      1: (I: Smallint);
      2: (L: Integer);
  end;
begin
  case FDataType of
    ftChar:       Result := Byte(ValueCharGetValue);
    ftString,
    ftMemo:       Result := Str.ToInt(ValueAnsiStringGetValue, 0);
    ftWideString: Result := Str.ToInt(ValueWideStringGetValue, 0);
    ftByte:
    begin
      DoRead(Data, FSize);
      Result := Data.B;
    end;
    ftSmallInt:
    begin
      DoRead(Data, FSize);
      Result := Data.I;
    end;
    ftInteger:
    begin
      DoRead(Data, FSize);
      Result := Data.L;
    end;
    ftLargeInt:   Result := ValueLargeIntGetValue;
    ftWord,
    ftLongWord:   Result := ValueLongWordGetValue;
    ftBoolean:    Result := Ord(ValueBooleanGetValue);
    ftFloat,
    ftDateTime:   Result := Trunc(ValueDoubleGetValue);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TSilField.ValueIntegerRead(const Default: Integer): Integer;
begin
  if not DoIsAssigned then
  begin
    VariableIntegerSetValue(Default);
    Result := Default;
  end else
    Result := ValueIntegerGetValue;
end;

function TSilField.ValueInterfaceGet(out Value: IInterface): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueInterfaceGetValue;
end;

function TSilField.ValueInterfaceGetValue: IInterface;
var
  P: Pointer;
begin
  case FDataType of
    ftInterface:
    begin
      DoRead(P, FSize);
      Result := IUnknown(P);
    end;
    else
      Result := nil;
  end;
end;

function TSilField.ValueInterfaceRead(const Default: IInterface): IInterface;
begin
  if not DoIsAssigned then
  begin
    VariableInterfaceSetValue(Default);
    Result := Default;
  end else
    Result := ValueInterfaceGetValue;
end;

function TSilField.ValueLargeIntGet(out Value: LargeInt): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueLargeIntGetValue;
end;

function TSilField.ValueLargeIntGetValue: LargeInt;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(ValueCharGetValue);
    ftString,
    ftMemo:       Result := Str.ToInt(ValueAnsiStringGetValue, 0);
    ftWideString: Result := Str.ToInt(ValueWideStringGetValue, 0);
    ftSmallInt,
    ftInteger:    Result := ValueIntegerGetValue;
    ftLargeInt:   DoRead(Result, FSize);
    ftWord,
    ftLongWord:   Result := ValueLongWordGetValue;
    ftBoolean:    Result := Ord(ValueBooleanGetValue);
    ftFloat,
    ftDateTime:   Result := Trunc(ValueDoubleGetValue);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TSilField.ValueLargeIntRead(const Default: LargeInt): LargeInt;
begin
  if not DoIsAssigned then
  begin
    VariableLargeIntSetValue(Default);
    Result := Default;
  end else
    Result := ValueLargeIntGetValue;
end;

function TSilField.ValueLongWordGet(out Value: LongWord): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueLongWordGetValue;
end;

function TSilField.ValueLongWordGetValue: LongWord;
var
  Data: record
    case Integer of
      0: (I: Word);
      1: (L: LongWord);
  end;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(ValueCharGetValue);
    ftString,
    ftMemo:       Result := Str.ToInt(ValueAnsiStringGetValue, 0);
    ftWideString: Result := Str.ToInt(ValueWideStringGetValue, 0);
    ftSmallInt,
    ftInteger:    Result := ValueIntegerGetValue;
    ftLargeInt:   Result := ValueLargeIntGetValue;
    ftWord:
    begin
      DoRead(Data, FSize);
      Result := Data.I;
    end;
    ftLongWord:
    begin
      DoRead(Data, FSize);
      Result := Data.L;
    end;
    ftBoolean:    Result := Ord(ValueBooleanGetValue);
    ftFloat,
    ftDateTime:   Result := Trunc(ValueDoubleGetValue);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TSilField.ValueLongWordRead(const Default: LongWord): LongWord;
begin
  if not DoIsAssigned then
  begin
    VariableLongWordSetValue(Default);
    Result := Default;
  end else
    Result := ValueLongWordGetValue;
end;

function TSilField.ValueWideStringGet(out Value: WideString): Boolean;
begin
  Result := DoIsAssigned;
  if Result then Value := ValueWideStringGetValue;
end;

function TSilField.ValueWideStringGetValue: WideString;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := ValueCharGetValue;
    ftString,
    ftMemo:       Result := ValueAnsiStringGetValue;
    ftWideString:
    begin
      SetLength(Result, FSize);
      DoRead(Result[1], Length(Result));
    end;
    ftSmallInt,
    ftInteger:    Result := Int.ToStr(ValueIntegerGetValue);
    ftLargeInt:   Result := Large.ToStr(ValueLargeIntGetValue);
    ftWord,
    ftLongWord:   Result := Int.ToStr(ValueLongWordGetValue);
    ftBoolean:    Result := Str.IIf(ValueBooleanGetValue, 'true', 'false');
    ftFloat:      Result := Float.ToStr(ValueDoubleGetValue);
    ftDateTime:   Result := Float.ToStr(ValueDateTimeGetValue);
    ftGuid:       Result := OsGuid.ToStr(ValueGuidGetValue);
    else          Result := '';
  end;
end;

function TSilField.ValueWideStringRead(const Default: WideString): WideString;
begin
  if not DoIsAssigned then
  begin
    VariableWideStringSetValue(Default);
    Result := Default;
  end else
    Result := ValueWideStringGetValue;
end;

procedure TSilField.DoCheckDataType(const DataType: TDataFieldType);
begin
  if FDataType = ftUnknown then
  begin
    FDataType := DataType;
    DoSetDefaultSize;
  end;
end;

procedure TSilField.DoSetDefaultSize;
begin
  case FDataType of
    ftByte,
    ftChar:       FSize := SizeOf(Char);
    ftSmallInt:   FSize := SizeOf(SmallInt);
    ftInteger:    FSize := SizeOf(Integer);
    ftLargeInt:   FSize := SizeOf(LargeInt);
    ftWord:       FSize := SizeOf(Word);
    ftLongWord:   FSize := SizeOf(LongWord);
    ftBoolean:    FSize := SizeOf(Boolean);
    ftFloat:      FSize := SizeOf(Double);
    ftCurrency:   FSize := SizeOf(Currency);
    ftDate,
    ftTime,
    ftDateTime:   FSize := SizeOf(TDateTime);
    ftGuid:       FSize := SizeOf(TGuid);
    ftInterface:  FSize := SizeOf(Pointer);
    else          FSize := 0;
  end
end;

procedure TSilField.VariableAnsiStringSetValue(const Value: AnsiString);
begin
  DoCheckDataType(ftString);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       DoWrite(Value[1], Length(Value));
    ftWideString: VariableWideStringSetValue(Value);
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Str.ToInt(Value, 0));
    ftLargeInt:   VariableLargeIntSetValue(Str.ToInt(Value, 0));
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Str.ToInt(Value, 0));
    ftBoolean:    VariableBooleanSetValue(Str.ToChr(Str.ToLower(Value), 'f') = 't');
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Str.ToInt(Value, 0));
    ftGuid:       VariableGuidSetValue(OsGuid.FromStr(Value));
  end;
end;

procedure TSilField.VariableBooleanSetValue(const Value: Boolean);
const
  AValues: array [Boolean] of String = ('False', 'True');
begin
  DoCheckDataType(ftBoolean);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(Char(Value));
    ftString,
    ftMemo:       VariableAnsiStringSetValue(AValues[Value]);
    ftWideString: VariableWideStringSetValue(AValues[Value]);
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Ord(Value));
    ftLargeInt:   VariableLargeIntSetValue(Ord(Value));
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Ord(Value));
    ftBoolean:    DoWrite(Value, FSize);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Ord(Value));
    //ftGuid:
  end;
end;

procedure TSilField.VariableByteSetValue(const Value: Byte);
begin
  VariableIntegerSetValue(Value);
end;

procedure TSilField.VariableCharSetValue(const Value: Char);
begin
  VariableAnsiStringSetValue(Value);
end;

procedure TSilField.VariableCurrencySetValue(const Value: Currency);
begin
  VariableDoubleSetValue(Value);
end;

procedure TSilField.VariableDateTimeSetValue(const Value: TDateTime);
begin
  VariableDoubleSetValue(Value);
end;

procedure TSilField.VariableDoubleSetValue(const Value: Double);
begin
  DoCheckDataType(ftFloat);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Float.ToStr(Value));
    ftWideString: VariableWideStringSetValue(Float.ToStr(Value));
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Trunc(Value));
    ftLargeInt:   VariableLargeIntSetValue(Trunc(Value));
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Trunc(Value));
    ftBoolean:    VariableBooleanSetValue(Value > 0);
    ftFloat,
    ftDateTime:   DoWrite(Value, FSize);
    //ftGuid:
  end;
end;

procedure TSilField.VariableGuidSetValue(const Value: TGuid);
begin
  DoCheckDataType(ftGuid);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(OsGuid.ToStr(Value));
    ftWideString: VariableWideStringSetValue(OsGuid.ToStr(Value));
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(0);
    ftLargeInt:   VariableLargeIntSetValue(0);
    ftWord,
    ftLongWord:   VariableLongWordSetValue(0);
    ftBoolean:    VariableBooleanSetValue(false);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(0);
    ftGuid:       DoWrite(Value, FSize);
  end;
end;

procedure TSilField.VariableIntegerSetValue(const Value: Integer);
begin
  DoCheckDataType(ftInteger);

  case FDataType of
    ftChar:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Int.ToStr(Value));
    ftWideString: VariableWideStringSetValue(Int.ToStr(Value));
    ftByte,
    ftSmallInt,
    ftInteger:    DoWrite(Value, FSize);
    ftLargeInt:   VariableLargeIntSetValue(Value);
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Value);
    ftBoolean:    VariableBooleanSetValue(Value > 0);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Value);
    //ftGuid:
  end;
end;

procedure TSilField.VariableInterfaceSetValue(const Value: IInterface);
var
  P: Pointer;
begin
  DoCheckDataType(ftInterface);

  case FDataType of
    ftInterface:
    begin
      DoRead(P, FSize);
      IUnknown(P) := Value;
      DoWrite(P, FSize);
    end;
  end;
end;

procedure TSilField.VariableLargeIntSetValue(const Value: LargeInt);
begin
  DoCheckDataType(ftLargeInt);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Large.ToStr(Value));
    ftWideString: VariableWideStringSetValue(Large.ToStr(Value));
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Value);
    ftLargeInt:   DoWrite(Value, FSize);
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Value);
    ftBoolean:    VariableBooleanSetValue(Value > 0);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Value);
    //ftGuid:
  end;
end;

procedure TSilField.VariableLongWordSetValue(const Value: LongWord);
begin
  DoCheckDataType(ftLongWord);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Large.ToStr(Value));
    ftWideString: VariableWideStringSetValue(Large.ToStr(Value));
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Value);
    ftLargeInt:   VariableLargeIntSetValue(Value);
    ftWord,
    ftLongWord:   DoWrite(Value, FSize);
    ftBoolean:    VariableBooleanSetValue(Value > 0);
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Value);
    //ftGuid:
  end;
end;

procedure TSilField.VariableWideStringSetValue(const Value: WideString);
begin
  DoCheckDataType(ftWideString);

  case FDataType of
    ftChar,
    ftByte:       VariableAnsiStringSetValue(#0);
    ftString,
    ftMemo:       VariableAnsiStringSetValue(Value);
    ftWideString: DoWrite(Value[1], Length(Value));
    ftSmallInt,
    ftInteger:    VariableIntegerSetValue(Str.ToInt(Value, 0));
    ftLargeInt:   VariableLargeIntSetValue(Str.ToInt(Value, 0));
    ftWord,
    ftLongWord:   VariableLongWordSetValue(Str.ToInt(Value, 0));
    ftBoolean:    VariableBooleanSetValue(Str.ToChr(Str.ToLower(Value), 'f') = 't');
    ftFloat,
    ftDateTime:   VariableDoubleSetValue(Str.ToInt(Value, 0));
    ftGuid:       VariableGuidSetValue(OsGuid.FromStr(Value));
  end;
end;

function TSilField.GetValueShortint: IValueShortint;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetValueSmallint: IValueSmallint;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetValueWord: IValueWord;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetValueExtended: IValueExtended;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetValuePointer: IValuePointer;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetValueSingle: IValueSingle;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariableExtended: IVariableExtended;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariablePointer: IVariablePointer;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariableShortint: IVariableShortint;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariableSingle: IVariableSingle;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariableSmallint: IVariableSmallint;
begin
  raise Error.Create('not implemented');
end;

function TSilField.GetVariableWord: IVariableWord;
begin
  raise Error.Create('not implemented');
end;

{ TSilFieldValue }

constructor TSilFieldValue.Create(const Value: Variant);
begin
  inherited Create;
  SetValue(Value);
end;

function TSilFieldValue.DoIsAssigned: Boolean;
begin
  Result := Length(FValue) > 0;
end;

procedure TSilFieldValue.DoRead(var Buffer; Size: LongWord);
var
  lwSize: LongWord;
begin
  lwSize := Length(FValue);
  if Size = 0 then Size := Self.Size;

  if Size > lwSize then
  begin
    FillChar((PChar(@Buffer) + lwSize)^, Size - lwSize, 0);
    Size := lwSize;
  end;

  if Size > 0 then Move(FValue[1], Buffer, Size);
end;

procedure TSilFieldValue.DoWrite(const Buffer; Size: LongWord);
var
  lwSize: LongWord;
begin
  lwSize := Length(FValue);
  if Size = 0 then Size := Self.Size;

  if Size > lwSize then
  begin
    SetSize(Size);
    lwSize := Size;
    SetLength(FValue, Size);
  end;

  if Size > 0 then Move(Buffer, FValue[1], Size);
  if lwSize > Size then FillChar(FValue[Size + 1], lwSize - Size, 0);
end;

{ TSilFieldDynamic }

constructor TSilFieldDynamic.Create(const Storage: IValueStorage; DataType: TDataFieldType; Size: LongWord);
begin
  inherited Create(DataType, Size);

  Pointer(FStorage) := Pointer(Storage);
  FStorage.Initialize(Self, FField);
end;

destructor TSilFieldDynamic.Destroy;
begin
  if Assigned(FStorage) then
    FStorage.Finalize(FField);

  Pointer(FStorage) := nil;
  FField := nil;
  inherited;
end;

function TSilFieldDynamic.DoIsAssigned: Boolean;
begin
  Result := FStorage.IsAssigned(FField);
end;

procedure TSilFieldDynamic.DoRead(var Buffer; Size: LongWord);
var
  FieldSize: LongWord;
begin
  if FStorage <> nil then
  begin
    FieldSize := Self.Size;

    if Size > FieldSize then
    begin
      FillChar((PChar(@Buffer) + FieldSize)^, Size - FieldSize, 0);
      Size := FieldSize;
    end;

    FStorage.Read(FField, Buffer, Size);
  end;
end;

procedure TSilFieldDynamic.DoWrite(const Buffer; Size: LongWord);
var
  FieldSize: LongWord;
  FieldBuffer, PBuf: PChar;
begin
  if FStorage <> nil then
  begin
    FieldSize := Self.Size;

    if Size > FieldSize then Size := FieldSize;

    if Size < FieldSize then
    begin
      GetMem(FieldBuffer, FieldSize);
      try
        Move(Buffer, FieldBuffer^, Size);
        PBuf := FieldBuffer + Size;
        FillChar(PBuf^, FieldSize - Size, 0);
        FStorage.Write(FField, FieldBuffer^, FieldSize);
      finally
        FreeMem(FieldBuffer);
      end;
    end else
      FStorage.Write(FField, Buffer, Size);
  end;
end;

end.
