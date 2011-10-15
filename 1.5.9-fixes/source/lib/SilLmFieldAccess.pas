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

unit SilLmFieldAccess;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilLkInterfaced,
  SilLiField,
  SilLiFiler,
  SilBeTypes;

type
  TFieldAccess = class (
    // extends
    TSilInterfacedObject,
    // implements
    IFieldAccess,
    IFieldDef,
    IFieldStore)
  protected
    FName: String;
    FValue: String;
    FSize: LongWord;
    FDataType: TDataFieldType;
    FChanged: Boolean;
    FPosition: LongWord;
    FStore: IFieldStore;
    FUseValue: Boolean;
  protected // IFieldDef;
    procedure Unbind;
  protected // IFieldStore
    procedure Write(const Buffer; Position, Size: LongWord); virtual;
    procedure Read(var Buffer; Position, Size: LongWord); virtual;
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; virtual;
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetAsChar: Char; virtual;
    function GetAsByte: Byte; virtual;
    function GetAsString: String; virtual;
    function GetAsWideString: WideString; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsLargeInt: LargeInt; virtual;
    function GetAsLongWord: LongWord; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsCurrency: Currency; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsVariant: Variant; virtual;
    function GetAsGuid: TGUID; virtual;
    function GetAsInterface: IUnknown; virtual;
    function GetAsPointer: Pointer; virtual;
  protected // IValueAccess
    procedure SetAsChar(Value: Char); virtual;
    procedure SetAsByte(Value: Byte); virtual;
    procedure SetAsString(const Value: String); virtual;
    procedure SetAsWideString(const Value: WideString); virtual;
    procedure SetAsInteger(Value: LongInt); virtual;
    procedure SetAsLargeInt(Value: LargeInt); virtual;
    procedure SetAsLongWord(Value: LongWord); virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsCurrency(Value: Currency); virtual;
    procedure SetAsDateTime(Value: TDateTime); virtual;
    procedure SetAsVariant(const Value: Variant); virtual;
    procedure SetAsGuid(const Value: TGUID); virtual;
    procedure SetAsInterface(const Value: IUnknown); virtual;
    procedure SetAsPointer(const Value: Pointer); virtual;
  protected // IFieldReader
    function GetName: String;
    function GetDataType: TDataFieldType;
    function GetSize: LongWord;
  protected // IFieldAccess
    procedure SetName(const Value: String); virtual;
    procedure SetSize(Value: LongWord); virtual;
    function GetPosition: LongWord;
    procedure SetPosition(Value: LongWord);
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
    function GetIsEmpty: Boolean; virtual;
  public
    constructor Create(const Name: String; const Store: IFieldStore = nil); virtual;
    destructor Destroy; override;
    class function CreateTyped(const Name: String; DataType: TDataFieldType = ftString; Size: LongWord = 0; const Store: IFieldStore = nil): TFieldAccess;
  end;

  TStringFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsString: String; override;
  protected // IValueAccess
    procedure SetAsString(const Value: String); override;
    function GetIsEmpty: Boolean; override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    constructor CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore); virtual;
  end;

  TWideStringFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsWideString: WideString; override;
  protected // IValueAccess
    procedure SetAsWideString(const Value: WideString); override;
    function GetIsEmpty: Boolean; override;
  protected // IFieldReader
    function GetSize: LongWord;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    constructor CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore); virtual;
  end;

  TIntegerFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsInteger: Integer; override;
  protected // IValueAccess
    procedure SetAsInteger(Value: Integer); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TSmallIntFieldAccess = class (
    // extends
    TIntegerFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TLargeIntFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsLargeInt: LargeInt; override;
  protected // IValueAccess
    procedure SetAsLargeInt(Value: LargeInt); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TLongWordFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsLongWord: LongWord; override;
  protected // IValueAccess
    procedure SetAsLongWord(Value: LongWord); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TWordFieldAccess = class (
    // extends
    TLongWordFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsLongWord: LongWord; override;
    procedure SetAsLongWord(Value: LongWord); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TBooleanFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsBoolean: Boolean; override;
  protected // IValueAccess
    procedure SetAsBoolean(Value: Boolean); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TFloatFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsFloat: Double; override;
  protected // IValueAccess
    procedure SetAsFloat(Value: Double); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TDateTimeFieldAccess = class (
    // extends
    TFloatFieldAccess)
  public
    constructor Create(const Name: String; const Store: IFieldStore; DataType: TDataFieldType = ftDateTime); reintroduce;
  end;

  TGuidFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsGuid: TGuid; override;
    function GetIsEmpty: Boolean; override;
  protected // IValueAccess
    procedure SetAsGuid(const Value: TGuid); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TPointerFieldAccess = class (
    // extends
    TFieldAccess)
  private
    function Get: Pointer;
    procedure Put(const Value: Pointer);
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsPointer: Pointer; override;
    function GetIsEmpty: Boolean; override;
  protected // IValueAccess
    procedure SetSize(Value: LongWord); override;
    procedure SetAsPointer(const Value: Pointer); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    destructor Destroy; override;
  end;

  TInterfaceFieldAccess = class (
    // extends
    TFieldAccess)
  private
    function Get: Pointer;
    procedure Put(const Value: Pointer);
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsInterface: IUnknown; override;
    function GetIsEmpty: Boolean; override;
  protected // IValueAccess
    procedure SetSize(Value: LongWord); override;
    procedure SetAsInterface(const Value: IUnknown); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    destructor Destroy; override;
  end;

  TVariantFieldAccess = class (
    // extends
    TFieldAccess)
  private
    procedure DoGet(out Data);
    procedure DoPut(const Data);
    function Get: Variant;
    procedure Put(const Value: Variant);
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetIsEmpty: Boolean; override;
  protected // IValueAccess
    procedure SetSize(Value: LongWord); override;
  protected // IValueReader
    function GetAsByte: Byte; override;
    function GetAsString: String; override;
    function GetAsWideString: WideString; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: LargeInt; override;
    function GetAsLongWord: LongWord; override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: Double; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsVariant: Variant; override;
    function GetAsGuid: TGUID; override;
    function GetAsInterface: IUnknown; override;
    function GetAsPointer: Pointer; override;
  protected // IValueAccess
    procedure SetAsChar(Value: Char); override;
    procedure SetAsByte(Value: Byte); override;
    procedure SetAsString(const Value: String); override;
    procedure SetAsWideString(const Value: WideString); override;
    procedure SetAsInteger(Value: LongInt); override;
    procedure SetAsLargeInt(Value: LargeInt); override;
    procedure SetAsLongWord(Value: LongWord); override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetAsGuid(const Value: TGUID); override;
    procedure SetAsInterface(const Value: IUnknown); override;
    procedure SetAsPointer(const Value: Pointer); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    destructor Destroy; override;
  end;

  TCharFieldAccess = class (
    // extends
    TFieldAccess)
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer = nil): Integer; override;
    function GetAsChar: Char; override;
  protected // IValueAccess
    procedure SetAsChar(Value: Char); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TByteFieldAccess = class (
    // extends
    TCharFieldAccess)
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
  end;

  TMemoFieldAccess = class (
    // extends
    TStringFieldAccess)
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    constructor CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore); override;
  end;

implementation

uses
  SysUtils,
  SilBtLarge,
  SilBtStr,
  SilBtInt,
  SilBtFloat,
  SilBtDateTime,
  SilBtTime,
  SilOsTool,
  SilBtMem,
  SilBtVoidPtr,
  SilBtVart,
  SilLiCompare,
  SilLtReference, SilOtWStr;

{ TFieldAccess }

function TFieldAccess.GetValue: String;
begin
  Result := FValue;
end;

procedure TFieldAccess.SetValue(const Value: String);
begin
  FValue := Value;
  if FUseValue and (Length(FValue) < Integer(FSize)) then SetLength(FValue, Integer(FSize));
end;

procedure TFieldAccess.Write(const Buffer; Position, Size: LongWord);
begin
  Move(Buffer, FValue[1], FSize);
end;

procedure TFieldAccess.Read(var Buffer; Position, Size: LongWord);
begin
  Move(FValue[1], Buffer, FSize);
end;

function TFieldAccess.Compare(const Item1; const Item2; Data: Pointer): Integer;
begin
  Result := 0;
end;

function TFieldAccess.GetAsBoolean: Boolean;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := GetAsChar <> #0;
    ftString,
    ftMemo:       Result := Str.ToLower(Str.ToChr(GetAsString, 'f')) = 't';
    ftWideString: Result := Str.ToLower(Str.ToChr(GetAsWideString, 'f')) = 't';
    ftSmallInt,
    ftInteger:    Result := GetAsInteger > 0;
    ftLargeInt:   Result := GetAsLargeInt > 0;
    ftWord,
    ftLongWord:   Result := GetAsLongWord > 0;
    ftBoolean:    Result := false;  //GetAsBoolean;
    ftFloat,
    ftDateTime:   Result := GetAsFloat > 0;
    ftGuid:       Result := false;
    else          Result := false;
  end;
end;

function TFieldAccess.GetAsByte: Byte;
begin
  Result := Byte(Str.ToChr(GetAsString, #0));
end;

function TFieldAccess.GetAsChar: Char;
begin
  Result := Str.ToChr(GetAsString, #0);
end;

function TFieldAccess.GetAsCurrency: Currency;
begin
  Result := GetAsFloat;
end;

function TFieldAccess.GetAsDateTime: TDateTime;
begin
  Result := GetAsFloat;
end;

function TFieldAccess.GetAsFloat: Double;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(GetAsChar);
    ftString,
    ftMemo:       Result := Str.ToFloat(GetAsString);
    ftWideString: Result := Str.ToFloat(GetAsWideString);
    ftSmallInt,
    ftInteger:    Result := GetAsInteger;
    ftLargeInt:   Result := GetAsLargeInt;
    ftWord,
    ftLongWord:   Result := GetAsLongWord;
    ftBoolean:    Result := Ord(GetAsBoolean);
    ftFloat,
    ftDateTime:   Result := 0;  //GetAsFloat;
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TFieldAccess.GetAsInteger: LongInt;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(GetAsChar);
    ftString,
    ftMemo:       Result := Str.ToInt(GetAsString, 0);
    ftWideString: Result := Str.ToInt(GetAsWideString, 0);
    ftSmallInt,
    ftInteger:    Result := 0;  //GetAsInteger;
    ftLargeInt:   Result := GetAsLargeInt;
    ftWord,
    ftLongWord:   Result := GetAsLongWord;
    ftBoolean:    Result := Ord(GetAsBoolean);
    ftFloat,
    ftDateTime:   Result := Trunc(GetAsFloat);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TFieldAccess.GetAsLargeInt: LargeInt;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(GetAsChar);
    ftString,
    ftMemo:       Result := Str.ToInt(GetAsString, 0);
    ftWideString: Result := Str.ToInt(GetAsWideString, 0);
    ftSmallInt,
    ftInteger:    Result := GetAsInteger;
    ftLargeInt:   Result := 0;  //GetAsLargeInt;
    ftWord,
    ftLongWord:   Result := GetAsLongWord;
    ftBoolean:    Result := Ord(GetAsBoolean);
    ftFloat,
    ftDateTime:   Result := Trunc(GetAsFloat);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TFieldAccess.GetAsLongWord: LongWord;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := Byte(GetAsChar);
    ftString,
    ftMemo:       Result := Str.ToInt(GetAsString, 0);
    ftWideString: Result := Str.ToInt(GetAsWideString, 0);
    ftSmallInt,
    ftInteger:    Result := GetAsInteger;
    ftLargeInt:   Result := GetAsLargeInt;
    ftWord,
    ftLongWord:   Result := 0; //GetAsLongWord;
    ftBoolean:    Result := Ord(GetAsBoolean);
    ftFloat,
    ftDateTime:   Result := Trunc(GetAsFloat);
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TFieldAccess.GetAsString: String;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := GetAsChar;
    ftString,
    ftMemo:       Result := ''; //GetAsString;
    ftWideString: Result := GetAsWideString;
    ftSmallInt,
    ftInteger:    Result := Int.ToStr(GetAsInteger);
    ftLargeInt:   Result := Large.ToStr(GetAsLargeInt);
    ftWord,
    ftLongWord:   Result := Int.ToStr(GetAsLongWord);
    ftBoolean:    Result := Str.IIf(GetAsBoolean, 'true', 'false');
    ftFloat:      Result := Float.ToStr(GetAsFloat);
    ftDateTime:   Result := DateTime.ToStr(GetAsDateTime);
    ftGuid:       Result := OsGuid.ToStr(GetAsGuid);
    else          Result := '';
  end;
end;

function TFieldAccess.GetAsWideString: WideString;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := GetAsChar;
    ftString,
    ftMemo:       Result := GetAsString;
    ftWideString: Result := ''; //GetAsWideString;
    ftSmallInt,
    ftInteger:    Result := Int.ToStr(GetAsInteger);
    ftLargeInt:   Result := Large.ToStr(GetAsLargeInt);
    ftWord,
    ftLongWord:   Result := Int.ToStr(GetAsLongWord);
    ftBoolean:    Result := Str.IIf(GetAsBoolean, 'true', 'false');
    ftFloat:      Result := Float.ToStr(GetAsFloat);
    ftDateTime:   Result := Float.ToStr(GetAsDateTime);
    ftGuid:       Result := OsGuid.ToStr(GetAsGuid);
    else          Result := '';
  end;
end;

function TFieldAccess.GetAsVariant: Variant;
begin
  case FDataType of
    ftChar,
    ftByte:       Result := GetAsChar;
    ftString,
    ftMemo:       Result := GetAsString;
    ftWideString: Result := GetAsWideString;
    ftSmallInt,
    ftInteger:    Result := GetAsInteger;
    ftLargeInt:   Result := Integer(GetAsLargeInt);
    ftWord,
    ftLongWord:   Result := Integer(GetAsLongWord);
    ftBoolean:    Result := GetAsBoolean;
    ftFloat:      Result := GetAsFloat;
    ftDateTime:   Result := GetAsDateTime;
    ftGuid:       Result := 0;
    else          Result := 0;
  end;
end;

function TFieldAccess.GetAsGuid: TGUID;

  procedure DoFill(var Buf: TGUID);
  begin
    FillChar(Buf, SizeOf(TGUID), 0);
  end;

begin
  case FDataType of
    ftChar,
    ftByte:       DoFill(Result);
    ftString,
    ftMemo:       Result := OsGuid.FromStr(GetAsString);
    ftWideString: Result := OsGuid.FromStr(GetAsWideString);
    ftSmallInt,
    ftInteger,
    ftLargeInt,
    ftWord,
    ftLongWord,
    ftBoolean,
    ftFloat,
    ftDateTime:   DoFill(Result);
    ftGuid:       DoFill(Result); //Result := GetAsGuid;
    else          DoFill(Result);
  end;
end;

function TFieldAccess.GetAsInterface: IUnknown;
begin
  Result := nil;
end;

function TFieldAccess.GetDataType: TDataFieldType;
begin
  Result := FDataType;
end;

function TFieldAccess.GetName: String;
begin
  Result := FName;
end;

function TFieldAccess.GetSize: LongWord;
begin
  Result := FSize;
end;

procedure TFieldAccess.SetAsBoolean(Value: Boolean);
const
  AValues: array [Boolean] of String = ('false', 'true');
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(Char(Value));
    ftString,
    ftMemo:       SetAsString(AValues[Value]);
    ftWideString: SetAsWideString(AValues[Value]);
    ftSmallInt,
    ftInteger:    SetAsInteger(Ord(Value));
    ftLargeInt:   SetAsLargeInt(Ord(Value));
    ftWord,
    ftLongWord:   SetAsLongWord(Ord(Value));
    ftBoolean:    ; //SetAsBoolean(Value);
    ftFloat,
    ftDateTime:   SetAsFloat(Ord(Value));
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsByte(Value: Byte);
begin
  SetAsInteger(Value);
end;

procedure TFieldAccess.SetAsChar(Value: Char);
begin
  SetAsString(Value);
end;

procedure TFieldAccess.SetAsCurrency(Value: Currency);
begin
  SetAsFloat(Value);
end;

procedure TFieldAccess.SetAsDateTime(Value: TDateTime);
begin
  SetAsFloat(Value);
end;

procedure TFieldAccess.SetAsFloat(Value: Double);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsInteger(Trunc(Value));
    ftString,
    ftMemo:       SetAsString(Float.ToStr(Value));
    ftWideString: SetAsWideString(Float.ToStr(Value));
    ftSmallInt,
    ftInteger:    SetAsInteger(Trunc(Value));
    ftLargeInt:   SetAsLargeInt(Trunc(Value));
    ftWord,
    ftLongWord:   SetAsLongWord(Trunc(Value));
    ftBoolean:    SetAsBoolean(Value > 0);
    ftFloat,
    ftDateTime:   ; //SetAsFloat(Value);
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsInteger(Value: Integer);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(Chr(Value and $FF));
    ftString,
    ftMemo:       SetAsString(Int.ToStr(Value));
    ftWideString: SetAsWideString(Int.ToStr(Value));
    ftSmallInt,
    ftInteger:    ; //SetAsInteger(Value);
    ftLargeInt:   SetAsLargeInt(Value);
    ftWord,
    ftLongWord:   SetAsLongWord(Value);
    ftBoolean:    SetAsBoolean(Value > 0);
    ftFloat,
    ftDateTime:   SetAsFloat(Value);
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsLargeInt(Value: LargeInt);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsInteger(Large.Low(Value));
    ftString,
    ftMemo:       SetAsString(Large.ToStr(Value));
    ftWideString: SetAsWideString(Large.ToStr(Value));
    ftSmallInt,
    ftInteger:    SetAsInteger(Value);
    ftLargeInt:   ; //SetAsLargeInt(Value);
    ftWord,
    ftLongWord:   SetAsLongWord(Value);
    ftBoolean:    SetAsBoolean(Value > 0);
    ftFloat,
    ftDateTime:   SetAsFloat(Value);
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsLongWord(Value: LongWord);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsInteger(Integer(Value));
    ftString,
    ftMemo:       SetAsString(Large.ToStr(Value));
    ftWideString: SetAsWideString(Large.ToStr(Value));
    ftSmallInt,
    ftInteger:    SetAsInteger(Value);
    ftLargeInt:   SetAsLargeInt(Value);
    ftWord,
    ftLongWord:   ; //SetAsLongWord(Value);
    ftBoolean:    SetAsBoolean(Value > 0);
    ftFloat,
    ftDateTime:   SetAsFloat(Value);
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsString(const Value: String);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(Str.ToChr(Value));
    ftString,
    ftMemo:       ; //SetAsString(Value);
    ftWideString: SetAsWideString(Value);
    ftSmallInt,
    ftInteger:    SetAsInteger(Str.ToInt(Value, 0));
    ftLargeInt:   SetAsLargeInt(Str.ToInt(Value, 0));
    ftWord,
    ftLongWord:   SetAsLongWord(Str.ToInt(Value, 0));
    ftBoolean:    SetAsBoolean(Str.ToChr(LowerCase(Value), 'f') = 't');
    ftFloat,
    ftDateTime:   SetAsFloat(Str.ToFloat(Value, 0));
    ftGuid:       SetAsGuid(OsGuid.FromStr(Value));
  end;
end;

procedure TFieldAccess.SetAsWideString(const Value: WideString);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(#0);
    ftString,
    ftMemo:       SetAsString(Value);
    ftWideString: ; //SetAsWideString(Value);
    ftSmallInt,
    ftInteger:    SetAsInteger(Str.ToInt(Value, 0));
    ftLargeInt:   SetAsLargeInt(Str.ToInt(Value, 0));
    ftWord,
    ftLongWord:   SetAsLongWord(Str.ToInt(Value, 0));
    ftBoolean:    SetAsBoolean(Str.ToChr(LowerCase(Value), 'f') = 't');
    ftFloat,
    ftDateTime:   SetAsFloat(Str.ToFloat(Value, 0));
    ftGuid:       SetAsGuid(OsGuid.FromStr(Value));
  end;
end;

procedure TFieldAccess.SetAsVariant(const Value: Variant);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(#0);
    ftString,
    ftMemo:       SetAsString(Value);
    ftWideString: SetAsWideString(Value);
    ftSmallInt,
    ftInteger:    SetAsInteger(Value);
    ftLargeInt:   SetAsLargeInt(Integer(Value));
    ftWord,
    ftLongWord:   SetAsLongWord(Value);
    ftBoolean:    SetAsBoolean(Value);
    ftFloat,
    ftDateTime:   SetAsFloat(Value);
    //ftGuid:
  end;
end;

procedure TFieldAccess.SetAsGuid(const Value: TGUID);
begin
  case FDataType of
    ftChar,
    ftByte:       SetAsChar(#0);
    ftString,
    ftMemo:       SetAsString(OsGuid.ToStr(Value));
    ftWideString: SetAsWideString(OsGuid.ToStr(Value));
    ftSmallInt,
    ftInteger:    SetAsInteger(0);
    ftLargeInt:   SetAsLargeInt(0);
    ftWord,
    ftLongWord:   SetAsLongWord(0);
    ftBoolean:    SetAsBoolean(false);
    ftFloat,
    ftDateTime:   SetAsFloat(0);
    ftGuid:       ; //SetAsGuid(Value);
  end;
end;

procedure TFieldAccess.SetAsInterface(const Value: IUnknown);
begin
end;

procedure TFieldAccess.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TFieldAccess.SetSize(Value: LongWord);
begin
  FSize := Value;
  if FUseValue then SetLength(FValue, Value);
end;

constructor TFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited Create;

  FUseValue := Store = nil;

  if FUseValue then
    Pointer(FStore) := Pointer(IFieldStore(Self)) else
    Pointer(FStore) := Pointer(Store);

  FName := Name;
  FPosition := 0;
  SetSize(0);
  FDataType := ftUnknown;
end;

destructor TFieldAccess.Destroy;
begin
  Unbind;
  inherited;
end;

class function TFieldAccess.CreateTyped(const Name: String; DataType: TDataFieldType; Size: LongWord; const Store: IFieldStore): TFieldAccess;
begin
  case DataType of
    ftChar,
    ftByte:       Result := TCharFieldAccess.Create(Name, Store);
    ftString:     Result := TStringFieldAccess.CreateSized(Name, Size, Store);
    ftMemo:       Result := TMemoFieldAccess.Create(Name, Store);
    ftWideString: Result := TWideStringFieldAccess.CreateSized(Name, Size, Store);
    ftSmallInt,
    ftInteger:    Result := TIntegerFieldAccess.Create(Name, Store);
    ftLargeInt:   Result := TLargeIntFieldAccess.Create(Name, Store);
    ftWord:       Result := TWordFieldAccess.Create(Name, Store);
    ftLongWord:   Result := TLongWordFieldAccess.Create(Name, Store);
    ftBoolean:    Result := TBooleanFieldAccess.Create(Name, Store);
    ftFloat:      Result := TFloatFieldAccess.Create(Name, Store);
    ftDateTime:   Result := TDateTimeFieldAccess.Create(Name, Store);
    ftGuid:       Result := TGuidFieldAccess.Create(Name, Store);
    ftInterface:  Result := TInterfaceFieldAccess.Create(Name, Store);
    ftPointer:    Result := TInterfaceFieldAccess.Create(Name, Store); 
  else
                  Result := nil;
  end;
end;

function TFieldAccess.GetChanged: Boolean;
begin
  Result := FChanged;
end;

function TFieldAccess.GetPosition: LongWord;
begin
  Result := FPosition;
end;

procedure TFieldAccess.SetChanged(Value: Boolean);
begin
  FChanged := Value;
end;

function TFieldAccess.GetIsEmpty: Boolean;
begin
  Result := False;  
end;

procedure TFieldAccess.SetPosition(Value: LongWord);
begin
  FPosition := Value;
end;

procedure TFieldAccess.Unbind;
begin
  Pointer(FStore) := nil;
end;

function TFieldAccess.GetAsPointer: Pointer;
begin
  Result := nil;
end;

procedure TFieldAccess.SetAsPointer(const Value: Pointer);
begin
end;

{ TStringFieldAccess }

function TStringFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  if (Data = nil) or not Boolean(Data^) then
    Result := StrLComp(PChar(@Item1), PChar(@Item2), FSize) else
    Result := StrLIComp(PChar(@Item1), PChar(@Item2), FSize);
end;

constructor TStringFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftString;
  SetSize(0);
end;

constructor TStringFieldAccess.CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore);
begin
  Create(Name, Store);
  SetSize(Size);
end;

function TStringFieldAccess.GetAsString: String;
begin
  SetLength(Result, FSize);
  if FSize > 0 then
  begin
    FStore.Read(Result[1], FPosition, FSize);
    {if Result[FSize] = #0 then} Result := PChar(Result);
  end;
end;

function TStringFieldAccess.GetIsEmpty: Boolean;
begin
  Result := (FSize = 0);
end;

procedure TStringFieldAccess.SetAsString(const Value: String);
var
  lwLen: LongWord;
  sBuf: String;
begin
  sBuf := Value;
  lwLen := Length(sBuf);

  if lwLen > FSize then {LMP.28/11/2000}
    SetSize(lwLen);

  if lwLen < FSize then
  begin
    SetLength(sBuf, FSize);
    FillChar(sBuf[lwLen + 1], FSize - lwLen, #0);
  end;

  if FSize > 0 then
    FStore.Write(sBuf[1], FPosition, FSize);

  FChanged := true;
end;

{ TWideStringFieldAccess }

function TWideStringFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
var
  IgnoreCase: Boolean;
begin
  IgnoreCase := Assigned(Data) and (Boolean(Data^) = True);
  Result := OsWStr.Compare(PWideChar(@Item1), PWideChar(@Item2), IgnoreCase, 0);
end;

constructor TWideStringFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftWideString;
  SetSize(0);
end;

constructor TWideStringFieldAccess.CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore);
begin
  Create(Name, Store);
  SetSize(Size * 2);
end;

function TWideStringFieldAccess.GetAsWideString: WideString;
begin
  SetLength(Result, FSize);

  if FSize > 0 then
  begin
    FStore.Read(Result[1], FPosition, FSize);
    Result := PWideChar(Result);
  end;
end;

function TWideStringFieldAccess.GetIsEmpty: Boolean;
begin
  Result := (FSize = 0);
end;

function TWideStringFieldAccess.GetSize: LongWord;
begin
  Result := FSize div 2;
end;

procedure TWideStringFieldAccess.SetAsWideString(const Value: WideString);
var
  lwLen, lwSize: LongWord;
  sBuf: WideString;
begin
  sBuf := Value;
  lwLen := Length(sBuf);
  lwSize := FSize div 2;

  if lwLen < lwSize then
  begin
    SetLength(sBuf, FSize);
    FillChar(sBuf[lwLen + 1], (lwSize - lwLen) * 2, #0);
  end;

  FStore.Write(sBuf[1], FPosition, FSize);
  FChanged := true;
end;

{ TIntegerFieldAccess }

function TIntegerFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2);
end;

constructor TIntegerFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftInteger;
  SetSize(SizeOf(Integer));
end;

function TIntegerFieldAccess.GetAsInteger: Integer;
var
  Data: record
    case Integer of
      0: (I: Smallint);
      1: (L: Longint);
  end;
begin
  FStore.Read(Data, FPosition, FSize);

  if FDataType = ftSmallint then
    Result := Data.I else
    Result := Data.L;
end;

procedure TIntegerFieldAccess.SetAsInteger(Value: Integer);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TLargeIntFieldAccess }

function TLargeIntFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Large.Compare(LargeInt(Item1), LargeInt(Item2));
end;

constructor TLargeIntFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftLargeInt;
  SetSize(SizeOf(LargeInt));
end;

function TLargeIntFieldAccess.GetAsLargeInt: LargeInt;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TLargeIntFieldAccess.SetAsLargeInt(Value: LargeInt);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TLongWordFieldAccess }

function TLongWordFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Large.Compare(LargeInt(LongWord(Item1)), LargeInt(LongWord(Item2)));
end;

constructor TLongWordFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftLongWord;
  SetSize(SizeOf(LongWord));
end;

function TLongWordFieldAccess.GetAsLongWord: LongWord;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TLongWordFieldAccess.SetAsLongWord(Value: LongWord);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TWordFieldAccess }

function TWordFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Word(Item1) - Word(Item2);
end;

constructor TWordFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftWord;
  SetSize(SizeOf(Word));
end;

function TWordFieldAccess.GetAsLongWord: LongWord;
var
  wValue: Word;
begin
  FStore.Read(wValue, FPosition, FSize);
  Result := wValue;
end;

procedure TWordFieldAccess.SetAsLongWord(Value: LongWord);
var
  wValue: Word;
begin
  wValue := Value;
  FStore.Write(wValue, FPosition, FSize);
  FChanged := true;
end;

{ TBooleanFieldAccess }

function TBooleanFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Ord(Boolean(Item1)) - Ord(Boolean(Item2));
end;

constructor TBooleanFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftBoolean;
  SetSize(SizeOf(Boolean));
end;

function TBooleanFieldAccess.GetAsBoolean: Boolean;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TBooleanFieldAccess.SetAsBoolean(Value: Boolean);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TFloatFieldAccess }

function TFloatFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  if Double(Item1) > Double(Item2) then Result := 1 else
  if Double(Item1) < Double(Item2) then Result := -1 else Result := 0;
end;

constructor TFloatFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftFloat;
  SetSize(SizeOf(Double));
end;

function TFloatFieldAccess.GetAsFloat: Double;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TFloatFieldAccess.SetAsFloat(Value: Double);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TGuidFieldAccess }

function TGuidFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := OsGuid.Compare(TGUID(Item1), TGUID(Item2));
end;

constructor TGuidFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftGuid;
  SetSize(SizeOf(TGuid));
end;

function TGuidFieldAccess.GetAsGuid: TGuid;
begin
  FStore.Read(Result, FPosition, FSize);
end;

function TGuidFieldAccess.GetIsEmpty: Boolean;
begin
  Result := OsGuid.IsEmpty(GetAsGUID);
end;

procedure TGuidFieldAccess.SetAsGuid(const Value: TGuid);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TCharFieldAccess }

function TCharFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Byte(Item1) - Byte(Item2);
end;

constructor TCharFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftChar;
  SetSize(SizeOf(Char));
end;

function TCharFieldAccess.GetAsChar: Char;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TCharFieldAccess.SetAsChar(Value: Char);
begin
  FStore.Write(Value, FPosition, FSize);
  FChanged := true;
end;

{ TByteFieldAccess }

constructor TByteFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftByte;
end;

{ TMemoFieldAccess }

constructor TMemoFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftMemo;
  SetSize(0);
end;

constructor TMemoFieldAccess.CreateSized(const Name: String; Size: LongWord; const Store: IFieldStore);
begin
  Create(Name, Store);
  SetSize(Size);
end;

{ TDateTimeFieldAccess }

constructor TDateTimeFieldAccess.Create(const Name: String; const Store: IFieldStore; DataType: TDataFieldType);
begin
  inherited Create(Name, Store);
  FDataType := DataType;
end;

{ TPointerFieldAccess }

constructor TPointerFieldAccess.Create(const Name: String;
  const Store: IFieldStore);
begin
  inherited;
  FDataType := ftInterface;
  SetSize(SizeOf(Pointer));
end;

destructor TPointerFieldAccess.Destroy;
begin
  SetAsPointer(nil);
  inherited;
end;

function TPointerFieldAccess.Compare(const Item1; const Item2; Data: Pointer): Integer;
begin
  Result := VoidHandler.Compare(Item1, Item2, Data);
end;

procedure TPointerFieldAccess.SetSize(Value: LongWord);
begin
  inherited;
  if Value > 0 then Mem.Clear(FValue[1], Value);
end;

function TPointerFieldAccess.GetIsEmpty: Boolean;
begin
  Result := Get() = nil;
end;

function TPointerFieldAccess.Get: Pointer;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TPointerFieldAccess.Put(const Value: Pointer);
begin
  FStore.Write(Value, FPosition, FSize);
end;

function TPointerFieldAccess.GetAsPointer: Pointer;
begin
  Result := Get;
end;

procedure TPointerFieldAccess.SetAsPointer(const Value: Pointer);
begin
  Put(Value);
  FChanged := true;
end;

{ TInterfaceFieldAccess }

constructor TInterfaceFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftInterface;
  SetSize(SizeOf(Pointer));
end;

destructor TInterfaceFieldAccess.Destroy;
begin
  SetAsInterface(nil);
  inherited;
end;

function TInterfaceFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
var
  U1, U2: IUnknown;
  C1, C2: IComparable;
begin
  U1 := IUnknown(Item1);
  U2 := IUnknown(Item2);

  if  Assigned(U1) and
      Assigned(U2) and
     (Reference.GetInterface(U1, IComparable, C1) or
      Reference.GetInterface(U2, IComparable, C2)) then
    begin
      if Assigned(C1) then
        Result := C1.CompareTo(C2) else
        Result := C2.CompareTo(C1);
    end
  else
    Result := Integer(U1) - Integer(U2);
end;

function TInterfaceFieldAccess.GetAsInterface: IUnknown;
var
  P: Pointer;
begin
  P := Get;
  Result := IUnknown(P);
end;

procedure TInterfaceFieldAccess.SetAsInterface(const Value: IUnknown);
var
  P: Pointer;
begin
  P := Get;
  if P <> nil then IUnknown(P)._Release;
  P := Pointer(Value);
  Put(P);
  if P <> nil then IUnknown(P)._AddRef;
  FChanged := true;
end;

procedure TInterfaceFieldAccess.SetSize(Value: LongWord);
begin
  inherited;
  if Value > 0 then Mem.Clear(FValue[1], Value);
end;

function TInterfaceFieldAccess.Get: Pointer;
begin
  FStore.Read(Result, FPosition, FSize);
end;

procedure TInterfaceFieldAccess.Put(const Value: Pointer);
begin
  FStore.Write(Value, FPosition, FSize);
end;

function TInterfaceFieldAccess.GetIsEmpty: Boolean;
begin
  Result := GetAsInterface() = nil;
end;

{ TSmallIntFieldAccess }

function TSmallIntFieldAccess.Compare(const Item1; const Item2; Data: Pointer): Integer;
begin
  Result := SmallInt(Item1) - SmallInt(Item2);
end;

constructor TSmallIntFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftSmallInt;
  SetSize(SizeOf(SmallInt));
end;

{ TVariantFieldAccess }

constructor TVariantFieldAccess.Create(const Name: String; const Store: IFieldStore);
begin
  inherited;
  FDataType := ftVariant;
  SetSize(SizeOf(Variant));
end;

destructor TVariantFieldAccess.Destroy;
begin
  inherited;
end;

function TVariantFieldAccess.Compare(const Item1, Item2; Data: Pointer): Integer;
begin
  Result := Vart.Compare(Variant(Item1), Variant(Item2));
end;

function TVariantFieldAccess.GetIsEmpty: Boolean;
begin
  Result := Vart.IsEmpty(GetAsVariant);
end;

procedure TVariantFieldAccess.DoGet(out Data);
begin
  FStore.Read(Data, FPosition, FSize);
end;

procedure TVariantFieldAccess.DoPut(const Data);
begin
  FStore.Write(Data, FPosition, FSize);
end;

procedure TVariantFieldAccess.Put(const Value: Variant);
var
  p: TVarData;
begin
  DoGet(p);
  Vart.Clear(Variant(p));
  Variant(p) := Value;
  DoPut(p);
end;

function TVariantFieldAccess.Get: Variant;
var
  p: TVarData;
begin
  DoGet(p);
  Result := Variant(p); 
end;

procedure TVariantFieldAccess.SetSize(Value: LongWord);
begin
  inherited;
end;

procedure TVariantFieldAccess.SetAsVariant(const Value: Variant);
begin
  Put(Value);
end;

function TVariantFieldAccess.GetAsVariant: Variant;
begin
  Result := Get;
end;

function TVariantFieldAccess.GetAsBoolean: Boolean;
begin
  Result := Vart.ToBool(GetAsVariant);
end;

function TVariantFieldAccess.GetAsByte: Byte;
begin
  Result := GetAsInteger and $ff;
end;

function TVariantFieldAccess.GetAsDateTime: TDateTime;
begin
  Result := Vart.ToDateTime(GetAsVariant);
end;

function TVariantFieldAccess.GetAsFloat: Double;
begin
  Result := Vart.ToFloat(GetAsVariant);
end;

function TVariantFieldAccess.GetAsGuid: TGUID;
begin
  Result := OsGuid.FromStr(Vart.ToStr(GetAsVariant));
end;

function TVariantFieldAccess.GetAsInteger: Integer;
begin
  Result := Vart.ToInt(GetAsVariant);
end;

function TVariantFieldAccess.GetAsInterface: IUnknown;
begin
  Result := Vart.ToUnknown(GetAsVariant);
end;

function TVariantFieldAccess.GetAsLargeInt: LargeInt;
begin
  Result := Vart.ToInt(GetAsVariant);
end;

function TVariantFieldAccess.GetAsLongWord: LongWord;
begin
  Result := Vart.ToInt(GetAsVariant);
end;

function TVariantFieldAccess.GetAsPointer: Pointer;
begin
  Result := Vart.ToPtr(GetAsVariant);
end;

function TVariantFieldAccess.GetAsString: String;
begin
  Result := Vart.ToStr(GetAsVariant);
end;

function TVariantFieldAccess.GetAsWideString: WideString;
begin
  Result := GetAsVariant;
end;

procedure TVariantFieldAccess.SetAsBoolean(Value: Boolean);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsByte(Value: Byte);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsChar(Value: Char);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsCurrency(Value: Currency);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsDateTime(Value: TDateTime);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsFloat(Value: Double);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsGuid(const Value: TGUID);
begin
  SetAsVariant(OsGuid.ToStr(Value));
end;

procedure TVariantFieldAccess.SetAsInteger(Value: Integer);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsInterface(const Value: IInterface);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsLargeInt(Value: LargeInt);
begin
  SetAsInteger(Value);
end;

procedure TVariantFieldAccess.SetAsLongWord(Value: LongWord);
begin
  SetAsInteger(Value);
end;

procedure TVariantFieldAccess.SetAsPointer(const Value: Pointer);
begin
  SetAsInteger(Integer(Value));
end;

procedure TVariantFieldAccess.SetAsString(const Value: String);
begin
  SetAsVariant(Value);
end;

procedure TVariantFieldAccess.SetAsWideString(const Value: WideString);
begin
  SetAsVariant(Value);
end;

end.

