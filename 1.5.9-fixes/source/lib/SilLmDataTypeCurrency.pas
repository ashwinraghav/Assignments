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

unit SilLmDataTypeCurrency;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueCurrency,
  SilLkValue;

type
  TSilDataHandlerCurrency = class(TSilDataHandler)
  protected 
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  public
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; override;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; override;
    class function TypeSize(Def: PDataTypeDef): LongWord; override;
  end;

type
  _Type   = SilLiValueCurrency._Type;
  _Value  = SilLiValueCurrency._Value;

  {$I SilTkValueInterface}

type
  TSilValueCurrency = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueCurrency;

type
  {$I SilTkVariableInterface}

type
  TSilVariableCurrency = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableCurrency;      

implementation

uses
  SilBtTypeInfo,
  SilBtInt,
  SilLfDataHandler;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerCurrency }

class function TSilDataHandlerCurrency.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(Currency);
end;

class function TSilDataHandlerCurrency.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftCurrency;
end;

class function TSilDataHandlerCurrency.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(Currency);
end;

function TSilDataHandlerCurrency.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PCurrency absolute Source;
  Target: PCurrency absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerCurrency.FromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PCurrency absolute Source;
  Target: PCurrency absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerCurrency.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PSmallint absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Target^ := System.Round(Origin);
    Result := tcSTruncated;
  end;
end;

function TSilDataHandlerCurrency.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PSingle absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerCurrency.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PDouble absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerCurrency.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PVariant absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerCurrency.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PExtended absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerCurrency.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PShortInt absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PByte absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end
    else if Origin < Low(Target^) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PWord absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end
    else if Origin < Low(Target^) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PLongWord absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end
    else if Origin < Low(Target^) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PLargeInt absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else 
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PInteger absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else 
      Result := tcEDataOverflow;
end;

function TSilDataHandlerCurrency.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PBoolean absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := (Origin <> 0);
end;

function TSilDataHandlerCurrency.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Currency;
  Target: PAnsiString absolute Destination;
begin
  Result := ToCurrency(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    System.Str(Origin, Target^);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueCurrency }

function TSilValueCurrency.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.Currency(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableCurrency }

function TSilVariableCurrency.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.Currency(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.
 