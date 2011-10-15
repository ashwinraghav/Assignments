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

unit SilLmDataTypeVariant;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueVariant,
  SilLkValue;

type
  TSilDataHandlerVariant = class(TSilDataHandler)
  protected
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected 
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
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
  _Type   = SilLiValueVariant._Type;
  _Value  = SilLiValueVariant._Value;

  {$I SilTkValueInterface}

type
  TSilValueVariant = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueVariant;

type
  {$I SilTkVariableInterface}

type
  TSilVariableVariant = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableVariant;

implementation

uses
  SilBtInt,
  SilBtVart,
  SilLfDataHandler;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerVariant }

class function TSilDataHandlerVariant.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(Variant);
end;

class function TSilDataHandlerVariant.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftVariant;
end;

class function TSilDataHandlerVariant.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(Variant);
end;

function TSilDataHandlerVariant.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PVariant absolute Source;
  Target: PVariant absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerVariant.FromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PVariant absolute Source;
  Target: PVariant absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerVariant.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PSmallint absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Smallint(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PSingle absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Single(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PDouble absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Double(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PDateTime absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.DateTime(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PCurrency absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Currency(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PExtended absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Extended(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PShortInt absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.ShortInt(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PByte absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Byte(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PWord absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Word(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PLongWord absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.LongWord(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PLargeInt absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.LargeInt(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PInteger absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Integer(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PBoolean absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.Boolean(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

function TSilDataHandlerVariant.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Variant;
  Target: PAnsiString absolute Destination;
begin
  Result := ToVariant(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Result := List.Get(TVarData(Origin).VType).Cast.AnsiString(@TVarData(Origin).VPointer, Destination, SourceSize, SizeOf(Target^), Param, ReturnedSize, Status);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueVariant }

function TSilValueVariant.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.Variant(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableVariant }

function TSilVariableVariant.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.Variant(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.


 