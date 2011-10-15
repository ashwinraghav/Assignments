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

unit SilLmDataTypeLargeInt;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueLargeInt,
  SilLkValue;

type
  TSilDataHandlerLargeInt = class(TSilDataHandler)
  protected 
    function ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  public
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; override;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; override;
    class function TypeSize(Def: PDataTypeDef): LongWord; override;
  end;

type
  _Type   = SilLiValueLargeInt._Type;
  _Value  = SilLiValueLargeInt._Value;

  {$I SilTkValueInterface}

type
  TSilValueLargeInt = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueLargeInt;

type
  {$I SilTkVariableInterface}

type
  TSilVariableLargeInt = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableLargeInt;

implementation

uses
  SilBtInt, SilLfDataHandler;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerLargeInt }

class function TSilDataHandlerLargeInt.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(Int64);
end;

class function TSilDataHandlerLargeInt.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftLargeInt;
end;

class function TSilDataHandlerLargeInt.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(Int64);
end;

function TSilDataHandlerLargeInt.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PLargeInt absolute Source;
  Target: PLargeInt absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerLargeInt.FromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PLargeInt absolute Source;
  Target: PLargeInt absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerLargeInt.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PSmallint absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Smallint(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PSingle absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerLargeInt.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PDouble absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerLargeInt.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PCurrency absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerLargeInt.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PVariant absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerLargeInt.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PExtended absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerLargeInt.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PShortInt absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := ShortInt(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PByte absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Byte(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PWord absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Word(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PLongWord absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := LongWord(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PInteger absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Integer(Origin);
  end;
end;

function TSilDataHandlerLargeInt.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PBoolean absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := (Origin <> 0);
end;

function TSilDataHandlerLargeInt.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: LargeInt;
  Target: PAnsiString absolute Destination;
begin
  Result := ToLargeInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    System.Str(Origin, Target^);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueLargeInt }

function TSilValueLargeInt.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.LargeInt(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableLargeInt }

function TSilVariableLargeInt.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.LargeInt(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.
 