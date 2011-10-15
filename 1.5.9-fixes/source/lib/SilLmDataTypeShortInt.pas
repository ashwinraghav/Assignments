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

unit SilLmDataTypeShortInt;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueShortInt,
  SilLkValue;

type
  TSilDataHandlerShortInt = class(TSilDataHandler)
  protected 
    function ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
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
  _Type   = SilLiValueShortInt._Type;
  _Value  = SilLiValueShortInt._Value;

  {$I SilTkValueInterface}

type
  TSilValueShortInt = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueShortInt;

type
  {$I SilTkVariableInterface}

type
  TSilVariableShortint = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableShortint;

implementation

uses
  SilLfDataHandler,
  SilBtInt;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerShortInt }

class function TSilDataHandlerShortInt.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(ShortInt);
end;

class function TSilDataHandlerShortInt.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftInteger;
end;

class function TSilDataHandlerShortInt.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(ShortInt);
end;

function TSilDataHandlerShortInt.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PShortInt absolute Source;
  Target: PShortInt absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerShortInt.FromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PShortInt absolute Source;
  Target: PShortInt absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerShortInt.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PSmallint absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PSingle absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PDouble absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PCurrency absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PVariant absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PExtended absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PByte absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Byte(Origin)
  end;
end;

function TSilDataHandlerShortInt.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PWord absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Word(Origin)
  end;
end;

function TSilDataHandlerShortInt.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PLongWord absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Byte(Origin)
  end;
end;

function TSilDataHandlerShortInt.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PLargeInt absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PInteger absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerShortInt.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PBoolean absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := (Origin <> 0);
end;

function TSilDataHandlerShortInt.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: ShortInt;
  Target: PAnsiString absolute Destination;
begin
  Result := ToShortInt(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    System.Str(Origin, Target^);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueShortInt }

function TSilValueShortInt.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.ShortInt(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableShortint }

function TSilVariableShortint.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.ShortInt(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.
 