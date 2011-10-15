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

unit SilLmDataTypeSmallint;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueSmallint,
  SilLkValue;

type
  TSilDataHandlerSmallint = class(TSilDataHandler)
  protected 
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
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
  _Type   = SilLiValueSmallint._Type;
  _Value  = SilLiValueSmallint._Value;

  {$I SilTkValueInterface}

type
  TSilValueSmallint = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueSmallint;

type
  {$I SilTkVariableInterface}

type
  TSilVariableSmallint = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableSmallint;

implementation

uses
  SilBtInt, SilLfDataHandler;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerSmallint }

class function TSilDataHandlerSmallint.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(Smallint);
end;

class function TSilDataHandlerSmallint.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftSmallInt;
end;

class function TSilDataHandlerSmallint.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(Smallint);
end;

function TSilDataHandlerSmallint.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PSmallint absolute Source;
  Target: PSmallint absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerSmallint.FromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PSmallint absolute Source;
  Target: PSmallint absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerSmallint.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PSingle absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerSmallint.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PDouble absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerSmallint.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PCurrency absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerSmallint.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PVariant absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerSmallint.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PExtended absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerSmallint.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PShortInt absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := ShortInt(Origin);
  end;
end;

function TSilDataHandlerSmallint.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PByte absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Byte(Origin);
  end;
end;

function TSilDataHandlerSmallint.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PWord absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := Word(Origin);
  end;
end;

function TSilDataHandlerSmallint.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PLongWord absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Result := DoCheckRange(Origin, Low(Target^), High(Target^));
    Target^ := LongWord(Origin);
  end;
end;

function TSilDataHandlerSmallint.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PLargeInt absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := LargeInt(Origin)
end;

function TSilDataHandlerSmallint.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PInteger absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Integer(Origin)
end;

function TSilDataHandlerSmallint.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PBoolean absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := (Origin <> 0);
end;

function TSilDataHandlerSmallint.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Smallint;
  Target: PAnsiString absolute Destination;
begin
  Result := ToSmallint(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    System.Str(Origin, Target^);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueSmallint }

function TSilValueSmallint.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.Smallint(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableSmallint }

function TSilVariableSmallint.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.Smallint(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.
 