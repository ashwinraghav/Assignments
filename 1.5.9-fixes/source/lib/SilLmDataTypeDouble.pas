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

unit SilLmDataTypeDouble;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLiValueDouble,
  SilLkValue;

type
  TSilDataHandlerDouble = class(TSilDataHandler)
  protected 
    function ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
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
  _Type   = SilLiValueDouble._Type;
  _Value  = SilLiValueDouble._Value;

  {$I SilTkValueInterface}

type
  TSilValueDouble = class(TSilValueTemplate)
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; override;
  end;

type
  _ValueClass  = TSilValueDouble;

type
  {$I SilTkVariableInterface}

type
  TSilVariableDouble = class(TSilVariableTemplate)
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; override;
  end;

type
  _VariableClass  = TSilVariableDouble;

implementation

uses
  SilBtInt;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerDouble }

class function TSilDataHandlerDouble.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(Double);
end;

class function TSilDataHandlerDouble.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftFloat;
end;

class function TSilDataHandlerDouble.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(Double);
end;

function TSilDataHandlerDouble.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDouble absolute Source;
  Target: PDouble absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerDouble.FromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDouble absolute Source;
  Target: PDouble absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerDouble.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PSmallint absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Target^ := System.Round(Origin);
    Result := tcSTruncated;
  end;
end;

function TSilDataHandlerDouble.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PSingle absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Target^ := Origin;
    Result := tcSRoundOff;
  end;
end;

function TSilDataHandlerDouble.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PCurrency absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    Target^ := Origin;
    Result := tcSRoundOff;
  end;
end;

function TSilDataHandlerDouble.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PVariant absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerDouble.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PExtended absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin;
end;

function TSilDataHandlerDouble.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PShortInt absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else
      Result := tcEDataOverflow;
end;

function TSilDataHandlerDouble.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PByte absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
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

function TSilDataHandlerDouble.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PWord absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
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

function TSilDataHandlerDouble.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PLongWord absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
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

function TSilDataHandlerDouble.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PLargeInt absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else 
      Result := tcEDataOverflow;
end;

function TSilDataHandlerDouble.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PInteger absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
    begin
      Target^ := System.Round(Origin);
      Result := tcSTruncated;
    end else 
      Result := tcEDataOverflow;
end;

function TSilDataHandlerDouble.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PBoolean absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := (Origin <> 0);
end;

function TSilDataHandlerDouble.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: Double;
  Target: PAnsiString absolute Destination;
begin
  Result := ToDouble(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    System.Str(Origin, Target^);
end;

{ TSilValueTemplate }

{$I SilTkValueImplementation}

{ TSilValueDouble }

function TSilValueDouble.DoGet(out Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.Cast.Double(Buffer.Memory, @Value, Buffer.Size, SizeOf(Value));
end;

{ TSilVariableTemplate }

{$I SilTkVariableImplementation}

{ TSilVariableDouble }

function TSilVariableDouble.DoSet(const Value: _Type): TDataTypecastStatus;
begin
  Result := Origin.From.Double(@Value, Buffer.Memory, SizeOf(Value), Buffer.Size);
end;

end.
 