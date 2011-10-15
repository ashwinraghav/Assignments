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

unit SilLmDataTypeDispatch;

{$I Defines.inc}

interface
  
uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLmDataHandler;

type
  TSilDataHandlerDispatch = class(TSilDataHandler)
  protected 
    function ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
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
    function ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  public
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; override;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; override;
    class function TypeSize(Def: PDataTypeDef): LongWord; override;
  end;

implementation

uses
  SilBtInt;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerDispatch }

class function TSilDataHandlerDispatch.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(IDispatch);
end;

class function TSilDataHandlerDispatch.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftInterface;
end;

class function TSilDataHandlerDispatch.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(IDispatch);
end;

function TSilDataHandlerDispatch.ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDispatch absolute Source;
  Target: PDispatch absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerDispatch.FromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PDispatch absolute Source;
  Target: PDispatch absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerDispatch.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PSmallint absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PSingle absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PDouble absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PCurrency absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PVariant absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PExtended absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Origin;(*)
end;

function TSilDataHandlerDispatch.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PShortInt absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
      Target^ := Shortint(Origin)
    else if Origin < Low(Target^) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;(*)
end;

function TSilDataHandlerDispatch.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PByte absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status); 
  (*)if Result in tcSucceeded then
    if (Low(Target^) <= Origin) and (Origin <= High(Target^)) then
      Target^ := Shortint(Origin)
    else if Origin < Low(Target^) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;(*)
end;

function TSilDataHandlerDispatch.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PWord absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    if (Smallint(Low(Target^)) <= Origin) {and (Word(Origin) <= Smallint(High(Target^)))} then
      Target^ := Word(Origin)
    else if Origin < Smallint(Low(Target^)) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;(*)
end;

function TSilDataHandlerDispatch.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PLongWord absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    if (Smallint(Low(Target^)) <= Origin) then
      Target^ := LongWord(Origin)
    else if Origin < Smallint(Low(Target^)) then
      Result := tcESignMismatch
    else
      Result := tcEDataOverflow;(*)
end;

function TSilDataHandlerDispatch.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PLargeInt absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := LargeInt(Origin)(*)
end;

function TSilDataHandlerDispatch.ToInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PInteger absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := Integer(Origin)(*)
end;

function TSilDataHandlerDispatch.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PBoolean absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    Target^ := (Origin <> 0);(*)
end;

function TSilDataHandlerDispatch.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: IDispatch;
  Target: PAnsiString absolute Destination;
begin
  Result := ToDispatch(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  (*)if Result in tcSucceeded then
    System.Str(Origin, Target^);(*)
end;

end.
