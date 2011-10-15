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

unit SilLmDataHandler;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType,
  SilLkDataHandler;

type
  TSilDataHandler = class(TSilAbstractDataHandler)
  protected
    function DoCheck(Source, Destination: PChar; SourceSize, DestinationSize, ExpectedSize: LongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
    function DoCheckLength(Length: LongWord): LongWord;
  protected
    function ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override; 
    function ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override; 
    function ToHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function FromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromArray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  end;

implementation

uses
  SilBtMem,
  SilBtStr,
  SilLfDataHandler,
  SilOtTool,
  SilOtGuid;

{$O+,W-}

{ TSilDataHandler }

function TSilDataHandler.DoCheck(Source, Destination: PChar; SourceSize, DestinationSize, ExpectedSize: LongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Status;

  if Result in tcSucceeded then
    if not Assigned(Source) then
      Result := tcECantConvert;

  if Result in tcSucceeded then
    if not Assigned(Destination) then
      Result := tcECantConvert;

  if Result in tcSucceeded then
    if DestinationSize < ExpectedSize then
      Result := tcEDataOverflow;
end;

function TSilDataHandler.DoCheckLength(Length: LongWord): LongWord;
begin
  Result := Data.Size;
  if Result > Length then
    Result := Length;
end;

function TSilDataHandler.ToSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PSmallInt absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Result := DoCheckRange(Value, Low(Target^), High(Target^));
    Target^ := Smallint(Value);
  end;
end;

function TSilDataHandler.ToLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PLongInt absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PSingle absolute Destination;
  Value: Double;
begin
  Result := DoToDouble(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PCurrency absolute Destination;
  Value: Double;
begin
  Result := DoToDouble(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PWideString absolute Destination;
  Value: AnsiString;
begin
  Result := DoToAnsiString(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PDispatch absolute Destination;
  Value: IInterface;
begin
  Result := DoToInterface(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value as IDispatch;
end;

function TSilDataHandler.ToWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PWordBool absolute Destination;
  Value: Boolean;
begin
  Result := DoToBoolean(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PVariant absolute Destination;
  Value: WideString;
begin
  Result := DoToWideString(Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PExtended absolute Destination;
  Value: Double;
begin
  Result := DoToDouble(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PShortint absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Result := DoCheckRange(Value, Low(Target^), High(Target^));
    Target^ := ShortInt(Value);
  end;
end;

function TSilDataHandler.ToByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PByte absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Result := DoCheckRange(Value, Low(Target^), High(Target^));
    Target^ := Byte(Value);
  end;
end;

function TSilDataHandler.ToWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PWord absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Result := DoCheckRange(Value, Low(Target^), High(Target^));
    Target^ := Word(Value);
  end;
end;

function TSilDataHandler.ToLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PLongWord absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Result := DoCheckRange(Value, Low(Target^), High(Target^));
    Target^ := LongWord(Value);
  end;
end;

function TSilDataHandler.ToLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PLargeInt absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PLargeInt absolute Destination;
  Value: Integer;
begin
  Result := DoToLargeInt(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PCardinal absolute Destination;
  Value: LongWord;
begin
  Result := DoToLongWord(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := Clear(Destination, DestinationSize, Param, ReturnedSize, Status);
end;

function TSilDataHandler.ToHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: ^HResult absolute Destination;
  Value: LongWord;
begin
  Result := DoToLongWord(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PAnsiChar absolute Destination;
  Value: AnsiString;
  Size: LongWord;
  Buffer: string;
begin
  Result := DoToAnsiString(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Buffer := Value;
    Size := Succ(Str.Len(Buffer)) * SizeOf(Target^);
    if DestinationSize < Size then
    begin
      Size := DestinationSize;
      Result := tcSTruncated;
    end;
    Str.Move(Buffer, Target, Size);
  end;
end;

function TSilDataHandler.ToPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PWideChar absolute Destination;
  Value: WideString;
  Size: LongWord;
begin
  Result := DoToWideString(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
  begin
    Size := Succ(Wstr.Len(Value)) * SizeOf(Target^);
    if DestinationSize < Size then
    begin
      Size := DestinationSize;
      Result := tcSTruncated;
    end;
    WStr.Move(Value, Target, Size);
  end;
end;

function TSilDataHandler.ToGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PGUID absolute Destination;
  Value: AnsiString;
begin
  Result := DoToAnsiString(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Guid.FromStr(Value);
end;

function TSilDataHandler.ToClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: ^TClass absolute Destination;
  Value: TObject;
begin
  Result := DoToObject(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value.ClassType;
end;

function TSilDataHandler.ToBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PBoolean absolute Destination;
  Value: Integer;
begin
  Result := DoToInteger(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := (Value <> 0);
end;

function TSilDataHandler.ToLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PLongBool absolute Destination;
  Value: Boolean;
begin
  Result := DoToBoolean(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := Value;
end;

function TSilDataHandler.ToAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PAnsiChar absolute Destination;
  Value: Byte;
begin
  Result := DoToByte(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := System.AnsiChar(Value);
end;

function TSilDataHandler.ToWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Target: PWideChar absolute Destination;
  Value: Word;
begin
  Result := DoToWord(Source, @Value, SourceSize, SizeOf(Value), Param, ReturnedSize, Status);
  if Result = tcSOk then
    Target^ := System.WideChar(Value);
end;

function TSilDataHandler.FromSmallint(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Smallint.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromLongInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.LongInt.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromSingle(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Single.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromDouble(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Double.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromCurrency(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Currency.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromDateTime(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.DateTime.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromWideString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.WideString.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Dispatch.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromError(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Error.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromWordBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.WordBool.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Variant.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.IInterface.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromDecimal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Decimal.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromExtended(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Extended.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromShortInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.ShortInt.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromByte(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Byte.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Word.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromLongWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.LongWord.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromLargeInt(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.LargeInt.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromLargeWord(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.LargeWord.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromInteger(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Integer.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromCardinal(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Cardinal.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromVoid(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Void.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromHResult(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.HResult.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromPointer(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Pointer.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromSafearray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Safearray.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromArray(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Dynarray.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromUserdefined(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Userdefined.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromPAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.PAnsiChar.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromPWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.PWideChar.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromGUID(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.GUID.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromClass(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.TClass.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.TObject.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromBoolean(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.Boolean.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromLongBool(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.LongBool.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromAnsiChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.AnsiChar.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromWideChar(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.WideChar.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

function TSilDataHandler.FromAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
begin
  Result := List.AnsiString.Convert(Data, Source, Destination, SourceSize, DestinationSize, Param, ReturnedSize, Status)
end;

end.
