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

unit SilLmDataTypeObject;

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
  TSilDataHandlerObject = class(TSilDataHandler)
  protected 
    function ToObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function FromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  protected
    function ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
    function ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer = nil; ReturnedSize: PLongWord = nil; Status: TDataTypecastStatus = tcSOk): TDataTypecastStatus; override;
  public
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; override;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; override;
    class function TypeSize(Def: PDataTypeDef): LongWord; override;
  end;

implementation

uses
  SilBtInt, SilLfDataHandler;

{$O+,W-,R-,V-,Q-}

{ TSilDataHandlerObject }

class function TSilDataHandlerObject.TypeInfo(Def: PDataTypeDef): PTypeInfo;
begin
  Result := System.TypeInfo(TObject);
end;

class function TSilDataHandlerObject.FieldType(Def: PDataTypeDef): TDataFieldType;
begin
  Result := ftUnknown;
end;

class function TSilDataHandlerObject.TypeSize(Def: PDataTypeDef): LongWord;
begin
  Result := System.SizeOf(TObject);
end;

function TSilDataHandlerObject.ToObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PObject absolute Source;
  Target: PObject absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerObject.FromObject(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: PObject absolute Source;
  Target: PObject absolute Destination;
begin
  Result := DoCheck(Source, Destination, SourceSize, DestinationSize, SizeOf(Target^), Status);
  if Result in tcSucceeded then
    Target^ := Origin^;
end;

function TSilDataHandlerObject.ToVariant(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: TObject;
  Target: PVariant absolute Destination;
  Unknown: IUnknown;
begin
  Result := ToObject(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    if Origin.GetInterface(IUnknown, Unknown) then
      Target^ := Unknown else
      Result := NotSupported(Data.Kind.Value, dtVariant, @Source);
  end;
end;

function TSilDataHandlerObject.ToInterface(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: TObject;
  Target: PInterface absolute Destination;
begin
  Result := ToObject(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    if not Origin.GetInterface(IUnknown, Target^) then
      Result := NotSupported(Data.Kind.Value, dtInterface, @Source);
  end;
end;

function TSilDataHandlerObject.ToDispatch(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: TObject;
  Target: PDispatch absolute Destination;
begin
  Result := ToObject(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
  begin
    if not Origin.GetInterface(IDispatch, Target^) then
      Result := NotSupported(Data.Kind.Value, dtDispatch, @Source);
  end;
end;

function TSilDataHandlerObject.ToAnsiString(Source, Destination: PChar; SourceSize, DestinationSize: LongWord; Param: Pointer; ReturnedSize: PLongWord; Status: TDataTypecastStatus): TDataTypecastStatus;
var
  Origin: TObject;
  Target: PAnsiString absolute Destination;
begin
  Result := ToObject(Source, @Origin, SourceSize, SizeOf(Origin), Param, ReturnedSize, Status);
  if Result in tcSucceeded then
    Target^ := Origin.ClassName;
end;

end.
 