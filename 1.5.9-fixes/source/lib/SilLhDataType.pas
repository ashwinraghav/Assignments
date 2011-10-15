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

unit SilLhDataType;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilBeTypes,
  SilBeError,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType;

type
  TSilDataHandlerClass = class of TSilDataHandlerType;
    
  PDataTypeDef = ^RDataTypeDef;
                                                                             
  RDataTypeDef = packed record
    Name: PChar;
    DataType: RDataType;
    Handler: TSilDataHandlerClass;
  end;

  TSilDataHandlerType = class(TSilObject)
  protected 
    class function TypeInfo(Def: PDataTypeDef): PTypeInfo; virtual; abstract;
    class function FieldType(Def: PDataTypeDef): TDataFieldType; virtual; abstract;
    class function TypeSize(Def: PDataTypeDef): LongWord; virtual; abstract;
  public
    class function Create(const List: IDataTypeList; Def: PDataTypeDef; const IID: TGUID; Ref: PUnknown; Parameter: Pointer = nil): Boolean; reintroduce; overload; virtual; abstract;
  end;

  ESilConversionFailed = class(SilLiDataType.ESilConversionFailed)
    constructor Create(Error: TDataTypecastStatus; Origin, Target: TDataType; const Value: string);
  end;

const
  GsvSilGlobalDatatypeList: TGUID = '{6BA88D2B-8EC4-432D-923D-BC086C87B1D5}';

implementation

uses
  SilBtTypeInfo,
  SilLdDataType;

{ ESilConversionFailed }

constructor ESilConversionFailed.Create(Error: TDataTypecastStatus; Origin, Target: TDataType; const Value: string);
begin
  inherited CreateResFmt(
    @SConversionFailed,
    [ Typ.Enum.Name(System.TypeInfo(TDataTypecastStatus), Ord(Error), 'tc'),
      Value,
      Typ.Enum.Name(System.TypeInfo(TDataType), Ord(Origin), 'dt'),
      Typ.Enum.Name(System.TypeInfo(TDataType), Ord(Target), 'dt')]);
end;

end.
