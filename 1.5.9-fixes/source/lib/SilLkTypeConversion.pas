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

unit SilLkTypeConversion;

{$INCLUDE Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiTypeConversion,
  SilLiDataType,
  SilLkObject;

type
  PTypeConvertParams = ^RTypeConvertParams;
  RTypeConvertParams = record
    Source: HData;
    Destination: HData;
    SourceSize: LongWord;
    DestinationSize: LongWord;
    Count: Integer;
    Format: Pointer;
    Result: TDataTypecastStatus; 
  end;

type
  TSilTypeConversionClass = class of TSilTypeConversion;

  TSilTypeConversion = class(
    TSilObject,
    ITypeConversion )
  private
    FConverter: Pointer;
  private
    function DoGetConverter: ITypeConverter;
  protected
    procedure DoCheck(Params: PTypeConvertParams);
  protected // ITypeConversion
    function Convert(
              Source, Destination: HData;
              SourceSize, DestinationSize: LongWord;
              Count: Integer = 1;
              Format: Pointer = nil
            ): TDataTypecastStatus; overload; stdcall;
  protected
    procedure Convert(Params: PTypeConvertParams; var Result: TDataTypecastStatus); overload; virtual; abstract;
  protected
  public
    constructor Create(const Converter: ITypeConverter); virtual; 
    destructor Destroy; override;
  public
    property Converter: ITypeConverter read DoGetConverter;
  end;

implementation

{ TSilTypeConversion }

constructor TSilTypeConversion.Create(const Converter: ITypeConverter);
begin
  inherited Create;
  FConverter := Pointer(Converter);
end;

destructor TSilTypeConversion.Destroy;
begin
  FConverter := nil;
  inherited;
end;

function TSilTypeConversion.Convert(
        Source, Destination: HData;
        SourceSize, DestinationSize: LongWord;
        Count: Integer;
        Format: Pointer
        ): TDataTypecastStatus;
begin
  DoCheck(@Source);
  Result := tcSOk;
  Convert(@Source, Result);
end;

procedure TSilTypeConversion.DoCheck(Params: PTypeConvertParams);
begin
  ASSERT(Params.SourceSize = LongWord(Converter.Source.Size));
  ASSERT(Params.DestinationSize = LongWord(Converter.Destination.Size));
  ASSERT(Assigned(Params.Source));
  ASSERT(Assigned(Params.Destination));
  ASSERT(Params.Count > 0);
end;

function TSilTypeConversion.DoGetConverter: ITypeConverter;
begin
  Result := ITypeConverter(FConverter);
end;

end.
