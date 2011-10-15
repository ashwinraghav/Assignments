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

unit SilLmTypeConverter;

{$INCLUDE Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiDataType,
  SilLiTypeConversion,
  SilLkObject,
  SilLkTypeConversion;


type
  TSilTypeConverter = class(
    TSilObject,
    ITypeConverter )
  private
    FSource: ITypeHandler;
    FDestination: ITypeHandler;
    FConversion: ITypeConversion; 
  protected // ITypeConverter
    function GetSource: ITypeHandler;
    function GetDestination: ITypeHandler;
    function GetConversion: ITypeConversion;
  public
    constructor Create(const Source, Destination: ITypeHandler; const Conversion: TSilTypeConversionClass);
    destructor Destroy; override; 
  end;

implementation

{ TSilTypeConverter }

constructor TSilTypeConverter.Create(const Source, Destination: ITypeHandler; const Conversion: TSilTypeConversionClass);
begin
  inherited Create;
  FSource := Source;
  FDestination := Destination;
  FConversion := Conversion.Create(Self);
end;

destructor TSilTypeConverter.Destroy;
begin
  FConversion := nil;
  FDestination := nil;
  FSource := nil;
  inherited;
end;

function TSilTypeConverter.GetSource: ITypeHandler;
begin
  Result := FSource;
end;

function TSilTypeConverter.GetDestination: ITypeHandler;
begin
  Result := FDestination;
end;

function TSilTypeConverter.GetConversion: ITypeConversion;
begin
  Result := FConversion;
end;

end.
 