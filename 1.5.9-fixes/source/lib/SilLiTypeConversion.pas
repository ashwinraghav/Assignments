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

unit SilLiTypeConversion;

{$INCLUDE Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiDataType;

type
  ITypeConversion = interface
    ['{D3119C87-5F66-4E99-BC2B-96B8489C8302}']
    function Convert(
              SourceData: HData;
              DestinationData: HData;
              SourceSize: LongWord;
              DestinationSize: LongWord;
              Count: Integer = 1;
              Format: Pointer = nil
          ): TDataTypecastStatus; stdcall;
  end;

type
  ITypeConverter = interface
    ['{AD41D410-417D-4E85-88B7-2843F32875A6}']
    function GetSource: ITypeHandler;
    function GetDestination: ITypeHandler;
    function GetConversion: ITypeConversion;
    property Source: ITypeHandler read GetSource;
    property Destination: ITypeHandler read GetDestination; 
    property Conversion: ITypeConversion read GetConversion; 
  end;

implementation
end.
