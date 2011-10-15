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

unit SilSfFirebirdDomain;

{$INCLUDE Defines.inc}

interface

uses
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

function Domain(Value: TFbDataType; AllowNulls: Boolean; Length: Short = 0; Scale: Short = 0; BlobType: TFbBlobType = fbbtBlobUntyped): RFbDomainData; overload;
function Domain(const Value: IFbDomain): RFbDomainData; overload;
function DoGetClass(Value: TFbType): TSilFirebirdDomainClass; overload;
function DoGetClass(Value: TFbDataType): TSilFirebirdDomainClass; overload;
function FbType(Value: TFbType): TFbDataType; overload;
function FbType(Value: TFbDataType): TFbType; overload;

implementation

uses
  SilSgFirebirdDomain;

function Domain(
    Value: TFbDataType;
    AllowNulls: Boolean;
    Length: Short = 0;
    Scale: Short = 0;
    BlobType: TFbBlobType = fbbtBlobUntyped): RFbDomainData;
begin
  Result.BaseType := FbType(Value);
  Result.BlobType := BlobType;
  Result.Length := Length;
  Result.Scale := Scale;
  Result.AllowNulls := AllowNulls;
end;

function Domain(const Value: IFbDomain): RFbDomainData;
begin
  Result.BaseType := Value.BaseType;
  Result.BlobType := Value.BlobType;
  Result.Length := Value.Length;
  Result.Scale := Value.Scale;
  Result.AllowNulls := Value.AllowNulls;
end;

function DoGetClass(Value: TFbType): TSilFirebirdDomainClass;
begin
  Result := DoGetClass(FbType(Value));
end;

function DoGetClass(Value: TFbDataType): TSilFirebirdDomainClass;
begin
  Result := GDomains[Value];
end;

function FbType(Value: TFbType): TFbDataType;
begin
  case Value of
    fbbtVarying     : Result := fbdtVarying;
    fbbtText        : Result := fbdtText;
    fbbtDouble      : Result := fbdtDouble;
    fbbtFloat       : Result := fbdtFloat;
    fbbtLong        : Result := fbdtLong;
    fbbtShort       : Result := fbdtShort;
    fbbtTimestamp   : Result := fbdtTimestamp;
    fbbtBlob        : Result := fbdtBlob;
    fbbtDFloat      : Result := fbdtDFloat;
    fbbtArray       : Result := fbdtArray;
    fbbtQuad        : Result := fbdtQuad;
    fbbtTypeTime    : Result := fbdtTypeTime;
    fbbtTypeDate    : Result := fbdtTypeDate;
    fbbtLarge       : Result := fbdtLarge;
    else              Result := fbdtUndefined;
  end;
end;

function FbType(Value: TFbDataType): TFbType;
begin
  case Value of
    fbdtVarying     : Result := fbbtVarying;
    fbdtText        : Result := fbbtText;
    fbdtDouble      : Result := fbbtDouble;
    fbdtFloat       : Result := fbbtFloat;
    fbdtLong        : Result := fbbtLong;
    fbdtShort       : Result := fbbtShort;
    fbdtTimestamp   : Result := fbbtTimestamp;
    fbdtBlob        : Result := fbbtBlob;
    fbdtDFloat      : Result := fbbtDFloat;
    fbdtArray       : Result := fbbtArray;
    fbdtQuad        : Result := fbbtQuad;
    fbdtTypeTime    : Result := fbbtTypeTime;
    fbdtTypeDate    : Result := fbbtTypeDate;
    fbdtLarge       : Result := fbbtLarge;
    else              Result := fbbtUndefined;
  end;
end;

end.
 