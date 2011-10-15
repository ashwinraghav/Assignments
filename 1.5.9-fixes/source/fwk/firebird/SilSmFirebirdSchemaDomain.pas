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

unit SilSmFirebirdSchemaDomain;

{$INCLUDE Defines.inc}

interface
uses
  Sil,
  SilSiFirebird,
  SilShFirebird,
  SilSmFirebirdCursor,
  SilSkFirebirdSchemaObject,
  SilSkFirebirdSchemaFields;
  
type
  TSilFirebirdSchemaDomains = class(
    TSilFirebirdSchemaObject,
    IFbSchemaDomains )
  private
    FFields: IUnknown;
  protected
    function DoGetFields: IUnknown; override;
  protected // IFbSchemaDomains
    function GetDomainFields: IFbSchemaDomain;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaDomain): Boolean; overload;
  public
    destructor Destroy; override;
  end;

type
  TSilFirebirdSchemaDomain = class(
    TSilFirebirdSchemaFields,
    IFbSchemaDomain )
  protected // IFbSchemaDomain
    function GetName: IFbData;
    function GetQueryName: IFbData;
    function GetValidationBlr: IFbData;
    function GetValidationSource: IFbData;
    function GetComputedBlr: IFbData;
    function GetComputedSource: IFbData;
    function GetDefaultValue: IFbData;
    function GetDefaultSource: IFbData;
    function GetLength: IFbData;
    function GetScale: IFbData;
    function GetDataType: IFbData;
    function GetDataSubtype: IFbData;
    function GetMissingValue: IFbData;
    function GetMissingSource: IFbData;
    function GetDescription: IFbData;
    function GetSystemFlag: IFbData;
    function GetQueryHeader: IFbData;
    function GetSegmentLength: IFbData;
    function GetEditString: IFbData;
    function GetExternalLength: IFbData;
    function GetExternalScale: IFbData;
    function GetExternalType: IFbData;
    function GetDimensions: IFbData;
    function GetNullFlag: IFbData;
    function GetCharacterLength: IFbData;
    function GetCollationId: IFbData;
    function GetCharsetId: IFbData;
    function GetPrecision: IFbData;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilSgFirebirdSchemaQueries;

const
  CSysIndexDomainName                   =   0;
  CSysIndexDomainQueryName              =   1;
  CSysIndexDomainValidationBlr          =   2;
  CSysIndexDomainValidationSource       =   3;
  CSysIndexDomainComputedBlr            =   4;
  CSysIndexDomainComputedSource         =   5;
  CSysIndexDomainDefaultValue           =   6;
  CSysIndexDomainDefaultSource          =   7;
  CSysIndexDomainLength                 =   8;
  CSysIndexDomainScale                  =   9;
  CSysIndexDomainType                   =  10;
  CSysIndexDomainSubtype                =  11;
  CSysIndexDomainMissingValue           =  12;
  CSysIndexDomainMissingSource          =  13;
  CSysIndexDomainDescription            =  14;
  CSysIndexDomainSystemFlag             =  15;
  CSysIndexDomainQueryHeader            =  16;
  CSysIndexDomainSegmentLength          =  17;
  CSysIndexDomainEditString             =  18;
  CSysIndexDomainExternalLength         =  19;
  CSysIndexDomainExternalScale          =  20;
  CSysIndexDomainExternalType           =  21;
  CSysIndexDomainDimensions             =  22;
  CSysIndexDomainNullFlag               =  23;
  CSysIndexDomainCharacterLength        =  24;
  CSysIndexDomainCollationId            =  25;
  CSysIndexDomainCharsetId              =  26;
  CSysIndexDomainPrecision              =  27;

{ TSilFirebirdSchemaDomains }

destructor TSilFirebirdSchemaDomains.Destroy;
begin
  FFields := nil;
  inherited;
end;

function TSilFirebirdSchemaDomains.GetDomainFields: IFbSchemaDomain;
begin
  if not Assigned(FFields) then FFields := TSilFirebirdSchemaDomain.Create(Self);
  Result := FFields as IFbSchemaDomain;
end;

function TSilFirebirdSchemaDomains.Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaDomain): Boolean;
begin
  Result := inherited Enumerate(Enum, Fields);
end;

function TSilFirebirdSchemaDomains.DoGetFields: IUnknown;
begin
  Result := GetDomainFields();
end;

{ TSilFirebirdSchemaDomain }

destructor TSilFirebirdSchemaDomain.Destroy;
begin
  inherited;
end;

function TSilFirebirdSchemaDomain.GetName: IFbData;
begin
  Result := DoCheck(CSysIndexDomainName, CSysFieldDomainName);
end;

function TSilFirebirdSchemaDomain.GetQueryName: IFbData;
begin
  Result := DoCheck(CSysIndexDomainQueryName, CSysFieldDomainQueryName);
end;

function TSilFirebirdSchemaDomain.GetValidationBlr: IFbData;
begin
  Result := DoCheck(CSysIndexDomainValidationBlr, CSysFieldDomainValidationBlr);
end;

function TSilFirebirdSchemaDomain.GetValidationSource: IFbData;
begin
  Result := DoCheck(CSysIndexDomainValidationSource, CSysFieldDomainValidationSource);
end;

function TSilFirebirdSchemaDomain.GetComputedBlr: IFbData;
begin
  Result := DoCheck(CSysIndexDomainComputedBlr, CSysFieldDomainComputedBlr);
end;

function TSilFirebirdSchemaDomain.GetComputedSource: IFbData;
begin
  Result := DoCheck(CSysIndexDomainComputedSource, CSysFieldDomainComputedSource);
end;

function TSilFirebirdSchemaDomain.GetDefaultValue: IFbData;
begin
  Result := DoCheck(CSysIndexDomainDefaultValue, CSysFieldDomainDefaultValue);
end;

function TSilFirebirdSchemaDomain.GetDefaultSource: IFbData;
begin
  Result := DoCheck(CSysIndexDomainDefaultSource, CSysFieldDomainDefaultSource);
end;

function TSilFirebirdSchemaDomain.GetLength: IFbData;
begin
  Result := DoCheck(CSysIndexDomainLength, CSysFieldDomainLength);
end;

function TSilFirebirdSchemaDomain.GetScale: IFbData;
begin
  Result := DoCheck(CSysIndexDomainScale, CSysFieldDomainScale);
end;

function TSilFirebirdSchemaDomain.GetDataType: IFbData;
begin
  Result := DoCheck(CSysIndexDomainType, CSysFieldDomainType);
end;

function TSilFirebirdSchemaDomain.GetDataSubtype: IFbData;
begin
  Result := DoCheck(CSysIndexDomainSubtype, CSysFieldDomainSubtype);
end;

function TSilFirebirdSchemaDomain.GetMissingValue: IFbData;
begin
  Result := DoCheck(CSysIndexDomainMissingValue, CSysFieldDomainMissingValue);
end;

function TSilFirebirdSchemaDomain.GetMissingSource: IFbData;
begin
  Result := DoCheck(CSysIndexDomainMissingSource, CSysFieldDomainMissingSource);
end;

function TSilFirebirdSchemaDomain.GetDescription: IFbData;
begin
  Result := DoCheck(CSysIndexDomainDescription, CSysFieldDomainDescription);
end;

function TSilFirebirdSchemaDomain.GetSystemFlag: IFbData;
begin
  Result := DoCheck(CSysIndexDomainSystemFlag, CSysFieldDomainSystemFlag);
end;

function TSilFirebirdSchemaDomain.GetQueryHeader: IFbData;
begin
  Result := DoCheck(CSysIndexDomainQueryHeader, CSysFieldDomainQueryHeader);
end;

function TSilFirebirdSchemaDomain.GetSegmentLength: IFbData;
begin
  Result := DoCheck(CSysIndexDomainSegmentLength, CSysFieldDomainSegmentLength);
end;

function TSilFirebirdSchemaDomain.GetEditString: IFbData;
begin
  Result := DoCheck(CSysIndexDomainEditString, CSysFieldDomainEditString);
end;

function TSilFirebirdSchemaDomain.GetExternalLength: IFbData;
begin
  Result := DoCheck(CSysIndexDomainExternalLength, CSysFieldDomainExternalLength);
end;

function TSilFirebirdSchemaDomain.GetExternalScale: IFbData;
begin
  Result := DoCheck(CSysIndexDomainExternalScale, CSysFieldDomainExternalScale);
end;

function TSilFirebirdSchemaDomain.GetExternalType: IFbData;
begin
  Result := DoCheck(CSysIndexDomainExternalType, CSysFieldDomainExternalType);
end;

function TSilFirebirdSchemaDomain.GetDimensions: IFbData;
begin
  Result := DoCheck(CSysIndexDomainDimensions, CSysFieldDomainDimensions);
end;

function TSilFirebirdSchemaDomain.GetNullFlag: IFbData;
begin
  Result := DoCheck(CSysIndexDomainNullFlag, CSysFieldDomainNullFlag);
end;

function TSilFirebirdSchemaDomain.GetCharacterLength: IFbData;
begin
  Result := DoCheck(CSysIndexDomainCharacterLength, CSysFieldDomainCharacterLength);
end;

function TSilFirebirdSchemaDomain.GetCollationId: IFbData;
begin
  Result := DoCheck(CSysIndexDomainCollationId, CSysFieldDomainCollationId);
end;

function TSilFirebirdSchemaDomain.GetCharsetId: IFbData;
begin
  Result := DoCheck(CSysIndexDomainCharsetId, CSysFieldDomainCharsetId);
end;

function TSilFirebirdSchemaDomain.GetPrecision: IFbData;
begin
  Result := DoCheck(CSysIndexDomainPrecision, CSysFieldDomainPrecision);
end;

end.
