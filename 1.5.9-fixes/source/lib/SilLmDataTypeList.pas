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

unit SilLmDataTypeList;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilBeTypes,
  SilBeTypeInfo,
  SilBeDataType,
  SilLiTypeInfo,
  SilLiDataType;

type
  TSilDataTypeList = class(
    TSilObject,
    IDataTypeList )
  private
    FList: array of IDataHandler;
  private
    function DoGet(DataType: TDataType): IDataHandler;
  protected // IDataTypeList
    function Get(VType: Word): IDataHandler; overload; 
    function Get(DataType: TDataType): IDataHandler; overload; 
    function Get(const DataType: RDataType): IDataHandler; overload; 
    function Get(TypeInfo: PTypeInfo): IDataHandler; overload;
    function Get(const TypeInfo: ITypeInfo): IDataHandler; overload;
    function GetTypeSmallint: IDataHandler;
    function GetTypeLongInt: IDataHandler;
    function GetTypeSingle: IDataHandler;
    function GetTypeDouble: IDataHandler;
    function GetTypeCurrency: IDataHandler;
    function GetTypeDateTime: IDataHandler;
    function GetTypeWideString: IDataHandler;
    function GetTypeDispatch: IDataHandler;
    function GetTypeError: IDataHandler;
    function GetTypeWordBool: IDataHandler;
    function GetTypeVariant: IDataHandler;
    function GetTypeInterface: IDataHandler;
    function GetTypeDecimal: IDataHandler;
    function GetTypeExtended: IDataHandler;
    function GetTypeShortInt: IDataHandler;
    function GetTypeByte: IDataHandler;
    function GetTypeWord: IDataHandler;
    function GetTypeLongWord: IDataHandler;
    function GetTypeLargeInt: IDataHandler;
    function GetTypeLargeWord: IDataHandler;
    function GetTypeInteger: IDataHandler;
    function GetTypeCardinal: IDataHandler;
    function GetTypeVoid: IDataHandler;
    function GetTypeHRESULT: IDataHandler;
    function GetTypePointer: IDataHandler;
    function GetTypeSafearray: IDataHandler;
    function GetTypeArray: IDataHandler;
    function GetTypeUserdefined: IDataHandler;
    function GetTypePAnsiChar: IDataHandler;
    function GetTypePWideChar: IDataHandler;
    function GetTypeGUID: IDataHandler;
    function GetTypeClass: IDataHandler;
    function GetTypeObject: IDataHandler;
    function GetTypeBoolean: IDataHandler;
    function GetTypeLongBool: IDataHandler;
    function GetTypeAnsiChar: IDataHandler;
    function GetTypeWideChar: IDataHandler;
    function GetTypeAnsiString: IDataHandler;
  public
    destructor Destroy; override; 
  end;

implementation

uses
  SilBtError,
  SilLgDataTypeDefs,
  SilLmDataType,
  SilBtDataType,
  SilLhDataType;

{ TSilDataTypeList }

destructor TSilDataTypeList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilDataTypeList.Get(VType: Word): IDataHandler;
begin
  Result := DoGet(DataType.FromVarType(VType).Value);
end;

function TSilDataTypeList.Get(DataType: TDataType): IDataHandler;
begin
  Result := DoGet(DataType);
end;

function TSilDataTypeList.Get(const DataType: RDataType): IDataHandler;
begin
  Result := DoGet(DataType.Value);
end;

function TSilDataTypeList.Get(TypeInfo: PTypeInfo): IDataHandler;
begin
  Result := DoGet(DataType.FromTypeInfo(TypeInfo).Value);
end;

function TSilDataTypeList.Get(const TypeInfo: ITypeInfo): IDataHandler; 
begin
  Result := Get(TypeInfo.Ptr);
end;

function TSilDataTypeList.GetTypeSmallint: IDataHandler;
begin
  Result := DoGet(dtSmallint);
end;

function TSilDataTypeList.GetTypeLongInt: IDataHandler;
begin
  Result := DoGet(dtLongInt);
end;

function TSilDataTypeList.GetTypeSingle: IDataHandler;
begin
  Result := DoGet(dtSingle);
end;

function TSilDataTypeList.GetTypeDouble: IDataHandler;
begin
  Result := DoGet(dtDouble);
end;

function TSilDataTypeList.GetTypeCurrency: IDataHandler;
begin
  Result := DoGet(dtCurrency);
end;

function TSilDataTypeList.GetTypeDateTime: IDataHandler;
begin
  Result := DoGet(dtDate);
end;

function TSilDataTypeList.GetTypeWideString: IDataHandler;
begin
  Result := DoGet(dtWideString);
end;

function TSilDataTypeList.GetTypeDispatch: IDataHandler;
begin
  Result := DoGet(dtDispatch);
end;

function TSilDataTypeList.GetTypeError: IDataHandler;
begin
  Result := DoGet(dtError);
end;

function TSilDataTypeList.GetTypeWordBool: IDataHandler;
begin
  Result := DoGet(dtWordBool);
end;

function TSilDataTypeList.GetTypeVariant: IDataHandler;
begin
  Result := DoGet(dtVariant);
end;

function TSilDataTypeList.GetTypeInterface: IDataHandler;
begin
  Result := DoGet(dtInterface);
end;

function TSilDataTypeList.GetTypeDecimal: IDataHandler;
begin
  Result := DoGet(dtDecimal);
end;

function TSilDataTypeList.GetTypeExtended: IDataHandler;
begin
  Result := DoGet(dtExtended);
end;

function TSilDataTypeList.GetTypeShortInt: IDataHandler;
begin
  Result := DoGet(dtShortInt);
end;

function TSilDataTypeList.GetTypeByte: IDataHandler;
begin
  Result := DoGet(dtByte);
end;

function TSilDataTypeList.GetTypeWord: IDataHandler;
begin
  Result := DoGet(dtWord);
end;

function TSilDataTypeList.GetTypeLongWord: IDataHandler;
begin
  Result := DoGet(dtLongWord);
end;

function TSilDataTypeList.GetTypeLargeInt: IDataHandler;
begin
  Result := DoGet(dtLargeInt);
end;

function TSilDataTypeList.GetTypeLargeWord: IDataHandler;
begin
  Result := DoGet(dtLargeWord);
end;

function TSilDataTypeList.GetTypeInteger: IDataHandler;
begin
  Result := DoGet(dtInteger);
end;

function TSilDataTypeList.GetTypeCardinal: IDataHandler;
begin
  Result := DoGet(dtCardinal);
end;

function TSilDataTypeList.GetTypeVoid: IDataHandler;
begin
  Result := DoGet(dtVoid);
end;

function TSilDataTypeList.GetTypeHRESULT: IDataHandler;
begin
  Result := DoGet(dtHRESULT);
end;

function TSilDataTypeList.GetTypePointer: IDataHandler;
begin
  Result := DoGet(dtPointer);
end;

function TSilDataTypeList.GetTypeSafearray: IDataHandler;
begin
  Result := DoGet(dtSafearray);
end;

function TSilDataTypeList.GetTypeArray: IDataHandler;
begin
  Result := DoGet(dtDynarray);
end;

function TSilDataTypeList.GetTypeUserdefined: IDataHandler;
begin
  Result := DoGet(dtUserdefined);
end;

function TSilDataTypeList.GetTypePAnsiChar: IDataHandler;
begin
  Result := DoGet(dtPAnsiChar);
end;

function TSilDataTypeList.GetTypePWideChar: IDataHandler;
begin
  Result := DoGet(dtPWideChar);
end;

function TSilDataTypeList.GetTypeGUID: IDataHandler;
begin
  Result := DoGet(dtGUID);
end;

function TSilDataTypeList.GetTypeClass: IDataHandler;
begin
  Result := DoGet(dtClass);
end;

function TSilDataTypeList.GetTypeObject: IDataHandler;
begin
  Result := DoGet(dtObject);
end;

function TSilDataTypeList.GetTypeBoolean: IDataHandler;
begin
  Result := DoGet(dtBoolean);
end;

function TSilDataTypeList.GetTypeLongBool: IDataHandler;
begin
  Result := DoGet(dtLongBool);
end;

function TSilDataTypeList.GetTypeAnsiChar: IDataHandler;
begin
  Result := DoGet(dtAnsiChar);
end;

function TSilDataTypeList.GetTypeWideChar: IDataHandler;
begin
  Result := DoGet(dtWideChar);
end;

function TSilDataTypeList.GetTypeAnsiString: IDataHandler;
begin
  Result := DoGet(dtAnsiString);
end;

{$O+,W-,R-,V-,Q-}

function TSilDataTypeList.DoGet(DataType: TDataType): IDataHandler;
var
  Def: PDataTypeDef;
  Handler: ^IDataHandler;
begin
  if Ord(DataType) > High(FList) then
    SetLength(FList, Succ(Ord(DataType)));

  Handler := @FList[Ord(DataType)];
   
  if Handler^ = nil then
  begin
    Def := GTypedefs[DataType];
    if Def.Handler.Create(Self, Def, IDataHandler, PUnknown(Handler)) then
      raise Error.Create('Interface IDataHandler no soportada');
  end;

  Result := Handler^;
end;

end.
