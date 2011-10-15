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

unit SilLkTypeInfo;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,

  SilBeTypes,
  SilBeTypeInfo,
  SilLiEnumerator,
  SilLiStringList,
  SilLiTypeInfo,

  SilLmInterfaceList;

type
  TSilTypeInfoClass = class of TSilTypeInfo;
  TSilTypeDefClass = class of TSilTypeDef;

  TSilTypeInfo = class(
    TSilInterfacedObject,
    ITypeInfo )
  protected // ITypeDef
    function GetName: string; virtual; abstract;
    function GetTypeKind: TTypeKind; virtual; abstract;
    function GetData: ITypeData; virtual;
    function GetTypeInfo: PTypeInfo; virtual; abstract;     
  public
    constructor Create; overload;
    constructor CreateNew(const Data: Pointer); reintroduce; virtual; 
    class function Create(const Data: Pointer): TSilTypeInfo; overload;
  protected
    class function DoLookup(const Data: Pointer): TSilTypeInfoClass; virtual; 
  end;

  TSilTypeDef = class(
    TSilTypeInfo)
  protected // ITypeDef
  end;

implementation

uses
  SilBtError;

{ TSilTypeInfo }

constructor TSilTypeInfo.Create;
begin
  inherited Create;
end;

class function TSilTypeInfo.Create(const Data: Pointer): TSilTypeInfo;
begin
  Result := DoLookup(Data).CreateNew(Data);
end;

constructor TSilTypeInfo.CreateNew(const Data: Pointer);
begin
  Create;
end;

class function TSilTypeInfo.DoLookup(const Data: Pointer): TSilTypeInfoClass;
begin
  Result := Self;
end;

function TSilTypeInfo.GetData: ITypeData;
begin
  raise Error.Create('el tipo no tiene datos extra');
end;

end.
