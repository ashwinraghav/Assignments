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

unit SilLmDataType;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilBeTypes,
  SilBeDataType,
  SilBeTypeInfo,
  SilLiDataType,
  SilLhDataType;
  
type
  TSilDataType = class(
    TSilObject,
    IDataType )
  private
    FHandler: Pointer;
    FDef: PDataTypeDef;
  protected // IDataType
    function GetName: PChar;
    function GetKind: RDataType;
    function GetInfo: PTypeInfo;
    function GetSize: LongWord;
    function GetHandler: IDataHandler;
  public
    constructor Create(const Handler: IDataHandler; Def: PDataTypeDef); 
    destructor Destroy; override;
  end;

implementation

type
  FriendClass = class of FriendType;
  FriendType  = class(TSilDataHandlerType);

{ TSilDataType }

constructor TSilDataType.Create(const Handler: IDataHandler; Def: PDataTypeDef);
begin
  inherited Create;
  FHandler := Pointer(Handler);
  FDef := Def;
end;

destructor TSilDataType.Destroy;
begin
  FDef := nil;
  FHandler := nil;
  inherited;
end;

function TSilDataType.GetName: PChar;
begin
  Result := FDef.Name;
end;

function TSilDataType.GetKind: RDataType;
begin
  Result := FDef.DataType;
end;

function TSilDataType.GetInfo: PTypeInfo;
begin
  Result := FriendClass(FDef.Handler).TypeInfo(FDef);
end;

function TSilDataType.GetSize: LongWord;
begin
  Result := FriendClass(FDef.Handler).TypeSize(FDef);
end;

function TSilDataType.GetHandler: IDataHandler;
begin
  Result := IDataHandler(FHandler);
end;

end.
