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

unit SilLtFields;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilBkTool,
  SilLiValue,
  SilLiFiler,
  SilLiField;

type
  Fld = class(Tool)
    class function Create( const AName: string; AValue: boolean ): IFieldAccess; overload;
    class function Create( const AName: string; AValue: integer ): IFieldAccess; overload;
    class function Create( const AName: string; AValue: double ): IFieldAccess; overload;
    class function Create( const AName: string; AValue: string ): IFieldAccess; overload;
    class function Create(const Value: Variant): IVariable; overload;
    class function CreateTyped( const AName: string; AType: TDataFieldType ): IFieldAccess; overload;
    class function CreateTyped( const AName: string; AType: TDataFieldType; const AValue: Variant ): IFieldAccess; overload;
  end;

implementation

uses
  SilLmField,
  SilLmFieldAccess;

{ Flds }

class function Fld.Create(const AName: string;
  AValue: integer): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, ftInteger );
  result.AsInteger := AValue;
end;

class function Fld.Create(const AName: string;
  AValue: boolean): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, ftBoolean );
  result.AsBoolean := AValue;
end;

class function Fld.Create(const AName: string;
  AValue: string): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, ftString );
  result.AsString := AValue;
end;

class function Fld.Create(const AName: string;
  AValue: double): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, ftFloat );
  result.AsFloat := AValue;
end;

class function Fld.CreateTyped(const AName: string;
  AType: TDataFieldType): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, AType );
end;

class function Fld.Create(const Value: Variant): IVariable;
begin
  Result := TSilFieldValue.Create(Value);
end;

class function Fld.CreateTyped(const AName: string; AType: TDataFieldType;
  const AValue: Variant): IFieldAccess;
begin
  result := TFieldAccess.CreateTyped( AName, AType );
  result.AsVariant := AValue;
end;

end.

