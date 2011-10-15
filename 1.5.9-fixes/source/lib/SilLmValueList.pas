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

unit SilLmValueList;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilLiValueList,
  SilLmInterfaceList,
  SilLiField,
  SilLiFiler;

type
  TValueList = class (
    // extends
    TSilInterfaceList,
    // implements
    IValueList)
  protected
    FIgnoreCase: Boolean;
    function FieldByName(const Name: string): IFieldAccess;
    function FieldByIndex(const Index: Integer): IFieldAccess;
  public // IValueList
    function GetField(const Indx: Variant): IFieldAccess;
    function New(const Name: String; DataType: TDataFieldType = ftString; Size: LongWord = 0): IFieldAccess;
    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(Value: Boolean);
    property Items[const Indx: Variant]: IFieldAccess read GetField; default;
  end;

implementation

uses
  SilBtText,
  SilLmFieldAccess,
  SilBtVart,
  SilBtStr;

{ TValueList }

function TValueList.New(const Name: String; DataType: TDataFieldType; Size: LongWord): IFieldAccess;
begin
  Result := TFieldAccess.CreateTyped(Name, DataType, Size);
  Add(Result);
end;

function TValueList.GetField(const Indx: Variant): IFieldAccess;
begin
  case Vart.VType(Indx) of
    varInteger, varSmallint:
      Result := FieldByIndex(Indx);
    varString, varOleStr:
      Result := FieldByName(Indx);
  else
      Result := nil;
  end;
end;

function TValueList.GetIgnoreCase: Boolean;
begin
  Result := FIgnoreCase;
end;

procedure TValueList.SetIgnoreCase(Value: Boolean);
begin
  FIgnoreCase := Value;
end;

function TValueList.FieldByIndex(const Index: Integer): IFieldAccess;
begin
  Result := IFieldAccess(inherited GetItem(Index));
end;

function TValueList.FieldByName(const Name: string): IFieldAccess;
var
  i: Integer;
begin
  for i := 0 to GetCount - 1 do
  begin
    Result := FieldByIndex(i);
    if (FIgnoreCase and (Text.Compare(Result.Name, Name) = 0)) or
      (not FIgnoreCase and (Result.Name = Name)) then Exit;
  end;
  Result := nil;
end;

end.
