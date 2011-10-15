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

unit SilLmValue;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiValue,
  SilLkAggregated;

type
  TValue = class (
    // extends
    TAggregatedObject,
    // implements
    IValue)
  private
    FValue: Pointer;
  protected // IValue
    function GetValue: Variant;
    function GetAsChar: Char;
    function GetAsByte: Byte;
    function GetAsString: String;
    function GetAsWideString: WideString;
    function GetAsInteger: Integer;
    function GetAsLargeInt: LargeInt;
    function GetAsLongWord: LongWord;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsGuid: TGUID;
    function GetAsInterface: IUnknown;
  public
    constructor Create(const Controller: IUnknown; const Value: IConstant = nil);
    destructor Destroy; override;
  end;

  TVariable = class (
    // extends
    TValue,
    // implements
    IVariable)
  protected // IVariable
    procedure SetValue(const Value: Variant);
    procedure SetAsChar(Value: Char);
    procedure SetAsByte(Value: Byte);
    procedure SetAsString(const Value: String);
    procedure SetAsWideString(const Value: WideString);
    procedure SetAsInteger(Value: LongInt);
    procedure SetAsLargeInt(Value: LargeInt);
    procedure SetAsLongWord(Value: LongWord);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsFloat(Value: Double);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsGuid(const Value: TGUID);
    procedure SetAsInterface(const Value: IUnknown);
  public
    constructor Create(const Controller: IUnknown; const Value: IVariant = nil);
  end;

implementation

uses
  SilBtVart,
  SilLtReference;

{ TValue }

constructor TValue.Create(const Controller: IUnknown; const Value: IConstant);
var
  Aux: IConstant;
begin
  inherited Create(Controller);

  if Value <> nil then
    FValue := Pointer(Value) else
  if Reference.GetInterface(Controller, IConstant, Aux) then
    FValue := Pointer(Aux);
end;

destructor TValue.Destroy;
begin
  FValue := nil;
  inherited;
end;

function TValue.GetAsBoolean: Boolean;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsByte: Byte;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsChar: Char;
var
  sValue: String;
begin
  sValue := IConstant(FValue).Value;
  if Length(sValue) > 0 then
    Result := sValue[1] else
    Result := #0;
end;

function TValue.GetAsCurrency: Currency;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsDateTime: TDateTime;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsFloat: Double;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsGuid: TGUID;
begin                                           
  Result := Vart.ToGuid(IConstant(FValue).Value);
end;

function TValue.GetAsInteger: Integer;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsInterface: IUnknown;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsLargeInt: LargeInt;
var
  iValue: Integer;
begin
  iValue := IConstant(FValue).Value;
  Result := iValue;
end;

function TValue.GetAsLongWord: LongWord;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsString: String;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetAsWideString: WideString;
begin
  Result := IConstant(FValue).Value;
end;

function TValue.GetValue: Variant;
begin
  Result := IConstant(FValue).Value;
end;

{ TVariable }

constructor TVariable.Create(const Controller: IUnknown; const Value: IVariant);
begin

end;

procedure TVariable.SetAsBoolean(Value: Boolean);
begin

end;

procedure TVariable.SetAsByte(Value: Byte);
begin

end;

procedure TVariable.SetAsChar(Value: Char);
begin

end;

procedure TVariable.SetAsCurrency(Value: Currency);
begin

end;

procedure TVariable.SetAsDateTime(Value: TDateTime);
begin

end;

procedure TVariable.SetAsFloat(Value: Double);
begin

end;

procedure TVariable.SetAsGuid(const Value: TGUID);
begin

end;

procedure TVariable.SetAsInteger(Value: Integer);
begin

end;

procedure TVariable.SetAsInterface(const Value: IUnknown);
begin

end;

procedure TVariable.SetAsLargeInt(Value: LargeInt);
begin

end;

procedure TVariable.SetAsLongWord(Value: LongWord);
begin

end;

procedure TVariable.SetAsString(const Value: String);
begin

end;

procedure TVariable.SetAsWideString(const Value: WideString);
begin

end;

procedure TVariable.SetValue(const Value: Variant);
begin

end;

end.
 