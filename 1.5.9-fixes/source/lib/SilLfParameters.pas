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

unit SilLfParameters;

{$I Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilBeTypes,
  SilLiParameters;

function ParamVoid(const Name: string): RParameter; 
function ParamNull(const Name: string): RParameter; 
function ParamEmpty(const Name: string): RParameter; 
function ParamBoolean(const Name: string; Value: Boolean): RParameter;
function ParamInteger(const Name: string; Value: Integer): RParameter;
function ParamLongWord(const Name: string; Value: LongWord): RParameter;
function ParamLargeint(const Name: string; Value: LargeInt): RParameter;
function ParamFloat(const Name: string; Value: Double): RParameter;
function ParamDateTime(const Name: string; Value: TDateTime): RParameter;
function ParamAnsiString(const Name: string; const Value: string): RParameter; 
function ParamWideString(const Name: string; const Value: WideString): RParameter;
function ParamVariant(const Name: string; const Value: Variant): RParameter; 

function Params(Locked: Boolean): IParameterList; overload;
function Params(const Items: array of RParameter; Locked: Boolean): IParameterList; overload;
function Params(const Items: RParameters; Locked: Boolean): IParameterList; overload;
function Params(const List: IParameterList; const Items: array of RParameter): IParameterList; overload;
function Params(const List: IParameterList; const Items: RParameters): IParameterList; overload;
function ParamPut(const List: IParameterList; const Param: RParameter): IParameterList; overload;
function ParamGet(const List: IParameters; const Name: string; Default: Variant): Variant; overload;  

implementation

uses
  SilBtVart,
  SilLmParameters;

function ParamVoid(const Name: string): RParameter;
begin
  Result.Name := Name;
  Result.Value := Vart.Unassigned;
end;

function ParamNull(const Name: string): RParameter;
begin
  Result.Name := Name;
  Result.Value := Vart.Null;
end;

function ParamEmpty(const Name: string): RParameter;
begin
  Result.Name := Name;
  Result.Value := Vart.EmptyParam;
end;

function ParamBoolean(const Name: string; Value: Boolean): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value;
end;

function ParamInteger(const Name: string; Value: Integer): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value; 
end;

function ParamLongWord(const Name: string; Value: LongWord): RParameter; 
begin
  Result.Name := Name;
{$IFDEF D60}
  Result.Value := Value;
{$ELSE}
  Result.Value := Integer(Value and $7FFFFFFF);
{$ENDIF}
end;

function ParamLargeint(const Name: string; Value: LargeInt): RParameter; 
begin
  Result.Name := Name;
{$IFDEF D60}
  Result.Value := Value;
{$ELSE}
  Result.Value := Integer(Value);
{$ENDIF}
end;

function ParamFloat(const Name: string; Value: Double): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value; 
end;

function ParamDateTime(const Name: string; Value: TDateTime): RParameter; 
begin
  Result.Name := Name;
  Result.Value := Value;
end;

function ParamAnsiString(const Name: string; const Value: string): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value; 
end;

function ParamWideString(const Name: string; const Value: WideString): RParameter; 
begin
  Result.Name := Name;
  Result.Value := Value; 
end;

function ParamVariant(const Name: string; const Value: Variant): RParameter;
begin
  Result.Name := Name;
  Result.Value := Value; 
end;

function Params(Locked: Boolean): IParameterList;
begin
  Result := TSilParameters.Create(Locked);
end;

function Params(const Items: array of RParameter; Locked: Boolean): IParameterList;
begin
  Result := Params(Params(Locked), Items);
end;

function Params(const Items: RParameters; Locked: Boolean): IParameterList;
begin
  Result := Params(Params(Locked), Items);
end;

function Params(const List: IParameterList; const Items: array of RParameter): IParameterList;
var
  I: Integer;
begin
  if Assigned(List) and (Length(Items) > 0) then
    for I := Low(Items) to High(Items) do
      with Items[I] do
        List[Name] := Value;
        
  Result := List;
end;

function Params(const List: IParameterList; const Items: RParameters): IParameterList; 
var
  I: Integer;
begin
  if Assigned(List) and (Length(Items) > 0) then
    for I := Low(Items) to High(Items) do
      with Items[I] do
        List[Name] := Value;
        
  Result := List;
end;

function ParamPut(const List: IParameterList; const Param: RParameter): IParameterList;
begin
  if not Assigned(List) then
    Result := TSilParameters.Create(False) else
    Result := List;

  Result[Param.Name] := Param.Value;
end;

function ParamGet(const List: IParameters; const Name: string; Default: Variant): Variant;
begin
  if not Assigned(List) or not List.Find(Name, Result) then
    Result := Default;
end; 

end.
