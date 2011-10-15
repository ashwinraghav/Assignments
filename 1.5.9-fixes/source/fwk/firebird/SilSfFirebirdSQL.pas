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

unit SilSfFirebirdSQL;

{$INCLUDE Defines.inc}

interface

uses
  Sil;

function SQLBuild(const Text: string; const Filter: string = ''; const Order: string = ''): string;
function SQLFilterJoin(const Filter, NewFilter: string; Operator: string = ''; Prepend: Boolean = True): string;
function SQLParamAdd(const Args: IParameterList; const Name: string; const Value: Variant): IParameterList;
function SQLParam(const Name: string): string;
function SQLCond(const LeftSide, RightSide: string; Operator: string = ''): string;
function SQLConds(const Conds: array of string; Operator: string = ''): string;

implementation

uses
  SilScFirebirdSQL, SilLtList;

function SQLBuild(const Text: string; const Filter: string = ''; const Order: string = ''): string;
begin
  Result := Text;
  if Sil.Str.IsAssigned(Filter) then
  begin
    Sil.Str.Add(Result, CSqlWhere, sLineBreak);
    Sil.Str.Add(Result, Filter, sLineBreak);
  end;
  if Sil.Str.IsAssigned(Order) then
  begin
    Sil.Str.Add(Result, CSqlOrder, sLineBreak);
    Sil.Str.Add(Result, Order, sLineBreak);
  end;
end;

function SQLFilterJoin(const Filter, NewFilter: string; Operator: string; Prepend: Boolean): string;
begin
  if Sil.Str.IsEmpty(Operator) then Operator := CSqlOpAnd;
  Operator := ccSPC + Operator + ccSPC;    
  if Prepend then
    Result := '' else
    Result := CSqlLParen + Filter + CSqlRParen;
  if Sil.Str.IsAssigned(NewFilter) then
  begin
    if not Sil.Str.IsEmpty(Result) and (Sil.Str.LastChar(Result) <> ccSPC) then
      Sil.Str.Add(Result, Operator);
    Sil.Str.Add(Result, CSqlLParen + NewFilter + CSqlRParen);
  end;
  if Prepend and Sil.Str.IsAssigned(Filter) then
    Sil.Str.Add(Result, CSqlLParen + Filter + CSqlRParen, Operator);
end;

function SQLParamAdd(const Args: IParameterList; const Name: string; const Value: Variant): IParameterList;
begin
  if Assigned(Args) then
    Result := Args else
    Result := Sil.List.Parameters;
  Result[Name] := Value;
end;

function SQLParam(const Name: string): string;
begin
  if Sil.Str.IsAssigned(Name) then
    Result := CSqlParamStart + Name else
    Result := CSqlParamMark;
end;

function SQLCond(const LeftSide, RightSide: string; Operator: string): string;
begin
  if Sil.Str.IsEmpty(Operator) then Operator := CSqlOpEqual;
  Operator := ccSPC + Operator + ccSPC;    
  Result := LeftSide + Operator + RightSide;
end;

function SQLConds(const Conds: array of string; Operator: string ): string;
var
  Index: Integer;
begin
  for Index := Low(Conds) to High(Conds) do
    if Sil.Str.IsAssigned(Conds[Index]) then
      Sil.Str.Add(Result, CSqlLParen + Conds[Index] + CSqlRParen, Operator);
end;


end.
