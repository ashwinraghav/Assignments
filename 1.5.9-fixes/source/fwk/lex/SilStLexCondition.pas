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

unit SilStLexCondition;

interface

uses
  Sil,
  SilSiLexDictionary;

type
  ConditionTool = class
    class function Empty: ILexCondition;
    class function Anything: ILexCondition;
    class function CharInSet( const ASet: array of Char ): ILexCondition; overload;
    class function CharInSet( const ASet: string ): ILexCondition; overload;
    class function CharBetween( AFrom, ATo: Char ): ILexCondition;
    class function Multiple( const ASet: array of ILexCondition ): ILexCondition;
    class function AnyOf( const ASet: array of ILexCondition ): ILexCondition;
    class function RefByName( const AName: string; const AList: ILexDefinitionList ): ILexCondition;
    class function Literal( const ALiteral: string ): ILexCondition;
    class function Optional( const ACondition: ILexCondition ): ILexCondition;
    class function Repetitions( const ACondition: ILexCondition; Min: integer = 1; Max: integer = -1 ): ILexCondition;
    class function Negate( const ACondition: ILexCondition ): ILexCondition;
    class function ConectedTo( const ACondition: ILexCondition ): ILexCondition;
    class function NotConectedTo( const ACondition: ILexCondition ): ILexCondition;
  end;

implementation

uses
  SilSmLexCondition;

{ ConditionTool }

class function ConditionTool.AnyOf(const ASet: array of ILexCondition): ILexCondition;
begin
  result := TSilLexAnyOf.Create( ASet );
end;

class function ConditionTool.Anything: ILexCondition;
begin
  result := TSilLexAnything.Create;
end;

class function ConditionTool.CharBetween(AFrom, ATo: Char): ILexCondition;
begin
  result := TSilLexCharBetween.Create( AFrom, ATo );
end;

class function ConditionTool.CharInSet(const ASet: array of Char): ILexCondition;
begin
  result := TSilLexCharInSet.Create( ASet );
end;

class function ConditionTool.CharInSet(const ASet: string ): ILexCondition;
begin
  result := TSilLexCharInSet.Create( ASet );
end;

class function ConditionTool.ConectedTo(const ACondition: ILexCondition): ILexCondition;
begin
  result := TSilLexConectedTo.Create( ACondition );
end;

class function ConditionTool.NotConectedTo(const ACondition: ILexCondition): ILexCondition;
begin
  result := TSilLexNotConectedTo.Create( ACondition );
end;

class function ConditionTool.Empty: ILexCondition;
begin
  result := TSilLexEmpty.Create;
end;

class function ConditionTool.Literal(const ALiteral: string): ILexCondition;
begin
  result := TSilLexLiteral.Create( ALiteral );
end;

class function ConditionTool.Multiple(const ASet: array of ILexCondition): ILexCondition;
begin
  result := TSilLexMultiple.Create( ASet );
end;

class function ConditionTool.Negate( const ACondition: ILexCondition): ILexCondition;
begin
  result := TSilLexNegate.Create( ACondition );
end;

class function ConditionTool.Optional(const ACondition: ILexCondition): ILexCondition;
begin
  result := TSilLexOptional.Create( ACondition );
end;

class function ConditionTool.RefByName(const AName: string; const AList: ILexDefinitionList): ILexCondition;
begin
  result := TSilLexRefByName.Create( AName, AList );
end;

class function ConditionTool.Repetitions(const ACondition: ILexCondition; Min, Max: integer): ILexCondition;
begin
  result := TSilLexRepetitions.Create( ACondition, Min, Max );
end;

end.

