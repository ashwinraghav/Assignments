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

unit SilStScript;

interface

uses
  Sil,
  SilLiTypeInfo,
  SilTokens,
  SilLexer,
  SilSiEvaluator,
  SilStParser;

type
  ScriptTool = class(ParserTool)
    class function Create(
      const Hook: ITokenHandler;
      const Stream: IStream;
      const Tokens: ITokenTable = nil): IEvaluator; overload;

    class function Create(
      const Hook: ITokenHandler;
      const Code: String;
      const Tokens: ITokenTable = nil): IEvaluator; overload;

    class function Execute(
      const Hook: ITokenHandler;
      const Code: String;
      const Tokens: ITokenTable = nil): Variant; overload;

    class function Execute(
      const Hook: ITokenHandler;
      const Stream: IStream;
      const Tokens: ITokenTable = nil): Variant; overload;
  end;

implementation

uses
  SilSmEvaluator,
  SilSfScriptTokens,
  SilSfScriptParser;

{ EvalTool }

class function ScriptTool.Create(
  const Hook: ITokenHandler;
  const Stream: IStream;
  const Tokens: ITokenTable): IEvaluator;
begin
  Result := TSilEvaluator.Create(Hook);
  ParseStatement(
    SilLexer.Tool.TokenLexer(Stream, SilSfScriptTokens.DefineTokens(Tokens), SilSfScriptTokens.DefineTokens(SilLexer.Dictionary.Create())),
    Result.Instructions);
end;

class function ScriptTool.Create(
  const Hook: ITokenHandler;
  const Code: String;
  const Tokens: ITokenTable): IEvaluator;
begin
  Result := TSilEvaluator.Create(Hook);
  ParseStatement(
    SilLexer.Tool.TokenLexer(Code, SilSfScriptTokens.DefineTokens(Tokens), SilSfScriptTokens.DefineTokens(SilLexer.Dictionary.Create())),
    Result.Instructions);
end;

class function ScriptTool.Execute(
  const Hook: ITokenHandler;
  const Code: String;
  const Tokens: ITokenTable): Variant;
begin
  Result := Create(Hook, Code, Tokens).Execute();
end;

class function ScriptTool.Execute(
  const Hook: ITokenHandler;
  const Stream: IStream;
  const Tokens: ITokenTable): Variant;
begin
  Result := Create(Hook, Stream, Tokens).Execute();
end;

end.
