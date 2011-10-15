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

unit SilSfScriptParser;

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSiEvaluator;

procedure ParseStatement(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseSubexpression(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseWhile(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseFor(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseRepeatUntil(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseDoWhile(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
procedure ParseSwitch(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);

implementation

uses
  SilScEvalTokens,
  SilScScriptTokens,
  SilSmEvalOperators,
  SilSfEvalParserHelpers,
  SilSeEvalParser,
  SilSfEvalParser;

type
  TOpCaseTest = class(TOpEqual)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

procedure DoParseStatement(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList); forward;

function DoParseBegin(const Lexer: ITokenLexer; out Closer: TToken): Boolean;
var
  Tok: IToken;
begin
  Result := Token.InValue(DoGetSymbol(Lexer, Tok), [TOKEN_STMT_BEGIN, TOKEN_LCURLY]);
  if Result then
  begin
    DoAdvance(Lexer);
    if Tok.Symbol = TOKEN_STMT_BEGIN then
      Closer := TOKEN_STMT_END else
      Closer := TOKEN_RCURLY;
  end;
end;

function DoParseEnd(const Lexer: ITokenLexer; const Closer: TToken): Boolean; overload;
begin
  Result := DoGetSymbol(Lexer) = Closer;
end;

function DoParseEnd(const Lexer: ITokenLexer; const Closers: array of TToken): Boolean; overload;
begin
  Result := Token.InValue(DoGetSymbol(Lexer), Closers);
end;

procedure DoParseStatements(const Lexer: ITokenLexer; const Closer: TToken; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList); overload;
begin
  Instructions.AddNop('//  __BLOCK_BEGIN::');
  while not DoParseEnd(Lexer, Closer) do
    DoParseStatement(Lexer, AtExit, Instructions);
  Instructions.AddNop('//  __BLOCK_END::');
end;

procedure DoParseStatements(const Lexer: ITokenLexer; const Closers: array of TToken; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList); overload;
begin
  while not DoParseEnd(Lexer, Closers) do
    DoParseStatement(Lexer, AtExit, Instructions);
end;

procedure DoParseExpression(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  Attributes: REvalAttributes;
begin
  Instructions.AddNop('//  __EXPRESSION::');
  Attributes := ParseExpression(Lexer, Instructions, Attributes);
  DoCheck(TOKEN_SEMICOLON, Lexer);
  Instructions.AddPop;
end;

procedure DoParseBreak(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
begin
  Instructions.AddNop('//  __BREAK::');
  DoCheck(TOKEN_SEMICOLON, Lexer);
  Instructions.AddJump(AtExit)
end;

procedure DoParseReturnExpression(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  Attributes: REvalAttributes;
begin
  case DoGetSymbol(Lexer) of
    TOKEN_SEMICOLON:  { parche!!!} ; 
    else              Attributes := ParseExpression(Lexer, Instructions, Attributes);
  end;
end;

procedure DoParseReturn(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
begin
  Instructions.AddNop('//  __RETURN::');
  DoParseReturnExpression(Lexer, Instructions);
  DoCheck(TOKEN_SEMICOLON, Lexer);
  Instructions.AddJump(AtExit)
end;

procedure DoParseElse(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
begin
  Instructions.AddNop('//  __IF_ELSE::');
  case DoGetSymbol(Lexer) of
    TOKEN_STMT_ELSE:  DoParseStatement(DoAdvance(Lexer), AtExit, Instructions);
    else              {@};
  end;
end;

procedure DoParseIf(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
var
  AtElse: IEvalLabel;
begin
  Instructions.AddNop('//  __IF::');
  ParseSubexpression(Lexer, Instructions);
  AtElse := Instructions.DefineLabel('@@else');
  Instructions.AddJump(AtElse, goIfFalse);
  Instructions.AddNop('//  __IF_THEN::');
  DoParseStatement(Lexer, AtExit, Instructions);
  Instructions.PlaceLabel(AtElse);
  DoParseElse(Lexer, AtExit, Instructions);
  Instructions.AddNop('//  __IF_EXIT::');
end;

procedure DoParseStatementBody(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
begin
  case DoGetSymbol(Lexer) of
    TOKEN_STMT_IF:      DoParseIf(DoAdvance(Lexer), AtExit, Instructions);
    TOKEN_STMT_FOR:     ParseFor(DoAdvance(Lexer), Instructions);
    TOKEN_STMT_WHILE:   ParseWhile(DoAdvance(Lexer), Instructions);
    TOKEN_STMT_DO:      ParseDoWhile(DoAdvance(Lexer), Instructions);
    TOKEN_STMT_REPEAT:  ParseRepeatUntil(DoAdvance(Lexer), Instructions);
    TOKEN_STMT_RETURN:  DoParseReturn(DoAdvance(Lexer), AtExit, Instructions);
    TOKEN_STMT_SWITCH:  ParseSwitch(DoAdvance(Lexer), Instructions);
    TOKEN_STMT_BREAK:   DoParseBreak(DoAdvance(Lexer), AtExit, Instructions);
    TOKEN_RCURLY,       // parche!!! :(
    TOKEN_STMT_END:     {@};
    else                DoParseExpression(Lexer, Instructions);
  end;
end;

procedure DoParseStatement(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Instructions: IEvalInstructionList);
var
  Closer: TToken;
begin
  if DoParseBegin(Lexer, Closer) then
  begin
    DoParseStatements(Lexer, Closer, AtExit, Instructions);
    DoCheck(Closer, Lexer);
  end else
    DoParseStatementBody(Lexer, AtExit, Instructions);
end;

procedure ParseStatement(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  AtExit: IEvalLabel;
begin
  AtExit := Instructions.DefineLabel('@stmt@exit');

  DoParseStatement(Lexer, AtExit, Instructions);

  Instructions.PlaceLabel(AtExit);
end;

procedure ParseSubexpression(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  Attributes: REvalAttributes;
begin
  DoCheck(TOKEN_LPAREN, Lexer);
  ParseExpression(Lexer, Instructions, Attributes);
  DoCheck(TOKEN_RPAREN, Lexer);
end;

procedure ParseWhile(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  AtLoop, AtExit: IEvalLabel;
begin
  Instructions.AddNop('//  __WHILE::');
  AtLoop := Instructions.AddLabel('@while@loop');
  AtExit := Instructions.DefineLabel('@while@exit');

  ParseSubexpression(Lexer, Instructions);

  Instructions.AddJump(AtExit, goIfFalse);

  Instructions.AddNop('//  __WHILE_DO::');
  DoParseStatement(Lexer, AtExit, Instructions);

  Instructions.AddJump(AtLoop);
  Instructions.PlaceLabel(AtExit)
end;

procedure ParseFor(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  AtLoop, AtExit, AtStmt, AtIncr: IEvalLabel;
  Attributes: REvalAttributes;
begin
  Instructions.AddNop('//  __FOR::');

  DoCheck(TOKEN_LPAREN, Lexer);

  Instructions.AddNop('//  __FOR_INIT::');

  ParseExpression(Lexer, Instructions, Attributes);

  Instructions.AddPop; // saca el resultado de la expresion anterior

  AtLoop := Instructions.DefineLabel('@for@loop');
  AtExit := Instructions.DefineLabel('@for@exit');
  AtStmt := Instructions.DefineLabel('@for@stmt');
  AtIncr := Instructions.DefineLabel('@for@incr');

  DoCheck(TOKEN_SEMICOLON, Lexer);

  Instructions.AddNop('//  __FOR_CHECK::');
  Instructions.PlaceLabel(AtLoop);
  
  ParseExpression(Lexer, Instructions, Attributes);

  Instructions.AddJump(AtExit, goIfFalse);

  Instructions.AddJump(AtStmt);

  DoCheck(TOKEN_SEMICOLON, Lexer);
  
  Instructions.AddNop('//  __FOR_INCR::');
  Instructions.PlaceLabel(AtIncr);

  ParseExpression(Lexer, Instructions, Attributes);
  Instructions.AddPop;
  Instructions.AddJump(AtLoop);

  DoCheck(TOKEN_RPAREN, Lexer);

  Instructions.AddNop('//  __FOR_STMT::');
  Instructions.PlaceLabel(AtStmt);

  DoParseStatement(Lexer, AtExit, Instructions);

  Instructions.AddJump(AtIncr);

  Instructions.AddNop('//  __FOR_EXIT::');
  Instructions.PlaceLabel(AtExit)
end;

procedure ParseRepeatUntil(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  AtLoop, AtExit: IEvalLabel;
begin
  Instructions.AddNop('//  __REPEAT::');
  AtLoop := Instructions.AddLabel('@repeat@loop');
  AtExit := Instructions.DefineLabel('@repeat@exit');

  DoParseStatements(Lexer, TOKEN_STMT_UNTIL, AtExit, Instructions);

  DoCheck(TOKEN_STMT_UNTIL, Lexer);
  Instructions.AddNop('//  __REPEAT_UNTIL::');
  ParseSubexpression(Lexer, Instructions);

  Instructions.AddJump(AtLoop, goIfFalse);
  Instructions.PlaceLabel(AtExit);
  Instructions.AddNop('//  __REPEAT_EXIT::');
end;

procedure ParseDoWhile(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  AtLoop, AtExit: IEvalLabel;
begin
  Instructions.AddNop('//  __DO::');
  AtLoop := Instructions.AddLabel('@do@loop');
  AtExit := Instructions.DefineLabel('@do@exit');

  DoParseStatement(Lexer, AtExit, Instructions);
  DoCheck(TOKEN_STMT_WHILE, Lexer);

  Instructions.AddNop('//  __DO_WHILE::');

  ParseSubexpression(Lexer, Instructions);

  Instructions.AddJump(AtLoop, goIfTrue);
  Instructions.PlaceLabel(AtExit);

  Instructions.AddNop('//  __DO_EXIT::');
end;

procedure DoParseSwitchAction(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closers: array of TToken; const Instructions: IEvalInstructionList);
begin
  DoCheck(TOKEN_COLON, Lexer);

  Instructions.AddNop('//  __SWITCH_CASE_ACTIONS_BEGIN::');

  DoParseStatements(Lexer, Closers, AtExit, Instructions);
  
  Instructions.AddNop('//  __SWITCH_CASE_ACTIONS_END::');
end;

procedure DoParseSwitchItem(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closer: TToken; const Instructions: IEvalInstructionList);
var
  AtEnd: IEvalLabel;
  Attributes: REvalAttributes;
begin
  Instructions.AddNop('//  __SWITCH_CASE_BEGIN::');
  AtEnd := Instructions.DefineLabel('@switch@end');
  
  ParseExpression(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpCaseTest, 'test');
  Instructions.AddJump(AtEnd, goIfFalse);

  DoParseSwitchAction(Lexer, AtExit, [Closer, TOKEN_STMT_CASE, TOKEN_STMT_DEFAULT], Instructions);

  Instructions.PlaceLabel(AtEnd);
  Instructions.AddNop('//  __SWITCH_CASE_END::');
end;

procedure DoParseSwitchItems(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closer: TToken; const Instructions: IEvalInstructionList);
var
  Tok: IToken;
begin
  while Token.IsEqual(DoGetSymbol(Lexer, Tok), TOKEN_STMT_CASE) do
    DoParseSwitchItem(DoAdvance(Lexer), AtExit, Closer, Instructions);
end;

procedure DoParseSwitchDefaultAction(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closers: array of TToken; const Instructions: IEvalInstructionList);
begin
  Instructions.AddNop('//  __SWITCH_CASE_DEFAULT::');
  Instructions.AddPop();
  DoParseSwitchAction(Lexer, AtExit, Closers, Instructions);
end;

procedure DoParseSwitchDefault(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closer: TToken; const Instructions: IEvalInstructionList);
begin
  case DoGetSymbol(Lexer) of
    TOKEN_STMT_DEFAULT: DoParseSwitchDefaultAction(DoAdvance(Lexer), AtExit, [Closer], Instructions);
    else                {@};
  end;
end;

procedure DoParseSwitchBody(const Lexer: ITokenLexer; const AtExit: IEvalLabel; const Closer: TToken; const Instructions: IEvalInstructionList);
begin
  DoParseSwitchItems(Lexer, AtExit, Closer, Instructions);
  DoParseSwitchDefault(Lexer, AtExit, Closer, Instructions);
end;

procedure ParseSwitch(const Lexer: ITokenLexer; const Instructions: IEvalInstructionList);
var
  Closer: TToken;
  AtExit: IEvalLabel;
begin
  Instructions.AddNop('//  __SWITCH::');
  AtExit := Instructions.DefineLabel('@switch@exit');

  ParseSubexpression(Lexer, Instructions);

  if not DoParseBegin(Lexer, Closer) then
    raise Sil.Error.Create('Syntax error: Invalid token (%s)', [Lexer.Current.Lexema]);

  DoParseSwitchBody(Lexer, AtExit, Closer, Instructions);

  if not DoParseEnd(Lexer, Closer) then
    raise Sil.Error.Create('Syntax error: Invalid token (%s)', [Lexer.Current.Lexema]);

  Instructions.PlaceLabel(AtExit);
  Instructions.AddNop('//  __SWITCH_EXIT::');
end;

{ TOpCaseTest }

procedure TOpCaseTest.Execute(const Stack: IEvaluationStack);
var
  Left, Right: IEvaluationItem;
  Test: Boolean;
begin
  Right := Stack.Pop;
  Left := Stack.Pop;
  Test := Execute(Left, Right);
  if not Test then Stack.PushItem(Left);
  Stack.PushValue(Test);
end;

end.
