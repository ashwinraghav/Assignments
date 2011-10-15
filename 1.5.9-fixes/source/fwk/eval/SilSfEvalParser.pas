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

unit SilSfEvalParser;

interface

uses
  Sil,
  SilLiTypeInfo, 
  SilTokens,
  SilLexer,
  SilSeEvalParser,
  SilSiEvaluator;

function ParseExpression(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;

implementation

uses
  SilScEvalTokens,
  SilSmEvalOperators,
  SilSgEvalOperators,
  SilSfEvalParserHelpers;

{ forwards }

function ParseExpressionList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAssignment(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAssignmentList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseLogicalOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseLogicalOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseLogicalAnd(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseLogicalAndList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseInclusiveOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseInclusiveOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseExclusiveOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseExclusiveOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAnd(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAndList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseEquality(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseEqualityList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseRelational(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseRelationalList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseShift(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseShiftList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAddition(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseAdditionList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseMultiplication(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseMultiplicationList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseUnary(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParsePostfix(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParsePostfixList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParsePrimary(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseArguments(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseArgumentList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

function ParseArgument(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes; forward;

{ helpers }

function DoParseIdentifier(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  Tok := Lexer.Next();
  
  Instructions.AddPush(Tok);

  Result.DataType := TypeInfo(Tok);
end;

function DoParseLiteral(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Value: Variant;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  case Token.ValueOf(Tok.Symbol) of
    tvBoolean:  Value := DoParseBoolean(Tok.Lexema);
    tvChar:     Value := DoParseChar(Tok.Lexema);
    tvInteger:  Value := DoParseInteger(Tok.Lexema);
    tvFloat:    Value := DoParseFloat(Tok.Lexema);
    tvString:   Value := DoParseString(Tok.Lexema);
    tvDateTime: Value := DoParseDateTime(Tok.Lexema);
    else        Value := Sil.Vart.Null;
  end;

  if Sil.Vart.IsOK(Value) then
  begin
    Tok.Value := Value;
    Tok.Data['type'] := TypeInfo(Tok.Symbol);
  end;
  
  Instructions.AddPush(Tok);

  Result.DataType := TypeInfo(Tok);
end;

function DoParseSubexpression(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Check(TOKEN_LPAREN);
  Result := ParseExpression(Lexer, Instructions, Attributes);
  Lexer.Check(TOKEN_RPAREN);
end;

{ globales }

function ParseExpression(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Assignment: REvalAttributes;
begin
  Assignment := ParseAssignment(Lexer, Instructions, Attributes);

  Result.DataType := Assignment.DataType;
    
  Result := ParseExpressionList(Lexer, Instructions, Result);
end;

function DoParseExpression(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Expression: REvalAttributes;
begin
  Lexer.Check(TOKEN_COMMA); // consumo el literal de la entrada

  Expression := ParseExpression(Lexer, Instructions, Attributes);
  
(*)
  if DataType.TypeKind <> Expression.DataType.TypeKind then
  begin
    Result.DataType :=

  if not IsSameType(DataType, Expression.DataType) then
    DoAddOperator(TOp


  end else
    Result.DataType := DataType;(*)

  Instructions.AddOperator(TOpComma, Tok.Lexema, Result.DataType, Expression.DataType);
end;

function ParseExpressionList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_COMMA:      Result := DoParseExpression(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseAssignment(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseLogicalOr(Lexer, Instructions, Attributes);
  Result := ParseAssignmentList(Lexer, Instructions, Result);
end;

function DoParseQuestion(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Check(TOKEN_QUESTION);
  Result := ParseExpression(Lexer, Instructions, Attributes);
  Lexer.Check(TOKEN_COLON);

  Result := ParseLogicalOr(Lexer, Instructions, Result);
  Result := ParseAssignmentList(Lexer, Instructions, Result);
  
  Instructions.AddOperator(TOpQuestion, Tok.Lexema);
end;

function DoParseAssign(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  Result := ParseLogicalOr(Lexer, Instructions, Attributes);
  Result := ParseAssignmentList(Lexer, Instructions, Result);
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
end;

function ParseAssignmentList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_QUESTION:   Result := DoParseQuestion(Lexer, Tok, Instructions, Attributes);
    TOKEN_ASSIGN_OP:  Result := DoParseAssign(Lexer, Tok, Instructions, Attributes);
    else              {@};
  end;
end;

function ParseLogicalOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseLogicalAnd(Lexer, Instructions, Attributes);
  Result := ParseLogicalOrList(Lexer, Instructions, Result);
end;

function DoParseLor(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  Result := ParseLogicalAnd(Lexer, Instructions, Attributes);
  Instructions.AddOperator(TOpLogicalOr, Tok.Lexema);
  Result := ParseLogicalOrList(Lexer, Instructions, Result);
end;

function ParseLogicalOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_OP_LOR:     Result := DoParseLor(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseLogicalAnd(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseInclusiveOr(Lexer, Instructions, Attributes);
  Result := ParseLogicalAndList(Lexer, Instructions, Result);
end;

function DoParseLand(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada

  Result := ParseInclusiveOr(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpLogicalAnd, Tok.Lexema);
  
  Result := ParseLogicalAndList(Lexer, Instructions, Result);
end;

function ParseLogicalAndList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_OP_LAND:    Result := DoParseLand(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseInclusiveOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseExclusiveOr(Lexer, Instructions, Attributes);
  Result := ParseInclusiveOrList(Lexer, Instructions, Result);
end;

function DoParseOr(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseExclusiveOr(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(TOpArithmeticOr, Tok.Lexema);

  Result := ParseInclusiveOrList(Lexer, Instructions, Result);
end;

function ParseInclusiveOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_OP_OR:      Result := DoParseOr(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseExclusiveOr(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseAnd(Lexer, Instructions, Attributes);
  Result := ParseExclusiveOrList(Lexer, Instructions, Result);
end;

function DoParseXor(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseAnd(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpArithmeticXor, Tok.Lexema);
  
  Result := ParseExclusiveOrList(Lexer, Instructions, Result);
end;

function ParseExclusiveOrList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_OP_XOR:     Result := DoParseXor(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseAnd(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseEquality(Lexer, Instructions, Attributes);
  Result := ParseAndList(Lexer, Instructions, Result);
end;

function DoParseAnd(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseEquality(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpArithmeticAnd, Tok.Lexema);
  
  Result := ParseAndList(Lexer, Instructions, Result);
end;

function ParseAndList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_OP_AND:     Result := DoParseAnd(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function ParseEquality(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseRelational(Lexer, Instructions, Attributes);
  Result := ParseEqualityList(Lexer, Instructions, Result);
end;

function DoParseEquality(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseRelational(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
  
  Result := ParseEqualityList(Lexer, Instructions, Result);
end;

function ParseEqualityList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_EQUALITY_OP:  Result := DoParseEquality(Lexer, Tok, Instructions, Attributes);
    else                Result := Attributes;
  end;
end;

function ParseRelational(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseShift(Lexer, Instructions, Attributes);
  Result := ParseRelationalList(Lexer, Instructions, Result);
end;

function DoParseRelational(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseShift(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
  
  Result := ParseRelationalList(Lexer, Instructions, Result);
end;

function ParseRelationalList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_RELATIONAL_OP:  Result := DoParseRelational(Lexer, Tok, Instructions, Attributes);
    else                  Result := Attributes;
  end;
end;

function ParseShift(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseAddition(Lexer, Instructions, Attributes);
  Result := ParseShiftList(Lexer, Instructions, Result);
end;

function DoParseShift(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseAddition(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
  
  Result := ParseShiftList(Lexer, Instructions, Result);
end;

function ParseShiftList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_SHIFT_OP:   Result := DoParseShift(Lexer, Tok, Instructions, Attributes) 
    else              Result := Attributes;
  end;
end;

function ParseAddition(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseMultiplication(Lexer, Instructions, Attributes);
  Result := ParseAdditionList(Lexer, Instructions, Result);
end;

function DoParseSum(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseMultiplication(Lexer, Instructions, Attributes);

  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);

  Result := ParseAdditionList(Lexer, Instructions, Result);
end;

function ParseAdditionList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_SUM_OP:     Result := DoParseSum(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function DoParseMultiplication(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseUnary(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
  
  Result := ParseMultiplicationList(Lexer, Instructions, Result);
end;
  
function ParseMultiplication(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseUnary(Lexer, Instructions, Attributes);
  Result := ParseMultiplicationList(Lexer, Instructions, Result);
end;

function ParseMultiplicationList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_MULT_OP:    Result := DoParseMultiplication(Lexer, Tok, Instructions, Attributes);
    else              Result := Attributes;
  end;
end;

function DoParsePreincrement(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseMultiplication(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol, True), Tok.Lexema);
end;

function DoParseUnary(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParseMultiplication(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol, True), Tok.Lexema);
end;

function ParseUnary(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_INCR_OP:
      Result := DoParsePreincrement(Lexer, Tok, Instructions, Attributes);
    TOKEN_UNARY_OP, TOKEN_SUM_OP:
      Result := DoParseUnary(Lexer, Tok, Instructions, Attributes);
    else
      Result := ParsePostfix(Lexer, Instructions, Attributes);
  end;
end;

function ParsePostfix(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParsePrimary(Lexer, Instructions, Attributes);
  Result := ParsePostfixList(Lexer, Instructions, Result);
end;

function DoParseIndex(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Check(TOKEN_LBRACKET);

  Result := ParseExpression(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpArrayIndex, '[]');
  Lexer.Check(TOKEN_RBRACKET);

  Result := ParsePostfixList(Lexer, Instructions, Result);
end;

function DoParseArguments(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Instructions.AddOperator(TOpFunctionFrame, '@');

  if DoGetSymbol(Lexer) <> TOKEN_RPAREN then // parche!!!
    Result := ParseArguments(Lexer, Instructions, Attributes);
    
  Lexer.Check(TOKEN_RPAREN);
end;

function DoParseCall(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Check(TOKEN_LPAREN);
  
  Result := DoParseArguments(Lexer, Instructions, Attributes);

  Instructions.AddOperator(TOpFunctionCall, '()');
  
  Result := ParsePostfixList(Lexer, Instructions, Result);
end;

function DoParsePostincrement(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada
  
  Result := ParsePostfixList(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
end;

function DoParseMember(
  const Lexer: ITokenLexer;
  const Tok: IToken;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Next(); // consumo el literal de la entrada (ARROW)

  Result := DoParseIdentifier(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(GetOperatorKind(Tok.Symbol), Tok.Lexema);
  
  Result := ParsePostfixList(Lexer, Instructions, Result);  
end;

function ParsePostfixList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
begin
  case DoGetSymbol(Lexer, Tok) of
    TOKEN_LBRACKET:     Result := DoParseIndex(Lexer, Instructions, Attributes);
    TOKEN_LPAREN:       Result := DoParseCall(Lexer, Instructions, Attributes);
    TOKEN_INCR_OP:      Result := DoParsePostincrement(Lexer, Tok, Instructions, Attributes);
    TOKEN_ARROW_OP:     Result := DoParseMember(Lexer, Tok, Instructions, Attributes);
    TOKEN_DOT_OP:       Result := DoParseMember(Lexer, Tok, Instructions, Attributes);
    else                Result := Attributes;
  end;
end;

function ParsePrimary(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
var
  Tok: IToken;
  Symbol: TToken;
begin
  Symbol := DoGetSymbol(Lexer, Tok);
  case Symbol of
    TOKEN_LPAREN:
        Result := DoParseSubexpression(Lexer, Instructions, Attributes);
    else if Token.TypeOf(Symbol) in [ttAny, ttIdentifier]  then
        Result := DoParseIdentifier(Lexer, Instructions, Attributes)
    else if Token.TypeOf(Symbol) in [ttAny, ttLiteral] then
        Result := DoParseLiteral(Lexer, Tok, Instructions, Attributes)
    else if not Token.IsEqual(Symbol, TOKEN_EOF) then
        raise Sil.Error.Create('Syntax error: Invalid token (''%s'')', [Tok.Lexema]);
  end;
end;

function ParseArguments(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseArgument(Lexer, Instructions, Attributes);
  Result := ParseArgumentList(Lexer, Instructions, Result);
end;

function DoParseArgument(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Lexer.Check(TOKEN_COMMA);
  Result := ParseArguments(Lexer, Instructions, Attributes);
end;
                         
function ParseArgumentList(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  case DoGetSymbol(Lexer) of
    TOKEN_COMMA:    Result := DoParseArgument(Lexer, Instructions, Attributes);
    else            Result := Attributes;
  end;
end;

function ParseArgument(
  const Lexer: ITokenLexer;
  const Instructions: IEvalInstructionList;
  const Attributes: REvalAttributes): REvalAttributes;
begin
  Result := ParseAssignment(Lexer, Instructions, Attributes);
  
  Instructions.AddOperator(TOpFunctionParam, '@');
end;

end.
