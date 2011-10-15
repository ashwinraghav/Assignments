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

unit SilSgEvalOperators;

interface

uses
  Sil,
  SilTokens,
  SilSiEvaluator;

function GetOperatorKind(const Symbol: TToken; Unary: Boolean = False): TSilOperatorKind;

implementation

uses
  SilScEvalTokens, SilSmEvaluator, SilSmEvalOperators;

function GetOperatorKind(const Symbol: TToken; Unary: Boolean): TSilOperatorKind;
begin
  Result := TSilOperator;
  case Token.ID(Symbol) of
    ID_OP_LOR:          Result := TOpLogicalOr;
    ID_OP_LAND:         Result := TOpLogicalAnd;
    ID_OP_OR:           Result := TOpArithmeticOr;
    ID_OP_XOR:          Result := TOpArithmeticXor;
    ID_OP_AND:          Result := TOpArithmeticAnd;
    ID_OP_EQUAL:        Result := TOpEqual;
    ID_OP_NOTEQUAL:     Result := TOpNotEqual;
    ID_OP_LESS:         Result := TOpLesser;
    ID_OP_GREATER:      Result := TOpGreater;
    ID_OP_LESSEQ:       Result := TOpLesserEqual;
    ID_OP_GREATEREQ:    Result := TOpGreaterEqual;
    ID_OP_SHIFT_RIGHT:  Result := TOpShiftRight;
    ID_OP_SHIFT_LEFT:   Result := TOpShiftLeft;
    ID_OP_PLUS:         if Unary then Result := TOpUnaryPlus  else Result := TOpPlus;
    ID_OP_MINUS:        if Unary then Result := TOpUnaryMinus else Result := TOpMinus;
    ID_OP_MUL:          Result := TOpMultiply;
    ID_OP_DIV:          Result := TOpDivide;
    ID_OP_MOD:          Result := TOpModulus;
    ID_OP_NEG:          if Unary then Result := TOpArithmeticNot;
    ID_OP_NOT:          if Unary then Result := TOpLogicalNot;
    ID_OP_INC:          if Unary then Result := TOpPreIncrement else Result := TOpPostIncrement;
    ID_OP_DEC:          if Unary then Result := TOpPreDecrement else Result := TOpPostDecrement;
    ID_OP_ASSIGN:       Result := TOpAssign;
    ID_OP_ASSIGN_MUL:   Result := TOpAssignMultiply;
    ID_OP_ASSIGN_DIV:   Result := TOpAssignDivide;
    ID_OP_ASSIGN_MOD:   Result := TOpAssignModulus;
    ID_OP_ASSIGN_PLUS:  Result := TOpAssignPlus;
    ID_OP_ASSIGN_MINUS: Result := TOpAssignMinus;
    ID_OP_ASSIGN_SHR:   Result := TOpAssignShiftRight;
    ID_OP_ASSIGN_SHL:   Result := TOpAssignShiftLeft;
    ID_OP_ASSIGN_AND:   Result := TOpAssignAnd;
    ID_OP_ASSIGN_XOR:   Result := TOpAssignXor;
    ID_OP_ASSIGN_OR:    Result := TOpAssignOr;
    ID_OP_CALL:         Result := TOpFunctionCall;
    ID_OP_INDEX:        Result := TOpArrayIndex;
    ID_OP_ARROW:        Result := TOpMember;
    ID_OP_DOT:          Result := TOpMember;
  end;
end;

end.
