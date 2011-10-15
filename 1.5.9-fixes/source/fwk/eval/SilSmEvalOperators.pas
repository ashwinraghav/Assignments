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

unit SilSmEvalOperators;

interface

uses
  Sil,
  SilTokens,
  SilSiEvaluator,
  SilSmEvaluator;

type
  TOpPlus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpMinus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpMultiply = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpDivide = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpModulus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpArithmeticOr = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpArithmeticAnd = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpArithmeticXor = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpArithmeticNot = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpLogicalOr  = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpLogicalAnd = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpLogicalNot = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpAssign = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignPlus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignMinus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignMultiply = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignDivide = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignModulus = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignOr = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignAnd = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignXor = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignShiftRight = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpAssignShiftLeft = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpEqual = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpNotEqual = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpGreater = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpLesser = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpLesserEqual = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpGreaterEqual = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpShiftRight = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpShiftLeft = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpPostIncrement = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpPostDecrement = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpPreIncrement = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpPreDecrement = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpComma = class(TSilBinaryOp)
    function Execute(const Left, Right: IEvaluationItem): Variant; override;
  end;

  TOpQuestion = class(TSilOperator)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

  TOpUnaryPlus  = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpUnaryMinus = class(TSilUnaryOp)
    function Execute(const Item: IEvaluationItem): Variant; override;
  end;

  TOpFunctionFrame = class(TSilUnaryOp)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

  TOpFunctionParam = class(TSilBinaryOp)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

  TOpFunctionCall = class(TSilOperator)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

  TOpArrayIndex = class(TSilOperator)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;
  
  TOpMember = class(TSilOperator)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

  TOpConversion = class(TSilOperator)
    procedure Execute(const Stack: IEvaluationStack); override;
  end;

implementation

{ TOpPlus }

function TOpPlus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Left.AsValue + Right.AsValue;
end;

{ TOpMinus }

function TOpMinus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Left.AsValue - Right.AsValue;
end;

{ TOpMultiply }

function TOpMultiply.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Left.AsValue * Right.AsValue;
end;

{ TOpDivide }

function TOpDivide.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Left.AsValue / Right.AsValue;
end;

{ TOpModulus }

function TOpModulus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Left.AsValue mod Right.AsValue;
end;

{ TOpArithmeticOr }

function TOpArithmeticOr.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue) or Integer(Right.AsValue);
end;

{ TOpArithmeticAnd }

function TOpArithmeticAnd.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue) and Integer(Right.AsValue);
end;

{ TOpArithmeticXor }

function TOpArithmeticXor.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue) xor Integer(Right.AsValue);
end;

{ TOpArithmeticNot }

function TOpArithmeticNot.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := not Integer(Item.AsValue);
end;

{ TOpLogicalOr }

function TOpLogicalOr.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := (Integer(Left.AsValue) <> 0) or (Integer(Right.AsValue) <> 0);
end;

{ TOpLogicalAnd }

function TOpLogicalAnd.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := (Integer(Left.AsValue) <> 0) and (Integer(Right.AsValue) <> 0);
end;

{ TOpLogicalNot }

function TOpLogicalNot.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := Integer(Item.AsValue) = 0;
end;

{ TOpAssign }

function TOpAssign.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignPlus }

function TOpAssignPlus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Left.AsValue + Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignMinus }

function TOpAssignMinus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Left.AsValue - Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignMultiply }

function TOpAssignMultiply.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Left.AsValue * Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignDivide }

function TOpAssignDivide.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Left.AsValue / Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignModulus }

function TOpAssignModulus.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Left.AsValue mod Right.AsValue;
  Result := Left.AsValue;
end;

{ TOpAssignOr }

function TOpAssignOr.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Integer(Left.AsValue) or Integer(Right.AsValue);
  Result := Left.AsValue;
end;

{ TOpAssignAnd }

function TOpAssignAnd.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Integer(Left.AsValue) and Integer(Right.AsValue);
  Result := Left.AsValue;
end;

{ TOpAssignXor }

function TOpAssignXor.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Integer(Left.AsValue) xor Integer(Right.AsValue);
  Result := Left.AsValue;
end;

{ TOpAssignShiftRight }

function TOpAssignShiftRight.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Integer(Left.AsValue) shr Integer(Right.AsValue);
  Result := Left.AsValue;
end;

{ TOpAssignShiftLeft }

function TOpAssignShiftLeft.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Left.AsValue := Integer(Left.AsValue) shl Integer(Right.AsValue);
  Result := Left.AsValue;
end;

{ TOpEqual }

function TOpEqual.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue = Right.AsValue) <> 0;
end;

{ TOpNotEqual }

function TOpNotEqual.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue <> Right.AsValue) <> 0;
end;

{ TOpGreater }

function TOpGreater.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue > Right.AsValue) <> 0;
end;

{ TOpLesser }

function TOpLesser.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue < Right.AsValue) <> 0;
end;

{ TOpLesserEqual }

function TOpLesserEqual.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue <= Right.AsValue) <> 0;
end;

{ TOpGreaterEqual }

function TOpGreaterEqual.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue >= Right.AsValue) <> 0;
end;

{ TOpShiftRight }

function TOpShiftRight.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue) shr Integer(Right.AsValue);
end;

{ TOpShiftLeft }

function TOpShiftLeft.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Integer(Left.AsValue) shl Integer(Right.AsValue);
end;

{ TOpPostIncrement }

function TOpPostIncrement.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := Item.AsValue;
  Item.AsValue := Result + 1;
end;

{ TOpPostDecrement }

function TOpPostDecrement.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := Item.AsValue;
  Item.AsValue := Result - 1;
end;

{ TOpPreIncrement }

function TOpPreIncrement.Execute(const Item: IEvaluationItem): Variant;
begin
  Item.AsValue := Item.AsValue + 1;
  Result := Item.AsValue;
end;

{ TOpPreDecrement }

function TOpPreDecrement.Execute(const Item: IEvaluationItem): Variant;
begin
  Item.AsValue := Item.AsValue - 1;
  Result := Item.AsValue;
end;

{ TOpComma }

function TOpComma.Execute(const Left, Right: IEvaluationItem): Variant;
begin
  Result := Right.AsValue;
end;

{ TOpQuestion }

procedure TOpQuestion.Execute(const Stack: IEvaluationStack);
var
  C, T, F: IEvaluationItem;
begin
  F := Stack.Pop;
  T := Stack.Pop;
  C := Stack.Pop;
  if Integer(C.AsValue) <> 0 then
    Stack.PushValue(T.AsValue) else
    Stack.PushValue(F.AsValue);
end;

{ TOpUnaryPlus }

function TOpUnaryPlus.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := + Item.AsValue;
end;

{ TOpUnaryMinus }

function TOpUnaryMinus.Execute(const Item: IEvaluationItem): Variant;
begin
  Result := - Item.AsValue;
end;

{ TOpFunctionFrame }

procedure TOpFunctionFrame.Execute(const Stack: IEvaluationStack);
var
  Proc: IToken;
  Frame: IEvalFunctionFrame;
begin
  Proc := Stack.Top.AsToken;                // id de la funcion a llamar

  Frame := TSilFunctionFrame.Create(Proc);
  Stack.PushObject(Frame);                  // y pongo una instancia para guardar los argumentos
end;

{ TOpFunctionParam }

procedure TOpFunctionParam.Execute(const Stack: IEvaluationStack);
var
  Frame: IEvalFunctionFrame;
  Param: Variant;
begin
  Param := Stack.Pop.AsValue;
  Frame := Stack.Top.AsObject as IEvalFunctionFrame;
  Frame.Add(Param);
end;

{ TOpFunctionCall }

procedure TOpFunctionCall.Execute(const Stack: IEvaluationStack);
var
  Frame: IEvalFunctionFrame;
begin
  Frame := Stack.Pop.AsObject as IEvalFunctionFrame;
  Stack.Pop;

  Stack.PushValue(Frame.Invoke());
end;

{ TOpArrayIndex }

procedure TOpArrayIndex.Execute(const Stack: IEvaluationStack);
begin

end;

{ TOpMember }

procedure TOpMember.Execute(const Stack: IEvaluationStack);
var
  Instance: IToken;
  Member: IToken;
begin
  Member := Stack.Pop.AsToken;
  Instance := Stack.Pop.AsToken;

  

end;

{ TOpConversion }

procedure TOpConversion.Execute(const Stack: IEvaluationStack);
begin

end;

end.
