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

unit SilScript;

interface

{$include Defines.inc}

{$IFDEF D60}

uses
  SilSiEvaluator,
  SilStScript;

type
  TInstructionKind      = SilSiEvaluator.TInstructionKind;

const
  ikPush                = TInstructionKind(SilSiEvaluator.ikPush);
  ikPop                 = TInstructionKind(SilSiEvaluator.ikPop);
  ikCopy                = TInstructionKind(SilSiEvaluator.ikCopy);
  ikOperator            = TInstructionKind(SilSiEvaluator.ikOperator);

type
  TItemKind             = SilSiEvaluator.TItemKind;

const
  ikValue               = TItemKind(SilSiEvaluator.ikValue);
  ikToken               = TItemKind(SilSiEvaluator.ikToken);
  ikObject              = TItemKind(SilSiEvaluator.ikObject);

type
  TIdentifierKind       = SilSiEvaluator.TIdentifierKind;

const
  ikConstant            = TIdentifierKind(SilSiEvaluator.ikConstant); 
  ikVariable            = TIdentifierKind(SilSiEvaluator.ikVariable);  
  ikFunction            = TIdentifierKind(SilSiEvaluator.ikFunction);  

type
  TSilOperatorKind      = SilSiEvaluator.TSilOperatorKind;
  TSilOperatorType      = SilSiEvaluator.TSilOperatorType;

type
  RFunctionMsg          = SilSiEvaluator.RFunctionMsg;

type
  IEvaluator            = SilSiEvaluator.IEvaluator;
  IEvalMachine          = SilSiEvaluator.IEvalMachine;
  ITokenHandler         = SilSiEvaluator.ITokenHandler;
  IEvaluationStack      = SilSiEvaluator.IEvaluationStack;
  IEvaluationItem       = SilSiEvaluator.IEvaluationItem;
  IEvalOperator         = SilSiEvaluator.IEvalOperator;
  IEvalInstructions     = SilSiEvaluator.IEvalInstructions;
  IEvalInstructionList  = SilSiEvaluator.IEvalInstructionList;
  IEvalInstruction      = SilSiEvaluator.IEvalInstruction;

type
  Tool                  = SilStScript.ScriptTool;

{$ENDIF}

implementation

end.
 