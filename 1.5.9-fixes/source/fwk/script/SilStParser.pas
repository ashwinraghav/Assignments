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

unit SilStParser;

{$I Defines.inc}

interface

uses
  Sil,
  SilLiTypeInfo,
  SilTokens,
  SilLexer,
  SilSiEvaluator;
  
type
  ParserTool = class(Tool)
    class function AddId( // identificador
      const Table: ITokenTable;
      const Lexema: string;
            Kind: TIdentifierKind;
            ID: TTokenID = 0;
            DataType: TTokenValue = tvUnknown;
            Data: Pointer = nil): IToken; overload;

    class function AddId( // identificador
      const Table: ITokenTable;
      const Lexema: string;
      const DataType: ITypeInfo;
            Kind: TIdentifierKind;
            ID: TTokenID = 0;
            Data: Pointer = nil): IToken; overload;

    class function AddFunction( // funcion
      const Table: ITokenTable;
      const Lexema: string;
      const Instance: IDispatchable;
            ID: TTokenID = 0;
      const Parameters: ITokenTable = nil;
            DataType: TTokenValue = tvUnknown;
            Message: Integer = -1;
            Data: Pointer = nil): IToken; overload;
            
    class function AddFunction( // funcion
      const Table: ITokenTable;
      const Lexema: string;
      const Instance: IDispatchable;
      const DataType: ITypeInfo;
            ID: TTokenID = 0;
      const Parameters: ITokenTable = nil;
            Message: Integer = -1;
            Data: Pointer = nil): IToken; overload;
            
    class function AddConstant( // constante
      const Table: ITokenTable;
      const Lexema: string;
      const Value: Variant;
            ID: TTokenID = 0;
            DataType: TTokenValue = tvUnknown;
            Data: Pointer = nil): IToken; overload;
            
    class function AddConstant( // constante 
      const Table: ITokenTable;
      const Lexema: string;
      const Instance: IConstant;
            ID: TTokenID = 0;
            DataType: TTokenValue = tvUnknown;
            Data: Pointer = nil): IToken; overload;
            
    class function AddValue( // constante & variable
      const Table: ITokenTable;
      const Lexema: string;
      const Instance: ITokenHandler;
            ID: TTokenID = 0;
            DataType: TTokenValue = tvUnknown;
            Data: Pointer = nil): IToken; overload;

    class function AddVariable( // variable
      const Table: ITokenTable;
      const Lexema: string;
      const Instance: IVariant;
            ID: TTokenID = 0;
            DataType: TTokenValue = tvUnknown;
            Data: Pointer = nil): IToken; overload;
  end;

implementation

class function ParserTool.AddId(
  const Table: ITokenTable;
  const Lexema: string;
        Kind: TIdentifierKind;
        ID: TTokenID;
        DataType: TTokenValue;
        Data: Pointer): IToken;
begin
  Result := Table.Add(Token.Make(ttIdentifier, ID, DataType), Lexema, Lexema);
  Result.Data['external'] := True;
  Result.Data['kind']     := Kind;
  Result.Data['function'] := Kind = ikFunction;
  Result.Data['variable'] := Kind = ikVariable;
  Result.Data['constant'] := Kind = ikConstant;
  Result.Data['data']     := Sil.Vart.FromPtr(Data);
end;

class function ParserTool.AddId(
  const Table: ITokenTable;
  const Lexema: string;
  const DataType: ITypeInfo;
        Kind: TIdentifierKind;
        ID: TTokenID;
        Data: Pointer): IToken;
begin
  Result := Table.Add(Token.Make(ttIdentifier, ID, Token.ValueOf(DataType)), Lexema, Lexema);
  Result.Data['external'] := True;
  Result.Data['kind']     := Kind;
  Result.Data['function'] := Kind = ikFunction;
  Result.Data['variable'] := Kind = ikVariable;
  Result.Data['constant'] := Kind = ikConstant;
  Result.Data['data']     := Sil.Vart.FromPtr(Data);
  Result.Data['type']     := DataType;
end;

class function ParserTool.AddFunction(
  const Table: ITokenTable;
  const Lexema: string;
  const Instance: IDispatchable;
        ID: TTokenID;
  const Parameters: ITokenTable;
        DataType: TTokenValue;
        Message: Integer;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikFunction, ID, DataType, Data);
  if Message = -1 then Message := Token.ID(Result.Symbol);
  Result.Data['instance'] := Instance;
  Result.Data['scope'] := Parameters;
  Result.Data['message'] := Message;
end;

class function ParserTool.AddFunction(
  const Table: ITokenTable;
  const Lexema: string;
  const Instance: IDispatchable;
  const DataType: ITypeInfo;
        ID: TTokenID;
  const Parameters: ITokenTable;
        Message: Integer;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikFunction, ID, Token.ValueOf(DataType), Data);
  if Message = -1 then Message := Token.ID(Result.Symbol);
  Result.Data['instance'] := Instance;
  Result.Data['scope'] := Parameters;
  Result.Data['message'] := Message;
end;

class function ParserTool.AddConstant(
  const Table: ITokenTable;
  const Lexema: string;
  const Value: Variant;
        ID: TTokenID;
        DataType: TTokenValue;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikConstant, ID, DataType, Data);
  Result.Value := Value;
end;

class function ParserTool.AddConstant(
  const Table: ITokenTable;
  const Lexema: string;
  const Instance: IConstant;
        ID: TTokenID;
        DataType: TTokenValue;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikConstant, ID, DataType, Data);
  Result.Data['instance'] := Instance;
end;

class function ParserTool.AddVariable(
  const Table: ITokenTable;
  const Lexema: string;
  const Instance: IVariant;
        ID: TTokenID;
        DataType: TTokenValue;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikVariable, ID, DataType, Data);
  Result.Data['instance'] := Instance;
end;

class function ParserTool.AddValue(
  const Table: ITokenTable;
  const Lexema: string;
  const Instance: ITokenHandler;
        ID: TTokenID;
        DataType: TTokenValue;
        Data: Pointer): IToken;
begin
  Result := AddId(Table, Lexema, ikVariable, ID, DataType, Data);
  Result.Data['instance'] := Instance;
end;

end.
