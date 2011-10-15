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

unit SilSiToken;

interface

uses
  Sil, SilSeTokens;

type
  IToken = interface;
  ITokenTable = interface;

  IToken = interface
    ['{4641010A-8200-474A-B7B4-65370A9D759A}']
    function GetIndex: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetSymbol: TToken;
    procedure SetSymbol(const Value: TToken);
    function GetLexema: string;
    function GetLinked: IToken;
    procedure SetLinked(const Value: IToken);
    function GetData: IParameterList;
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
    property Index: Integer read GetIndex;
    property Name: string read GetName write SetName;
    property Symbol: TToken read GetSymbol write SetSymbol;
    property Lexema: string read GetLexema;
    property Linked: IToken read GetLinked write SetLinked;
    property Data: IParameterList read GetData;
    property Value: Variant read GetValue write SetValue;
  end;

  ITokens = interface
    ['{261125A2-5AF2-472E-9D8D-517E210A0990}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IToken;
    function ValidIndex(Index: Integer): Boolean;
    function Find(Symbol: TToken; out Token: IToken): Boolean; overload; 
    function Find(const Lexema: string; out Token: IToken): Boolean; overload; 
    function Lookup(Symbol: TToken): IToken; overload;
    function Lookup(const Lexema: string): IToken; overload;
    function Enumerate(var Enum: IEnumerator; out Token: IToken): Boolean;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: IToken read GetItem; default;
  end;

  ITokenTable = interface (ITokens)
    ['{439361CB-EFEB-4FCF-91C2-2F2002B7307D}']
    procedure Clear;
    function Add(Symbol: TToken; const Name: string; const Lexema: string): IToken;
    function Remove(Symbol: TToken): Integer; overload; 
    function Remove(const Item: IToken): Integer; overload;
    procedure Delete(Index: Integer);   
  end;

implementation
end.
