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

unit SilSiTokenSyntaxTree;

interface

uses
  Sil, SilSeTokens;

type
  ISyntaxTree = interface;
  ISyntaxNode = interface;
  ISyntaxNodes = interface;

  ISyntaxTree = interface
    ['{EB53082A-D12A-4946-9C4D-E184CCFBA9EE}']
    function GetRoot: ISyntaxNode;
    function GetNodes: ISyntaxNodes;
    function AddNode(Token: TToken): ISyntaxNode;
    property Root: ISyntaxNode read GetRoot;
    property Nodes: ISyntaxNodes read GetNodes;
  end;

  ISyntaxNodes = interface
    ['{72F5DBAB-531B-4475-8545-136940678A13}']
    function GetCount: Integer;
    function Get(Index: Integer): ISyntaxNode;
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: ISyntaxNode read Get; default;
  end;

  ISyntaxNode = interface
    ['{A028C754-7AA5-4D79-BC2A-BD0C1634910A}']
    function GetParent: ISyntaxNode;
    procedure SetParent(const AValue: ISyntaxNode);
    function GetSymbol: TToken;
    function GetChilds: ISyntaxNodes;
    property Parent: ISyntaxNode read GetParent write SetParent;
    property Symbol: TToken read GetSymbol;
    property Childs: ISyntaxNodes read GetChilds;
  end;

implementation
end.
