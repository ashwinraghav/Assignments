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

unit SilSiAvlTree;

{$I Defines.inc}

interface

uses
  Sil;

type
  PAvlNodeData = ^RAvlNodeData;
  RAvlNodeData = packed record
    Left: LongWord;
    Right: LongWord;
    Balance: ShortInt;
  end;

  PAvlNode = ^RAvlNode;
  RAvlNode = record
    Valid: Boolean;
    Index: LongWord;
    Data: RAvlNodeData;
    PBuffer: Pointer;
    Buffer: String;
  end;

  IAvlNodeManager = interface
    ['{30722F93-2036-11D4-987F-00104B0FA1EF}']
    function ReadRoot: LongWord;
    procedure UpdateRoot(Index: LongWord);
    function CompareNode(const e1; const e2; Param: Pointer = nil): Integer;
    function AddNode(const Data; Size: LongWord; Param: Pointer = nil): LongWord;
    function ReadNode(Index: LongWord; out Node: RAvlNode): Boolean;
    procedure WriteNode(var Node: RAvlNode);
    procedure DeleteNode(var Node: RAvlNode);
    procedure UpdateNodes;
    procedure SetBalance(var Node: RAvlNode; Value: ShortInt);
    procedure SetLeft(var Node: RAvlNode; Value: LongWord);
    procedure SetRight(var Node: RAvlNode; Value: LongWord);
    procedure Increment(var Node: RAvlNode);
    procedure Decrement(var Node: RAvlNode);
  end;

  IAvlTree = interface
    ['{30E1CC31-79D2-11D4-9894-00104B0FA1EF}']
    function GetRootNode: RAvlNode;
    function GetCurrentNode: RAvlNode;
    function GetNodeManager: IAvlNodeManager;
    procedure SetNodeManager(const Value: IAvlNodeManager);
    procedure Add(const Data; Size: LongWord; Param: Pointer = nil);
    function Find(const Data; Param: Pointer = nil): Integer;
    function Delete(const Data; Param: Pointer = nil): LongWord;
    function First: Boolean;
    function Last: Boolean;
    function Next: Boolean;
    function Prior: Boolean;
    property RootNode: RAvlNode read GetRootNode;
    property CurrentNode: RAvlNode read GetCurrentNode;
    property NodeManager: IAvlNodeManager read GetNodeManager write SetNodeManager;
  end;

implementation

end.
