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

unit SilSmAvlTree;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiAvlTree;

type
  AvlNodeHandler = class (DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override; 
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

  TNodeSide = (nsLeft, nsRight);
     
  TAvlTree = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAvlTree)
  private
    FManager: IAvlNodeManager;
    FRoot: RAvlNode;
    FStack: IPointerList;
    FLastDeleted: LongWord;
  protected
    function DoInsert(var Node: RAvlNode; const Data; Size: LongWord; Param: Pointer): LongWord;
    function DoRotateLeft(var Node: RAvlNode): LongWord;
    function DoRotateRight(var Node: RAvlNode): LongWord;
    function DoBalance(var Node: RAvlNode): LongWord;
    function DoRestoreRightBalance(var Node: RAvlNode; OldBal: ShortInt): LongWord;
    function DoRestoreLeftBalance(var Node: RAvlNode; OldBal: ShortInt): LongWord;
    function DoRemoveLeftmostDescendent(var Node: RAvlNode; out Child: RAvlNode): LongWord;
    function DoRemove(var Node: RAvlNode; const Data; Param: Pointer): LongWord;
    function DoFindMin(Node: RAvlNode): LongWord;
    function DoFindMax(Node: RAvlNode): LongWord;
    procedure DoPush(const Node: RAvlNode);
    function DoPop(var Node: RAvlNode; out Side: TNodeSide): Boolean;
    function DoFind(const Data; Param: Pointer): Integer;
    procedure DoDelete(var Node: RAvlNode);
  protected // IAvlTree
    function GetRootNode: RAvlNode;
    function GetCurrentNode: RAvlNode;
    function GetNodeManager: IAvlNodeManager;
    procedure SetNodeManager(const Value: IAvlNodeManager);
    procedure Add(const Data; Size: LongWord; Param: Pointer);
    function Find(const Data; Param: Pointer): Integer;
    function Delete(const Data; Param: Pointer): LongWord;
    function First: Boolean;
    function Last: Boolean;
    function Next: Boolean;
    function Prior: Boolean;
  public
    constructor Create(const NodeManager: IAvlNodeManager);
    destructor Destroy; override;
  end;

implementation

constructor TAvlTree.Create(const NodeManager: IAvlNodeManager);
begin
  inherited Create;
  FStack := Sil.List.PointerList(false, AvlNodeHandler);
  FRoot.Valid := false;
  SetNodeManager(NodeManager);
end;

destructor TAvlTree.Destroy;
begin
  FManager.UpdateNodes;
  inherited;
end;

function TAvlTree.DoInsert(var Node: RAvlNode; const Data; Size: LongWord; Param: Pointer): LongWord;
var
  iOldBal: ShortInt;
  NodeSide: RAvlNode;
begin
  if FManager.CompareNode(Data, Node.PBuffer^, Param) < 0 then
  begin
    if Node.Data.Left <> 0 then
    begin
      FManager.ReadNode(Node.Data.Left, NodeSide);
      iOldBal := NodeSide.Data.Balance;
      FManager.SetLeft(Node, DoInsert(NodeSide, Data, Size, Param));
      if (NodeSide.Data.Balance <> iOldBal) and (NodeSide.Data.Balance <> 0) then FManager.Decrement(Node);
    end else
    begin
      FManager.SetLeft(Node, FManager.AddNode(Data, Size, Param));
      FManager.Decrement(Node);
    end;
  end else
  begin
    if Node.Data.Right <> 0 then
    begin
      FManager.ReadNode(Node.Data.Right, NodeSide);
      iOldBal := NodeSide.Data.Balance;
      FManager.SetRight(Node, DoInsert(NodeSide, Data, Size, Param));
      if (NodeSide.Data.Balance <> iOldBal) and (NodeSide.Data.Balance <> 0) then FManager.Increment(Node);
    end else
    begin
      FManager.SetRight(Node, FManager.AddNode(Data, Size, Param));
      FManager.Increment(Node);
    end;
  end;

  if (Node.Data.Balance < -1) or (Node.Data.Balance > 1) then
    Result := DoBalance(Node) else
    Result := Node.Index;
end;

function TAvlTree.DoBalance(var Node: RAvlNode): LongWord;
var
  NodeSide: RAvlNode;
begin
  if Node.Data.Balance < 0 then
  begin
    FManager.ReadNode(Node.Data.Left, NodeSide);
    if NodeSide.Data.Balance > 0 then
    begin
      FManager.SetLeft(Node, DoRotateLeft(NodeSide));
      Result := DoRotateRight(Node);
    end else
      Result := DoRotateRight(Node);
  end else
  begin
    FManager.ReadNode(Node.Data.Right, NodeSide);
    if NodeSide.Data.Balance < 0 then
    begin
      FManager.SetRight(Node, DoRotateRight(NodeSide));
      Result := DoRotateLeft(Node);
    end else
      Result := DoRotateLeft(Node);
  end;
end;

function TAvlTree.DoRotateLeft(var Node: RAvlNode): LongWord;
var
  NodeB: RAvlNode;
  iBalA, iBalB: ShortInt;
begin
  FManager.ReadNode(Node.Data.Right, NodeB);

  FManager.SetRight(Node, NodeB.Data.Left);
  FManager.SetLeft(NodeB, Node.Index);

  iBalA := Node.Data.Balance;
  iBalB := NodeB.Data.Balance;

  if iBalB <= 0 then
  begin
    if iBalA >= 1 then
      FManager.SetBalance(NodeB, iBalB - 1) else
      FManager.SetBalance(NodeB, iBalA + iBalB - 2);

    FManager.SetBalance(Node, iBalA - 1);
  end else
  begin
    if iBalA <= iBalB then
      FManager.SetBalance(NodeB, iBalA - 2) else
      FManager.SetBalance(NodeB, iBalB - 1);

    FManager.SetBalance(Node, (iBalA - iBalB) - 1);
  end;

  Result := NodeB.Index;
end;

function TAvlTree.DoRotateRight(var Node: RAvlNode): LongWord;
var
  NodeB: RAvlNode;
  iBalA, iBalB: ShortInt;
begin
  FManager.ReadNode(Node.Data.Left, NodeB);

  FManager.SetLeft(Node, NodeB.Data.Right);
  FManager.SetRight(NodeB, Node.Index);

  iBalA := Node.Data.Balance;
  iBalB := NodeB.Data.Balance;

  if iBalB <= 0 then
  begin
    if iBalB > iBalA then
      FManager.SetBalance(NodeB, iBalB + 1) else
      FManager.SetBalance(NodeB, iBalA + 2);

    FManager.SetBalance(Node, 1 + iBalA - iBalB);
  end else
  begin
    if iBalA <= -1 then
      FManager.SetBalance(NodeB, iBalB + 1) else
      FManager.SetBalance(NodeB, iBalA + iBalB + 2);

    FManager.SetBalance(Node, 1 + iBalA);
  end;

  Result := NodeB.Index;
end;

procedure TAvlTree.DoDelete(var Node: RAvlNode);
var
  Side: TNodeSide;
begin
  FLastDeleted := Node.Index;
  FManager.DeleteNode(Node);
  DoPop(Node, Side);
end;

function TAvlTree.DoRemove(var Node: RAvlNode; const Data; Param: Pointer): LongWord;
var
  iComp, iOldBal: Integer;
  NodeSide, NewRoot: RAvlNode;
begin
  iComp := FManager.CompareNode(Data, Node.PBuffer^, Param);

  if iComp = 0 then
  begin
    if Node.Data.Left = 0 then
    begin
      Result := Node.Data.Right;
      DoDelete(Node);
    end else
    if Node.Data.Right = 0 then
    begin
      Result := Node.Data.Left;
      DoDelete(Node);
    end else
    begin
      FManager.ReadNode(Node.Data.Right, NodeSide);
      iOldBal := NodeSide.Data.Balance;
      FManager.SetRight(Node, DoRemoveLeftmostDescendent(NodeSide, NewRoot));

      FManager.SetLeft(NewRoot, Node.Data.Left);
      FManager.SetRight(NewRoot, Node.Data.Right);
      FManager.SetBalance(NewRoot, Node.Data.Balance);
      Result := DoRestoreRightBalance(NewRoot, iOldBal);
      DoDelete(Node);
    end;
  end else
  if iComp < 0 then
  begin
    if Node.Data.Left = 0 then
    begin
      Result := Node.Index;
      Exit;
    end;

    FManager.ReadNode(Node.Data.Left, NodeSide);
    iOldBal := NodeSide.Data.Balance;
    FManager.SetLeft(Node, DoRemove(NodeSide, Data, Param));
    Result := DoRestoreLeftBalance(Node, iOldBal);
  end else
  begin
    if Node.Data.Right = 0 then
    begin
      Result := Node.Index;
      Exit;
    end;

    FManager.ReadNode(Node.Data.Right, NodeSide);
    iOldBal := NodeSide.Data.Balance;
    FManager.SetRight(Node, DoRemove(NodeSide, Data, Param));
    Result := DoRestoreRightBalance(Node, iOldBal);
  end;
end;

function TAvlTree.DoRestoreRightBalance(var Node: RAvlNode; OldBal: ShortInt): LongWord;
var
  NodeSide: RAvlNode;
begin
  FManager.ReadNode(Node.Data.Right, NodeSide);

  if not NodeSide.Valid then FManager.Decrement(Node) else
  if (NodeSide.Data.Balance <> OldBal) and (NodeSide.Data.Balance = 0) then FManager.Decrement(Node);

  if Node.Data.Balance < -1 then
    Result := DoBalance(Node) else
    Result := Node.Index;
end;

function TAvlTree.DoRestoreLeftBalance(var Node: RAvlNode; OldBal: ShortInt): LongWord;
var
  NodeSide: RAvlNode;
begin
  FManager.ReadNode(Node.Data.Left, NodeSide);

  if not NodeSide.Valid then FManager.Increment(Node) else
  if (NodeSide.Data.Balance <> OldBal) and (NodeSide.Data.Balance = 0) then FManager.Increment(Node);

  if Node.Data.Balance > 1 then
    Result := DoBalance(Node) else
    Result := Node.Index;
end;

function TAvlTree.DoRemoveLeftmostDescendent(var Node: RAvlNode; out Child: RAvlNode): LongWord;
var
  NodeSide: RAvlNode;
  iOldBal: ShortInt;
begin
  FManager.ReadNode(Node.Data.Left, NodeSide);

  if not NodeSide.Valid then
  begin
    Child := Node;
    Result := Node.Data.Right;
    Exit;
  end;

  iOldBal := NodeSide.Data.Balance;
  FManager.SetLeft(Node, DoRemoveLeftmostDescendent(NodeSide, Child));
  Result := DoRestoreLeftBalance(Node, iOldBal);
end;

procedure TAvlTree.Add(const Data; Size: LongWord; Param: Pointer);
var
  lwIndex: LongWord;
begin
  if not FRoot.Valid then
    lwIndex := FManager.AddNode(Data, Size, Param) else
    lwIndex := DoInsert(FRoot, Data, Size, Param);

  if not FRoot.Valid or (lwIndex <> FRoot.Index) then
  begin
    FManager.UpdateRoot(lwIndex);
    FManager.ReadNode(lwIndex, FRoot);
  end;
end;

function TAvlTree.Delete(const Data; Param: Pointer): LongWord;
var
  lwIndex: LongWord;
begin
  if FRoot.Valid then
  begin
    FLastDeleted := 0;
    lwIndex := DoRemove(FRoot, Data, Param);

    if lwIndex <> FRoot.Index then
    begin
      FManager.UpdateRoot(lwIndex);
      FManager.ReadNode(lwIndex, FRoot);
    end;

    Result := FLastDeleted;
  end else
    Result := 0;
end;

function TAvlTree.DoFind(const Data; Param: Pointer): Integer;
var
  Node: RAvlNode;
begin
  Result := -1;
  Node := FRoot;

  while Node.Valid do
  begin
    DoPush(Node);
    Result := FManager.CompareNode(Node.PBuffer^, Data, Param);

    if Result = 0 then
    begin
      Break;
    end else
    if Result < 0 then
      FManager.ReadNode(Node.Data.Right, Node) else
    if Node.Data.Left > 0 then
      FManager.ReadNode(Node.Data.Left, Node) else
      Break;
  end;
end;

function TAvlTree.Find(const Data; Param: Pointer): Integer;
begin
  FStack.Clear;
  Result := DoFind(Data, Param);
end;

function TAvlTree.DoFindMin(Node: RAvlNode): LongWord;
begin
  Result := 0;
  if not Node.Valid then Exit;

  while true do
  begin
    DoPush(Node);

    if Node.Data.Left = 0 then
    begin
      Result := Node.Index;
      Break;
    end else
      FManager.ReadNode(Node.Data.Left, Node);
  end;
end;

function TAvlTree.DoFindMax(Node: RAvlNode): LongWord;
begin
  Result := 0;
  if not Node.Valid then Exit;

  while true do
  begin
    DoPush(Node);

    if Node.Data.Right = 0 then
    begin
      Result := Node.Index;
      Break;
    end else
      FManager.ReadNode(Node.Data.Right, Node);
  end;
end;

function TAvlTree.First: Boolean;
begin
  FStack.Clear;
  Result := DoFindMin(FRoot) > 0;
end;

function TAvlTree.Next: Boolean;
var
  NodeSide, Node: RAvlNode;
  Side: TNodeSide;
begin
  Result := false;

  if FStack.Count = 0 then
    if not FRoot.Valid then Exit else DoPush(FRoot);

  Node := PAvlNode(FStack.Last)^;
  FManager.ReadNode(Node.Index, Node);

  if Node.Data.Right <> 0 then
  begin
    FManager.ReadNode(Node.Data.Right, NodeSide);
    DoFindMin(NodeSide);
    Result := true;
  end else
  begin
    while DoPop(Node, Side) do
      if Side = nsLeft then
      begin
        Result := true;
        Break;
      end;
    if not Result then DoFindMax(Node);
  end;
end;

function TAvlTree.Last: Boolean;
begin
  FStack.Clear;
  Result := DoFindMax(FRoot) > 0;
end;

function TAvlTree.Prior: Boolean;
var
  NodeSide, Node: RAvlNode;
  Side: TNodeSide;
begin
  Result := false;

  if FStack.Count = 0 then
    if not FRoot.Valid then Exit else DoPush(FRoot);

  Node := PAvlNode(FStack.Last)^;
  FManager.ReadNode(Node.Index, Node);

  if Node.Data.Left <> 0 then
  begin
    FManager.ReadNode(Node.Data.Left, NodeSide);
    DoFindMax(NodeSide);
    Result := true;
  end else
  begin
    while DoPop(Node, Side) do
      if Side = nsRight then
      begin
        Result := true;
        Break;
      end;
    if not Result then DoFindMin(Node);
  end;
end;

procedure TAvlTree.DoPush(const Node: RAvlNode);
begin
  FStack.Add(@Node);
end;

function TAvlTree.DoPop(var Node: RAvlNode; out Side: TNodeSide): Boolean;
begin
  Result := FStack.Count > 1;

  if Result then
  begin
    Result := true;
    Node := PAvlNode(FStack.Last)^;
    FManager.ReadNode(Node.Index, Node);
    FStack.Delete(FStack.Count - 1);

    if PAvlNode(FStack.Last)^.Data.Left = Node.Index then
      Side := nsLeft else
      Side := nsRight;
  end;
end;

function TAvlTree.GetCurrentNode: RAvlNode;
begin
  if FStack.Count > 0 then
    FManager.ReadNode(PAvlNode(FStack.Last)^.Index, Result) else
    Result.Valid := false;
end;

function TAvlTree.GetRootNode: RAvlNode;
begin
  Result := FRoot;
end;

function TAvlTree.GetNodeManager: IAvlNodeManager;
begin
  Result := FManager;
end;

procedure TAvlTree.SetNodeManager(const Value: IAvlNodeManager);
begin
  FStack.Clear;
  FManager := Value;
  FManager.ReadNode(FManager.ReadRoot, FRoot);
end;

{ AvlNodeHandler }

class procedure AvlNodeHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PAvlNode(Result));
end;

class procedure AvlNodeHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PAvlNode(Value));
end;

class procedure AvlNodeHandler.Clear(var Obj; const Data: Pointer);
begin
end;

class function AvlNodeHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := 0;
end;

class procedure AvlNodeHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PAvlNode(Dest)^ := PAvlNode(Source)^;
end;

(*
class procedure AvlNodeHandler.Clear(var Value);
begin
end;

class function AvlNodeHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
end;

class function AvlNodeHandler.Copy(Value: Pointer; out Obj; Data: Pointer): Boolean;
begin
  Result := (Value <> nil);
  if Result then PAvlNode(Obj) := PAvlNode(Value);
end;

class procedure AvlNodeHandler.Free(var Value; Data: Pointer);
begin
end;

class function AvlNodeHandler.ToPtr(const Obj; out Value: Pointer; Data: Pointer): Boolean;
var
  PBuf: PAvlNode;
begin
  Result := Pointer(Obj) <> nil;
  if Result then
  begin
    Value := Pointer(Obj);
    {System.New(PBuf);
    PBuf^ := RAvlNode(Obj);
    Value := PBuf;}
  end;
end;*)

end.
