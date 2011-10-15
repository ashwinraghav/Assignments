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

unit SilSmAvlMemoryNodes;

{$I Defines.inc}

interface

uses
  Sil,
  SilSmAvlTree,
  SilSiAvlTree;

type
  AvlNodeDataHandler = class (DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override; 
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

  TAvlMemoryNodeManager = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAvlNodeManager)
  private
    FList: IStringList;
    FRoot: LongWord;
    FDataSize: LongWord;
    FDeleted: TIntegerArray;
  protected // IAvlNodeManager
    function ReadRoot: LongWord;
    procedure UpdateRoot(Index: LongWord);
    function CompareNode(const e1; const e2; Param: Pointer): Integer; virtual;
    function AddNode(const Data; Size: LongWord; Param: Pointer): LongWord; virtual;
    function ReadNode(Index: LongWord; out Node: RAvlNode): Boolean;
    procedure WriteNode(var Node: RAvlNode);
    procedure DeleteNode(var Node: RAvlNode);
    procedure UpdateNodes;
    procedure SetBalance(var Node: RAvlNode; Value: Int8);
    procedure SetLeft(var Node: RAvlNode; Value: LongWord);
    procedure SetRight(var Node: RAvlNode; Value: LongWord);
    procedure Increment(var Node: RAvlNode);
    procedure Decrement(var Node: RAvlNode);
  public
    constructor Create(DataSize: LongWord);
  end;

implementation

uses
  SysUtils;

constructor TAvlMemoryNodeManager.Create(DataSize: LongWord);
begin
  inherited Create;
  FDataSize := DataSize;
  FList := Sil.List.StringList(false, AvlNodeDataHandler);
  SetLength(FDeleted, 0);
end;

function TAvlMemoryNodeManager.CompareNode(const e1; const e2; Param: Pointer): Integer;
begin
  Result := StrComp(PChar(@e1), PChar(@e2));
end;

function TAvlMemoryNodeManager.AddNode(const Data; Size: LongWord; Param: Pointer): LongWord;
var
  Buf: String;
  Rec: RAvlNodeData;
  iIdx: Integer;
begin
  FillChar(Rec, SizeOf(RAvlNodeData), 0);
  SetString(Buf, PChar(@Data), Size);

  if Length(FDeleted) > 0 then
  begin
    iIdx := Length(FDeleted) - 1;
    Result := FDeleted[iIdx] + 1;
    FList[FDeleted[iIdx]] := Buf;
    FList.Ptrs[FDeleted[iIdx]] := @Rec;
    Int.ArrayDelete(FDeleted, iIdx);
  end else
    Result := FList.Add(Buf, @Rec) + 1;
end;

procedure TAvlMemoryNodeManager.UpdateRoot(Index: LongWord);
begin
  FRoot := Index;
end;

function TAvlMemoryNodeManager.ReadRoot: LongWord;
begin
  Result := FRoot;
end;

procedure TAvlMemoryNodeManager.DeleteNode(var Node: RAvlNode);
begin
  Int.ArrayAdd(FDeleted, Node.Index - 1);
  FillChar(PAvlNodeData(FList.Ptrs[Node.Index - 1])^, SizeOf(RAvlNodeData), 0);
end;

procedure TAvlMemoryNodeManager.UpdateNodes;
begin
end;

procedure TAvlMemoryNodeManager.Decrement(var Node: RAvlNode);
begin
  Dec(Node.Data.Balance);
  WriteNode(Node);
end;

procedure TAvlMemoryNodeManager.Increment(var Node: RAvlNode);
begin
  Inc(Node.Data.Balance);
  WriteNode(Node);
end;

procedure TAvlMemoryNodeManager.SetBalance(var Node: RAvlNode; Value: Int8);
begin
  if Node.Data.Balance = Value then Exit;
  Node.Data.Balance := Value;
  WriteNode(Node);
end;

procedure TAvlMemoryNodeManager.SetLeft(var Node: RAvlNode; Value: LongWord);
begin
  if Node.Data.Left = Value then Exit;
  Node.Data.Left := Value;
  WriteNode(Node);
end;

procedure TAvlMemoryNodeManager.SetRight(var Node: RAvlNode; Value: LongWord);
begin
  if Node.Data.Right = Value then Exit;
  Node.Data.Right := Value;
  WriteNode(Node);
end;

function TAvlMemoryNodeManager.ReadNode(Index: LongWord; out Node: RAvlNode): Boolean;
var
  Rec: PAvlNodeData;
begin
  Result := Index > 0;
  Node.Valid := Result;
  if not Result then Exit;

  Rec := FList.Ptrs[Index - 1];

  Node.Index := Index;
  Node.Data.Balance := Rec.Balance;
  Node.Data.Left := Rec.Left;
  Node.Data.Right := Rec.Right;
  Node.Buffer := FList[Index - 1];
  Node.PBuffer := PChar(Node.Buffer);
end;

procedure TAvlMemoryNodeManager.WriteNode(var Node: RAvlNode);
var
  Buf: String;
  Rec: RAvlNodeData;
begin
  
  Rec.Balance := Node.Data.Balance;
  Rec.Left := Node.Data.Left;
  Rec.Right := Node.Data.Right;

  SetString(Buf, PChar(Node.PBuffer), FDataSize);
  FList[Node.Index - 1] := Buf;
  FList.Ptrs[Node.Index - 1] := @Rec;
end;

{ AvlNodeDataHandler }

class procedure AvlNodeDataHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PAvlNodeData(Result));
end;

class procedure AvlNodeDataHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PAvlNodeData(Value));
end;

class procedure AvlNodeDataHandler.Clear(var Obj; const Data: Pointer);
begin
end;

class function AvlNodeDataHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := 0;
end;

class procedure AvlNodeDataHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PAvlNodeData(Dest)^ := PAvlNodeData(Source)^;
end;

(*
class procedure AvlNodeDataHandler.Clear(var Value);
begin
end;

class function AvlNodeDataHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := 0;
end;

class function AvlNodeDataHandler.Copy(Value: Pointer; out Obj; Data: Pointer): Boolean;
begin
  Result := (Value <> nil);
  if Result then PAvlNodeData(Obj) := PAvlNodeData(Value);
end;

class procedure AvlNodeDataHandler.Free(var Value; Data: Pointer);
begin
  Dispose(PAvlNodeData(Value));
end;

class function AvlNodeDataHandler.ToPtr(const Obj; out Value: Pointer; Data: Pointer): Boolean;
var
  PBuf: PAvlNodeData;
begin
  Result := Pointer(Obj) <> nil;
  if Result then
  begin
    System.New(PBuf);
    PBuf^ := RAvlNodeData(Obj);
    Value := PBuf;
  end;
end;
*)

end.
