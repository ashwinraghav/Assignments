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

unit SilSmDataIndex;

{$I Defines.inc}

interface

uses
  Sil,
  SilAvl,

  SilLmInterfaceList,  
  SilSiDataAccess,
  SilSmDataField;

type
  TDataIndexList = class;

  TDataIndex = class (
    // extends
    TSilInterfacedObject,
    // implements
    IDataIndex,
    IDataIndexDef,
    IAvlNodeManager,
    IDataIndexInternal)
  private
    FName: String;
    FRowsetDef: IDataRowsetDef;
    FFieldNames: IStringList;
    FFieldDefs: IDataFieldDefList;
    FFields: IDataFieldList;
    FOptions: TDataIndexOptions;
    FRoot: LongWord;
    FTree: IAvlTree;
    FPosition: LongWord;
    FInternal: Boolean;
    FCurrentRecord: LongWord;
    FRootPosition: LongWord;
    FInternalCompare: Boolean;
  private
    procedure DoCheckTree;
  protected // IDataIndex
    procedure Close;
    function GetName: String;
    function GetFieldList: String;
    function GetFields: IStrings;
    function GetOptions: TDataIndexOptions;
    function CompareBuffer(Buffer: PChar): Integer;
  protected // IDataIndexDef
    procedure SetName(const Value: String);
    function GetFieldNames: IStringList;
    procedure SetOptions(Value: TDataIndexOptions);
    function GetRoot: LongWord;
    function GetInternal: IDataIndexInternal;
    function GetRootBuffer: String;
    function GetIsInternal: Boolean;
    procedure SetRoot(Value: LongWord);
    procedure Bind;
    procedure ClearTree;
    procedure SetRootPosition(Value: LongWord);
    function GetRootPosition: LongWord;
  protected // IDataIndexInternal
    function GetFieldDefs: IDataFieldDefList;
    procedure AppendEntry(var Buffer: String; Internal: Boolean);
    function DeleteEntry(var Buffer: String; Internal: Boolean): Integer;
    function FindEntry(Buffer: String; FieldCount: LongWord; Internal: Boolean): Integer;
    function First: LongWord;
    function Last: LongWord;
    function Next: LongWord;
    function Prior: LongWord;
  protected // IAvlNodeManager
    function ReadRoot: LongWord;
    procedure UpdateRoot(Index: LongWord);
    function CompareNode(const e1; const e2; Param: Pointer): Integer;
    function AddNode(const Data; Size: LongWord; Param: Pointer): LongWord;
    function ReadNode(Index: LongWord; out Node: RAvlNode): Boolean;
    procedure WriteNode(var Node: RAvlNode);
    procedure DeleteNode(var Node: RAvlNode);
    procedure UpdateNodes;
    procedure SetBalance(var Node: RAvlNode; Value: ShortInt);
    procedure SetLeft(var Node: RAvlNode; Value: LongWord);
    procedure SetRight(var Node: RAvlNode; Value: LongWord);
    procedure Increment(var Node: RAvlNode);
    procedure Decrement(var Node: RAvlNode);
  public
    constructor Create(const Rowset: IDataRowsetDef; Position: LongWord; Internal: Boolean);
    destructor Destroy; override;
  end;

  TDataIndexList = class (
    // extends
    TSilInterfaceList,
    // implements
    IDataIndexList,
    IDataIndexDefList)
  private
    FRowsetDef: IDataRowsetDef;
  private
    procedure DoBindFields(const Fields: String; const Index: IDataIndexDef);
  protected // IDataIndexList
    function GetByName(const Name: String): IDataIndex;
    function IDataIndexList.First = AccFirst;
    function AccFirst: IDataIndex;
    function IDataIndexList.Last = AccLast;
    function AccLast: IDataIndex;
    function IDataIndexList.GetItem = AccGetItem;
    function AccGetItem(Index: Integer): IDataIndex;
    function FieldIndex(const FieldName: string; FindFirst: Boolean = True): IDataIndex;
  protected // IDataIndexDefList
    function CreateItem(const Name: String = ''; const Fields: String = ''; Options: TDataIndexOptions = []): IDataIndexDef;
    function CreateInternal(const Name: String = ''; const Fields: String = ''): IDataIndexInternal;
    function First: IDataIndexDef;
    function Last: IDataIndexDef;
    function GetItem(Index: Integer): IDataIndexDef;
    procedure SetItem(Index: Integer; const Value: IDataIndexDef);
  public
    constructor Create(const Rowset: IDataRowsetDef); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

implementation

uses
  SilSmDataRowset{,
  log};

{ TDataIndex }

constructor TDataIndex.Create(const Rowset: IDataRowsetDef; Position: LongWord; Internal: Boolean);
begin
  inherited Create;

  Pointer(FRowsetDef) := Pointer(Rowset);
  FFieldDefs := TDataFieldList.Create(nil);
  Reference.GetInterface(FFieldDefs, IDataFieldList, FFields);
  FFieldNames := Sil.List.StringList;
  FPosition := Position;
  FInternal := Internal;
  FInternalCompare := false;
end;

destructor TDataIndex.Destroy;
begin
  Close;
  inherited;
end;

function TDataIndex.GetFieldList: String;
var
  e: IEnumerator;
  Field: String;
begin
  Result := '';
  while FFieldNames.Enumerate(e, Field) do Result := Result + Field + ',';
  SetLength(Result, Length(Result) - 1);
end;

function TDataIndex.GetFields: IStrings;
begin
  Result := FFieldNames;  
end;

function TDataIndex.GetFieldNames: IStringList;
begin
  Result := FFieldNames;
end;

function TDataIndex.GetOptions: TDataIndexOptions;
begin
  Result := FOptions;
end;

function TDataIndex.GetRoot: LongWord;
begin
  Result := FRoot;
end;

procedure TDataIndex.SetOptions(Value: TDataIndexOptions);
begin
  FOptions := Value;
end;

procedure TDataIndex.SetRoot(Value: LongWord);
begin
  FRoot := Value;
end;

procedure TDataIndex.Decrement(var Node: RAvlNode);
begin
  Dec(Node.Data.Balance);
  WriteNode(Node);
end;

procedure TDataIndex.Increment(var Node: RAvlNode);
begin
  Inc(Node.Data.Balance);
  WriteNode(Node);
end;

function TDataIndex.ReadRoot: LongWord;
begin
  Result := FRoot;
end;

procedure TDataIndex.DoCheckTree;
begin
  if FTree = nil then FTree := SilAvl.Tk.Tree(Self);
end;

procedure TDataIndex.AppendEntry(var Buffer: String; Internal: Boolean);
begin
  try
    FInternalCompare := Internal;
    FCurrentRecord := FRowsetDef.CurrentRecord;
    DoCheckTree;
    FTree.Add(Buffer[1], Length(Buffer));
    FRowsetDef.ChangeCurrentRecord(FCurrentRecord);
  except
    Sil.Error.Throw('TDataIndex.AppendEntry:indice corrupto');
  end;
end;

function TDataIndex.DeleteEntry(var Buffer: String; Internal: Boolean): Integer;
begin
  try
    FInternalCompare := Internal;
    FCurrentRecord := FRowsetDef.CurrentRecord;
    DoCheckTree;
    Result := FTree.Delete(Buffer[1]);
    FRowsetDef.ChangeCurrentRecord(FCurrentRecord);
  except
    Result := -1;
    Sil.Error.Throw('TDataIndex.DeleteEntry:indice corrupto');
  end;
end;

function TDataIndex.FindEntry(Buffer: String; FieldCount: LongWord; Internal: Boolean): Integer;
var
  Param: Pointer;
begin
  try
    FInternalCompare := Internal;
    DoCheckTree;

    if FieldCount > 0 then
      Param := Pointer(FieldCount) else
      Param := nil;

    Result := FTree.Find(Buffer[1], Param);
    FTree.CurrentNode;  // se asegura de posicionarse en el registro que encontro
  except
    Result := -1;
    Sil.Error.Throw('TDataIndex.FindEntry:indice corrupto');
  end;
end;

function TDataIndex.First: LongWord;
begin
  DoCheckTree;
  if FTree.First then
    Result := FTree.CurrentNode.Index else
    Result := 0;
end;

function TDataIndex.Last: LongWord;
begin
  DoCheckTree;
  if FTree.Last then
    Result := FTree.CurrentNode.Index else
    Result := 0;
end;

function TDataIndex.Next: LongWord;
begin
  DoCheckTree;
  if FTree.Next then
    Result := FTree.CurrentNode.Index else
    Result := 0;
end;

function TDataIndex.Prior: LongWord;
begin
  DoCheckTree;
  if FTree.Prior then
    Result := FTree.CurrentNode.Index else
    Result := 0;
end;

procedure TDataIndex.SetBalance(var Node: RAvlNode; Value: ShortInt);
begin
  if Node.Data.Balance = Value then Exit;
  Node.Data.Balance := Value;
  WriteNode(Node);
end;

procedure TDataIndex.SetLeft(var Node: RAvlNode; Value: LongWord);
begin
  if Node.Data.Left = Value then Exit;
  if (Value = Node.Index) or ((Node.Data.Right = Value) and (Value <> 0)) then
    Sil.Error.Throw('TDataIndex.SetLeft:informacion invalida');

  Node.Data.Left := Value;
  WriteNode(Node);
end;

procedure TDataIndex.SetRight(var Node: RAvlNode; Value: LongWord);
begin
  if Node.Data.Right = Value then Exit;
  if (Value = Node.Index) or ((Node.Data.Left = Value) and (Value <> 0)) then
    Sil.Error.Throw('TDataIndex.SetLeft:informacion invalida');

  Node.Data.Right := Value;
  WriteNode(Node);
end;

procedure TDataIndex.UpdateNodes;
begin
end;

procedure TDataIndex.UpdateRoot(Index: LongWord);
begin
  FRoot := Index;
  FRowsetDef.WriteIndexRoot(FRoot, Self);
end;

function TDataIndex.CompareNode(const e1; const e2; Param: Pointer): Integer;
var
  i: Integer;
  wCount: Word;
  Field: IFieldDef;
  lwFieldPos: LongWord;
  bIgnoreCase: Boolean;
begin
  Result := 0;

  if Assigned(Param) then
    wCount := LongWord(Param) + 1 - LongWord(Int.IIf(FInternalCompare, 1, 2)) else
    wCount := FFieldDefs.Count - Int.IIf(FInternalCompare, 1, 2);

  for i := 0 to wCount do
  begin
    Field := FFieldDefs.Items[i];
    lwFieldPos := Field.Position;

    if Field.DataType = ftString then
      bIgnoreCase := ixIgnoreCase in FOptions else
      bIgnoreCase := false;

    Result := Field.Compare(PChar(@e1)[lwFieldPos], PChar(@e2)[lwFieldPos], @bIgnoreCase);
    if Result <> 0 then Break;
  end;
end;

procedure TDataIndex.Bind;
var
  e: IEnumerator;
  sFieldName: String;
begin
  FFieldDefs.Clear;

  if FName = DEL_NAME then
    FFieldDefs.Add(TDataDeletedField.Create(FRowsetDef))
  else
    while FFieldNames.Enumerate(e, sFieldName) do
      FFieldDefs.Add(FRowsetDef.Fields.Names[sFieldName]);

  FFieldDefs.Add(TDataRecordField.Create(FRowsetDef));
end;

function TDataIndex.AddNode(const Data; Size: LongWord; Param: Pointer): LongWord;
begin
  Result := FCurrentRecord;
end;

procedure TDataIndex.DeleteNode(var Node: RAvlNode);
begin
  FillChar(Node.Data, SizeOf(RAvlNodeData), 0);
  WriteNode(Node);
end;

function TDataIndex.ReadNode(Index: LongWord; out Node: RAvlNode): Boolean;
begin
  Node.Valid := Index > 0;
  Result := Node.Valid;
  if not Result then Exit;

  FRowsetDef.ChangeCurrentRecord(Index);
  Node.Index := Index;
  SetLength(Node.Buffer, FRowsetDef.RecordSize + SizeOf(LongWord));
  Move(FRowsetDef.RecordBuffer[1], Node.Buffer[1], FRowsetDef.RecordSize);
  Move(Node.Index, Node.Buffer[FRowsetDef.RecordSize + 1], SizeOf(LongWord));
  Node.PBuffer := PChar(Node.Buffer);
  FRowsetDef.Read(Node.Data, FPosition - 1, SizeOf(RAvlNodeData));

  if (Node.Data.Left = Node.Index) or (Node.Data.Right = Node.Index) or
    ((Node.Data.Left = Node.Data.Right) and (Node.Data.Left <> 0)) then
    Sil.Error.Throw('TDataIndex.ReadNode:informacion invalida');
end;

procedure TDataIndex.WriteNode(var Node: RAvlNode);
begin
  FRowsetDef.ChangeCurrentRecord(Node.Index);
  FRowsetDef.Write(Node.Data, FPosition - 1, SizeOf(RAvlNodeData));
  FRowsetDef.WriteRecord(Node.Index);
end;

function TDataIndex.GetInternal: IDataIndexInternal;
begin
  Result := Self;
end;

function TDataIndex.GetIsInternal: Boolean;
begin
  Result := FInternal;
end;

procedure TDataIndex.SetRootPosition(Value: LongWord);
begin
  FRootPosition := Value;
end;

function TDataIndex.GetRootPosition: LongWord;
begin
  Result := FRootPosition;
end;

function TDataIndex.GetName: String;
begin
  Result := FName;
end;

procedure TDataIndex.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TDataIndex.Close;
begin
  Pointer(FRowsetDef) := nil;
  ClearTree;
end;

function TDataIndex.GetRootBuffer: String;
begin
  DoCheckTree;
  Result := FTree.RootNode.Buffer;
  if Length(Result) = 0 then Result := #0;
end;

function TDataIndex.GetFieldDefs: IDataFieldDefList;
begin
  Result := FFieldDefs;
end;

procedure TDataIndex.ClearTree;
begin
  FTree := nil;
end;

function TDataIndex.CompareBuffer(Buffer: PChar): Integer;
var
  i, FieldPos: Integer;
  Field: IFieldDef;
  IgnoreCase: Boolean;
begin
  Result := 0;

  for i := 0 to FFieldDefs.Count - 2 do
  begin
    Field := FFieldDefs.Items[i];
    FieldPos := Field.Position;

    if Field.DataType = ftString then
      IgnoreCase := ixIgnoreCase in FOptions else
      IgnoreCase := false;

    Result := Field.Compare(Buffer[FieldPos], FRowsetDef.RecordBuffer[FieldPos + 1], @IgnoreCase);
    if Result <> 0 then Break;
  end;
end;

{ TDataIndexList }

constructor TDataIndexList.Create(const Rowset: IDataRowsetDef);
begin
  inherited Create;
  Pointer(FRowsetDef) := Pointer(Rowset);
end;

destructor TDataIndexList.Destroy;
begin
  Pointer(FRowsetDef) := nil;
  inherited;
end;

procedure TDataIndexList.DoBindFields(const Fields: String; const Index: IDataIndexDef);
var
  i: Integer;
  sField: String;
begin
  i := 1;
  repeat
    sField := Str.Trim(Str.Token(Fields, ',', i));
    if Str.NotEmpty(sField) then Index.FieldNames.Add(sField);
  until i = 0;
end;

function TDataIndexList.CreateItem(const Name, Fields: String; Options: TDataIndexOptions): IDataIndexDef;
begin
  Result := TDataIndex.Create(FRowsetDef, Count * SizeOf(RAvlNodeData) + REC_POS_INDEX, false);
  Add(Result);
  Result.Name := Name;
  Result.Options := Options;
  DoBindFields(Fields, Result);
end;

function TDataIndexList.CreateInternal(const Name, Fields: String): IDataIndexInternal;
begin
  Result := TDataIndex.Create(FRowsetDef, Count * SizeOf(RAvlNodeData) + REC_POS_INDEX, true);
  Add(Result);
  Result.Name := Name;
  if Str.NotEmpty(Fields) then DoBindFields(Fields, Result);
end;

function TDataIndexList.First: IDataIndexDef;
begin
  Result := IDataIndexDef(inherited First);
end;

function TDataIndexList.GetByName(const Name: String): IDataIndex;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := AccGetItem(i);
    if Sil.Text.Compare(Result.Name, Name) = 0 then Exit;
  end;
  Result := nil;
end;

function TDataIndexList.FieldIndex(const FieldName: string; FindFirst: Boolean): IDataIndex;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := AccGetItem(i);
    if (Result.Fields.IndexOf(FieldName) <> -1) and (FindFirst or (Result.Fields.Count = 1)) then Exit;
  end;
  Result := nil;
end;

function TDataIndexList.GetItem(Index: Integer): IDataIndexDef;
begin
  Result := IDataIndexDef(inherited GetItem(Index));
end;

function TDataIndexList.Last: IDataIndexDef;
begin
  Result := IDataIndexDef(inherited Last);
end;

procedure TDataIndexList.SetItem(Index: Integer; const Value: IDataIndexDef);
begin
  inherited SetItem(Index, Value);
end;

function TDataIndexList.AccFirst: IDataIndex;
begin
  Result := IDataIndex(inherited First);
end;

function TDataIndexList.AccGetItem(Index: Integer): IDataIndex;
begin
  Result := IDataIndex(inherited GetItem(Index));
end;

function TDataIndexList.AccLast: IDataIndex;
begin
  Result := IDataIndex(inherited Last);
end;

procedure TDataIndexList.Clear;
var
  e: IEnumerator;
  Index: IDataIndex;
begin
  while Enumerate(e, Index) do Index.Close;
  inherited;
end;

end.
