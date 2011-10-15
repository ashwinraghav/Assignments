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

unit SilSmDataRowset;

{$I Defines.inc}

{
  falta:
  .indices de clave unica
  .indices case insensitive
  .auto-reindex
}

interface

uses
  Sil,
  SilSiAvlTree,
  SilSiDataAccess;

type
  TDataRowset = class (
    // extends
    TSilInterfacedObject,
    // implements
    IDataRowset,
    IDataRowsetDef,
    IFieldStore)
  private
    FVersion: Word;
    FStream: IRandomStream;
    FName: String;
    FRecordBuffer: String;
    FTotalRecords: LongWord;
    FLogicalRecords: LongWord;
    FBof: Boolean;
    FEof: Boolean;
    FModified: Boolean;
    FCurrentRecord: LongWord;
    FRecordSize: LongWord;
    FHeaderSize: LongWord;
    FStatus: TDataRowsetStatus;
    FFields: IDataFieldList;
    FFieldDefs: IDataFieldDefList;
    FIndexes: IDataIndexList;
    FIndexesDefs: IDataIndexDefList;
    FActiveIndex: IDataIndexInternal;
    FDeletedIndex: IDataIndexInternal;
    FRecordChanged: Boolean;
    FCheckDeleted: Boolean;
    FCheckMemo: Boolean;
    FCheckIndexes: Boolean;
    FDisplays: IFieldDisplayList;
    procedure DoFillDisplays;
  private
    function DoGetBlankRecord(Logical: Boolean): LongWord;
    procedure DoUpdateIndexes(Status: TDataRowsetStatus; const Changed: TStringArray = nil);
    procedure DoCheckMemo(Status: TDataRowsetStatus);
    function DoMemoWalk(const Field: IFieldDef; Status: TDataRowsetStatus): String;
    procedure DoCheckIndexPos;
    function DoGoto(Dest: LongWord; Move: Integer): Boolean;
    function DoSetCurrent(Value: LongWord): Boolean;
    procedure DoAppendPost;
    procedure DoEditPost;
    procedure DoInternalMove(RecordNumber: LongWord);
    function DoMoveTo(RecordNumber: LongWord): Boolean;
    procedure DoUpdateRecordCount(TotalRecords, LogicalRecords: LongWord);
    procedure DoReadRecord;
    procedure DoAppendDeleted(const Buffer: String; RecordNumber: LongWord);
    procedure DoClearChanges;
  protected // IFieldStore
    procedure Write(const Buffer; Position, Size: LongWord);
    procedure Read(var Buffer; Position, Size: LongWord);
  protected // IDataRowset
    function CreateDisplay(const FieldName: String; const Caption: String = ''; const Mask: String = ''): IFieldDisplay;
    function GetName: String;
    function GetBof: Boolean;
    function GetEof: Boolean;
    function GetBookmark: String;
    procedure SetBookmark(const Value: String);
    function GetModified: Boolean;
    function GetCanModify: Boolean;
    function GetFields: IDataFieldList;
    function GetIndexes: IDataIndexList;
    function GetRecordCount: LongWord;
    function GetCurrentRecord: LongWord;
    procedure SetCurrentRecord(Value: LongWord);
    function GetRecordSize: LongWord;
    procedure SetRecordSize(Value: LongWord);
    function GetStatus: TDataRowsetStatus;
    function GetRowsetDef: IDataRowsetDef;
    function GetActiveIndex: IDataIndex;
    procedure SetActiveIndex(const Value: IDataIndex);
    function GetActiveIndexName: String;
    function GetDisplays: IFieldDisplayList;
    procedure SetActiveIndexName(const Value: String);
    procedure AppendRecord(const Values: array of const);
    procedure UpdateRecord(const Values: array of const);
    function AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
    function GetStream: IRandomStream;
    procedure AppendRecordFrom(const Source: IReadOnlyDataRowset);
    procedure Delete;
    procedure Open;
    procedure Close;
    function IsEmpty: Boolean;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    function MoveBy(Distance: LargeInt): Integer;
    procedure Append;
    procedure Edit;
    procedure Post;
    procedure Reindex;
    function Find(const Values: array of Variant; Nearest: Boolean = true): Boolean;
    function FindBuffer(Buffer: PChar; Size: LongWord): Boolean;
    function FindValues: IDataRowsetFind;
  protected // IDataRowsetDef
    function GetFieldDefs: IDataFieldDefList;
    function GetIndexDefs: IDataIndexDefList;
    function GetRowset: IDataRowset;
    function GetStore: IFieldStore;
    function GetRecordBuffer: String;
    function ChangeCurrentRecord(Position: LongWord): Boolean;
    function ReadMemo(const Field: IFieldDef): String;
    procedure Build(const Name: String = '');
    procedure CopyFrom(const RowsetDef: IDataRowsetDef);
    procedure Initialize;
    procedure WriteRecord(RecordNumber: LongWord);
    procedure WriteIndexRoot(RecordNumber: LongWord; const Index: IDataIndexDef);
  public
    constructor Create(const Stream: IRandomStream);
    constructor OpenFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone);
    constructor CreateFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone);
    destructor Destroy; override;
  end;

type
  TRowsetFind = class (TSilObject, IDataRowsetFind)
  private
    FRowset: IDataRowset;
    FIndex: IDataIndexInternal;
    FIdxFld: Integer;
    FPacket: IPacket;
  protected
    procedure DoFindEnd; virtual;
  protected // IDataRowsetFind
    procedure Add(const Value: String); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: Longword); overload;
    procedure Add(const Value: TGuid); overload;
    procedure Add(const Value: Double); overload;
    procedure Add(Value: Char); overload;
    procedure Add(Value: Byte); overload;
    procedure Add(Value: Word); overload;
    procedure AddValue(const Value: Variant); 
    {$IFDEF D60}
    procedure Add(const Value: Variant); overload;
    {$ENDIF}
    function Find(First: Boolean): Boolean; virtual;
  public
    constructor Create(const Rowset: IDataRowset);
    destructor Destroy; override;
  end;

const
  REC_POS_DELETED = 1;
  REC_POS_INDEX = 2;

  REC_NAME = '$Recno';
  DEL_NAME = '$Deleted';

const
  ID_TABLE = 'SIL.DATA';
  ID_VER = 2;

  REC_TYPE_NORMAL = #32;
  REC_TYPE_DELETED = 'd';
  REC_TYPE_MEMO = 'm';

implementation

uses
  SilDataClasses,
  SilBtMem;

{ TDataRowset }

constructor TDataRowset.Create(const Stream: IRandomStream);
begin
  inherited Create;

  FVersion := ID_VER;
  FStream := Stream;
  FStatus := rsUnknown;

  FCheckDeleted := true;
  FCheckMemo := true;
  FCheckIndexes := true;
  FDisplays := TDataFieldDisplayList.Create;

  FFieldDefs := TDataFieldList.Create(Self);
  Reference.GetInterface(FFieldDefs, IDataFieldList, FFields);
  FIndexesDefs := TDataIndexList.Create(Self);
  Reference.GetInterface(FIndexesDefs, IDataIndexList, FIndexes);
end;

constructor TDataRowset.OpenFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode);
var
  Source: IFile;
begin
  Source := OS.FileSystem.OpenFile(FileName, Access, Share, true);
  Create(Source.Stream);
  Open;
end;

constructor TDataRowset.CreateFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode);
var
  Source: IFile;
begin
  Source := OS.FileSystem.CreateFile(FileName, Access, Share);
  Create(Source.Stream);
  FName := FileName;
end;

destructor TDataRowset.Destroy;
begin
  if FDisplays <> nil then FDisplays.Clear;
  FDisplays := nil;

  if FFieldDefs <> nil then
  begin
    FFieldDefs.Clear;
    FFieldDefs.Unbind;
    FFieldDefs := nil;
  end;

  FFields := nil;

  Close;
  inherited;
end;

procedure TDataRowset.CopyFrom(const RowsetDef: IDataRowsetDef);
var
  e: IEnumerator;
  FieldDef: IFieldDef;
  IndexDef: IDataIndexDef;
begin
  while RowsetDef.Fields.Enumerate(e, FieldDef) do
    FFieldDefs.CreateItem(FieldDef.Name, FieldDef.DataType, FieldDef.Size);

  while RowsetDef.Indexes.Enumerate(e, IndexDef) do
    if Str.ToChr(IndexDef.Name, #0) <> '$' then
      FIndexesDefs.CreateItem(IndexDef.Name, IndexDef.FieldList, IndexDef.Options);
end;

procedure TDataRowset.Build(const Name: String);
var
  bSize: Byte;
  wSize: Word;
  lwSize: LongWord;
  e: IEnumerator;
  Field: IFieldDef;
  Index: IDataIndexDef;
  sText: String;
  IdxOpt: TDataIndexOptions;
begin
  FStream.Position := 0;
  FRecordSize := 1;

  sText := ID_TABLE;
  bSize := Length(sText);
  FStream.Write(bSize, SizeOf(bSize));
  FStream.Write(sText[1], bSize);

  FStream.Write(FVersion, SizeOf(FVersion));

  if Str.NotEmpty(Name) then FName := Name;
  bSize := Length(FName);
  FStream.Write(bSize, SizeOf(bSize));
  FStream.Write(FName[1], bSize);

  wSize := FFieldDefs.Count;
  FStream.Write(wSize, SizeOf(wSize));

  while FFieldDefs.Enumerate(e, Field) do
  begin
    sText := Field.Name;
    bSize := Length(sText);
    FStream.Write(bSize, SizeOf(bSize));
    FStream.Write(sText[1], bSize);

    bSize := Ord(Field.DataType);
    FStream.Write(bSize, SizeOf(bSize));

    wSize := Field.Size;
    FStream.Write(wSize, SizeOf(wSize));
    Inc(FRecordSize, wSize);
  end;

  FDeletedIndex := FIndexesDefs.CreateInternal(DEL_NAME);

  wSize := FIndexesDefs.Count;
  FStream.Write(wSize, SizeOf(wSize));

  while FIndexesDefs.Enumerate(e, Index) do
  begin
    sText := Index.Name;
    bSize := Length(sText);
    FStream.Write(bSize, SizeOf(bSize));
    FStream.Write(sText[1], bSize);

    IdxOpt := Index.Options;
    FStream.Write(IdxOpt, SizeOf(IdxOpt));

    sText := Index.FieldList;
    bSize := Length(sText);
    FStream.Write(bSize, SizeOf(bSize));
    FStream.Write(sText[1], bSize);

    Index.RootPosition := FStream.Position;
    Index.Root := 0;

    lwSize := Index.Root;
    FStream.Write(lwSize, SizeOf(lwSize));

    Inc(FRecordSize, SizeOf(RAvlNodeData));
  end;

  FFieldDefs.PositionOffset := 1 + FIndexesDefs.Count * SizeOf(RAvlNodeData);
  while FIndexesDefs.Enumerate(e, Index) do Index.Bind;

  FTotalRecords := 0;
  FLogicalRecords := 0;

  FStream.Write(FTotalRecords, SizeOf(FTotalRecords));
  FStream.Write(FLogicalRecords, SizeOf(FLogicalRecords));

  FHeaderSize := FStream.Position + 1;
  FStream.Truncate;
  FStatus := rsBrowse;
end;

procedure TDataRowset.Initialize;
var
  bSize: Byte;
  e: IEnumerator;
  wIndexCount, wFieldCount, wSize: Word;
  i, lwSize: LongWord;
  Field: IFieldDef;
  Index: IDataIndexDef;
  sText1, sText2: String;
  IdxOpt: TDataIndexOptions;
begin
  Close;
  FStream.Position := 0;
  FRecordSize := 1;

  FStream.Read(bSize, SizeOf(bSize));
  SetLength(sText1, bSize);
  FStream.Read(sText1[1], bSize);

  if sText1 <> ID_TABLE then
  begin
    Sil.Error.Throw('TDataRowset.Initialize: Formato invalido');
    Exit;
  end;

  FStream.Read(FVersion, SizeOf(FVersion));

  FStream.Read(bSize, SizeOf(bSize));
  SetLength(FName, bSize);
  FStream.Read(FName[1], bSize);

  FStream.Read(wFieldCount, SizeOf(wFieldCount));

  for i := 1 to wFieldCount do
  begin
    FStream.Read(bSize, SizeOf(bSize));
    SetLength(sText1, bSize);
    FStream.Read(sText1[1], bSize);
    FStream.Read(bSize, SizeOf(bSize));
    FStream.Read(wSize, SizeOf(wSize));

    Field := FFieldDefs.CreateItem(sText1, TDataFieldType(bSize), wSize);
    Inc(FRecordSize, wSize);
  end;

  FStream.Read(wIndexCount, SizeOf(wSize));

  for i := 1 to wIndexCount do
  begin
    FStream.Read(bSize, SizeOf(bSize));
    SetLength(sText1, bSize);
    FStream.Read(sText1[1], bSize);
    FStream.Read(IdxOpt, SizeOf(bSize));

    FStream.Read(bSize, SizeOf(bSize));
    SetLength(sText2, bSize);
    FStream.Read(sText2[1], bSize);

    if Str.ToChr(sText1, #0) = '$' then
    begin
      FDeletedIndex := FIndexesDefs.CreateInternal(sText1);
      Index := FDeletedIndex;
    end else
      Index := FIndexesDefs.CreateItem(sText1, sText2, IdxOpt);

    Index.RootPosition := FStream.Position;
    FStream.Read(lwSize, SizeOf(lwSize));
    Index.Root := lwSize;

    Inc(FRecordSize, SizeOf(RAvlNodeData));
  end;

  FFieldDefs.PositionOffset := 1 + FIndexesDefs.Count * SizeOf(RAvlNodeData);
  while FIndexesDefs.Enumerate(e, Index) do Index.Bind;

  FStream.Read(FTotalRecords, SizeOf(FTotalRecords));

  if FVersion > 1 then
    FStream.Read(FLogicalRecords, SizeOf(FLogicalRecords));

  FHeaderSize := FStream.Position + 1;
  FStatus := rsBrowse;
end;

procedure TDataRowset.Open;
begin
  Initialize;
  First;
end;

procedure TDataRowset.Close;
begin
  FName := Str.Null;
  FRecordBuffer := Str.Null;

  FTotalRecords := 0;
  FLogicalRecords := 0;

  FBof := false;
  FEof := false;
  FModified := false;
  FCurrentRecord := 0;
  FRecordSize := 0;
  FHeaderSize := 0;
  FStatus := rsUnknown;
  if FFields <> nil then FFields.Clear;
  if FIndexes <> nil then FIndexes.Clear;
end;

procedure TDataRowset.Append;
begin
  if FStatus <> rsBrowse then Exit;
  FStatus := rsAppend;
  SetLength(FRecordBuffer, FRecordSize);
  FillChar(FRecordBuffer[1], FRecordSize, 0);
  FRecordBuffer[REC_POS_DELETED] := REC_TYPE_NORMAL;
end;

procedure TDataRowset.AppendRecord(const Values: array of const);
begin
  if FStatus <> rsBrowse then Exit;
end;

procedure TDataRowset.Edit;
begin
  if FStatus <> rsBrowse then Exit;
  FStatus := rsEdit;
end;

function TDataRowset.DoGoto(Dest: LongWord; Move: Integer): Boolean;
var
  lwLast: LongWord;
begin
  Result := false;
  lwLast := FCurrentRecord;

  while DoSetCurrent(Dest) do
  begin
    if FRecordBuffer[REC_POS_DELETED] = REC_TYPE_NORMAL then
    begin
      Result := true;
      Break;
    end;

    if (Move > 0) and not FEof then Inc(Dest) else
    if (Move < 0) and not FBof then Dec(Dest) else Break;
  end;

  if not Result then
  begin
    DoSetCurrent(lwLast);
    if Move > 0 then FEof := true;
    if Move < 0 then FBof := true;
  end;
end;

procedure TDataRowset.First;
begin
  //if FTotalRecords = 0 then Exit;

  if FActiveIndex = nil then
    DoGoto(1, 1) else
    DoGoto(FActiveIndex.First, 1);
end;

function TDataRowset.GetBof: Boolean;
begin
  Result := FBof;
end;

function TDataRowset.GetBookmark: String;
begin
  Result := Int.ToStr(FCurrentRecord);
end;

function TDataRowset.GetCanModify: Boolean;
begin
  Result := true;
end;

function TDataRowset.GetEof: Boolean;
begin
  Result := FEof;
end;

function TDataRowset.GetFieldDefs: IDataFieldDefList;
begin
  Reference.GetInterface(FFields, IDataFieldDefList, Result);
end;

function TDataRowset.GetFields: IDataFieldList;
begin
  Result := FFields;
end;

function TDataRowset.GetIndexDefs: IDataIndexDefList;
begin
  Result := FIndexesDefs;
end;

function TDataRowset.GetIndexes: IDataIndexList;
begin
  Result := FIndexes;
end;

function TDataRowset.GetModified: Boolean;
begin
  Result := FModified;
end;

function TDataRowset.GetName: String;
begin
  Result := FName;
end;

function TDataRowset.GetRecordCount: LongWord;
begin
  if FVersion > 1 then
    Result := FLogicalRecords else
    Result := FTotalRecords;
end;

function TDataRowset.GetCurrentRecord: LongWord;
begin
  Result := FCurrentRecord;
end;

function TDataRowset.GetRecordSize: LongWord;
begin
  Result := FRecordSize;
end;

procedure TDataRowset.Read(var Buffer; Position, Size: LongWord);
begin
  if Position + Size <= Str.Len(FRecordBuffer) then
    Move(FRecordBuffer[Position + 1], Buffer, Size);
end;

procedure TDataRowset.Write(const Buffer; Position, Size: LongWord);
begin
  if Position + Size <= Str.Len(FRecordBuffer) then
  begin
    Move(Buffer, FRecordBuffer[Position + 1], Size);
    if not FRecordChanged and (Position >= FFieldDefs.PositionOffset) then
      FRecordChanged := true;
  end;
end;

function TDataRowset.GetRowset: IDataRowset;
begin
  Result := Self;
end;

function TDataRowset.GetStore: IFieldStore;
begin
  Result := Self;
end;

function TDataRowset.GetRowsetDef: IDataRowsetDef;
begin
  Result := Self;
end;

function TDataRowset.GetStatus: TDataRowsetStatus;
begin
  Result := FStatus;
end;

function TDataRowset.IsEmpty: Boolean;
begin
  if FVersion > 1 then
    Result := FLogicalRecords = 0 else
    Result := FTotalRecords = 0;
end;

procedure TDataRowset.Last;
begin
  //if FTotalRecords = 0 then Exit;

  if FActiveIndex = nil then
    DoGoto(FTotalRecords, -1) else
    DoGoto(FActiveIndex.Last, 1);
end;

function TDataRowset.MoveBy(Distance: LargeInt): Integer;
var
  lwNewPos: LargeInt;
begin
  Result := 0;
  if Distance = 0 then Exit;

  if FActiveIndex = nil then
  begin
    lwNewPos := FCurrentRecord + Distance;
    if FTotalRecords = 0 then lwNewPos := 0 else
    if lwNewPos < 1 then lwNewPos := 1 else
    if lwNewPos >= FTotalRecords then lwNewPos := FTotalRecords;
    if DoSetCurrent(lwNewPos) then Result := lwNewPos;
  end else
  begin
    while true do
    begin
      if Distance > 0 then
        lwNewPos := FActiveIndex.Next else
        lwNewPos := FActiveIndex.Prior;

      if lwNewPos > 0 then
      begin
        DoGoto(lwNewPos, Distance);
        Result := FCurrentRecord;
      end else
        Break;
    end;
  end;
end;

procedure TDataRowset.Next;
begin
  if FActiveIndex = nil then
    DoGoto(FCurrentRecord + 1, 1) else
    DoGoto(FActiveIndex.Next, 1);
end;

procedure TDataRowset.DoClearChanges;
var
  Field: IFieldDef;
  e: IEnumerator;
begin
  while FFieldDefs.Enumerate(e, Field) do Field.IsChanged := false;
  FRecordChanged := false;
end;

function TDataRowset.DoMoveTo(RecordNumber: LongWord): Boolean;
begin
  Result := (FTotalRecords > 0) and (RecordNumber > 0) and (RecordNumber <= FTotalRecords);
  if not Result then Exit;

  if FRecordChanged then DoClearChanges;
  DoInternalMove(RecordNumber);

  FBof := RecordNumber < 1;
  FEof := RecordNumber > FTotalRecords;
end;

procedure TDataRowset.DoInternalMove(RecordNumber: LongWord);
begin
  FStream.Position := FHeaderSize + (RecordNumber - 1) * FRecordSize - 1;
  FCurrentRecord := RecordNumber;
end;

procedure TDataRowset.DoAppendDeleted(const Buffer: String; RecordNumber: LongWord);
var
  sBuffer: String;
begin
  sBuffer := Buffer;
  Str.Append(sBuffer, RecordNumber, SizeOf(LongWord));
  FDeletedIndex.AppendEntry(sBuffer, true);
end;

procedure TDataRowset.DoUpdateIndexes(Status: TDataRowsetStatus; const Changed: TStringArray);
var
  e: IEnumerator;
  Index: IDataIndexDef;
  sBuffer: String;
  bFound: Boolean;

  function DoFieldsChanged: Boolean;
  var
    i: Integer;
  begin
    for i := 0 to High(Changed) do
    begin
      Result := Index.FieldNames.IndexOf(Changed[i]) >= 0;
      if Result then Exit;
    end;
    Result := false;
  end;

begin
  if not FCheckIndexes then Exit;

  bFound := false;
  sBuffer := FRecordBuffer;
  Str.Append(sBuffer, FCurrentRecord, SizeOf(LongWord));

  while FIndexesDefs.Enumerate(e, Index) do
    if not Index.IsInternal then
    begin
      if not bFound then bFound := true;
      if (Changed <> nil) and not DoFieldsChanged then Continue;
      case Status of
        rsAppend: Index.GetInternal.AppendEntry(sBuffer, true);
        rsDelete: Index.GetInternal.DeleteEntry(sBuffer, true);
      end;
    end;

  FCheckIndexes := bFound;
end;

procedure TDataRowset.WriteRecord(RecordNumber: LongWord);
begin
  DoInternalMove(RecordNumber);
  FStream.Write(FRecordBuffer[1], FRecordSize);
end;

procedure TDataRowset.DoReadRecord;
begin
  SetLength(FRecordBuffer, FRecordSize);
  FStream.Read(FRecordBuffer[1], FRecordSize);
end;

function TDataRowset.DoGetBlankRecord(Logical: Boolean): LongWord;
var
  lwRecord: LongWord;
  sRec, sBuffer: String;
begin
  sRec := FRecordBuffer;

  if FCheckDeleted then
  begin
    SetLength(sBuffer, FRecordSize);
    sBuffer[1] := REC_TYPE_DELETED;
    lwRecord := 0;
    Str.Append(sBuffer, lwRecord, SizeOf(LongWord));

    lwRecord := FCurrentRecord;
    Result := FDeletedIndex.DeleteEntry(sBuffer, false);

    if Result < 1 then
    begin
      Result := FTotalRecords + 1;
      FCheckDeleted := false;
    end;
  end else
    Result := FTotalRecords + 1;

  if (Result > FTotalRecords) or Logical then
    DoUpdateRecordCount(Result, FLogicalRecords + Byte(Ord(Logical)));

  if FCheckDeleted and (FCurrentRecord <> lwRecord) then DoInternalMove(lwRecord);
  FRecordBuffer := sRec;
end;

procedure TDataRowset.DoUpdateRecordCount(TotalRecords, LogicalRecords: LongWord);
begin
  FTotalRecords := TotalRecords;
  FStream.Position := FHeaderSize - SizeOf(LongWord) * 2 - 1; // + 1;
  FStream.Write(FTotalRecords, SizeOf(FTotalRecords));

  if FVersion > 1 then
  begin
    FLogicalRecords := LogicalRecords;
    FStream.Write(FLogicalRecords, SizeOf(FLogicalRecords));
  end;
end;

procedure TDataRowset.Delete;
begin
  if FStatus <> rsBrowse then Exit;

  FCheckDeleted := true;
  DoUpdateIndexes(rsDelete);
  FRecordBuffer[REC_POS_DELETED] := REC_TYPE_DELETED;

  WriteRecord(FCurrentRecord);
  DoCheckMemo(rsDelete);

  DoUpdateRecordCount(FTotalRecords, FLogicalRecords - 1);

  DoInternalMove(FCurrentRecord);
  DoAppendDeleted(FRecordBuffer, FCurrentRecord);

  FillChar(FRecordBuffer[1], FRecordSize, 0);
  DoGoto(FCurrentRecord, 1)
end;

procedure TDataRowset.DoAppendPost;
var
  lwNextRecord: LongWord;
begin
  lwNextRecord := DoGetBlankRecord(true);
  WriteRecord(lwNextRecord);
  DoCheckMemo(rsAppend);

  DoInternalMove(FCurrentRecord);
  DoUpdateIndexes(rsAppend);
end;

procedure TDataRowset.DoEditPost;
var
  sBuffer: String;
  lwPos: LongWord;
  Changed: TStringArray;

  procedure DoGetChanged;
  var
    e: IEnumerator;
    Field: IFieldDef;
  begin
    while FFieldDefs.Enumerate(e, Field) do
      if Field.IsChanged then Str.ArrayAdd(Changed, Field.Name);
  end;

begin
  if FCurrentRecord < 1 then Exit;

  Changed := nil;
  if FRecordChanged then DoGetChanged;

  sBuffer := FRecordBuffer;
  ChangeCurrentRecord(FCurrentRecord);
  DoUpdateIndexes(rsDelete, Changed);

  lwPos := FFieldDefs.PositionOffset + 1;
  Move(sBuffer[lwPos], FRecordBuffer[lwPos], FRecordSize - lwPos + 1);

  WriteRecord(FCurrentRecord);
  DoCheckMemo(rsEdit);
  if FRecordChanged then DoClearChanges;

  DoUpdateIndexes(rsAppend, Changed);
  DoCheckIndexPos;
end;

procedure TDataRowset.DoCheckMemo(Status: TDataRowsetStatus);
var
  e: IEnumerator;
  FieldDef: IFieldDef;
  bFound: Boolean;
begin
  if not FCheckMemo then Exit;
  bFound := false;

  while FFieldDefs.Enumerate(e, FieldDef) do
    if FieldDef.DataType = ftMemo then
    begin
      if not bFound then bFound := true;

      if Status = rsEdit then
      begin
        if FieldDef.IsChanged then
        begin
          DoMemoWalk(FieldDef, rsDelete);
          DoMemoWalk(FieldDef, rsAppend);
        end;
      end else
        DoMemoWalk(FieldDef, Status);
    end;

  FCheckMemo := bFound;
end;

function TDataRowset.ReadMemo(const Field: IFieldDef): String;
begin
  Result := DoMemoWalk(Field, rsBrowse);
end;

function TDataRowset.DoMemoWalk(const Field: IFieldDef; Status: TDataRowsetStatus): String;
var
  lwRecord, lwMemo, lwPos, lwSize, lwNextRecord: LongWord;
  Data: TDataMemoValue;
  sRecBuf: String;
  PBuf: PChar;

  procedure DoCollect;
  begin
    repeat
      if not ChangeCurrentRecord(lwMemo) then Break;
      PBuf := PChar(FRecordBuffer);

      if PDataMemoHeader(PBuf)^.RecType = REC_TYPE_MEMO then
      begin
        lwMemo := PDataMemoHeader(PBuf)^.NextRecord;

        case Status of
          rsBrowse:
          begin
            if lwPos + lwSize - 1 > Data.Size then lwSize := Data.Size - lwPos + 1;
            Move(FRecordBuffer[SizeOf(TDataMemoHeader) + 1], Result[lwPos], lwSize);
            Inc(lwPos, lwSize);
          end;

          rsDelete:
          begin
            PDataMemoHeader(PBuf)^.RecType := REC_TYPE_DELETED;
            WriteRecord(FCurrentRecord);
          end;
        end;
      end else
        lwMemo := 0;
    until lwMemo = 0;
  end;

begin
  Result := '';
  Read(Data, Field.Position, Field.Size);

  lwRecord := FCurrentRecord;
  lwMemo := Data.NextRecord;
  lwPos := 1;
  lwSize := FRecordSize - SizeOf(TDataMemoHeader);

  case Status of
    rsBrowse:
    begin
      SetLength(Result, Data.Size);
      DoCollect;
    end;

    rsDelete:
      DoCollect;

    rsAppend:
    begin
      sRecBuf := Field.GetValue;

      if Str.NotEmpty(sRecBuf) then
        lwMemo := DoGetBlankRecord(false) else
        lwMemo := 0;

      Data.NextRecord := lwMemo;
      Write(Data, Field.Position, Field.Size);
      WriteRecord(lwRecord);

      if lwMemo > 0 then
        while lwPos <= Str.Len(sRecBuf) do
        begin
          if lwPos + lwSize - 1 > Str.Len(sRecBuf) then
          begin
            lwSize := Str.Len(sRecBuf) - lwPos + 1;
            lwNextRecord := 0;
          end else
            lwNextRecord := DoGetBlankRecord(false);

          PDataMemoHeader(PChar(FRecordBuffer))^.RecType := REC_TYPE_MEMO;
          PDataMemoHeader(PChar(FRecordBuffer))^.NextRecord := lwNextRecord;
          Move(sRecBuf[lwPos], FRecordBuffer[SizeOf(TDataMemoHeader) + 1], lwSize);

          WriteRecord(lwMemo);
          Inc(lwPos, lwSize);

          lwMemo := lwNextRecord;
        end;
    end;
  end;

  ChangeCurrentRecord(lwRecord);
end;

procedure TDataRowset.Post;
begin
  case FStatus of
    rsAppend: DoAppendPost;
    rsEdit:   DoEditPost;
    else Exit;
  end;

  FStatus := rsBrowse;
end;

procedure TDataRowset.Reindex;
var
  e: IEnumerator;
  Active, Index: IDataIndexDef;
  lwRecord, lwRest: LongWord;
begin
  lwRecord := FCurrentRecord;
  lwRest := 0;
  Active := FActiveIndex;
  SetActiveIndex(nil);
  DoMoveTo(1);

  while FIndexesDefs.Enumerate(e, Index) do
  begin
    Index.ClearTree;
    Index.Root := 0;
  end;

  while lwRest < FTotalRecords do
  begin
    Inc(lwRest);
    DoInternalMove(lwRest);

    DoReadRecord;
    FillChar(FRecordBuffer[2], FIndexesDefs.Count * SizeOf(RAvlNodeData), 0);
    WriteRecord(FCurrentRecord);

    case FRecordBuffer[REC_POS_DELETED] of
      REC_TYPE_NORMAL:  DoUpdateIndexes(rsAppend);
      REC_TYPE_DELETED: DoAppendDeleted(FRecordBuffer, FCurrentRecord);
    end;
  end;

  SetActiveIndex(Active);
  SetCurrentRecord(lwRecord);
end;

function TDataRowset.Find(const Values: array of Variant; Nearest: Boolean): Boolean;
var
  sBuffer: String;
  lwRecord, lwSize: LongWord;
  e: IEnumerator;
  Field: IFieldDef;
  Data: Pointer;
begin
  Result := false;
  if FActiveIndex = nil then Exit;
  if FRecordChanged then DoClearChanges;

  lwRecord := 0;
  sBuffer := FRecordBuffer;
  Str.Fill(FRecordBuffer, FRecordSize);
  Str.Append(FRecordBuffer, lwRecord, SizeOf(LongWord));

  while FActiveIndex.FieldDefs.Enumerate(e, Field) do
  begin
    if e.Iteration > High(Values) then Break;
    lwSize := Vart.GetInfo(Values[e.Iteration], Data);

    if Vart.VType(Values[e.Iteration]) = 256 then
      Field.AsString := PChar(Data) else
      Write(Data^, Field.Position, lwSize);
  end;

  lwRecord := FCurrentRecord;
  Result := FActiveIndex.FindEntry(FRecordBuffer, 0, false) = 0;

  if not Result and not Nearest then
  begin
    DoMoveTo(lwRecord);
    FRecordBuffer := sBuffer;
  end else
  if Result then
  begin
    DoMoveTo(FCurrentRecord);
    DoReadRecord;
  end;
end;

function TDataRowset.FindBuffer(Buffer: PChar; Size: LongWord): Boolean;
var
  sBuffer: String;
  FieldCount, lwRecord: LongWord;
  e: IEnumerator;
  Field: IFieldDef;
begin
  Result := false;
  if FActiveIndex = nil then Exit;
  if FRecordChanged then DoClearChanges;

  lwRecord := 0;
  FieldCount := 0;
  
  sBuffer := FRecordBuffer;
  Str.Fill(FRecordBuffer, FRecordSize);
  Str.Append(FRecordBuffer, lwRecord, SizeOf(LongWord));

  while (Size > 1) and FActiveIndex.FieldDefs.Enumerate(e, Field) do
    if e.Iteration < FActiveIndex.FieldDefs.Count - 1 then
    begin
      Inc(FieldCount);
      Write(Buffer^, Field.Position, Field.Size);
      Inc(Buffer, Field.Size);
      Dec(Size, Field.Size);
    end;

  lwRecord := FCurrentRecord;
  Result := FActiveIndex.FindEntry(FRecordBuffer, FieldCount, false) = 0;

  if Result then
  begin
    DoMoveTo(FCurrentRecord);
    DoReadRecord;
  end;
end;

procedure TDataRowset.Prior;
begin
  if FCurrentRecord > 0 then
    if FActiveIndex = nil then
      DoGoto(FCurrentRecord - 1, -1) else
      DoGoto(FActiveIndex.Prior, -1);
end;

procedure TDataRowset.DoCheckIndexPos;
var
  sBuffer: String;
begin
  sBuffer := FRecordBuffer;
  Str.Append(sBuffer, FCurrentRecord, SizeOf(LongWord));
  if FActiveIndex <> nil then FActiveIndex.FindEntry(sBuffer, 0, true);
end;

procedure TDataRowset.SetBookmark(const Value: String);
begin
  DoSetCurrent(Str.ToInt(Value, 0));
  DoCheckIndexPos;
end;

function TDataRowset.DoSetCurrent(Value: LongWord): Boolean;
begin
  if FRecordChanged then DoClearChanges;

  Result := DoMoveTo(Value);
  if Result then DoReadRecord;
end;

function TDataRowset.ChangeCurrentRecord(Position: LongWord): Boolean;
begin
  Result := (FTotalRecords > 0) and (Position > 0) and (Position <= FTotalRecords);
  if Result then
  begin
    DoInternalMove(Position);
    DoReadRecord;
  end;
end;

procedure TDataRowset.SetCurrentRecord(Value: LongWord);
begin
  DoSetCurrent(Value);
  DoCheckIndexPos;
  //if FRecordBuffer[REC_POS_DELETED] <> REC_TYPE_NORMAL then DoGoto(Value, 1);
end;

procedure TDataRowset.SetRecordSize(Value: LongWord);
begin
  FRecordSize := Value;
end;

procedure TDataRowset.UpdateRecord(const Values: array of const);
begin
  if FStatus <> rsBrowse then Exit;
end;

function TDataRowset.AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
begin
  Result := 0;
  Source.First;

  while not Source.IsEof do
  begin
    AppendRecordFrom(Source);
    Inc(Result);
    Source.Next;
  end;
end;

procedure TDataRowset.AppendRecordFrom(const Source: IReadOnlyDataRowset);
var
  e: IEnumerator;
  Src, Dst: IFieldAccess;
begin
  if Source.IsEof then Exit;

  try
    Append;

    while Source.Fields.Enumerate(e, Src) do
    begin
      Dst := FFields[Src.Name];

      if (Dst = nil) and
        (FFields.Count < e.Iteration) and
        (FFields.Items[e.Iteration].DataType = Src.DataType) and
        (FFields.Items[e.Iteration].Size = Src.Size) then
        Dst := FFields.Items[e.Iteration];

      if Dst <> nil then
        case Src.DataType of
          ftSmallInt,
          ftInteger,
          ftWord,
          ftAutoInc:  Dst.AsInteger := Src.AsInteger;
          ftString:   Dst.AsString := Src.AsString;
          ftFloat,
          ftCurrency,
          ftDate,
          ftTime,
          ftDateTime: Dst.AsDateTime := Src.AsFloat;
          ftBoolean:  Dst.AsBoolean := Src.AsBoolean;
        end;
    end;

    Post;
  except end;
end;

function TDataRowset.GetActiveIndex: IDataIndex;
begin
  Result := FActiveIndex;
end;

procedure TDataRowset.SetActiveIndex(const Value: IDataIndex);
begin
  if (Value <> nil) and Reference.GetInterface(Value, IDataIndexInternal, FActiveIndex) then
    DoCheckIndexPos else
    FActiveIndex := nil;
end;

function TDataRowset.GetRecordBuffer: String;
begin
  Result := FRecordBuffer;
end;

function TDataRowset.GetActiveIndexName: String;
begin
  if FActiveIndex <> nil then
    Result := FActiveIndex.Name else
    Result := '';
end;

procedure TDataRowset.SetActiveIndexName(const Value: String);
begin
  if (FActiveIndex = nil) or (Sil.Text.Compare(Value, FActiveIndex.Name) <> 0) then
    SetActiveIndex(FIndexes.Names[Value]);
end;

procedure TDataRowset.WriteIndexRoot(RecordNumber: LongWord; const Index: IDataIndexDef);
var
  dwRoot, dwPosition: LongWord;
begin
  dwPosition := FStream.Position;
  FStream.Position := Index.RootPosition;
  dwRoot := Index.Root;
  FStream.Write(dwRoot, SizeOf(LongWord));
  FStream.Position := dwPosition;
end;

procedure TDataRowset.DoFillDisplays;
var
  e: IEnumerator;
  Field: IFieldAccess;
begin
  while FFields.Enumerate(e, Field) do
    FDisplays.CreateItem(Field, Field.Name);
end;

function TDataRowset.GetDisplays: IFieldDisplayList;
begin
  Result := FDisplays;
  if Result.Count = 0 then DoFillDisplays;
end;

function TDataRowset.CreateDisplay(const FieldName, Caption, Mask: String): IFieldDisplay;
begin
  Result := FDisplays.CreateItem(FFields[FieldName], Caption, Mask);
end;

function TDataRowset.GetStream: IRandomStream;
begin
  Result := FStream;
end;

function TDataRowset.FindValues: IDataRowsetFind;
begin
  Result := TRowsetFind.Create(Self);
end;

{ TRowsetFind }

constructor TRowsetFind.Create(const Rowset: IDataRowset);
begin
  inherited Create;

  FRowset := Rowset;
  FPacket := Sil.Stream.Raw.Packet;
  FIndex := FRowset.ActiveIndex as IDataIndexInternal;
  FIdxFld := -1;

  if not Assigned(FIndex) then
    raise Error.Create('no hay index activo');
end;

destructor TRowsetFind.Destroy;
begin
  FRowset := nil;
  FPacket := nil;
  inherited;
end;

procedure TRowsetFind.Add(Value: Longword);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteLongWord(Value);
end;

procedure TRowsetFind.Add(const Value: TGuid);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteGuid(Value);
end;

procedure TRowsetFind.Add(const Value: String);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteString(Str.PadR(Value, FIndex.FieldDefs.Items[FIdxFld].Size, #0));
end;

procedure TRowsetFind.Add(Value: Integer);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteInteger(Value);
end;

procedure TRowsetFind.Add(Value: Byte);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteByte(Value);
end;

procedure TRowsetFind.Add(Value: Word);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteWord(Value);
end;

procedure TRowsetFind.AddValue(const Value: Variant);
begin
  with FIndex.FieldDefs.Items[FIdxFld + 1] do
    case DataType of
      ftByte: Add(Byte(Value));
      ftChar: Add(System.Chr(Byte(Value)));
      ftInteger, ftLongWord: Add(Integer(Value));
      ftString: Add(String(Value));
      ftWideString: Add(WideString(Value));
      ftSmallInt: Add(SmallInt(Value));
      ftGuid: Add(Sil.GUID.FromStr(Value));
      ftWord: Add(Word(Value));
      ftBoolean: Add(Byte(Value));
      ftFloat: Add(Double(Value));
      ftCurrency: Add(Currency(Value));
      ftDate, ftTime, ftDateTime: Add(TDateTime(Value));
      else raise Error.Create('Field con tipo incompatible');
    end;
end;

{$IFDEF D60}
procedure TRowsetFind.Add(const Value: Variant);
begin
  AddValue(Value)
end;
{$ENDIF}

procedure TRowsetFind.Add(Value: Char);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteChar(Value);
end;

procedure TRowsetFind.Add(const Value: Double);
begin
  Inc(FIdxFld);
  FPacket.Writer.WriteDouble(Value);
end;

function TRowsetFind.Find(First: Boolean): Boolean;
var
  FoundPos: LongWord;
  Buf: String;
begin
  Result := FRowset.FindBuffer(FPacket.Buffer.Memory, FPacket.Buffer.Size);
  Buf := FRowset.RecordBuffer;

  if Result and First then
  begin
    repeat
      FoundPos := FRowset.CurrentRecord;
      FRowset.Prior;
    until FRowset.IsBof or (FIndex.CompareBuffer(PChar(Buf)) <> 0);

    FRowset.CurrentRecord := FoundPos;
  end;

  DoFindEnd;
end;

procedure TRowsetFind.DoFindEnd;
begin
end;

end.
