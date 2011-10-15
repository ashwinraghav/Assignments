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

unit SilSmDataRowsetFilter;

{$include Defines.inc}

interface

uses
  Sil,
  SilEval,
  SilTokens,

  SilSiDataAccess,
  SilSmDataRowset;

type
  IInternalFilter = interface
    ['{B1DAF752-213B-40B0-86D3-DE91FB57B618}']
    procedure SyncWithMaster;
  end;

  TSilDataRowsetFilter = class (
    TSilObject,
    IDataRowset,
    IDataRowsetDef,
    IInternalFilter,
    ITokenHandler)
  private
    FSource: IDataRowset;
    FMap: IDataRowset;
    FEvaluator: IEvaluator;
  private
    procedure DoSyncSource;
    function DoSyncMap: Boolean;
    procedure DoInitMap;
  protected // IInternalFilter
    procedure SyncWithMaster;
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
    procedure Write(const Buffer; Position, Size: LongWord);
    procedure Read(var Buffer; Position, Size: LongWord);
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
  protected // ITokenHandler
    function GetTokenValue(const Token: IToken): Variant;
    procedure SetTokenValue(const Token: IToken; const Value: Variant);
  public
    constructor Create(const Source: IDataRowset; const FilterCond: String; const IndexName: String = '');
    destructor Destroy; override;
  end;

  TFilterRowsetFind = class (TRowsetFind)
  private
    FFilter: IInternalFilter;
  protected
    procedure DoFindEnd; override;
  public
    constructor Create(const Rowset, Filter: IDataRowset);
  end;

implementation

uses
  SilData,
  SilStDataRowset;

{ TSilDataRowsetFilter }

constructor TSilDataRowsetFilter.Create(const Source: IDataRowset; const FilterCond, IndexName: String);
begin
  inherited Create;

  FSource := Source;
  FEvaluator := SilEval.Tool.Create(Self, FilterCond);

  FSource.ActiveIndexName := IndexName;
  DoInitMap;
end;

destructor TSilDataRowsetFilter.Destroy;
begin
  FSource := nil;
  FEvaluator := nil;
  FMap := nil;

  inherited;
end;

procedure TSilDataRowsetFilter.DoInitMap;
var
  Table: IDataRowsetDef;
  HasIndex: Boolean;
begin
  FMap := nil;
  
  Table := SilData.Tk.Memory;

  Table.Fields.CreateItem('index', ftLongWord);
  Table.Indexes.CreateItem('index', 'index');
  Table.Build;

  FMap := Table.Rowset;
  FMap.ActiveIndexName := 'index';

  HasIndex := Assigned(FSource.ActiveIndex);
  if not HasIndex then FSource.First;

  while not FSource.IsEof do
  begin
    if FEvaluator.Execute then
    begin
      FMap.Append;
      FMap.Fields['index'].AsLongWord := FSource.CurrentRecord;
      FMap.Post;
    end
    else if HasIndex then
      Break;

    FSource.Next;
  end;

  First;
end;

function TSilDataRowsetFilter.GetTokenValue(const Token: IToken): Variant;
var
  Field: IFieldAccess;
begin
  Field := FSource.Fields[Token.Lexema];

  if Field.DataType = ftGuid then
    Result := Field.AsString else
    Result := Field.AsVariant;
end;

procedure TSilDataRowsetFilter.SetTokenValue(const Token: IToken; const Value: Variant);
begin
end;

procedure TSilDataRowsetFilter.DoSyncSource;
begin
  FSource.CurrentRecord := FMap.Fields['index'].AsLongWord;
end;

function TSilDataRowsetFilter.DoSyncMap: Boolean;
begin
  Result := FMap.Find([FSource.CurrentRecord], false);
end;

procedure TSilDataRowsetFilter.Initialize;
begin
end;

procedure TSilDataRowsetFilter.Build(const Name: String);
begin
end;

function TSilDataRowsetFilter.ChangeCurrentRecord(Position: LongWord): Boolean;
begin
  Result := FMap.RowsetDef.ChangeCurrentRecord(Position);
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.CopyFrom(const RowsetDef: IDataRowsetDef);
begin
end;

function TSilDataRowsetFilter.GetFieldDefs: IDataFieldDefList;
begin
  Result := FSource.RowsetDef.Fields;
end;

function TSilDataRowsetFilter.GetIndexDefs: IDataIndexDefList;
begin
  Result := FSource.RowsetDef.Indexes;
end;

function TSilDataRowsetFilter.GetRecordBuffer: String;
begin
  Result := FSource.RecordBuffer;
end;

function TSilDataRowsetFilter.GetRowset: IDataRowset;
begin
  Result := FSource;
end;

function TSilDataRowsetFilter.GetStore: IFieldStore;
begin
  Result := FSource.RowsetDef.Store;
end;

function TSilDataRowsetFilter.ReadMemo(const Field: IFieldDef): String;
begin
end;

procedure TSilDataRowsetFilter.WriteIndexRoot(RecordNumber: LongWord; const Index: IDataIndexDef);
begin
end;

procedure TSilDataRowsetFilter.WriteRecord(RecordNumber: LongWord);
begin
end;

procedure TSilDataRowsetFilter.Append;
begin
end;

procedure TSilDataRowsetFilter.AppendRecord(const Values: array of const);
begin
end;

procedure TSilDataRowsetFilter.AppendRecordFrom(const Source: IReadOnlyDataRowset);
begin
end;

function TSilDataRowsetFilter.AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
begin
  Result := 0;
end;

procedure TSilDataRowsetFilter.Close;
begin
end;

function TSilDataRowsetFilter.CreateDisplay(const FieldName, Caption, Mask: String): IFieldDisplay;
begin
end;

procedure TSilDataRowsetFilter.Delete;
begin
end;

procedure TSilDataRowsetFilter.Edit;
begin
end;

function TSilDataRowsetFilter.Find(const Values: array of Variant; Nearest: Boolean): Boolean;
begin
  if FSource.Find(Values, Nearest) then
    Result := DoSyncMap else
    Result := false;
end;

function TSilDataRowsetFilter.FindBuffer(Buffer: PChar; Size: LongWord): Boolean;
begin
  if FSource.FindBuffer(Buffer, Size) then
    Result := DoSyncMap else
    Result := false;
end;

function TSilDataRowsetFilter.FindValues: IDataRowsetFind;
begin
  Result := TFilterRowsetFind.Create(FSource, Self);
end;

procedure TSilDataRowsetFilter.First;
begin
  FMap.First;
  DoSyncSource;
end;

function TSilDataRowsetFilter.GetActiveIndex: IDataIndex;
begin
  Result := FSource.ActiveIndex;
end;

function TSilDataRowsetFilter.GetActiveIndexName: String;
begin
  Result := FSource.ActiveIndexName;
end;

function TSilDataRowsetFilter.GetBof: Boolean;
begin
  Result := FMap.IsBof;
end;

function TSilDataRowsetFilter.GetBookmark: String;
begin
  Result := FMap.Bookmark;
end;

function TSilDataRowsetFilter.GetCanModify: Boolean;
begin
  Result := false;
end;

function TSilDataRowsetFilter.GetCurrentRecord: LongWord;
begin
  Result := FMap.CurrentRecord;
end;

function TSilDataRowsetFilter.GetDisplays: IFieldDisplayList;
begin
end;

function TSilDataRowsetFilter.GetEof: Boolean;
begin
  Result := FMap.IsEof;
end;

function TSilDataRowsetFilter.GetFields: IDataFieldList;
begin
  Result := FSource.Fields;
end;

function TSilDataRowsetFilter.GetIndexes: IDataIndexList;
begin
  Result := FSource.Indexes;
end;

function TSilDataRowsetFilter.GetModified: Boolean;
begin
  Result := FSource.IsModified;
end;

function TSilDataRowsetFilter.GetName: String;
begin
  Result := FSource.Name;
end;

function TSilDataRowsetFilter.GetRecordCount: LongWord;
begin
  Result := FMap.RecordCount;
end;

function TSilDataRowsetFilter.GetRecordSize: LongWord;
begin
  Result := FSource.RecordSize;
end;

function TSilDataRowsetFilter.GetRowsetDef: IDataRowsetDef;
begin
  Result := FSource.RowsetDef;
end;

function TSilDataRowsetFilter.GetStatus: TDataRowsetStatus;
begin
  Result := FSource.Status;
end;

function TSilDataRowsetFilter.GetStream: IRandomStream;
begin
  Result := FSource.Stream;
end;

function TSilDataRowsetFilter.IsEmpty: Boolean;
begin
  Result := FMap.RecordCount = 0;
end;

procedure TSilDataRowsetFilter.Last;
begin
  FMap.Last;
  DoSyncSource;
end;

function TSilDataRowsetFilter.MoveBy(Distance: LargeInt): Integer;
begin
  Result := FMap.MoveBy(Distance);
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.Next;
begin
  FMap.Next;
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.Open;
begin
end;

procedure TSilDataRowsetFilter.Post;
begin
end;

procedure TSilDataRowsetFilter.Prior;
begin
  FMap.Prior;
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.Reindex;
begin
  FSource.Reindex;
end;

procedure TSilDataRowsetFilter.SetActiveIndex(const Value: IDataIndex);
begin
  if Assigned(Value) then
    SetActiveIndexName(Value.Name) else
    SetActiveIndexName('');
end;

procedure TSilDataRowsetFilter.SetActiveIndexName(const Value: String);
begin
  FSource.ActiveIndexName := Value;
  DoSyncMap;
  //DoInitMap;
end;

procedure TSilDataRowsetFilter.SetBookmark(const Value: String);
begin
  FMap.Bookmark := Value;
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.SetCurrentRecord(Value: LongWord);
begin
  FMap.CurrentRecord := Value;
  DoSyncSource;
end;

procedure TSilDataRowsetFilter.SetRecordSize(Value: LongWord);
begin
end;

procedure TSilDataRowsetFilter.UpdateRecord(const Values: array of const);
begin
end;

procedure TSilDataRowsetFilter.Read(var Buffer; Position, Size: LongWord);
begin
end;

procedure TSilDataRowsetFilter.Write(const Buffer; Position, Size: LongWord);
begin
end;

procedure TSilDataRowsetFilter.SyncWithMaster;
begin
  DoSyncMap;
end;

{ TFilterRowsetFind }

constructor TFilterRowsetFind.Create(const Rowset, Filter: IDataRowset);
begin
  inherited Create(Rowset);
  FFilter := Filter as IInternalFilter;
end;

procedure TFilterRowsetFind.DoFindEnd;
begin
  FFilter.SyncWithMaster;
end;

end.
