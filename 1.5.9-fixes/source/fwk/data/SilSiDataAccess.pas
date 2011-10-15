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

unit SilSiDataAccess;

{$I Defines.inc}

interface

uses
  Sil;

type
  TDataRowsetStatus = (rsUnknown, rsBrowse, rsAppend, rsEdit, rsDelete);
  IDataRowsetDef = interface;
  IDataFieldList = interface;
  IDataFieldDefList = interface;
  IDataIndexList = interface;
  IDataIndexDefList = interface;
  IDataIndexInternal = interface;

  IFieldDisplay = interface
    ['{0E662637-69A6-40B4-8AC0-F98F26721F20}']
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    function GetMask: String;
    function GetDisplay: String;
    function GetField: IFieldAccess;
    procedure SetMask(const Value: String);
    property Caption: String read GetCaption write SetCaption;
    property Mask: String read GetMask write SetMask;
    property Display: String read GetDisplay;
    property Field: IFieldAccess read GetField;
  end;

  IFieldDisplayList = interface (IList)
    ['{1C5E990D-8096-4742-8C64-B4187052E209}']
    function CreateItem(const Field: IFieldAccess; const Caption: String = ''; const Mask: String = ''): IFieldDisplay;
    function GetItem(Index: Integer): IFieldDisplay;
    function GetByName(const Name: String): IFieldDisplay;
    function GetDisplay(Index: Integer): String;
    property Items[Index: Integer]: IFieldDisplay read GetItem;
    property Names[const Name: String]: IFieldDisplay read GetByName;
    property Display[Index: Integer]: String read GetDisplay; default;
  end;

  IDataRowsetSupport = interface
    ['{2641EC93-FE10-11D4-98B5-00104B0FA1EF}']
    function GetVersion: Word;
    function GetStream: IRandomStream;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetRecordCount: LongWord;
    function GetEof: Boolean;
    function GetModified: Boolean;
    function GetRecordSize: LongWord;
    function GetHeaderSize: LongWord;
    function GetFields: IDataFieldList;
    function GetFieldDefs: IDataFieldDefList;
    function GetIndexes: IDataIndexList;
    function GetIndexesDefs: IDataIndexDefList;
    function GetDeletedIndex: IDataIndexInternal;
    procedure Build(const Name: String = '');
    procedure Initialize;
    procedure Close;
    property Stream: IRandomStream read GetStream;
  end;

  IDataFieldList = interface (IItemization)
    ['{73928379-567E-11D4-988A-00104B0FA1EF}']
    function GetCount: Integer;
    function GetByName(const Name: String): IFieldAccess;
    function Locked: ILock;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    procedure Clear;
    //function Get(Index: Integer): Pointer;
    function First: IFieldAccess;
    function Last: IFieldAccess;
    property Count: Integer read GetCount;
    function GetItem(Index: Integer): IFieldAccess;
    function GetNames: String;
    property Items[Index: Integer]: IFieldAccess read GetItem;
    property Names[const Name: String]: IFieldAccess read GetByName; default;
  end;

  IDataFieldDefList = interface (IList)
    ['{7392837A-567E-11D4-988A-00104B0FA1EF}']
    function CreateItem(const Name: String = ''; DataType: TDataFieldType = ftUnknown; Size: LongWord = 0): IFieldDef;
    function First: IFieldDef;
    procedure AddItems(const Source: IDataFieldDefList);
    function Add(const Value: IFieldDef): Integer;
    function Last: IFieldDef;
    procedure Clear;
    procedure Unbind;
    function GetItem(Index: Integer): IFieldDef;
    procedure SetItem(Index: Integer; const Value: IFieldDef);
    function GetByName(const Name: String): IFieldDef;
    function GetPositionOffset: LongWord;
    procedure SetPositionOffset(Value: LongWord);
    property Items[Index: Integer]: IFieldDef read GetItem write SetItem;
    property Names[const Name: String]: IFieldDef read GetByName; default;
    property PositionOffset: LongWord read GetPositionOffset write SetPositionOffset;
  end;

  TDataIndexOption = (ixUnique, ixDescending, ixIgnoreCase);
  TDataIndexOptions = set of TDataIndexOption;

  IDataIndex = interface
    ['{B68181E2-573E-11D4-988A-00104B0FA1EF}']
    procedure Close;
    function GetName: String;
    function GetFieldList: String;
    function GetFields: IStrings;
    function GetOptions: TDataIndexOptions;
    function CompareBuffer(Buffer: PChar): Integer;
    property Name: String read GetName;
    property FieldList: String read GetFieldList;
    property Fields: IStrings read GetFields;
    property Options: TDataIndexOptions read GetOptions;
  end;

  IDataIndexDef = interface (IDataIndex)
    ['{B68181E1-573E-11D4-988A-00104B0FA1EF}']
    procedure SetName(const Value: String);
    function GetFieldNames: IStringList;
    procedure SetOptions(Value: TDataIndexOptions);
    function GetRoot: LongWord;
    procedure SetRootPosition(Value: LongWord);
    function GetRootPosition: LongWord;
    function GetInternal: IDataIndexInternal;
    function GetRootBuffer: String;
    function GetIsInternal: Boolean;
    procedure SetRoot(Value: LongWord);
    procedure Bind;
    procedure ClearTree;
    property Name: String read GetName write SetName;
    property FieldNames: IStringList read GetFieldNames;
    property Options: TDataIndexOptions read GetOptions write SetOptions;
    property Root: LongWord read GetRoot write SetRoot;
    property RootPosition: LongWord read GetRootPosition write SetRootPosition;
    property IsInternal: Boolean read GetIsInternal;
  end;

  IDataIndexInternal = interface (IDataIndexDef)
    ['{FFB6C804-8E43-11D4-989D-00104B0FA1EF}']
    function GetFieldDefs: IDataFieldDefList;
    procedure AppendEntry(var Buffer: String; Internal: Boolean);
    function DeleteEntry(var Buffer: String; Internal: Boolean): Integer;
    function FindEntry(Buffer: String; FieldCount: LongWord; Internal: Boolean): Integer;
    function First: LongWord;
    function Last: LongWord;
    function Next: LongWord;
    function Prior: LongWord;
    property FieldDefs: IDataFieldDefList read GetFieldDefs;
  end;

  IDataIndexList = interface (IItemization)
    ['{B68181E5-573E-11D4-988A-00104B0FA1EF}']
    function GetCount: Integer;
    function GetByName(const Name: String): IDataIndex;
    function Locked: ILock;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    function First: IDataIndex;
    function Last: IDataIndex;
    procedure Clear;
    function GetItem(Index: Integer): IDataIndex;
    function FieldIndex(const FieldName: string; FindFirst: Boolean = True): IDataIndex;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IDataIndex read GetItem;
    property Names[const Name: String]: IDataIndex read GetByName; default;
  end;

  IDataIndexDefList = interface (IList)
    ['{B68181E9-573E-11D4-988A-00104B0FA1EF}']
    function CreateItem(const Name: String = ''; const Fields: String = ''; Options: TDataIndexOptions = []): IDataIndexDef;
    function CreateInternal(const Name: String = ''; const Fields: String = ''): IDataIndexInternal;
    function First: IDataIndexDef;
    function Last: IDataIndexDef;
    function GetItem(Index: Integer): IDataIndexDef;
    procedure SetItem(Index: Integer; const Value: IDataIndexDef);
    property Items[Index: Integer]: IDataIndexDef read GetItem write SetItem; default;
  end;

  IReadOnlyDataRowset = interface
    ['{68389B33-7E94-11D4-9897-00104B0FA1EF}']
    function GetName: String;
    function GetBof: Boolean;
    function GetEof: Boolean;
    function GetBookmark: String;
    procedure SetBookmark(const Value: String);
    function GetFields: IDataFieldList;
    function GetIndexes: IDataIndexList;
    function GetRecordCount: LongWord;
    function GetCurrentRecord: LongWord;
    procedure SetCurrentRecord(Value: LongWord);
    function GetRecordSize: LongWord;
    function GetStatus: TDataRowsetStatus;
    function GetRowsetDef: IDataRowsetDef;
    function GetActiveIndex: IDataIndex;
    procedure SetActiveIndex(const Value: IDataIndex);
    function GetActiveIndexName: String;
    function GetDisplays: IFieldDisplayList;
    function CreateDisplay(const FieldName: String; const Caption: String = ''; const Mask: String = ''): IFieldDisplay;
    procedure SetActiveIndexName(const Value: String);
    function GetStream: IRandomStream;
    procedure Open;
    procedure Close;
    function IsEmpty: Boolean;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    function MoveBy(Distance: LargeInt): Integer;
    property Name: String read GetName;
    property IsBof: Boolean read GetBof;
    property IsEof: Boolean read GetEof;
    property Bookmark: String read GetBookmark write SetBookmark;
    property Fields: IDataFieldList read GetFields;
    property Indexes: IDataIndexList read GetIndexes;
    property RecordCount: LongWord read GetRecordCount;
    property CurrentRecord: LongWord read GetCurrentRecord write SetCurrentRecord;
    property RecordSize: LongWord read GetRecordSize;
    property Status: TDataRowsetStatus read GetStatus;
    property RowsetDef: IDataRowsetDef read GetRowsetDef;
    property ActiveIndex: IDataIndex read GetActiveIndex write SetActiveIndex;
    property ActiveIndexName: String read GetActiveIndexName write SetActiveIndexName;
    property Displays: IFieldDisplayList read GetDisplays;
    property Stream: IRandomStream read GetStream;
  end;

  IDataRowsetFind = interface
    ['{80890FE9-5E34-4AF3-982C-9FBE6F994935}']
    procedure Add(const Value: String); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: Longword); overload;
    procedure Add(const Value: TGuid); overload;
    procedure Add(const Value: Double); overload;
    procedure Add(Value: Char); overload;
    procedure Add(Value: Byte); overload;
    {$IFDEF D60}
    procedure Add(const Value: Variant); overload;
    {$ENDIF}
    procedure AddValue(const Value: Variant);
    function Find(First: Boolean = true): Boolean;
  end;

  IDataRowset = interface (IReadOnlyDataRowset)
    ['{B68181E3-573E-11D4-988A-00104B0FA1EF}']
    function GetModified: Boolean;
    function GetCanModify: Boolean;
    procedure AppendRecord(const Values: array of const);
    procedure UpdateRecord(const Values: array of const);
    function AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
    function GetRecordBuffer: String;
    procedure AppendRecordFrom(const Source: IReadOnlyDataRowset);
    procedure Delete;
    procedure Append;
    procedure Edit;
    procedure Post;
    procedure Reindex;
    function Find(const Values: array of Variant; Nearest: Boolean = true): Boolean;
    function FindBuffer(Buffer: PChar; Size: LongWord): Boolean;
    function FindValues: IDataRowsetFind;
    property IsModified: Boolean read GetModified;
    property CanModify: Boolean read GetCanModify;
    property RecordBuffer: String read GetRecordBuffer;
  end;

  IDataRowsetDef = interface
    ['{B68181E6-573E-11D4-988A-00104B0FA1EF}']
    function GetName: String;
    function GetFieldDefs: IDataFieldDefList;
    function GetIndexDefs: IDataIndexDefList;
    function GetRecordCount: LongWord;
    function GetCurrentRecord: LongWord;
    procedure SetCurrentRecord(Value: LongWord);
    function GetRecordSize: LongWord;
    procedure SetRecordSize(Value: LongWord);
    function GetRowset: IDataRowset;
    function GetStore: IFieldStore;
    function GetStream: IRandomStream;
    procedure Write(const Buffer; Position, Size: LongWord);
    procedure Read(var Buffer; Position, Size: LongWord);
    function GetRecordBuffer: String;
    function ChangeCurrentRecord(Position: LongWord): Boolean;
    function ReadMemo(const Field: IFieldDef): String;
    procedure CopyFrom(const RowsetDef: IDataRowsetDef);
    procedure Build(const Name: String = '');
    procedure Initialize;
    procedure WriteRecord(RecordNumber: LongWord);
    procedure WriteIndexRoot(RecordNumber: LongWord; const Index: IDataIndexDef);
    property Name: String read GetName;
    property Fields: IDataFieldDefList read GetFieldDefs;
    property Indexes: IDataIndexDefList read GetIndexDefs;
    property RecordCount: LongWord read GetRecordCount;
    property RecordSize: LongWord read GetRecordSize write SetRecordSize;
    property Rowset: IDataRowset read GetRowset;
    property RecordBuffer: String read GetRecordBuffer;
    property CurrentRecord: LongWord read GetCurrentRecord write SetCurrentRecord;
    property Store: IFieldStore read GetStore;
    property Stream: IRandomStream read GetStream;
  end;

  IDataRowsetList = interface
    ['{B68181E7-573E-11D4-988A-00104B0FA1EF}']
    function GetCount: Integer;
    function GetByName(const Name: String): IDataRowset;
    function Locked: ILock;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    //function Get(Index: Integer): Pointer;
    function First: IDataRowset;
    function Last: IDataRowset;
    property Count: Integer read GetCount;
    function GetItem(Index: Integer): IDataRowset;
    property Items[Index: Integer]: IDataRowset read GetItem;
    property Names[const Name: String]: IDataRowset read GetByName; default;
  end;

  IDataRowsetDefList = interface (IList)
    ['{B68181E8-573E-11D4-988A-00104B0FA1EF}']
    function CreateItem(const Name: String = ''): IDataRowsetDef;
    function First: IDataRowsetDef;
    function Last: IDataRowsetDef;
    function GetItem(Index: Integer): IDataRowsetDef;
    procedure SetItem(Index: Integer; const Value: IDataRowsetDef);
    property Items[Index: Integer]: IDataRowsetDef read GetItem write SetItem; default;
  end;

implementation

end.
