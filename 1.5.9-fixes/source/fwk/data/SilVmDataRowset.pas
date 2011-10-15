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

unit SilVmDataRowset;

interface

uses
  SysUtils, Forms, Classes, Db,

  Sil,
  SilData,

  SilVmDataConnection,
  SilViConnection;

type
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  TSilDataset = class (
    // extends
    TDataSet,
    // implements
    IUnknown,
    IConnectionClient)
  private
    FCursor: IDataRowset;
    FDef: IDataRowsetDef;
    FDataSize: LongWord;
    FRecordSize: LongWord;
    FCalcOffset: LongWord;
    FDataOffset: LongWord;
    FConnection: TSilSqlConnection;
    FParams: TParams;
    FSort: String;
    function DoGetRecBuf(var Buf: PChar): Boolean;
    procedure SetCursor(const Value: IDataRowset);
    procedure SetConnection(const Value: TSilSqlConnection);
    procedure DoRelease;
    procedure SetParamsList(const Value: TParams);
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    function LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    procedure DoCalcBuffers;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  protected { abstract methods }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); overload; override;
  protected
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetCanModify: Boolean; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure SetActive(Value: Boolean); override;
  protected
    function DoOpen: Boolean; virtual; abstract;
    procedure SetSort(const Value: String); virtual;
  protected // IConnectionClient
    function GetActive: Boolean;
    procedure Cleanup;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWrapper(const Rowset: IDataRowset; AOwner: TComponent = nil);
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function ParamByName(const Value: string): TParam;
    property Cursor: IDataRowset read FCursor write SetCursor;
    property Sort: String read FSort write SetSort;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    procedure Requery; virtual;
  published
    property Active;
    property Connection: TSilSqlConnection read FConnection write SetConnection;
    property Parameters: TParams read FParams write SetParamsList;
    property FieldDefs;
    property OnCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
  end;

  TSilQuery = class(TSilDataset)
  private
    FCommand: TStrings;
    procedure SetCommand(const Value: TStrings);
  protected
    function DoOpen: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Command: TStrings read FCommand write SetCommand;
  end;

  TSilStoredProc = class(TSilDataset)
  private
    FProcName: String;
    FExec: Boolean;
    procedure SetProcName(const Value: String);
    function DoExec(WithRecords: Boolean): Boolean;
    procedure DoReadParams;
  protected
    function DoOpen: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecProc;
    procedure Requery; override;
    procedure Refresh;
  published
    property ProcName: String read FProcName write SetProcName;
  end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('SIL', [TSilQuery, TSilStoredProc]);
end;

function ConvertToDb(DataType: TDataFieldType): TFieldType;
begin
  case DataType of
    ftString:         Result := Db.ftString;
    ftSmallInt:       Result := Db.ftSmallInt;
    ftInteger:        Result := Db.ftInteger;
    ftWord:           Result := Db.ftWord;
    ftBoolean:        Result := Db.ftBoolean;
    ftFloat:          Result := Db.ftFloat;
    ftCurrency:       Result := Db.ftCurrency;
    ftDate:           Result := Db.ftDate;
    ftTime:           Result := Db.ftTime;
    ftDateTime:       Result := Db.ftDateTime;
    ftAutoInc:        Result := Db.ftAutoInc;
    ftLongWord:       Result := Db.ftInteger;
    ftGuid:           Result := Db.ftGuid;
    ftMemo:           Result := Db.ftString;
    else              Result := Db.ftUnknown;
  end;
end;

function ConvertFromDb(DataType: TFieldType): TDataFieldType;
begin
  case DataType of
    Db.ftString:      Result := ftString;
    Db.ftSmallInt:    Result := ftSmallInt;
    Db.ftInteger:     Result := ftInteger;
    Db.ftWord:        Result := ftWord;
    Db.ftBoolean:     Result := ftBoolean;
    Db.ftFloat:       Result := ftFloat;
    Db.ftCurrency:    Result := ftCurrency;
    Db.ftDate:        Result := ftDate;
    Db.ftTime:        Result := ftTime;
    Db.ftDateTime:    Result := ftDateTime;
    Db.ftAutoInc:     Result := ftAutoInc;
    Db.ftGuid:        Result := ftGuid;
    Db.ftMemo:        Result := ftMemo;
    else              Result := ftUnknown;
  end;
end;

{ TSilDataset }

constructor TSilDataset.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TParams.Create(Self);
end;

constructor TSilDataset.CreateWrapper(const Rowset: IDataRowset; AOwner: TComponent);
begin
  Create(AOwner);
  Self.Cursor := Rowset;
end;

destructor TSilDataset.Destroy;
begin
  SetConnection(nil);
  FParams.Free;
  inherited;
  DoRelease;
end;

procedure TSilDataset.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TSilDataset(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

procedure TSilDataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Parameters);
end;

function TSilDataset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TSilDataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Parameters);
end;

function TSilDataset.AllocRecordBuffer: PChar;
begin
  //GetMem(Result, FRecordSize);
  Result := AllocMem(FRecordSize);
end;

procedure TSilDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer, FRecordSize);
end;

procedure TSilDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer + FDataSize)^.Bookmark;
end;

function TSilDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FDataSize)^.BookmarkFlag;
end;

function TSilDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;

  case GetMode of
    gmNext:
    begin
      if FCursor.IsEof then
        Result := grEOF
      else
      begin
        if FCursor.IsBof then
          FCursor.First else
          FCursor.Next;

        if FCursor.IsEof then
          Result := grEOF;
      end;
    end;

    gmPrior:
    begin
      if FCursor.IsBof then
        Result := grBOF
      else
      begin
        if FCursor.IsEof then
          FCursor.Last else
          FCursor.Prior;

        if FCursor.IsBof then
          Result := grBOF;
      end;
    end;

    gmCurrent:
      if FCursor.IsEof or FCursor.IsBof then
        Result := grError;
  end;

  if Result = grOK then
  begin
    FDef.Read(Buffer^, FDataOffset, FDataSize);

    with PRecInfo(Buffer + FDataSize)^ do
    begin
      BookmarkFlag := bfCurrent;
      Bookmark := FCursor.CurrentRecord;
    end;

    if not (csDesigning in ComponentState) then
      GetCalcFields(Buffer);
  end else
  if (Result = grError) and DoCheck then
    DatabaseError('No Records');
end;

function TSilDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TSilDataset.GetRecordCount: Integer;
begin
  Result := FCursor.RecordCount;
end;

procedure TSilDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
end;

procedure TSilDataset.InternalDelete;
begin
end;

procedure TSilDataset.InternalFirst;
begin
  FCursor.First;
  FCursor.Prior;
end;

procedure TSilDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCursor.CurrentRecord := PInteger(Bookmark)^;
end;

procedure TSilDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TSilDataset.InternalInitFieldDefs;
var
  e: IEnumerator;
  Item: IFieldAccess;
  lwSize: LongWord;
begin
  if FCursor = nil then Exit;
  FieldDefs.Clear;

  while FCursor.Fields.Enumerate(e, Item) do
  begin
    if Item.DataType = ftString then
      lwSize := Item.Size + 1 else
      lwSize := 0;

    with FieldDefs.AddFieldDef do
    begin
      FieldNo := e.Iteration;
      Name := Item.Name;
      DataType := ConvertToDb(Item.DataType);
      Size := lwSize;
    end;
  end;
end;

procedure TSilDataset.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FRecordSize, 0);
end;

procedure TSilDataset.InternalLast;
begin
  FCursor.Last;
  FCursor.Next;
end;

procedure TSilDataset.DoRelease;
begin
  FCursor := nil;
  FDef := nil;
end;

procedure TSilDataset.InternalOpen;
begin
  if (FCursor = nil) and not DoOpen then Exit;

  FieldDefs.Updated := False;
  FieldDefs.Update;

  if DefaultFields then CreateFields;
  BindFields(true);

  DoCalcBuffers;
  InternalFirst;
end;

procedure TSilDataset.DoCalcBuffers;
begin
  FDataOffset := FDef.Fields.PositionOffset;
  FDataSize := FCursor.RecordSize - FDataOffset;
  BookmarkSize := SizeOf(Integer);
  FCalcOffset := FDataSize + SizeOf(TRecInfo);
  FRecordSize := FCalcOffset + LongWord(CalcFieldsSize);
end;

procedure TSilDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then DestroyFields;
  DoRelease;
end;

procedure TSilDataset.InternalPost;
begin
end;

procedure TSilDataset.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PRecInfo(Buffer + FDataSize)^.Bookmark);
end;

function TSilDataset.IsCursorOpen: Boolean;
begin
  Result := FCursor <> nil;
end;

procedure TSilDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;

procedure TSilDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FDataSize)^.BookmarkFlag := Value;
end;

procedure TSilDataset.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
var
  pBuf, pAct: PChar;
begin
  if DoGetRecBuf(pAct) and (Field.FieldKind = fkCalculated) then
  begin
    pBuf := pAct + FCalcOffset + Field.Offset;
    Boolean(pBuf[0]) := LongBool(Buffer);
    if Boolean(pBuf[0]) then Move(Buffer^, pBuf[1], Field.DataSize);
  end;
end;

procedure TSilDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

function TSilDataset.DoGetRecBuf(var Buf: PChar): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Buf := nil else
        Buf := ActiveBuffer;
    dsCalcFields:
      Buf := CalcBuffer;
  else
    Buf := nil;
  end;

  Result := Buf <> nil;
end;

function TSilDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Fld: IFieldDef;
  dt: Double;
  pBuf, pAct: PChar;
  lwSize: LongWord;
  Stamp: TTimeStamp;
  Memo: string;
begin
  Result := DoGetRecBuf(pAct);
  if not Result then Exit;

  if Field.FieldKind = fkCalculated then
  begin
    if csDesigning in ComponentState then Exit;

    pBuf := pAct + FCalcOffset + Field.Offset;
    lwSize := Field.DataSize;
    Result := Boolean(pBuf[0]);
    Inc(pBuf);
  end else
  begin
    Fld := FDef.Fields.Items[Field.FieldNo - 1];
    pBuf := pAct + Fld.Position - FDataOffset;
    lwSize := Fld.Size;
  end;

  if (Buffer = nil) and (lwSize > 0) then
  begin
    Result := true;
    exit;
  end;

  if Result then
    case Field.DataType of
      Db.ftDate,
      Db.ftTime:
      begin
        Stamp.Date := 0;
        Stamp.Time := PInteger(pBuf)^;
        dt := TimeStampToMSecs(Stamp);
        Move(dt, Buffer^, lwSize);
      end;

      Db.ftDateTime:
      begin
        dt := PDouble(pBuf)^;
        dt := TimeStampToMSecs(DateTimeToTimeStamp(dt));
        Move(dt, Buffer^, lwSize);
      end;

      Db.ftString:
      begin
        if Fld.DataType = ftMemo then
        begin
          Memo := Fld.AsString;
          lwSize := Length(Memo);
          Move(Memo[1], Buffer^, lwSize);
        end else
          Move(pBuf^, Buffer^, lwSize);

        PChar(Buffer)[lwSize] := #0;
      end;

      else
        Move(pBuf^, Buffer^, lwSize);
    end;
end;

function TSilDataset.GetRecNo: Integer;
begin
  Result := FCursor.CurrentRecord;
end;

procedure TSilDataset.SetRecNo(Value: Integer);
begin
  FCursor.CurrentRecord := Value;
end;

function TSilDataset.GetCanModify: Boolean;
begin
  Result := false;
end;

procedure TSilDataset.SetCursor(const Value: IDataRowset);
begin
  Close;
  if Assigned(FDef) then FDef := nil;
  FCursor := Value;
  if Assigned(FCursor) then
  begin
    FDef := FCursor.RowsetDef;
    Open;
  end;
end;

procedure TSilDataset.SetConnection(const Value: TSilSqlConnection);
var
  OldValue: TSilSqlConnection;
begin
  if FConnection <> Value then
  begin
    OldValue := FConnection;
    FConnection := Value;

    if OldValue <> nil then OldValue.RemoveClient(Self);
    if FConnection <> nil then FConnection.AddClient(Self);
  end;
end;

procedure TSilDataset.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FCalcOffset], CalcFieldsSize, 0);
end;

procedure TSilDataset.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
    begin
      if (FConnection <> nil) and not FConnection.Active then FConnection.Active := true;
    end;
    inherited;
  end;
end;

procedure TSilDataset.SetParamsList(const Value: TParams);
begin
  FParams := Value;
end;

function TSilDataset.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  i: Integer;
  Fields: TList;
  bArray: Boolean;
begin
  Fields := TList.Create;
  
  try
    GetFieldList(Fields, KeyFields);

    Result := false;
    First;
    bArray := Vart.IsArray(KeyValues);

    while not Eof do
    begin
      Result := true;

      if bArray then
      begin
        for i := 0 to Fields.Count - 1 do
          if TField(Fields[i]).AsVariant <> KeyValues[i] then
          begin
            Result := false;
            Break;
          end;
      end else
      begin
        if TField(Fields[0]).AsVariant <> KeyValues then
          Result := false;
      end;

      if Result then Break;
      Next;
    end;

    if not Result then CursorPosChanged;
  finally
    Fields.Free;
  end;
end;

function TSilDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TSilDataset.Requery;
begin
  Close;
  Open;
end;

procedure TSilDataset.SetSort(const Value: String);
var
  Index: IDataIndex;
  Rowset: IDataRowset;
  Def: IDataRowsetDef;
begin
  if not IsCursorOpen then Exit;

  Index := FCursor.Indexes[Value];

  if Index = nil then
  begin
    Def := SilData.Tk.Memory;
    Def.CopyFrom(FCursor.RowsetDef);

    Index := Def.Indexes.CreateItem(Value, Value);
    Def.Build;
    Rowset := Def.Rowset;
    Rowset.AppendRecords(FCursor);

    FCursor := Rowset;
    FDef := FCursor.RowsetDef;
    DoCalcBuffers;
  end;

  if Index <> nil then
  begin
    FCursor.ActiveIndex := Index;
    First;
  end;
end;

procedure TSilDataset.Cleanup;
begin
  FConnection := nil;
end;

function TSilDataset.GetActive: Boolean;
begin
  Result := Active;
end;

{ TSilQuery }

constructor TSilQuery.Create(AOwner: TComponent);
begin
  inherited;
  FCommand := TStringList.Create;
end;

destructor TSilQuery.Destroy;
begin
  FCommand.Free;
  inherited;
end;

function TSilQuery.DoOpen: Boolean;
var
  sCmd, sIdent, sValue: String;
  i: Integer;
  Param: TParam;

  function DoReadIdent(Buf: PChar): String;
  begin
    Result := '';
    while Buf^ in ['_', '0'..'9', 'A'..'Z', 'a'..'z'] do
    begin
      Result := Result + Buf^;
      Inc(Buf);
    end;
  end;
  
begin
  Result := (FConnection <> nil) and FConnection.Active;

  if Result then
  begin
    sCmd := FCommand.Text;

    if FParams.Count > 0 then
    begin
      repeat
        i := Str.Pos(':', sCmd);
        if (i > 0) and (Length(sCmd) > i) then
        begin
          sIdent := DoReadIdent(PChar(sCmd) + i);
          Param := ParamByName(sIdent);

          if Param <> nil then
          begin
            sValue := Param.AsString;
            sCmd := Str.Copy(sCmd, 1, i - 1) + sValue + Str.Copy(sCmd, i + Length(sIdent) + 1);
          end;
        end;
      until i = 0;
    end;

    FCursor := FConnection.ProtSql.Query(sCmd);
    FDef := FCursor.RowsetDef;
  end;
end;

procedure TSilQuery.SetCommand(const Value: TStrings);
begin
  if FCommand.Text <> Value.Text then
  begin
    SetActive(false);

    FCommand.BeginUpdate;
    try
      FCommand.Assign(Value);
    finally
      FCommand.EndUpdate;
    end;
  end;
end;

{ TSilStoredProc }

constructor TSilStoredProc.Create(AOwner: TComponent);
begin
  inherited;
  FProcName := '';
  FExec := false;
end;

procedure TSilStoredProc.SetProcName(const Value: String);
begin
  if FProcName <> Value then
  begin
    FProcName := Value;
    //if csDesigning in ComponentState then DoReadParams;
    DoReadParams;
  end;
end;

procedure TSilStoredProc.ExecProc;
begin
  FExec := true;
  DoExec(false);
end;

function TSilStoredProc.DoOpen: Boolean;
begin
  Result := DoExec(true);
end;

procedure TSilStoredProc.DoReadParams;
var
  e: IEnumerator;
  Params: IValueList;
  Item: IFieldAccess;
  Param: TParam;
  sName: String;
  PType: TParamType;
  cType: Char;
begin
  FParams.Clear;
  
  if (FConnection <> nil) and FConnection.Active and Str.NotEmpty(FProcName) then
  begin
    FConnection.ProtSql.QueryStoredProcParams(FProcName, Params);

    if Params <> nil then
      while Params.Enumerate(e, Item) do
      begin
        sName := Item.Name;
        cType := Str.LastChar(Item.Name);
        if Ord(cType) < 10 then
        begin
          PType := TParamType(cType);
          Str.Delete(sName, -1);
        end else
          PType := ptInputOutput;
        Param := FParams.CreateParam(ConvertToDb(Item.DataType), sName, PType);
        Param.Value := Item.AsVariant;
      end;
  end;
end;

function TSilStoredProc.DoExec(WithRecords: Boolean): Boolean;
var
  Param: TParam;
  Params: IValueList;
  Item: IFieldAccess;
  i: Integer;
begin
  Result := (FConnection <> nil) and FConnection.Active;

  if Result then
  begin
    Params := Sil.List.ValueList(true);

    for i := 0 to FParams.Count - 1 do
      if Vart.IsOk(FParams[i].Value) then
      begin
        Item := Params.New(FParams[i].Name, ConvertFromDb(FParams[i].DataType), 0);
        Item.AsVariant := FParams[i].Value;
      end;

    FCursor := FConnection.ProtSql.StoredProc(FProcName, Params, WithRecords);
    FDef := FCursor.RowsetDef;

    for i := 0 to Params.Count - 1 do
    begin
      Param := ParamByName(Params[i].Name);
      if Param <> nil then Param.Value := Item.AsVariant;
    end;
  end;
end;

procedure TSilStoredProc.Requery;
begin
  if FExec then
    ExecProc else
    inherited;
end;

procedure TSilStoredProc.Refresh;
begin
  DoReadParams;
end;

end.
