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

unit SilVmAdoRowset;

interface

{$include Defines.inc}

uses
  Db, ADOInt, ADODB, SysUtils,

  Sil,
  SilData;

type
  TAdoRowset = class (
    // extends
    TSilInterfacedObject,
    // implements
    IDataRowset,
    IDataRowsetDef,
    IFieldStore)
  private
    FDb: TADOConnection;
    FQryCur: TCustomADODataSet;
    FFields: IDataFieldList;
    FFieldDefs: IDataFieldDefList;
    FWordAsInt: Boolean;
    function DoExecuteQuery(const QueryStr: String; WithRecords: Boolean): Boolean;
    function DoStoredProc(const ProcName: String; var Params: IValueList; WithRecords: Boolean): Boolean;
    procedure DoFreeResources;
    function DoNewField(Idx: Integer; const Name: String; DataType: TDataFieldType; Size: LongWord): IFieldDef;
    function DoQueryFields(const Command: String; out Fields: IValueList): Boolean;
    function DoQueryParams(const ProcName: String; out Fields: IValueList): Boolean;
  protected // IDataRowset
    function GetName: String;
    function GetBof: Boolean;
    function GetEof: Boolean;
    function GetBookmark: String;
    function GetStream: IRandomStream;
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
    procedure SetActiveIndexName(const Value: String);
    procedure AppendRecord(const Values: array of const);
    procedure UpdateRecord(const Values: array of const);
    function AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
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
    function GetDisplays: IFieldDisplayList;
    function CreateDisplay(const FieldName: String; const Caption: String = ''; const Mask: String = ''): IFieldDisplay;
  protected // IDataRowsetDef
    procedure Write(const Buffer; Position, Size: LongWord);
    procedure Read(var Buffer; Position, Size: LongWord);
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
    constructor Create(const Database: TADOConnection);
    constructor CreateQuery(const Database: TADOConnection; const QueryStr: String; WithRecords: Boolean; WordAsInt: Boolean = false);
    constructor CreateStoredProc(const Database: TADOConnection; const ProcName: String; var Params: IValueList; WithRecords: Boolean);
    constructor CreateQueryFields(const Database: TADOConnection; const Command: String; out Fields: IValueList);
    constructor CreateStoredProcParams(const Database: TADOConnection; const ProcName: String; out Params: IValueList);
    destructor Destroy; override;
  end;

implementation

uses
  SilClasses,
  SilDataClasses;

function DoCheckDatabase(const Database: TADOConnection; const QueryStr: String): Boolean;
begin
  Result := Database.Connected;

  if not Result then
  begin
    Database.Connected := true;
    Result := Database.Connected;
  end;
end;

function ADOTypeToFieldType(const ADOType: DataTypeEnum; EnableBCD: Boolean = True): TFieldType;
begin
  case ADOType of
    adEmpty: Result := Db.ftUnknown;
    adTinyInt, adSmallInt: Result := Db.ftSmallint;
    adError, adInteger, adUnsignedInt: Result := Db.ftInteger;
    adBigInt, adUnsignedBigInt: Result := Db.ftLargeInt;
    adUnsignedTinyInt, adUnsignedSmallInt: Result := Db.ftWord;
    adSingle, adDouble: Result := Db.ftFloat;
    adCurrency: Result := Db.ftBCD;
    adBoolean: Result := Db.ftBoolean;
    adDBDate: Result := Db.ftDate;
    adDBTime: Result := Db.ftTime;
    adDate, adDBTimeStamp, adFileTime, adDBFileTime: Result := Db.ftDateTime;
    adChar: Result := Db.ftFixedChar;
    adVarChar: Result := Db.ftString;
    adBSTR, adWChar, adVarWChar: Result := Db.ftWideString;
    adLongVarChar, adLongVarWChar: Result := Db.ftMemo;
    adLongVarBinary: Result := Db.ftBlob;
    adBinary: Result := Db.ftBytes;
    adVarBinary: Result := Db.ftVarBytes;
    adChapter: Result := Db.ftDataSet;
    adPropVariant, adVariant: Result := Db.ftVariant;
    adIUnknown: Result := Db.ftInterface;
    adIDispatch: Result := Db.ftIDispatch;
    adGUID: Result := Db.ftGUID;
    adDecimal, adNumeric, adVarNumeric:
      if EnableBCD then Result := Db.ftBCD
      else Result := Db.ftFloat;
  else
    Result := Db.ftUnknown;
  end;
end;

function DbTypeToFieldType(DataType: TFieldType; WordAsInt: Boolean = false): TDataFieldType;
begin
  case DataType of
    Db.ftFixedChar,
    Db.ftWideString,
    Db.ftString:      Result := ftString;
    Db.ftSmallInt:    Result := ftSmallInt;
    Db.ftInteger:     Result := ftInteger;

    Db.ftWord:
      if WordAsInt then
        Result := ftInteger else
        Result := ftWord;

    Db.ftBoolean:     Result := ftBoolean;
    Db.ftFloat:       Result := ftFloat;
    Db.ftCurrency:    Result := ftCurrency;
    Db.ftDate:        Result := ftDate;
    Db.ftTime:        Result := ftTime;
    Db.ftDateTime:    Result := ftDateTime;
    Db.ftAutoInc:     Result := ftAutoInc;
    else              Result := ftUnknown;
  end;
end;

{ TAdoRowset }

constructor TAdoRowset.Create(const Database: TADOConnection);
begin
  inherited Create;

	FDb := Database;
  FFieldDefs := TDataFieldList.Create(Self);
  Reference.GetInterface(FFieldDefs, IDataFieldList, FFields);
end;

constructor TAdoRowset.CreateQuery(const Database: TADOConnection; const QueryStr: String; WithRecords: Boolean; WordAsInt: Boolean);
begin
  Create(Database);
  FWordAsInt := WordAsInt;
  DoExecuteQuery(QueryStr, WithRecords);
end;

constructor TAdoRowset.CreateStoredProc(const Database: TADOConnection; const ProcName: String; var Params: IValueList; WithRecords: Boolean);
begin
  Create(Database);
  DoStoredProc(ProcName, Params, WithRecords);
end;

constructor TAdoRowset.CreateQueryFields(const Database: TADOConnection; const Command: String; out Fields: IValueList);
begin
  Create(Database);
  DoQueryFields(Command, Fields);
end;

constructor TAdoRowset.CreateStoredProcParams(const Database: TADOConnection; const ProcName: String; out Params: IValueList);
begin
  Create(Database);
  DoQueryParams(ProcName, Params);
end;

destructor TAdoRowset.Destroy;
begin
  FFieldDefs.Unbind;
  FFieldDefs := nil;
  DoFreeResources;
  inherited;
end;

function TAdoRowset.DoNewField(Idx: Integer; const Name: String; DataType: TDataFieldType; Size: LongWord): IFieldDef;
begin
  case DataType of
    ftChar,
    ftByte:       Result := TCharFieldAccess.Create(Name, Self);
    ftString:     Result := TStringFieldAccess.CreateSized(Name, Size, Self);
    ftMemo:       Result := TDataMemoField.Create(Name, Self);
    ftWideString: Result := TWideStringFieldAccess.CreateSized(Name, Size, Self);
    ftSmallInt:   Result := TSmallIntFieldAccess.Create(Name, Self);
    ftAutoInc,
    ftInteger:    Result := TIntegerFieldAccess.Create(Name, Self);
    ftLargeInt:   Result := TLargeIntFieldAccess.Create(Name, Self);
    ftWord:       Result := TWordFieldAccess.Create(Name, Self);
    ftLongWord:   Result := TLongWordFieldAccess.Create(Name, Self);
    ftBoolean:    Result := TBooleanFieldAccess.Create(Name, Self);
    ftCurrency,
    ftFloat:      Result := TFloatFieldAccess.Create(Name, Self);
    ftDate,
    ftTime,
    ftDateTime:   Result := TDateTimeFieldAccess.Create(Name, Self, DataType);
    ftGuid:       Result := TGuidFieldAccess.Create(Name, Self);
    else                Result := nil;
  end;

  if Result <> nil then
  begin
    Result.Position := Idx;
    FFieldDefs.Add(Result);
  end;
end;

procedure TAdoRowset.DoFreeResources;
begin
  Sil.Ref.Free(FQryCur);
end;

procedure TAdoRowset.Append;
begin
end;

procedure TAdoRowset.AppendRecord(const Values: array of const);
begin
end;

function TAdoRowset.AppendRecords(const Source: IReadOnlyDataRowset): LongWord;
begin
  Result := 0;
end;

procedure TAdoRowset.Build(const Name: String);
begin
end;

function TAdoRowset.ChangeCurrentRecord(Position: LongWord): Boolean;
begin
  Result := false;
end;

procedure TAdoRowset.Close;
begin
end;

procedure TAdoRowset.CopyFrom(const RowsetDef: IDataRowsetDef);
begin
end;

procedure TAdoRowset.Delete;
begin
end;

procedure TAdoRowset.Edit;
begin
end;

function TAdoRowset.Find(const Values: array of Variant; Nearest: Boolean): Boolean;
begin
  Result := false;
end;

procedure TAdoRowset.First;
begin
	if FQryCur <> nil then FQryCur.First;
end;

function TAdoRowset.GetActiveIndex: IDataIndex;
begin
  Result := nil;
end;

function TAdoRowset.GetActiveIndexName: String;
begin
  Result := '';
end;

function TAdoRowset.GetBof: Boolean;
begin
  Result := (FQryCur = nil) or FQryCur.BOF;
end;

function TAdoRowset.GetBookmark: String;
begin
  Result := '';
end;

function TAdoRowset.GetCanModify: Boolean;
begin
  Result := false;
end;

function TAdoRowset.GetCurrentRecord: LongWord;
begin
  Result := 0;
end;

function TAdoRowset.GetEof: Boolean;
begin
  Result := (FQryCur = nil) or FQryCur.EOF;
end;

function TAdoRowset.GetFieldDefs: IDataFieldDefList;
begin
  Reference.GetInterface(FFields, IDataFieldDefList, Result);
end;

function TAdoRowset.GetFields: IDataFieldList;
begin
  Result := FFields;
end;

function TAdoRowset.GetIndexDefs: IDataIndexDefList;
begin
  Result := nil;
end;

function TAdoRowset.GetIndexes: IDataIndexList;
begin
  Result := nil;
end;

function TAdoRowset.GetModified: Boolean;
begin
  Result := false;
end;

function TAdoRowset.GetName: String;
begin
  Result := '';
end;

function TAdoRowset.GetRecordBuffer: String;
begin
  Result := '';
end;

function TAdoRowset.GetRecordCount: LongWord;
begin
  if (FQryCur = nil) and FQryCur.Active then
    Result := FQryCur.RecordCount else
    Result := 0;
end;

function TAdoRowset.GetRecordSize: LongWord;
begin
  Result := 0;
end;

function TAdoRowset.GetRowset: IDataRowset;
begin
  Result := Self;
end;

function TAdoRowset.GetRowsetDef: IDataRowsetDef;
begin
  Result := Self;
end;

function TAdoRowset.GetStatus: TDataRowsetStatus;
begin
  Result := rsUnknown;
end;

function TAdoRowset.GetStore: IFieldStore;
begin
  Result := Self;
end;

procedure TAdoRowset.Initialize;
begin
end;

function TAdoRowset.IsEmpty: Boolean;
begin
  Result := GetRecordCount = 0;
end;

procedure TAdoRowset.Last;
begin
	if FQryCur <> nil then FQryCur.Last;
end;

function TAdoRowset.MoveBy(Distance: LargeInt): Integer;
begin
  if FQryCur = nil then
  	Result := 0 else
    Result := FQryCur.MoveBy(Distance);
end;

procedure TAdoRowset.Next;
begin
  if FQryCur <> nil then FQryCur.Next;
end;

procedure TAdoRowset.Open;
begin
end;

procedure TAdoRowset.Post;
begin
end;

procedure TAdoRowset.Prior;
begin
  if FQryCur <> nil then FQryCur.Prior;
end;

function TAdoRowset.ReadMemo(const Field: IFieldDef): String;
begin
end;

procedure TAdoRowset.Reindex;
begin
end;

procedure TAdoRowset.SetActiveIndex(const Value: IDataIndex);
begin
end;

procedure TAdoRowset.SetActiveIndexName(const Value: String);
begin
end;

procedure TAdoRowset.SetBookmark(const Value: String);
begin
end;

procedure TAdoRowset.SetCurrentRecord(Value: LongWord);
begin
end;

procedure TAdoRowset.SetRecordSize(Value: LongWord);
begin
end;

procedure TAdoRowset.UpdateRecord(const Values: array of const);
begin
end;

procedure TAdoRowset.WriteIndexRoot(RecordNumber: LongWord; const Index: IDataIndexDef);
begin
end;

procedure TAdoRowset.WriteRecord(RecordNumber: LongWord);
begin
end;

procedure TAdoRowset.Read(var Buffer; Position, Size: LongWord);
type
  RDataTypes = packed record
    VString: String;
    case Byte of
      0: (VChar: Char);
      1: (VSmallInt: SmallInt);
      2: (VInteger: Integer);
      3: (VWord: Word);
      4: (VBoolean: Boolean);
      5: (VFloat: Double);
      6: (VCurrency: Currency);
      7: (VDateTime: TDateTime);
  end;
var
  Field: TField;
  dt: RDataTypes;
  lwLen: LongWord;
  pBuf: PChar;
begin
  Field := FQryCur.Fields[Position];

  case Field.DataType of
    Db.ftString:
    begin
      dt.VString := Field.AsString;
      lwLen := Length(dt.VString);

      if lwLen > 0 then
      begin
        Mem.Move(dt.VString[1], Buffer, Int.Min(lwLen, Size));
        if lwLen < Size then
        begin      
          pBuf := PChar(@Buffer) + lwLen;
          Mem.Clear(pBuf^, Size - lwLen);
        end;
      end else
        Mem.Clear(Buffer, Size);

      Exit;
    end;
    Db.ftSmallInt:    dt.VSmallInt := Field.AsInteger;
    Db.ftAutoInc,
    Db.ftInteger:     dt.VInteger := Field.AsInteger;
    
    Db.ftWord:
      if FWordAsInt then
        dt.VInteger := Field.AsInteger else
        dt.VWord := Field.AsInteger;

    Db.ftBoolean:     dt.VBoolean := Field.AsBoolean;
    Db.ftFloat:       dt.VFloat := Field.AsFloat;
    Db.ftCurrency:    dt.VCurrency := Field.AsCurrency;
    Db.ftDate,
    Db.ftTime,
    Db.ftDateTime:    dt.VDateTime := Field.AsDateTime;
    else
    begin
      Mem.Clear(Buffer, Size, 0);
      Exit;
    end;
  end;

  Mem.Move(dt.VChar, Buffer, Size);
end;

procedure TAdoRowset.Write(const Buffer; Position, Size: LongWord);
begin
end;

function TAdoRowset.DoQueryFields(const Command: String; out Fields: IValueList): Boolean;
{
  COLUMN_DEFAULT
  IS_NULLABLE
}
const
  COLUMN_NAME = 'COLUMN_NAME';
  DATA_TYPE = 'DATA_TYPE';
  CHARACTER_MAXIMUM_LENGTH = 'CHARACTER_MAXIMUM_LENGTH';
var
  Sp: TADODataSet;
  sName: String;
  lwType, lwSize: LongWord;
begin
	Result := false;
	DoFreeResources;
  if (FDb = nil) or not DoCheckDatabase(FDb, Command) then Exit;

  Sp := TADODataSet.Create(nil);
  FQryCur := Sp;
  FDb.OpenSchema(siColumns, VArray.Build([Vart.Null, Vart.Null, Command]), Vart.EmptyParam, Sp);
  Fields := Sil.List.ValueList(true);

  while not Sp.EOF do
  begin
    sName := Sp.FieldByName(COLUMN_NAME).Value;
    lwType := Sp.FieldByName(DATA_TYPE).Value;
    lwSize := Vart.ToInt(Sp.FieldByName(CHARACTER_MAXIMUM_LENGTH).Value, 0);

    with Fields.New(sName, DbTypeToFieldType(ADOTypeToFieldType(lwType)), lwSize) do
      AsVariant := Vart.Unassigned;

    Sp.Next;
  end;

  Result := true;
end;

function TAdoRowset.DoQueryParams(const ProcName: String; out Fields: IValueList): Boolean;
{
  PARAMETER_TYPE
  PARAMETER_DEFAULT
  IS_NULLABLE
  TYPE_NAME
}
const
  PARAMETER_NAME = 'PARAMETER_NAME';
  DATA_TYPE = 'DATA_TYPE';
  CHARACTER_MAXIMUM_LENGTH = 'CHARACTER_MAXIMUM_LENGTH';
  PARAMETER_TYPE = 'PARAMETER_TYPE';
var
  Sp: TADODataSet;
  sName: String;
  lwType, lwSize, lwKind: LongWord;
begin
	Result := false;
	DoFreeResources;
  if (FDb = nil) or not DoCheckDatabase(FDb, ProcName) then Exit;

  Sp := TADODataSet.Create(nil);
  FQryCur := Sp;

  FDb.OpenSchema(siProcedureParameters, VArray.Build([Vart.Null, Vart.Null, ProcName]), Vart.EmptyParam, Sp);
  Fields := Sil.List.ValueList(true);

  while not Sp.EOF do
  begin
    sName := Sp.FieldByName(PARAMETER_NAME).Value;
    lwType := Sp.FieldByName(DATA_TYPE).Value;
    lwKind := Sp.FieldByName(PARAMETER_TYPE).Value;
    lwSize := Vart.ToInt(Sp.FieldByName(CHARACTER_MAXIMUM_LENGTH).Value, 0);
    if Integer(lwSize) = MaxInt then lwSize := 0;

    with Fields.New(sName + Char(lwKind), DbTypeToFieldType(ADOTypeToFieldType(lwType)), lwSize) do
      AsVariant := Vart.Unassigned;

    Sp.Next;
  end;

  Result := true;
end;

function TAdoRowset.DoStoredProc(const ProcName: String; var Params: IValueList; WithRecords: Boolean): Boolean;
var
  Sp: TADOStoredProc;
  Outp: IValueList;
  i: Integer;
  Fld: IFieldAccess;
  Par: TParameter;
  e: IEnumerator;
begin
	Result := false;
	DoFreeResources;
  if (FDb = nil) or not DoCheckDatabase(FDb, ProcName) then Exit;

  Sp := TADOStoredProc.Create(nil);
  FQryCur := Sp;

  Sp.Connection := FDb;
  Sp.ProcedureName := ProcName;
  Outp := Sil.List.ValueList(true);
  Sp.Parameters.Refresh;

  for i := 0 to Sp.Parameters.Count - 1 do
  begin
    Par := Sp.Parameters.Items[i];

    case Par.Direction of
      pdInput,
      pdInputOutput:
      begin
        Fld := Params[Par.Name];

        if Fld <> nil then
          Par.Value := Fld.AsVariant else
          Par.Value := Vart.Unassigned;

        if Par.Direction = pdInputOutput then
          Outp.New(Par.Name, DbTypeToFieldType(Par.DataType), Par.Size);
      end;

      pdOutput,
      pdReturnValue:
        Outp.New(Par.Name, DbTypeToFieldType(Par.DataType), Par.Size);
    end;
  end;

  if WithRecords then
  begin
    Sp.Open;

    for i := 0 to Sp.FieldDefs.Count - 1 do
      with Sp.FieldDefs.Items[i] do
        DoNewField(i, Name, DbTypeToFieldType(DataType), Size);

    First;
  end else
    Sp.ExecProc;

  Params := Outp;

  while Params.Enumerate(e, Fld) do
    Fld.AsVariant := Sp.Parameters.FindParam(Fld.Name).Value;

  Result := true;
end;

function TAdoRowset.DoExecuteQuery(const QueryStr: String; WithRecords: Boolean): Boolean;
var
  i: Integer;
  Qry: TADOQuery;
begin
	Result := false;
	DoFreeResources;
  if (FDb = nil) or not DoCheckDatabase(FDb, QueryStr) then Exit;

  Qry := TADOQuery.Create(nil);
  FQryCur := Qry;

  Qry.Connection := FDb;
  Qry.SQL.Text := QueryStr;

  if WithRecords then
  begin
    Qry.Open;
    Result := true;

    for i := 0 to Qry.FieldDefs.Count - 1 do
      with Qry.FieldDefs.Items[i] do
        DoNewField(i, Name, DbTypeToFieldType(DataType, FWordAsInt), Size);

    First;
  end else
    Qry.ExecSQL;
end;

procedure TAdoRowset.AppendRecordFrom(const Source: IReadOnlyDataRowset);
begin
end;

function TAdoRowset.GetDisplays: IFieldDisplayList;
begin
  Result := nil;
end;

function TAdoRowset.CreateDisplay(const FieldName, Caption, Mask: String): IFieldDisplay;
begin
  Result := nil;
end;

function TAdoRowset.GetStream: IRandomStream;
begin
  Result := nil;
end;

function TAdoRowset.FindBuffer(Buffer: PChar; Size: LongWord): Boolean;
begin
  raise Error.Create('TAdoRowset.FindBuffer no implementado');
end;

function TAdoRowset.FindValues: IDataRowsetFind;
begin
  raise Error.Create('TAdoRowset.FindBuffer no implementado');
end;

end.

