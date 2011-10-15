(****
autor......:  mariano d. podesta - antiriad@gmail.com      
empresa....:
fecha......:
descripcion:  creacion y ejecucion de comandos SQL
notas......:
****)
unit Qb;

interface

{$include Defines.inc}

uses
  SysUtils, Classes, Bde, Db, DBTables, DBCommon,

  Sil,
  Error;

type
  TQueryResult = class;
  IQueryResult = interface;
  TQueryStatement = class;
  IQueryStatement = interface;

  TQBKind = (qbInvalid, qbInsert, qbUpdate, qbDelete, qbQuery, qbWhere);

  TQueryField = class
  private
    FName: String;
    FValue: String;
    FOperator: String;
    FKind: TQBKind;
    constructor Create(const FieldName: String; const Kind: TQBKind);
    procedure SetAsString(const Value: String);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsBytes(const Value: String);
    procedure SetAsText(const Value: String);
  public
    property Name: String read FName;
    property AsString: String write SetAsString;
    property AsInteger: Integer write SetAsInteger;
    property AsFloat: Double write SetAsFloat;
    property AsBoolean: Boolean write SetAsBoolean;
    property AsDateTime: TDateTime write SetAsDateTime;
    property AsBytes: String write SetAsBytes;
    property AsText: String write SetAsText;
  end;

  IQueryBuilder = interface
    function QueryBuilder: IQueryBuilder;
    procedure Insert;
    procedure Update;
    procedure Delete;
    procedure Query;
    procedure RowCount(const Value: Cardinal);
    function Field(const FieldName: String): TQueryField;
    function Where(const FieldName, Operator: String): TQueryField;
    function Build(const QueryStr: String): String;
    function BuildAndQuery(const QueryStr: String): IQueryResult;
    function BuildAndUpdate(const TableName: String): Boolean;
    function LastQuery: IQueryResult;
  end;

  TQueryBuilder = class(TSilInterfacedObject, IQueryBuilder)
  private
    FDb: TDatabase;
    FKind: TQBKind;
    FFields: IPointerList;
    FRowCount: Cardinal;
    FResult: IQueryResult;
    FCritical: Boolean;
    procedure ClearFields;
  public
    constructor Create(Database: TDatabase; Critical: Boolean = false);
    destructor Destroy; override;
    function QueryBuilder: IQueryBuilder;
    procedure Insert;
    procedure Update;
    procedure Delete;
    procedure Query;
    procedure RowCount(const Value: Cardinal);
    function Field(const FieldName: String): TQueryField;
    function Where(const FieldName, Operator: String): TQueryField;
    function Build(const QueryStr: String): String;
    function BuildAndQuery(const QueryStr: String): IQueryResult;
    function BuildAndUpdate(const TableName: String): Boolean;
    function LastQuery: IQueryResult;
  end;

  TQueryResultField = class
  private
    FOwner: TQueryResult;
    FName: String;
    FNumber: Integer;
    FDataType: TFieldType;
    FSize: Integer;
    constructor Create(Owner: TQueryResult; const FieldName: String; Number, Size: Integer);
  public
    function GetData(Buffer: Pointer): Boolean;
    function AsString: String;
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
    function GetValueType: Byte;
    property Name: String read FName;
    property DataType: TFieldType read FDataType;
    property Size: Integer read FSize;
  end;

  IQueryResult = interface
    function QueryResult: IQueryResult;
    function ExecuteQuery(const QueryStr: String): Boolean;
    function ExecuteUpdate(const QueryStr: String): Boolean;
    function Field(const FieldName: String): TQueryResultField;
    function MoveBy(const Distance: Integer): Integer;
    procedure First;
    procedure Last;
    function Next: Boolean;
    function Prior: Boolean;
    function GetBookmark: String;
    procedure SetBookmark(const Bookmark: String);
    function GetRowCount: Integer;
    function IsEOF: Boolean;
    function IsBOF: Boolean;
    function Database: TDatabase;
    function GetFields: IPointerList;
  end;

  TQueryResult = class(TSilInterfacedObject, IQueryResult)
  private
    FDb: TDatabase;
    FStmt: hDBIStmt;
    FQryCur: hDBICur;
    FRecBuff: Pointer;
    FCurProps: CurProps;
    FBookmarkSize: Integer;
    FEOF: Boolean;
    FBOF: Boolean;
    FRowCount: Integer;
    FActive: Boolean;
    FFields: IPointerList;
    FCritical: Boolean;
    procedure CreateField(const FieldDesc: FLDDesc; const i: Integer);
    procedure ClearFields;
    procedure FreeResources;
  public
    constructor Create(Database: TDatabase; Critical: Boolean = false);
    destructor Destroy; override;
    function QueryResult: IQueryResult;
    function ExecuteQuery(const QueryStr: String): Boolean;
    function ExecuteUpdate(const QueryStr: String): Boolean;
    function Field(const FieldName: String): TQueryResultField;
    function MoveBy(const Distance: Integer): Integer;
    procedure First;
    procedure Last;
    function Next: Boolean;
    function Prior: Boolean;
    function GetBookmark: String;
    procedure SetBookmark(const Bookmark: String);
    function GetRowCount: Integer;
    function IsEOF: Boolean;
    function IsBOF: Boolean;
    function Database: TDatabase;
    function GetFields: IPointerList;
  end;

  IQueryStatement = interface
    function QueryStatement: IQueryStatement;
    function GetQueryBuilder: IQueryBuilder;
    function ExecuteQuery(const QueryStr: String): IQueryResult;
    function ExecuteUpdate(const QueryStr: String): Boolean;
    function Database: TDatabase;
  end;

  TQueryStatement = class(TSilInterfacedObject, IQueryStatement)
  private
    FDb: TDatabase;
    FCritical: Boolean;
  public
    constructor Create(Database: TDatabase; Critical: Boolean = false);
    function QueryStatement: IQueryStatement;
    function GetQueryBuilder: IQueryBuilder;
    function ExecuteQuery(const QueryStr: String): IQueryResult;
    function ExecuteUpdate(const QueryStr: String): Boolean;
    function Database: TDatabase;
  end;

  {TQueryManager = class
  private
    FErrorFile: String;
    FIsConnecting: Boolean;
  public
  end;}

function f_QueryBuilder(Database: TDatabase = nil; Critical: Boolean = false): IQueryBuilder;
function f_QueryStatement(Database: TDatabase = nil; Critical: Boolean = false): IQueryStatement;
function f_QueryResult(Database: TDatabase = nil; Critical: Boolean = false): IQueryResult;
function f_ExecQuery(Database: TDatabase; const QueryStr: String; Critical: Boolean = false): Boolean;
function f_SqlDateTime(const Value: TDateTime): String;

var
  QbErrorFile: String = 'sql';
  QbErrorProc: procedure(Database: TDatabase; const Text: String) = nil;

implementation

function f_SqlDateTime(const Value: TDateTime): String;
begin
  Result := #39 + FormatDateTime('mm/dd/yyyy hh:nn:ss', Value) + #39;
end;

function DoCheckDatabase(Database: TDatabase; const QueryStr: String): Boolean;
begin
  Result := Database.Connected;

  if not Result then
    try
      Database.Connected := true;
      Result := Database.Connected;
    except
      on Err: Exception do f_SaveError(QbErrorFile, Err.Message + #13#10 + QueryStr);
    end;
end;

procedure DoSaveError(Db: TDatabase; Err: EDBEngineError; const QueryStr: String);
var
  i: Integer;
begin
  f_SaveError(QbErrorFile, Err.Message + #13#10 + QueryStr);
  if Assigned(QbErrorProc) then QbErrorProc(Db, Err.Message + #13#10 + QueryStr);

  for i := 0 to Err.ErrorCount - 1 do
    if Err.Errors[i].ErrorCode = 11277 then Db.Connected := false;
end;

function f_QueryBuilder(Database: TDatabase; Critical: Boolean): IQueryBuilder;
begin
  Result := TQueryBuilder.Create(Database, Critical);
end;

function f_QueryStatement(Database: TDatabase; Critical: Boolean): IQueryStatement;
begin
  Result := TQueryStatement.Create(Database, Critical);
end;

function f_QueryResult(Database: TDatabase; Critical: Boolean): IQueryResult;
begin
  Result := TQueryResult.Create(Database, Critical);
end;

function f_ExecQuery(Database: TDatabase; const QueryStr: String; Critical: Boolean): Boolean;
var
  wStatus: Word;
begin
  try
    if (Database = nil) or not DoCheckDatabase(Database, QueryStr) then
    begin
      Result := false;
      Exit;
    end;

    wStatus := DbiQExecDirect(Database.Handle, qrylangSQL, PChar(QueryStr), nil);
    Check(wStatus);
    Result := (wStatus = DBIERR_NONE);
  except
    on Err: EDBEngineError do
    begin
      DoSaveError(Database, Err, QueryStr);
      raise;
    end;
  end;
end;

{ TQueryField }

constructor TQueryField.Create(const FieldName: String; const Kind: TQBKind);
begin
  inherited Create;
  FKind := Kind;

  if (Kind = qbQuery) then
    FName := ':' + FieldName else
    FName := FieldName;
end;

procedure TQueryField.SetAsString(const Value: String);
begin
  FValue := #39 + Str.Translate(Value, #39, #39#39) + #39;
end;

procedure TQueryField.SetAsInteger(const Value: Integer);
begin
  FValue := Int.ToStr(Value);
end;

procedure TQueryField.SetAsFloat(const Value: Double);
begin
  FValue := Float.ToStr(Value, '.');
end;

procedure TQueryField.SetAsBoolean(const Value: Boolean);
begin
  FValue := Bin.ToChr(Value);
end;

procedure TQueryField.SetAsDateTime(const Value: TDateTime);
begin
  FValue := f_SqlDateTime(Value);
end;

procedure TQueryField.SetAsBytes(const Value: String);
const
  Digits: String = '0123456789ABCDEF';
var
  i, n: Integer;
  Calc: String;
begin
  SetString(Calc, PChar('0x'), Length(Value) * 2 + 2);
  i := 3;
  n := 1;

  while (n <= Length(Value)) do
  begin
    Calc[i] := Digits[Ord(Value[n]) shr 4 + 1];
    Calc[i + 1] := Digits[Ord(Value[n]) and 15 + 1];
    Inc(i, 2);
    Inc(n);
  end;

  FValue := #39 + Calc + #39;
end;

procedure TQueryField.SetAsText(const Value: String);
begin
  FValue := Value;
end;

{ TQueryBuilder }

constructor TQueryBuilder.Create(Database: TDatabase; Critical: Boolean);
begin
  inherited Create;
  FCritical := Critical;
  FDb := Database;
  FKind := qbInvalid;
  FFields := Sil.List.PointerList;
  FRowCount := 0;
end;

destructor TQueryBuilder.Destroy;
begin
  FResult := nil;
  ClearFields;
  inherited Destroy;
end;

procedure TQueryBuilder.ClearFields;
var
  i: Integer;
begin
  for i := 0 to FFields.Count - 1 do
    TQueryField(FFields[i]).Free;

  FFields.Clear;
end;

procedure TQueryBuilder.Insert;
begin
  FKind := qbInsert;
end;

procedure TQueryBuilder.Update;
begin
  FKind := qbUpdate;
end;

procedure TQueryBuilder.Delete;
begin
  FKind := qbDelete;
end;

procedure TQueryBuilder.Query;
begin
  FKind := qbQuery;
end;

procedure TQueryBuilder.RowCount(const Value: Cardinal);
begin
  FRowCount := Value;
end;

function TQueryBuilder.Field(const FieldName: String): TQueryField;
begin
  Result := TQueryField.Create(FieldName, fKind);
  FFields.Add(Result);
end;

function TQueryBuilder.Where(const FieldName, Operator: String): TQueryField;
begin
  Result := TQueryField.Create(FieldName, qbWhere);
  Result.FOperator := Operator;
  FFields.Add(Result);
end;

function TQueryBuilder.Build(const QueryStr: String): String;
var
  i: Integer;
  bFirst, bUpdate, bInsert: Boolean;
  sQuery: String;
begin
  if (FRowCount > 0) then
    sQuery := 'SET ROWCOUNT ' + Int.ToStr(FRowCount) + ' ' + QueryStr + ' SET ROWCOUNT 0' else
    sQuery := QueryStr;

  if FFields.Count = 0 then
  begin
    Result := sQuery;
    Exit;
  end;

  if (FKind = qbQuery) then
  begin
    sQuery := QueryStr;

    for i := 0 to FFields.Count - 1 do
      with TQueryField(FFields[i]) do
        if (FKind = qbQuery) then
          sQuery := Str.Translate(sQuery, FName, FValue);
  end else
  begin
    bUpdate := false;
    bInsert := false;
    sQuery := '';

    if (FKind = qbUpdate) then
    begin
      bFirst := true;
      for i := 0 to FFields.Count - 1 do
        with TQueryField(FFields[i]) do
          if (FKind = qbUpdate) then
          begin
            if bFirst then
            begin
              bFirst := false;
              bUpdate := true;
              sQuery := sQuery + 'UPDATE ' + QueryStr + ' SET ' + FName + '=' + FValue;
            end else
              sQuery := sQuery + ', ' + FName + '=' + FValue;
          end;
    end;

    if (FKind = qbDelete) then
    begin
      bUpdate := true;
      sQuery := 'DELETE ' + QueryStr;
    end;

    if bUpdate then
    begin
      bFirst := true;
      for i := 0 to FFields.Count - 1 do
        with TQueryField(FFields[i]) do
          if (FKind = qbWhere) then
          begin
            if bFirst then
            begin
              bFirst := false;
              sQuery := sQuery + ' WHERE ' + FName + FOperator + FValue;
            end else
              sQuery := sQuery + ' AND ' + FName + FOperator + FValue;
          end;
    end;                                                    

    if (FKind = qbInsert) then
    begin
      bFirst := true;
      for i := 0 to FFields.Count - 1 do
        with TQueryField(FFields[i]) do
          if (FKind = qbInsert) then
            if bFirst then
            begin
              bFirst := false;
              bInsert := true;
              //if bUpdate then sQuery := sQuery + ' IF (@@ROWCOUNT = 0) ';
              sQuery := sQuery + 'INSERT INTO ' + QueryStr + '(' + FName;
            end else
              sQuery := sQuery + ', ' + FName;
    end;

    if bInsert then
    begin
      bFirst := true;
      for i := 0 to FFields.Count - 1 do
        with TQueryField(FFields[i]) do
          if (FKind = qbInsert) then
            if bFirst then
            begin
              bFirst := false;
              sQuery := sQuery + ') VALUES (' + FValue;
            end else
              sQuery := sQuery + ', ' + FValue;

      if (not bFirst) then sQuery := sQuery + ')';
    end;
  end;

  ClearFields;
  Result := sQuery;
end;

function TQueryBuilder.BuildAndQuery(const QueryStr: String): IQueryResult;
begin
  FResult := nil;
  FResult := f_QueryResult(FDb, FCritical);
  FResult.ExecuteQuery(Build(QueryStr));
  Result := FResult;
end;

function TQueryBuilder.BuildAndUpdate(const TableName: String): Boolean;
begin
  Result := f_ExecQuery(FDb, Build(TableName), FCritical);
end;

function TQueryBuilder.LastQuery: IQueryResult;
begin
  Result := FResult;
end;

function TQueryBuilder.QueryBuilder: IQueryBuilder;
begin
  Result := Self;
end;

{ TQueryStatement }

constructor TQueryStatement.Create(Database: TDatabase; Critical: Boolean);
begin
  inherited Create;
  FCritical := Critical;
  FDb := Database;
end;

function TQueryStatement.Database: TDatabase;
begin
  Result := FDb;
end;

function TQueryStatement.GetQueryBuilder: IQueryBuilder;
begin
  Result := TQueryBuilder.Create(FDb);
end;

function TQueryStatement.ExecuteQuery(const QueryStr: String): IQueryResult;
begin
  Result := TQueryResult.Create(FDb);
  Result.ExecuteQuery(QueryStr);
end;

function TQueryStatement.ExecuteUpdate(const QueryStr: String): Boolean;
begin
  Result := f_ExecQuery(FDb, QueryStr, FCritical);
end;

function TQueryStatement.QueryStatement: IQueryStatement;
begin
  Result := Self;
end;

{ TQueryResultField }

constructor TQueryResultField.Create(Owner: TQueryResult; const FieldName: String; Number, Size: Integer);
begin
  inherited Create;
  FOwner := Owner;
  FName := FieldName;
  FNumber := Number;
  FSize := Size;
end;

function TQueryResultField.GetData(Buffer: Pointer): Boolean;
var
  bIsBlank: LongBool;
begin
  Result := (DbiGetField(FOwner.FQryCur, FNumber, FOwner.FRecBuff, Buffer, bIsBlank) = DBIERR_NONE) and not bIsBlank;
end;

function TQueryResultField.AsString: String;

  procedure _Trim(var Buffer: String);
  var
    i: Integer;
  begin
    for i := 1 to Length(Buffer) do
      if (Buffer[i] = #0) then
      begin
        SetLength(Buffer, i - 1);
        Exit;
      end;
  end;

begin
  case FDataType of
    Db.ftSmallint, Db.ftInteger, Db.ftWord, Db.ftAutoInc:
      Result := Int.ToStr(AsInteger);

    Db.ftString:
    begin
      SetLength(Result, FSize);
      FillChar(Result[1], FSize, #0);

      if GetData(PChar(Result)) then
        _Trim(Result) else
        Result := '';
    end;

    Db.ftDate, Db.ftTime, Db.ftDateTime:
      Result := DateTimeToStr(AsDateTime);

    Db.ftBoolean:
      if AsBoolean then
        Result := 'true' else
        Result := 'false';

    Db.ftFloat, Db.ftCurrency:
      Result := FloatToStr(AsFloat);

    else Result := '';
  end;
end;

function TQueryResultField.AsInteger: Integer;
var
  Data: record
    case Integer of
      0: (I: Smallint);
      1: (W: Word);
      2: (L: Longint);
  end;
begin
  case FDataType of
    Db.ftSmallint, Db.ftInteger, Db.ftWord, Db.ftAutoInc:
    begin
      if GetData(@Data) then
      begin
        case FDataType of
          Db.ftSmallint:  Result := Data.I;
          Db.ftWord:      Result := Data.W;
        else              Result := Data.L;
        end;
      end else
        Result := 0;
    end;

    Db.ftString:
      Result := StrToInt(AsString);

    Db.ftDate, Db.ftTime, Db.ftDateTime:
      Result := 0;

    Db.ftBoolean:
      Result := Ord(AsBoolean);

    Db.ftFloat, Db.ftCurrency:
      Result := Trunc(AsFloat);

    else
      Result := 0;
  end;
end;

function TQueryResultField.AsFloat: Double;
var
  Data: Double;
begin
  case FDataType of
    Db.ftSmallint, Db.ftInteger, Db.ftWord, Db.ftAutoInc:
      Result := AsInteger;

    Db.ftString:
      Result := StrToFloat(AsString);

    Db.ftDate, Db.ftTime, Db.ftDateTime:
      Result := AsDateTime;

    Db.ftBoolean:
      Result := Ord(GetData(@Data));

    Db.ftFloat, Db.ftCurrency:
      if GetData(@Data) then
        Result := Data else
        Result := 0;

    else
      Result := 0;
  end;
end;

function TQueryResultField.AsBoolean: Boolean;
var
  Data: WordBool;
begin
  case FDataType of
    Db.ftSmallint, Db.ftInteger, Db.ftWord, Db.ftAutoInc:
      Result := AsInteger > 0;

    Db.ftString:
      Result := LowerCase(Str.ToChr(AsString, 'f')) = 't';

    Db.ftDate, Db.ftTime, Db.ftDateTime:
      Result := false;

    Db.ftBoolean:
      if GetData(@Data) then
        Result := Data else
        Result := False;

    Db.ftFloat, Db.ftCurrency:
      Result := false;

    else
      Result := false;
  end;
end;

function TQueryResultField.AsDateTime: TDateTime;
var
  TimeStamp: TTimeStamp;
  Data: TDateTimeRec;
begin
  case FDataType of
    Db.ftSmallint, Db.ftInteger, Db.ftWord, Db.ftAutoInc:
      Result := 0;

    Db.ftString:
      Result := StrToDateTime(AsString);

    Db.ftDate, Db.ftTime, Db.ftDateTime:
      if GetData(@Data) then
      begin
        case FDataType of
          Db.ftDate:
            begin
              TimeStamp.Time := 0;
              TimeStamp.Date := Data.Date;
            end;
          Db.ftTime:
            begin
              TimeStamp.Time := Data.Time;
              TimeStamp.Date := DateDelta;
            end;
        else
          try
            TimeStamp := MSecsToTimeStamp(Data.DateTime);
          except
            TimeStamp.Time := 0;
            TimeStamp.Date := 0;
          end;
        end;
        Result := TimeStampToDateTime(TimeStamp);
      end else
        Result := 0;

    Db.ftBoolean:
      Result := 0;

    Db.ftFloat, Db.ftCurrency:
      Result := 0;

    else Result := 0;
  end;
end;

function TQueryResultField.GetValueType: Byte;
begin
  case FDataType of
    Db.ftSmallint,
    Db.ftInteger,
    Db.ftWord,
    Db.ftAutoInc:    Result := vtInteger;
    Db.ftString:    Result := vtAnsiString;
    Db.ftFloat,
    Db.ftCurrency,
    Db.ftDate,
    Db.ftTime,
    Db.ftDateTime:  Result := vtExtended;
    Db.ftBoolean:    Result := vtBoolean;
    else            Result := 0;
  end;
end;

{ TQueryResult }

constructor TQueryResult.Create(Database: TDatabase; Critical: Boolean);
begin
  inherited Create;
  FCritical := Critical;
  FDb := Database;
  FStmt := nil;
  FQryCur := nil;
  FRecBuff := nil;
  FRowCount := -1;
  FActive := false;
  FFields := Sil.List.PointerList;
end;

destructor TQueryResult.Destroy;
begin
  if (FDb <> nil) and FDb.Connected then
  begin
    FreeResources;
    ClearFields;
  end;
  inherited Destroy;
end;

function TQueryResult.Database: TDatabase;
begin
  Result := FDb;
end;

procedure TQueryResult.ClearFields;
var
  i: Integer;
begin
  for i := 0 to FFields.Count - 1 do
    TQueryResultField(FFields[i]).Free;

  FFields.Clear;
end;

procedure TQueryResult.CreateField(const FieldDesc: FLDDesc; const i: Integer);
var
  Field: TQueryResultField;
begin
  with FieldDesc do
  begin
    Field := TQueryResultField.Create(Self, szName, i, iLen);

    if (iFldType < MAXLOGFLDTYPES) then
      Field.FDataType := DataTypeMap[iFldType] else
      Field.FDataType := Db.ftUnknown;

    case iFldType of
      fldZSTRING:
        if iUnits1 = 0 then { Ignore MLSLABEL field type on Oracle }
          Field.FDataType := Db.ftUnknown else
          Field.FSize := iUnits1;
      fldINT16, fldUINT16:
        if iLen <> 2 then Field.FDataType := Db.ftUnknown;
      fldINT32:
        if iSubType = fldstAUTOINC then
          Field.FDataType := Db.ftAutoInc;
      fldFLOAT:
        if iSubType = fldstMONEY then Field.FDataType := Db.ftCurrency;
      fldBYTES, fldVARBYTES:
        Field.FSize := iUnits1;
      fldBLOB:
        begin
          Field.FSize := iUnits1;
          if (iSubType >= fldstMEMO) and (iSubType <= fldstTYPEDBINARY) then
            Field.FDataType := BlobTypeMap[iSubType];
        end;
    end;
  end;

  FFields.Items[i - 1] := Field;
end;

procedure TQueryResult.FreeResources;
begin
  if (FRecBuff <> nil) then FreeMem(FRecBuff);
  if (FStmt <> nil) then Check(DbiQFree(FStmt));
  if (FQryCur <> nil) then Check(DbiCloseCursor(FQryCur));

  FStmt := nil;
  FQryCur := nil;
  FRecBuff := nil;

  FEOF := true;
  FBOF := true;
  FActive := false;
end;

procedure TQueryResult.First;
begin
  DbiSetToBegin(FQryCur);
  Next;
end;

procedure TQueryResult.Last;
begin
  DbiSetToEnd(FQryCur);
  Prior;
end;

function TQueryResult.Next: Boolean;
begin
  if FActive then FEOF := DbiGetNextRecord(FQryCur, dbiNOLOCK, FRecBuff, nil) = DBIERR_EOF;
  Result := not FEOF;
end;

function TQueryResult.Prior: Boolean;
begin
  if FActive then FBOF := DbiGetPriorRecord(FQryCur, dbiNOLOCK, FRecBuff, nil) = DBIERR_BOF;
  Result := not FBOF;
end;

function TQueryResult.Field(const FieldName: String): TQueryResultField;
var
  i: Integer;
begin
  for i := 0 to FFields.Count - 1 do
    if StrIComp(PChar(TQueryResultField(FFields[i]).FName), PChar(FieldName)) = 0 then
    begin
      Result := FFields[I];
      Exit;
    end;

  DatabaseErrorFmt('FieldNotFound: %s', [FieldName]);
  Result := nil;
end;

function TQueryResult.MoveBy(const Distance: Integer): Integer;
begin
  Result := 0;

  if (Distance > 0) then
    FEOF := DbiGetRelativeRecord(FQryCur, Distance, dbiNOLOCK, FRecBuff, nil) = DBIERR_EOF else
  if (Distance < 0) then
    FBOF := DbiGetRelativeRecord(FQryCur, Distance, dbiNOLOCK, FRecBuff, nil) = DBIERR_BOF;
end;

function TQueryResult.GetBookmark: String;
begin
  if FActive then
  begin
    SetLength(Result, FBookmarkSize);
    Check(DbiGetBookmark(FQryCur, Pointer(Result)));
  end else
    Result := '';
end;

procedure TQueryResult.SetBookmark(const Bookmark: String);
begin
  if FActive then
  begin
    Check(DbiSetToBookmark(FQryCur, Pointer(Bookmark)));
    DbiGetRecord(FQryCur, dbiNOLOCK, FRecBuff, nil);
  end;
end;

function TQueryResult.IsEOF: Boolean;
begin
  Result := FEOF;
end;

function TQueryResult.IsBOF: Boolean;
begin
  Result := FEOF;
end;

function TQueryResult.GetRowCount: Integer;
var
  wLength: Word;
begin
  if (FRowCount = -1) then
  begin
    Check(DbiGetProp(hDBIObj(FStmt), stmtROWCOUNT, @FRowCount, SizeOf(FRowCount), wLength));
    if (FRowCount < 1) and (DbiGetRecordCount(FQryCur, FRowCount) <> DBIERR_NONE) then
      FRowCount := -1;
  end;
  Result := FRowCount;
end;

function TQueryResult.QueryResult: IQueryResult;
begin
  Result := Self;
end;

function TQueryResult.ExecuteQuery(const QueryStr: String): Boolean;
type
  PFieldDescList = ^TOldFieldDescList;
  TOldFieldDescList = array[0..1023] of FLDDesc;
var
  FieldDescs: PFieldDescList;
  i: Integer;
begin
  Result := false;
  if (FDb = nil) or not DoCheckDatabase(FDb, QueryStr) then Exit;
  FreeResources;

  try
    Check(DbiQAlloc(FDb.Handle, qrylangSQL, FStmt));
    Check(DbiQPrepare(FStmt, PChar(QueryStr)));
    Check(DbiQExec(FStmt, @FQryCur));

    FRowCount := -1;

    if FQryCur <> nil then
    begin
      Check(DbiGetCursorProps(FQryCur, FCurProps));
      FRecBuff := AllocMem(FCurProps.iRecBufSize);
      FieldDescs := AllocMem(FCurProps.iFields * SizeOf(FLDDesc));
      FBookmarkSize := FCurProps.iBookmarkSize;

      FActive := true;
      Result := true;

      try
        DbiGetFieldDescs(FQryCur, PFLDDesc(FieldDescs));
        FFields.Count := FCurProps.iFields;
        for i := 0 to FCurProps.iFields - 1 do CreateField(FieldDescs^[i], i + 1);
      finally
        FreeMem(FieldDescs);
      end;

      First;
    end;
  except
    on Err: EDBEngineError do
    begin
      DoSaveError(Database, Err, QueryStr);
      FreeResources;
      raise;
    end;
  end;
end;

function TQueryResult.ExecuteUpdate(const QueryStr: String): Boolean;
begin
  Result := f_ExecQuery(FDb, QueryStr, FCritical);
end;

function TQueryResult.GetFields: IPointerList;
begin
  Result := FFields;
end;

{
0 10024 Table does not exist.
208 13059 Invalid object name 'pp'.

0 11277 Cannot locate or connect to SQL server.
10004 13059 Unable to connect: SQL Server is unavailable or does not exist.  Specified SQL server not found.
0 11277 Alias: dd

0 13059 General SQL error.
911 13059 Attempt to locate entry in Sysdatabases for database 'xprueba' by name failed - no entry found under that name. Make sure that
0 13059 Alias: dd
}

end.
