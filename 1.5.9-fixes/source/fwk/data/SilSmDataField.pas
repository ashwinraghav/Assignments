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

unit SilSmDataField;

{$I Defines.inc}

interface

uses
  Sil,

  SilLmInterfaceList,
  SilLmFieldAccess,
  SilSiDataAccess;

type
  TDataFieldList = class;

  PDataMemoHeader = ^TDataMemoHeader;
  TDataMemoHeader = packed record
    RecType: Char;
    NextRecord: LongWord;
  end;

  PDataMemoValue = ^TDataMemoValue;
  TDataMemoValue = packed record
    Size: LongWord;
    NextRecord: LongWord;
  end;

  TDataMemoField = class (
    // extends
    TMemoFieldAccess)
  private
    FRowsetDef: IDataRowsetDef;
  protected // IValueReader
    function Compare(const Item1, Item2; Data: Pointer): Integer; override;
    function GetAsString: String; override;
  protected // IValueAccess
    procedure SetAsString(const Value: String); override;
  public
    constructor Create(const Name: String; const Store: IFieldStore); override;
    destructor Destroy; override;
  end;

  TDataRecordField = class (
    // extends
    TLongWordFieldAccess)
  private
    FRowsetDef: IDataRowsetDef;
  protected // IValueReader
    function GetAsLongWord: LongWord; override;
  protected // IValueAccess
    procedure SetAsLongWord(Value: LongWord); override;
  public
    constructor Create(const RowsetDef: IDataRowsetDef); reintroduce;
    destructor Destroy; override;
  end;

  TDataDeletedField = class (
    // extends
    TByteFieldAccess)
  private
    FRowsetDef: IDataRowsetDef;
  public
    constructor Create(const RowsetDef: IDataRowsetDef); reintroduce;
    destructor Destroy; override;
  end;

  TDataFieldList = class (
    // extends
    TSilInterfaceList,
    // implements
    IDataFieldList,
    IDataFieldDefList)
  private
    FRowsetDef: IDataRowsetDef;
    FPositionOffset: LongWord;
  private
    class function CreateField(const Name: String; DataType: TDataFieldType; Size: LongWord; const Store: IFieldStore): IFieldDef;
  protected // IDataFieldList
    function IDataFieldList.GetByName = AccGetByName;
    function AccGetByName(const Name: String): IFieldAccess;
    function IDataFieldList.First = AccFirst;
    function AccFirst: IFieldAccess;
    function IDataFieldList.Last = AccLast;
    function AccLast: IFieldAccess;
    function IDataFieldList.GetItem = AccGetItem;
    function AccGetItem(Index: Integer): IFieldAccess;
    function GetNames: String;
  protected // IDataFieldDefList
    function CreateItem(const Name: String = ''; DataType: TDataFieldType = ftUnknown; Size: LongWord = 0): IFieldDef;
    function First: IFieldDef;
    procedure AddItems(const Source: IDataFieldDefList);
    function Add(const Value: IFieldDef): Integer; reintroduce;
    function Last: IFieldDef;
    function GetItem(Index: Integer): IFieldDef;
    function GetByName(const Name: String): IFieldDef;
    procedure SetItem(Index: Integer; const Value: IFieldDef);
    function GetPositionOffset: LongWord;
    procedure SetPositionOffset(Value: LongWord);
    procedure Unbind;
  public
    constructor Create(const Rowset: IDataRowsetDef); reintroduce;
    destructor Destroy; override;
  end;

  TDataFieldDisplay = class (
    // extends
    TSilInterfacedObject,
    // implements
    IFieldDisplay)
  private
    FCaption: String;
    FMask: String;
    FField: IFieldAccess;
  protected  // IFieldDisplay
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    function GetMask: String;
    function GetDisplay: String;
    function GetField: IFieldAccess;
    procedure SetMask(const Value: String);
  public
    constructor Create(const Field: IFieldAccess; const Caption, Mask: String);
    destructor Destroy; override;
  end;

  TDataFieldDisplayList = class (
    // extends
    TSilInterfaceList,
    // implements
    IFieldDisplayList)
  protected  // IFieldDisplayList
    function CreateItem(const Field: IFieldAccess; const Caption: String; const Mask: String = ''): IFieldDisplay;
    function GetItem(Index: Integer): IFieldDisplay;
    function GetByName(const Name: String): IFieldDisplay;
    function GetDisplay(Index: Integer): String;
  end;

implementation

uses
  SilSmDataRowset;

{ TDataField }
{
procedure TDataField.SetSize(Value: LongWord);
var
  e: IEnumerator;
  Field: IFieldDef;
  lwSize: LongWord;
begin
  inherited;

  FSize := Value;
  lwSize := FRowsetDef.GetFieldDefs.PositionOffset;

  while FRowsetDef.GetFieldDefs.Enumerate(e, Field) do
  begin
    Field.Position := lwSize;
    Inc(lwSize, Field.Size);
  end;
end;
}

{ TDataFieldList }

constructor TDataFieldList.Create(const Rowset: IDataRowsetDef);
begin
  inherited Create(true);
  if Rowset <> nil then Pointer(FRowsetDef) := Pointer(Rowset);
end;

function TDataFieldList.AccFirst: IFieldAccess;
begin
  Result := IFieldAccess(inherited First);
end;

function TDataFieldList.AccGetByName(const Name: String): IFieldAccess;
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

function TDataFieldList.AccGetItem(Index: Integer): IFieldAccess;
begin
  Result := IFieldAccess(inherited GetItem(Index));
end;

function TDataFieldList.AccLast: IFieldAccess;
begin
  Result := IFieldAccess(inherited Last);
end;

function TDataFieldList.GetNames: String;
var
  e: IEnumerator;
  Field: IFieldDef;
begin
  Result := '';
  while Enumerate(e, Field) do Result := Result + Field.Name + ',';
  SetLength(Result, Length(Result) - 1);
end;

class function TDataFieldList.CreateField(const Name: String; DataType: TDataFieldType; Size: LongWord; const Store: IFieldStore): IFieldDef;
begin
  case DataType of
    ftChar,
    ftByte:       Result := TCharFieldAccess.Create(Name, Store);
    ftString:     Result := TStringFieldAccess.CreateSized(Name, Size, Store);
    ftMemo:       Result := TDataMemoField.Create(Name, Store);
    ftWideString: Result := TWideStringFieldAccess.CreateSized(Name, Size, Store);
    ftSmallInt:   Result := TSmallIntFieldAccess.Create(Name, Store);
    ftInteger:    Result := TIntegerFieldAccess.Create(Name, Store);
    ftLargeInt:   Result := TLargeIntFieldAccess.Create(Name, Store);
    ftWord:       Result := TWordFieldAccess.Create(Name, Store);
    ftLongWord:   Result := TLongWordFieldAccess.Create(Name, Store);
    ftBoolean:    Result := TBooleanFieldAccess.Create(Name, Store);
    ftFloat:      Result := TFloatFieldAccess.Create(Name, Store);
    ftDateTime:   Result := TDateTimeFieldAccess.Create(Name, Store);
    ftGuid:       Result := TGuidFieldAccess.Create(Name, Store);
    ftVariant:    Result := TVariantFieldAccess.Create(Name, Store);
    else          raise Error.Create('TDataFieldList.CreateField: tipo invalido');
  end;
end;

function TDataFieldList.CreateItem(const Name: String; DataType: TDataFieldType; Size: LongWord): IFieldDef;
var
  e: IEnumerator;
  Field: IFieldDef;
  lwSize: LongWord;
begin
  if FRowsetDef = nil then
    Result := CreateField(Name, DataType, Size, nil) else
    Result := CreateField(Name, DataType, Size, FRowsetDef.Store);

  Add(Result);

  if FRowsetDef <> nil then
  begin
    lwSize := FRowsetDef.GetFieldDefs.PositionOffset;

    while FRowsetDef.GetFieldDefs.Enumerate(e, Field) do
    begin
      Field.Position := lwSize;
      Inc(lwSize, Field.Size);
    end;
  end;
end;

function TDataFieldList.First: IFieldDef;
begin
  Result := IFieldDef(inherited First);
end;

function TDataFieldList.GetItem(Index: Integer): IFieldDef;
begin
  Result := IFieldDef(inherited GetItem(Index));
end;

function TDataFieldList.Last: IFieldDef;
begin
  Result := IFieldDef(inherited Last);
end;

procedure TDataFieldList.SetItem(Index: Integer; const Value: IFieldDef);
begin
  inherited SetItem(Index, Value);
end;

destructor TDataFieldList.Destroy;
begin
  Clear;
  Unbind;
  inherited;
end;

function TDataFieldList.GetPositionOffset: LongWord;
begin
  Result := FPositionOffset;
end;

procedure TDataFieldList.SetPositionOffset(Value: LongWord);
var
  e: IEnumerator;
  Field: IFieldDef;
begin
  while Enumerate(e, Field) do Field.Position := Field.Position - FPositionOffset + Value;
  FPositionOffset := Value;
end;

procedure TDataFieldList.AddItems(const Source: IDataFieldDefList);
var
  i: Integer;
begin
  for i := 0 to Source.Count - 1 do
    with Source.Items[i] do CreateItem(Name, DataType, Size);
end;

function TDataFieldList.Add(const Value: IFieldDef): Integer;
begin
  Result := inherited Add(Value);
end;

function TDataFieldList.GetByName(const Name: String): IFieldDef;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Sil.Text.Compare(Result.Name, Name) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TDataFieldList.Unbind;
var
  e: IEnumerator;
  Field: IFieldDef;
begin
  while Enumerate(e, Field) do Field.Unbind;
  Clear;
  Pointer(FRowsetDef) := nil;
end;

{ TDataRecordField }

constructor TDataRecordField.Create(const RowsetDef: IDataRowsetDef);
begin
  inherited Create('$Recno', nil);

  FDataType := ftLongWord;
  SetSize(SizeOf(LongWord));

  Pointer(FRowsetDef) := Pointer(RowsetDef);
  FPosition := FRowsetDef.RecordSize;
end;

destructor TDataRecordField.Destroy;
begin
  Pointer(FRowsetDef) := nil;
  inherited;
end;

function TDataRecordField.GetAsLongWord: LongWord;
begin
  Result := FRowsetDef.CurrentRecord;
end;

procedure TDataRecordField.SetAsLongWord(Value: LongWord);
begin
end;

{ TDataMemoField }

function TDataMemoField.Compare(const Item1; const Item2; Data: Pointer): Integer;
begin
  Result := 0;
end;

constructor TDataMemoField.Create(const Name: String; const Store: IFieldStore);
var
  Def: IDataRowsetDef;
begin
  inherited;
  FDataType := ftMemo;
  SetSize(SizeOf(TDataMemoValue));
  Reference.GetInterface(Store, IDataRowsetDef, Def);
  Pointer(FRowsetDef) := Pointer(Def);
end;

destructor TDataMemoField.Destroy;
begin
  Pointer(FRowsetDef) := nil;
  inherited;
end;

function TDataMemoField.GetAsString: String;
begin
  Result := FRowsetDef.ReadMemo(Self);
end;

procedure TDataMemoField.SetAsString(const Value: String);
var
  Data: LongWord;
begin
  Data := Length(Value);
  FValue := Value;
  FStore.Write(Data, FPosition, SizeOf(LongWord));
  FChanged := true;
end;

{ TDataDeletedField }

constructor TDataDeletedField.Create(const RowsetDef: IDataRowsetDef);
begin
  inherited Create(DEL_NAME, nil);

  Pointer(FRowsetDef) := Pointer(RowsetDef);
  FPosition := 0;
end;

destructor TDataDeletedField.Destroy;
begin
  Pointer(FRowsetDef) := nil;
  inherited;
end;

{ TDataFieldDisplay }

constructor TDataFieldDisplay.Create(const Field: IFieldAccess; const Caption, Mask: String);
begin
  inherited Create;

  FField := Field;
  FCaption := Caption;
  FMask := Mask;
end;

destructor TDataFieldDisplay.Destroy;
begin
  FField := nil;
  inherited;
end;

function TDataFieldDisplay.GetCaption: String;
begin
  Result := FCaption;
end;

function TDataFieldDisplay.GetDisplay: String;
begin
  if Length(FMask) = 0 then
    Result := FField.AsString else
    case FField.DataType of
      ftDateTime: Result := DateTime.ToStr(FField.AsDateTime, FMask);
      ftInteger:  Result := Str.Format(FMask, [FField.AsInteger]);
      else        Result := Str.Format(FMask, [FField.AsString]);
    end;
end;

function TDataFieldDisplay.GetField: IFieldAccess;
begin
  Result := FField;
end;

function TDataFieldDisplay.GetMask: String;
begin
  Result := FMask;
end;

procedure TDataFieldDisplay.SetCaption(const Value: String);
begin
  FCaption := Value;
end;

procedure TDataFieldDisplay.SetMask(const Value: String);
begin
  FMask := Value;
end;

{ TDataFieldDisplayList }

function TDataFieldDisplayList.CreateItem(const Field: IFieldAccess; const Caption, Mask: String): IFieldDisplay;
begin
  Result := TDataFieldDisplay.Create(Field, Str.IIf(Str.IsEmpty(Caption), Field.Name, Caption), Mask);
  Add(Result);
end;

function TDataFieldDisplayList.GetByName(const Name: String): IFieldDisplay;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Sil.Text.Compare(Result.Field.Name, Name) = 0 then Exit;
  end;
  Result := nil;
end;

function TDataFieldDisplayList.GetDisplay(Index: Integer): String;
begin
  Result := GetItem(Index).Display;
end;

function TDataFieldDisplayList.GetItem(Index: Integer): IFieldDisplay;
begin
  Result := IFieldDisplay(inherited GetItem(Index));
end;

end.
