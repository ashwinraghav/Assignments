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

unit SilSmSqlProtocol;

{$I Defines.inc}

interface

uses
  SysUtils,

  Sil,
  SilData,
  SilCoder,

  SilSiAbstractConnection,
  SilSiProtocolPacket,
  SilSiProtocolBase,
  SilSiSqlProtocol,
  
  SilSmProtocolBase;

const
  PROT_BUILD = 3;

  PS_BASE                 = $400;
  PS_GREETINGS            = PS_BASE                 + 1;
  PS_OPENDATABASE         = PS_GREETINGS            + 1;
  PS_CLOSEDATABASE        = PS_OPENDATABASE         + 1;
  PS_QUERY                = PS_CLOSEDATABASE        + 1;
  PS_EXECUTE              = PS_QUERY                + 1;
  PS_OPENDATABASE_REPLY   = PS_EXECUTE              + 1;
  PS_CLOSEDATABASE_REPLY  = PS_OPENDATABASE_REPLY   + 1;
  PS_QUERY_REPLY          = PS_CLOSEDATABASE_REPLY  + 1;
  PS_EXECUTE_REPLY        = PS_QUERY_REPLY          + 1;
  PS_QUERY2               = PS_EXECUTE_REPLY        + 1;
  PS_QUERY_REPLY2         = PS_QUERY2               + 1;
  PS_STOREDPROC           = PS_QUERY_REPLY2         + 1;
  PS_STOREDPROC_REPLY     = PS_STOREDPROC           + 1;
  PS_QUERYFIELDS          = PS_STOREDPROC_REPLY     + 1;
  PS_QUERYFIELDS_REPLY    = PS_QUERYFIELDS          + 1;
  PS_QUERYPARAMS          = PS_QUERYFIELDS_REPLY    + 1;
  PS_QUERYPARAMS_REPLY    = PS_QUERYPARAMS          + 1;

type
  TCacheItem = record
    VInteger: Integer;
    VString: String;
    VBoolean: Boolean;
    VFloat: Double;
    VDateTime: TDateTime;
  end;

  ACacheItems = array of TCacheItem;

  TProtSql = class (
    // extends
    TProtocolBase,
    // implements
    IProtocol,
    IProtSqlClient,
    IProtSqlServer)
  private
    FGreetings: String;
    FDbSession: String;
    FReady: Boolean;
    FTimeout: LongWord;
    FRemoteBuild: LongWord;
    FDefaultAppID: String;
  private
    procedure DoGetStream(const Source: IDataRowset; var Buffer: String);
    procedure DoWriteFields(const Packet: IProtocolPacket; const Fields: IValueList);
    procedure DoReadFields(const Packet: IProtocolPacket; out Fields: IValueList);
  private
    procedure FireOpenDatabase(var Msg: TProtocolBaseMessage); message PS_OPENDATABASE;
    procedure FireCloseDatabase(var Msg: TProtocolBaseMessage); message PS_CLOSEDATABASE;
    procedure FireQuery(var Msg: TProtocolBaseMessage); message PS_QUERY;
    procedure FireQuery2(var Msg: TProtocolBaseMessage); message PS_QUERY2;
    procedure FireExecute(var Msg: TProtocolBaseMessage); message PS_EXECUTE;
    procedure FireStoredProc(var Msg: TProtocolBaseMessage); message PS_STOREDPROC;
    procedure FireQueryFields(var Msg: TProtocolBaseMessage); message PS_QUERYFIELDS;
    procedure FireQueryStoredProcParams(var Msg: TProtocolBaseMessage); message PS_QUERYPARAMS;
  private
    procedure FireGreetings(var Msg: TProtocolBaseMessage); message PS_GREETINGS;
  protected // IProtocolBase
    function GetName: String; override;
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket; override;
    procedure SetConnection(const Value: IAbstractConnection); override;
  protected // IProtSqlClient
    function GetTimeout: LongWord;
    procedure SetTimeout(Value: LongWord);
    function WaitGreetings: String;
    procedure OpenDatabase(const AppID, Url: String); overload;
    procedure OpenDatabase(const Url: String); overload;
    procedure CloseDatabase;
    function Query(const Text: String): IDataRowset;
    function Execute(const Text: String): LongWord;
    function StoredProc(const Text: String; var Params: IValueList; WithRecords: Boolean = true): IDataRowset;
    function QueryFields(const Command: String; out Fields: IValueList): Boolean;
    function QueryStoredProcParams(const ProcName: String; out Params: IValueList): Boolean;
  protected // IProtSqlServer
    function GetDbSession: String;
    function GetRemoteBuild: LongWord;
    procedure SetDbSession(const Value: String);
    procedure Greetings;
  protected // IConnectedEvents
    procedure OnDisconnected(const Event: TConnectionBreakEvent); override;
  public
    constructor CreateServer(ID: LongWord; const Connection: IAbstractConnection = nil);
    constructor CreateClient(ID: LongWord; const Connection: IAbstractConnection = nil);
    destructor Destroy; override;
    class function ParseDatabaseUrl(Url: String): RDbConnectionInfo;
    class function FindParam(const List: TStringArray; const Find: String): String;
    class function DefaultPort: LongWord; override;
  end;

implementation

{ TProtSql }

constructor TProtSql.CreateServer(ID: LongWord; const Connection: IAbstractConnection);
begin
  inherited Create(ID);
  FTimeout := 1000 * 60;
  SetConnection(Connection);
end;

constructor TProtSql.CreateClient(ID: LongWord; const Connection: IAbstractConnection);
begin
  inherited Create(ID);
  FTimeout := 1000 * 60;
  FDefaultAppID := '(default)';
  SetConnection(Connection);
end;

destructor TProtSql.Destroy;
begin
  inherited;
end;

procedure TProtSql.SetConnection(const Value: IAbstractConnection);
begin
  if FConnection <> nil then Sil.Sink.Disconnect(FConnection, Self);
  if Value <> nil then Sil.Sink.Connect(Value, Self);
  inherited SetConnection(Value);
end;

procedure TProtSql.Greetings;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_GREETINGS);
  Packet.Data.WriteString('SQL Remote Protocol (Build: ' + Int.ToStr(PROT_BUILD) + ')');
  Send(Packet, 'TProtSql.Greetings:Send');
end;

function TProtSql.WaitGreetings: String;
var
  Packet: IProtocolPacket;
begin
  if Str.IsEmpty(FGreetings) then
  begin
    Greetings;
    Packet := WaitReply(PS_GREETINGS, FTimeout);
  end;
  Result := FGreetings;
end;

function TProtSql.GetTimeout: LongWord;
begin
  Result := FTimeout;
end;

procedure TProtSql.SetTimeout(Value: LongWord);
begin
  FTimeout := Value;
end;

function TProtSql.GetDbSession: String;
begin
  Result := FDbSession;
end;

procedure TProtSql.SetDbSession(const Value: String);
begin
  FDbSession := Value;
end;

function TProtSql.GetRemoteBuild: LongWord;
begin
  Result := FRemoteBuild;
end;

function TProtSql.GetName: String;
begin
  Result := 'Sql';
end;

// [driver://][user@]<server>[:password][?par1=val1[&parn=valn]]

procedure TProtSql.OpenDatabase(const Url: String);
begin
  OpenDatabase(FDefaultAppID, Url);
end;

procedure TProtSql.OpenDatabase(const AppID, Url: String);
var
  Packet: IProtocolPacket;
begin
  FDefaultAppID := AppID;
  Packet := CreatePacket(PS_OPENDATABASE);
  Packet.Data.WriteString(AppID);
  Packet.Data.WriteString(Url);
  Send(Packet, 'TProtSql.OpenDatabase:Send');

  Packet := WaitReply(PS_OPENDATABASE_REPLY, FTimeout);
  FReady := true;
end;

procedure TProtSql.FireOpenDatabase(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlServerEvent;
  Packet: IProtocolPacket;
begin
  FRemoteBuild := Msg.Packet.ProtoVer;

  if HasConnections then 
  begin
    Event.Sender := Self;
    Event.AppID := Msg.Packet.Data.ReadString;
    Event.Info := ParseDatabaseUrl(Msg.Packet.Data.ReadString);
    Event.ResultCode := 0;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnOpenDatabase(Event);
  end;

  Packet := CreatePacket(PS_OPENDATABASE_REPLY);
  Packet.Data.WriteInteger(Event.ResultCode);
  Send(Packet, 'TProtSql.FireOpenDatabase:Send');
end;

procedure TProtSql.CloseDatabase;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_CLOSEDATABASE);
  Send(Packet, 'TProtSql.CloseDatabase:Send');
  WaitReply(PS_CLOSEDATABASE_REPLY, FTimeout);
end;

procedure TProtSql.FireCloseDatabase(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlCloseEvent;
  Packet: IProtocolPacket;
begin
  {if FDbSession.ClientCount > 0 then
  begin
    FDbSession.ClientCount := FDbSession.ClientCount - 1;
    if FDbSession.ClientCount > 0 then Exit;
  end;}

  if HasConnections then
  begin
    Event.Sender := Self;
    Event.ResultCode := 0;
    
    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnCloseDatabase(Event);
  end;

  Packet := CreatePacket(PS_CLOSEDATABASE_REPLY);
  Packet.Data.WriteInteger(Event.ResultCode);
  Send(Packet, 'TProtSql.FireCloseDatabase:Send');
end;

function TProtSql.Execute(const Text: String): LongWord;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_EXECUTE);
  Packet.Data.WriteString(Text);
  Result := 0;

  if FReady then
  begin
    Send(Packet, 'TProtSql.Execute:Send');
    Packet := WaitReply(PS_EXECUTE_REPLY, FTimeout);
    if Packet <> nil then Result := Packet.Data.ReadLongWord;
  end else
  begin
    QueuePacket(Packet);
  end;
end;

procedure TProtSql.FireExecute(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlExecuteEvent;
  Packet: IProtocolPacket;
begin
  if HasConnections then 
  begin
    Event.Sender := Self;
    Event.QueryStr := Msg.Packet.Data.ReadString;
    Event.Records := 0;
    Event.ResultCode := 0;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnExecute(Event);
  end;

  Packet := CreatePacket(PS_EXECUTE_REPLY);
  Packet.Data.WriteLongWord(Event.Records);
  Packet.Data.WriteLongWord(Event.ResultCode);

  Send(Packet, 'TProtSql.FireExecute');
end;

type
  TOldFieldType = (old_Unknown, old_String, old_Smallint, old_Integer, old_Word,
    old_Boolean, old_Float, old_Currency, old_BCD, old_Date, old_Time, old_DateTime,
    old_Bytes, old_VarBytes, old_AutoInc, old_Blob, old_Memo, old_Graphic, old_FmtMemo,
    old_ParadoxOle, old_DBaseOle, old_TypedBinary, old_Cursor, old_FixedChar, old_WideString,
    old_Largeint, old_ADT, old_Array, old_Reference, old_DataSet, old_OraBlob, old_OraClob,
    old_Variant, old_Interface, old_IDispatch, old_Guid);

function ToOldFieldType(DataType: TDataFieldType): TOldFieldType;
begin
  case DataType of
    ftString:         Result := old_String;
    ftSmallInt:       Result := old_SmallInt;
    ftInteger:        Result := old_Integer;
    ftWord:           Result := old_Word;
    ftBoolean:        Result := old_Boolean;
    ftFloat:          Result := old_Float;
    ftCurrency:       Result := old_Currency;
    ftDate:           Result := old_Date;
    ftTime:           Result := old_Time;
    ftDateTime:       Result := old_DateTime;
    ftAutoInc:        Result := old_AutoInc;
    else              Result := old_Unknown;
  end;
end;

function FromOldFieldType(DataType: TOldFieldType): TDataFieldType;
begin
  case DataType of
    old_String:       Result := ftString;
    old_SmallInt:     Result := ftSmallInt;
    old_Integer:      Result := ftInteger;
    old_Word:         Result := ftWord;
    old_Boolean:      Result := ftBoolean;
    old_Float:        Result := ftFloat;
    old_Currency:     Result := ftCurrency;
    old_Date:         Result := ftDate;
    old_Time:         Result := ftTime;
    old_DateTime:     Result := ftDateTime;
    old_AutoInc:      Result := ftAutoInc;
    else              Result := ftUnknown;
  end;
end;

(*function TProtSql.Query(const Text: String): IDataRowset;
var
  Packet: IProtocolPacket;
  RowsetDef: IDataRowsetDef;
  i1, i2, lwCount, lwSize, lwField: LongWord;
  sName: String;
  DataType: TDataFieldType;
  Field: IFieldAccess;
  Item: TCacheItem;
  Cache: ACacheItems;
  FFirst: Boolean;
begin
  Result := nil;
  if not FReady then Exit;

  Packet := CreatePacket(PS_QUERY);
  Packet.Data.WriteString(Text);
  Send(Packet, 'TProtSql.Query:Send');

  Packet := WaitReply(PS_QUERY_REPLY, FTimeout);
  if Packet = nil then Exit;

  RowsetDef := Sil.Sv.DataRowset.Memory;
  Result := RowsetDef.Rowset;

  lwField := Packet.Data.ReadLongWord;
  if lwField = 0 then Exit;
  SetLength(Cache, lwField);

  for i1 := 1 to lwField do
  begin
    sName := Packet.Data.ReadString;
    if FRemoteBuild = 1 then
      DataType := FromOldFieldType(TOldFieldType(Packet.Data.ReadLongWord)) else
      DataType := TDataFieldType(Packet.Data.ReadLongWord);
    lwSize := Packet.Data.ReadLongWord;
    RowsetDef.Fields.CreateItem(sName, DataType, lwSize);
  end;

  RowsetDef.Build;
  lwCount := Packet.Data.ReadLongWord;
  FFirst := true;

  for i1 := 1 to lwCount do
  begin
    Result.Append;

    for i2 := 0 to lwField - 1 do
    begin
      Field := Result.Fields.Items[i2];

      case Field.DataType of
        ftSmallInt,
        ftInteger,
        ftWord,
        ftAutoInc:
        begin
          if FFirst or ((Packet.Data.ReadValueType <> tiByte) or (Packet.Data.ReadByte <> 0)) then
          begin
            Item.VInteger := Packet.Data.ReadInteger;
            Cache[i2].VInteger := Item.VInteger;
            Field.AsInteger := Item.VInteger;
          end else
            Field.AsInteger := Cache[i2].VInteger;
        end;

        ftString:
        begin
          if FFirst or ((Packet.Data.ReadValueType <> tiByte) or (Packet.Data.ReadByte <> 0)) then
          begin
            Item.VString := Packet.Data.ReadString;
            Cache[i2].VString := Item.VString;
            Field.AsString := Item.VString;
          end else
            Field.AsString := Cache[i2].VString;
        end;

        ftFloat,
        ftCurrency,
        ftDate,
        ftTime,
        ftDateTime:
        begin
          if FFirst or ((Packet.Data.ReadValueType <> tiByte) or (Packet.Data.ReadByte <> 0)) then
          begin
            Item.VFloat := Packet.Data.ReadFloat;
            Cache[i2].VFloat := Item.VFloat;
            Field.AsFloat := Item.VFloat;
          end else
            Field.AsFloat := Cache[i2].VFloat;
        end;

        ftBoolean:
        begin
          if FFirst or ((Packet.Data.ReadValueType <> tiByte) or (Packet.Data.ReadByte <> 0)) then
          begin
            Item.VBoolean := Packet.Data.ReadBoolean;
            Cache[i2].VBoolean := Item.VBoolean;
            Field.AsBoolean := Item.VBoolean;
          end else
            Field.AsBoolean := Cache[i2].VBoolean;
        end;
      end;
    end;

    Result.Post;
    if FFirst then FFirst := false;
  end;

  Result.First;
end;*)

function TProtSql.Query(const Text: String): IDataRowset;
var
  Packet: IProtocolPacket;
  RowsetDef: IDataRowsetDef;
  PBuf: PChar;
  Stream: IRandomStream;
  dwSize: LongWord;
begin
  Result := nil;
  if not FReady then Exit;

  Packet := CreatePacket(PS_QUERY2);
  Packet.Data.WriteString(Text);
  Send(Packet, 'TProtSql.Query:Send');

  Packet := WaitReply(PS_QUERY_REPLY2, FTimeout);
  if Packet = nil then Exit;

  PBuf := Packet.Data.ReadPChar(dwSize);
  Stream := Sil.Tk.MemoryStream(PBuf, dwSize);
  RowsetDef := SilData.Tk.Stream(Stream).RowsetDef;
  Result := RowsetDef.Rowset;
  Result.Open;
end;

procedure TProtSql.DoGetStream(const Source: IDataRowset; var Buffer: String);
var
  Stream: IRandomStream;
  i: Integer;
  Fields1, Fields2: IDataFieldList;
  Field: IFieldAccess;
  RowsetDef: IDataRowsetDef;
  Rowset: IDataRowset;
begin
  Stream := Sil.Tk.MemoryStream;
  RowsetDef := SilData.Tk.Stream(Stream).RowsetDef;
  Fields1 := Source.Fields;

  for i := 0 to Fields1.Count - 1 do
  begin
    Field := Fields1.Items[i];
    RowsetDef.Fields.CreateItem(Field.Name, Field.DataType, Field.Size);
  end;

  RowsetDef.Build;
  Rowset := RowsetDef.Rowset;
  Fields2 := Rowset.Fields;

  while not Source.IsEOF do
  begin
    Rowset.Append;

    for i := 0 to Fields1.Count - 1 do
    begin
      Field := Fields1.Items[i];

      case Field.DataType of
        ftSmallint,
        ftInteger,
        ftWord,
        ftAutoInc:    Fields2.Items[i].AsInteger := Field.AsInteger;
        ftString:     Fields2.Items[i].AsString := Field.AsString;
        ftFloat,
        ftCurrency,
        ftDate,
        ftTime,
        ftDateTime:   Fields2.Items[i].AsFloat := Field.AsFloat;
        ftBoolean:    Fields2.Items[i].AsBoolean := Field.AsBoolean;
      end;
    end;

    Source.Next;
    Rowset.Post;
  end;

  Stream.Position := 0;
  SetLength(Buffer, Stream.Size);
  Stream.Read(Buffer[1], Length(Buffer));
end;

procedure TProtSql.FireQuery2(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlQueryEvent;
  Packet: IProtocolPacket;
  sBuffer: String;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.QueryStr := Msg.Packet.Data.ReadString;
    Event.ResultCode := 0;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnQuery(Event);
  end;

  DoGetStream(Event.Rowset, sBuffer);
  Packet := CreatePacket(PS_QUERY_REPLY2);
  Packet.Data.WriteBuffer(sBuffer[1], Length(sBuffer));
  Send(Packet, 'TProtSql.FireQuery2');
end;

procedure TProtSql.FireQuery(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlQueryEvent;
  Packet: IProtocolPacket;
  i: Integer;
  Fields: IDataFieldList;
  Field: IFieldAccess;
  Item: TCacheItem;
  Cache: ACacheItems;
  FFirst: Boolean;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.QueryStr := Msg.Packet.Data.ReadString;
    Event.ResultCode := 0;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnQuery(Event);
  end;

  Packet := CreatePacket(PS_QUERY_REPLY);
  Fields := Event.Rowset.Fields;
  Packet.Data.WriteLongWord(Fields.Count);

  for i := 0 to Fields.Count - 1 do
  begin
    Field := Fields.Items[i];
    Packet.Data.WriteString(Field.Name);

    if FRemoteBuild = 1 then
      Packet.Data.WriteLongWord(Ord(ToOldFieldType(Field.DataType))) else
      Packet.Data.WriteLongWord(Ord(Field.DataType));

    Packet.Data.WriteLongWord(Field.Size);
  end;

  Packet.Data.WriteLongWord(Event.Rowset.RecordCount);
  SetLength(Cache, Fields.Count);
  FFirst := true;

  while not Event.Rowset.IsEOF do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      Field := Fields.Items[i];

      case Field.DataType of
        ftSmallint,
        ftInteger,
        ftWord,
        ftAutoInc:
        begin
          Item.VInteger := Field.AsInteger;
          if FFirst or (Cache[i].VInteger <> Item.VInteger) then
          begin
            Packet.Data.WriteInteger(Item.VInteger);
            Cache[i].VInteger := Item.VInteger;
          end else
            Packet.Data.WriteByte(0);
        end;

        ftString:
        begin
          Item.VString := Field.AsString;
          if FFirst or (Cache[i].VString <> Item.VString) then
          begin
            Packet.Data.WriteString(Item.VString);
            Cache[i].VString := Item.VString;
          end else
            Packet.Data.WriteByte(0);
        end;

        ftFloat,
        ftCurrency,
        ftDate,
        ftTime,
        ftDateTime:
        begin
          Item.VFloat := Field.AsFloat;
          if FFirst or (Cache[i].VFloat <> Item.VFloat) then
          begin
            Packet.Data.WriteFloat(Item.VFloat);
            Cache[i].VFloat := Item.VFloat;
          end else
            Packet.Data.WriteByte(0);
        end;

        ftBoolean:
        begin
          Item.VBoolean := Field.AsBoolean;
          if FFirst or (Cache[i].VBoolean <> Item.VBoolean) then
          begin
            Packet.Data.WriteBoolean(Item.VBoolean);
            Cache[i].VBoolean := Item.VBoolean;
          end else
            Packet.Data.WriteByte(0);
        end;
      end;
    end;

    Event.Rowset.Next;
    if FFirst then FFirst := false;
  end;

  Send(Packet, 'TProtSql.FireQuery');
end;

procedure TProtSql.FireGreetings(var Msg: TProtocolBaseMessage);
begin
  FGreetings := Msg.Packet.Data.ReadString;
  FRemoteBuild := Msg.Packet.ProtoVer;
end;

function TProtSql.StoredProc(const Text: String; var Params: IValueList; WithRecords: Boolean): IDataRowset;
var
  Packet: IProtocolPacket;
  RowsetDef: IDataRowsetDef;
  PBuf: PChar;
  Stream: IRandomStream;
  dwSize: LongWord;
begin
  Result := nil;
  if not FReady then Exit;

  Packet := CreatePacket(PS_STOREDPROC);
  Packet.Data.WriteString(Text);
  DoWriteFields(Packet, Params);
  Packet.Data.WriteBoolean(WithRecords);
  Send(Packet, 'TProtSql.StoredPrco:Send');

  Packet := WaitReply(PS_STOREDPROC_REPLY, FTimeout);
  if Packet = nil then Exit;

  DoReadFields(Packet, Params);

  if WithRecords then
  begin
    PBuf := Packet.Data.ReadPChar(dwSize);
    Stream := Sil.Tk.MemoryStream(PBuf, dwSize);
    RowsetDef := SilData.Tk.Stream(Stream).RowsetDef;
    Result := RowsetDef.Rowset;
    Result.Open;
  end;
end;

procedure TProtSql.FireStoredProc(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlStoredProcEvent;
  Packet: IProtocolPacket;
  sBuffer: String;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.ProcName := Msg.Packet.Data.ReadString;
    Event.ResultCode := 0;
    DoReadFields(Msg.Packet, Event.Params);
    Event.WithRecords := Msg.Packet.Data.ReadBoolean;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnStoredProc(Event);
  end;

  Packet := CreatePacket(PS_STOREDPROC_REPLY);
  DoWriteFields(Packet, Event.Params);

  if Event.WithRecords then
  begin
    DoGetStream(Event.Rowset, sBuffer);
    Packet.Data.WriteBuffer(sBuffer[1], Length(sBuffer));
  end;

  Send(Packet, 'TProtSql.FireStoredProc');
end;

function TProtSql.CreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := inherited CreatePacket(DataID);

  Result.ProtoVer := PROT_BUILD;
  Result.HeaderVer := 1;
  Result.SessionID := 0;
end;

class function TProtSql.ParseDatabaseUrl(Url: String): RDbConnectionInfo;
var
  i: Integer;
  sParams, sItem: String;

  function FromBegin(const Sep: String): String;
  var
    i: Integer;
  begin
    i := Str.Pos(Sep, Url);
    if i > 0 then
    begin
      Result := Str.Extract(Url, 1, i - 1);
      Str.Delete(Url, 1, Length(Sep));
    end else
      Result := '';
  end;

  function FromEnd(const Sep: String): String;
  var
    i: Integer;
  begin
    i := Str.Pos(Sep, Url);
    if i > 0 then
    begin
      Result := Str.Copy(Url, i + 1);
      Str.Delete(Url, i);
    end else
      Result := '';
  end;

begin
  if Str.Pos('%', Url) > 0 then
  begin
    Str.Replace(Url, '%', '=');
    Url := SilCoder.Mime.ISO88591Coder.DecodeLine(Url);
  end;

  sParams := FromEnd('?');
  Result.Driver := FromBegin('://');
  Result.User := FromBegin('@');
  Result.Password := FromEnd(':');
  Result.Server := Url;

  repeat
    sItem := Str.Token(sParams, '&', i);
    if Length(sItem) > 0 then Str.ArrayAdd(Result.Params, sItem);
  until i = 0;
end;

class function TProtSql.FindParam(const List: TStringArray; const Find: String): String;
var
  i: Integer;
begin
  for i := 0 to High(List) do
    if Sil.Text.Compare(List[i], Find) = 0 then
    begin
      Result := Str.Copy(List[i], Length(Find) + 1);
      Exit;
    end;
  Result := '';
end;

class function TProtSql.DefaultPort: LongWord;
begin
  Result := 43363;
end;

procedure TProtSql.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  FReady := false;
  FSession := 0;
  FGreetings := Str.Null;
  inherited;
end;

procedure TProtSql.DoWriteFields(const Packet: IProtocolPacket; const Fields: IValueList);
var
  e: IEnumerator;
  Item: IFieldAccess;
begin
  Packet.Data.WriteLongWord(Fields.Count);

  while Fields.Enumerate(e, Item) do
  begin
    Packet.Data.WriteString(Item.Name);
    Packet.Data.WriteWord(Ord(Item.DataType));
    Packet.Data.WriteLongWord(Item.Size);

    case Item.DataType of
      ftString:     Packet.Data.WriteString(Item.AsString);
      ftSmallInt,
      ftAutoInc,
      ftInteger:    Packet.Data.WriteInteger(Item.AsInteger);
      ftWord:       Packet.Data.WriteLongWord(Item.AsLongWord);
      ftBoolean:    Packet.Data.WriteBoolean(Item.AsBoolean);
      ftFloat:      Packet.Data.WriteFloat(Item.AsFloat);
      ftCurrency:   Packet.Data.WriteFloat(Item.AsCurrency);
      ftDate,
      ftTime,
      ftDateTime:   Packet.Data.WriteFloat(Item.AsDateTime);
      else          Packet.Data.WriteString(Item.GetValue);
    end;
  end;
end;

procedure TProtSql.DoReadFields(const Packet: IProtocolPacket; out Fields: IValueList);
var
  lwCount, lwSize: LongWord;
  sName: String;
  DataType: TDataFieldType;
  Item: IFieldAccess;
begin
  Fields := Sil.List.ValueList(true);
  lwCount := Packet.Data.ReadLongWord;

  while lwCount > 0 do
  begin
    Dec(lwCount);
    sName := Packet.Data.ReadString;
    DataType := TDataFieldType(Packet.Data.ReadWord);
    lwSize := Packet.Data.ReadLongWord;
    Item := Fields.New(sName, DataType, lwSize);

    case DataType of
      ftString:     Item.AsString := Packet.Data.ReadString;
      ftSmallInt,
      ftAutoInc,
      ftInteger:    Item.AsInteger := Packet.Data.ReadInteger;
      ftWord:       Item.AsLongWord := Packet.Data.ReadLongWord;
      ftBoolean:    Item.AsBoolean := Packet.Data.ReadBoolean;
      ftFloat:      Item.AsFloat := Packet.Data.ReadFloat;
      ftCurrency:   Item.AsCurrency := Packet.Data.ReadFloat;
      ftDate,
      ftTime,
      ftDateTime:   Item.AsDateTime := Packet.Data.ReadFloat;
      else          Item.SetValue(Packet.Data.ReadString);
    end;
  end;
end;

function TProtSql.QueryFields(const Command: String; out Fields: IValueList): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_QUERYFIELDS);
  Packet.Data.WriteString(Command);
  DoWriteFields(Packet, Fields);
  Send(Packet, 'TProtSql.QueryFields:Send');

  Packet := WaitReply(PS_QUERYFIELDS_REPLY, FTimeout);
  Result := Packet <> nil;
  if Result then DoReadFields(Packet, Fields);
end;

procedure TProtSql.FireQueryFields(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlQueryFieldsEvent;
  Packet: IProtocolPacket;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Command := Msg.Packet.Data.ReadString;
    DoReadFields(Msg.Packet, Event.Fields);

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnQueryFields(Event);
  end;

  Packet := CreatePacket(PS_QUERYFIELDS_REPLY);
  DoWriteFields(Packet, Event.Fields);

  Send(Packet, 'TProtSql.FireQueryFields');
end;

function TProtSql.QueryStoredProcParams(const ProcName: String; out Params: IValueList): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(PS_QUERYPARAMS);
  Packet.Data.WriteString(ProcName);
  Send(Packet, 'TProtSql.QueryStoredProcParams:Send');

  Packet := WaitReply(PS_QUERYPARAMS_REPLY, FTimeout);
  Result := Packet <> nil;
  if Result then DoReadFields(Packet, Params);
end;

procedure TProtSql.FireQueryStoredProcParams(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IProtSqlServerEvents;
  Event: TProtSqlQueryFieldsEvent;
  Packet: IProtocolPacket;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Command := Msg.Packet.Data.ReadString;

    while Events.Enumerate(n, Adapter, IProtSqlServerEvents) do
      Adapter.OnQueryStoredProcParams(Event);
  end;

  if Event.Fields = nil then Event.Fields := Sil.List.ValueList(true);
  Packet := CreatePacket(PS_QUERYPARAMS_REPLY);
  DoWriteFields(Packet, Event.Fields);

  Send(Packet, 'TProtSql.FireQueryStoredProcParams');
end;

end.
