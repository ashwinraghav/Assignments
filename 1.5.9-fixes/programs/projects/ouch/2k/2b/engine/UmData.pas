unit UmData;

interface

uses
  Sil,
  SilData,

  UiProtocol,
  UiClient;

type
  TOpenProc = procedure(const TableDef: IDataRowsetDef) of object;

  TData = class (
    TSilObject,
    IData)
  private
    FPath: String;
    FLocal: IDataRowset;
    FUser: IDataRowset;
    FHistory: IDataRowset;
    FLock: ICriticalSection;
  private
    function DoOpenTable(const Name: String; BuildProc: TOpenProc): IDataRowset;
    procedure DoOpenHistory(const TableDef: IDataRowsetDef);
    procedure DoOpenUser(const TableDef: IDataRowsetDef);
    procedure DoOpenLocal(const TableDef: IDataRowsetDef);
  protected // IData
    procedure Initialize(const Path: String);
    procedure Finalize;
    function FindAccount(const Id: TGuid; out Info: IParameterList): Boolean;
    function QueryUser(const Id: TGuid; out Info: IParameterList): Boolean;
    function AccountExists(const Id: TGuid; const Password: String): Boolean;
    procedure CreateLocalAccount(const Id: TGuid; const Nick, Password: String);
    procedure UpdateUser(const Id: TGuid; const Info: IParameters);
    procedure AppendHistory(const MsgId: TGUID; const SndTime, RcvTime: TDateTime; const FromId, ToId: TGuid; const Kind, Status, Text: String);
    function OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
  public
    constructor CreateNew(const Owner: IUnknown; Param: Pointer); override; 
    destructor Destroy; override;
  end;

implementation

{ TData }

constructor TData.CreateNew(const Owner: IUnknown; Param: Pointer);   
begin
  inherited;
  FLock := Sil.OS.Ipc.CriticalSection;

  FPath := Sil.OS.Process.Current.Info.Path;
  FLocal := DoOpenTable('local.d', DoOpenLocal);
end;

destructor TData.Destroy;
begin
  FLocal := nil;
  inherited;
end;

procedure TData.Initialize(const Path: String);
begin
  Sil.Trace.Enter('TData.Initialize');

  FPath := Sil.OS.FileSystem.AddSlash(Path);
  Sil.OS.FileSystem.ForceDirectories(Path);

  FUser := DoOpenTable('user.d', DoOpenUser);
  FHistory := DoOpenTable('history.d', DoOpenHistory);

  Sil.Trace.Leave;
end;

procedure TData.Finalize;
begin
  Sil.Trace.Enter('TData.Finalize');

  FUser := nil;
  FHistory := nil;

  Sil.Trace.Leave;
end;

function TData.DoOpenTable(const Name: String; BuildProc: TOpenProc): IDataRowset;
var
  TableDef: IDataRowsetDef;
  FileName: String;
begin
  FileName := FPath + Name;
  Sil.Trace.Enter('TData.DoOpenTable', [FileName]);

  if not Sil.OS.FileSystem.Exists(FileName) then
  begin
    TableDef := SilData.Tk.CreateFile(FileName, fmAccessReadWrite, fmShareReadWrite);
    BuildProc(TableDef);
    TableDef.Build(Name);
    Result := TableDef.Rowset;
  end else
    Result := SilData.Tk.OpenFile(FileName, fmAccessReadWrite, fmShareReadWrite);

  Sil.Trace.Leave;
end;

procedure TData.DoOpenLocal(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('id', ftGuid);
    CreateItem('nick', ftString, 30);
    CreateItem('password', ftString, 30);
    CreateItem('mail', ftString, 3);
  end;

  with TableDef.Indexes do
  begin
    CreateItem('id', 'id');
  end;
end;

procedure TData.DoOpenUser(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('id', ftGuid);
    CreateItem('nick', ftString, 30);
    CreateItem('mail', ftString, 3);
    CreateItem('updated', ftDateTime);
    CreateItem('lastip', ftLongWord);
  end;

  with TableDef.Indexes do
  begin
    CreateItem('id', 'id');
    CreateItem('nick', 'nick');
  end;
end;

procedure TData.DoOpenHistory(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('msgid', ftGuid);
    CreateItem('sndtime', ftDateTime);
    CreateItem('rcvtime', ftDateTime);
    CreateItem('from', ftGuid);
    CreateItem('to', ftGuid);
    CreateItem('kind', ftString, 2);
    CreateItem('status', ftString, 2);
    CreateItem('text', ftMemo);
  end;

  with TableDef.Indexes do
  begin
    //CreateItem('msgid', 'msgid');
    //CreateItem('msgfrom', 'msgid,from');
    CreateItem('msgto', 'msgid,to');
    CreateItem('from', 'from,sndtime');
    CreateItem('to', 'to,sndtime');
  end;
end;

function TData.FindAccount(const Id: TGuid; out Info: IParameterList): Boolean;
var
  Values: IDataRowsetFind;
  List: IParameterList;
begin
  Sil.Trace.Enter('TData.FindAccount', [Guid.ToStr(Id)]);

  FLock.Locked;
  FLocal.ActiveIndexName := 'id';

  Values := FLocal.FindValues;
  Values.Add(Id);
  Result := Values.Find;

  if Result then
  begin
    List := Sil.List.Parameters;
    Info := List;
    List['id'] := FLocal.Fields['id'].AsString;
    List['nick'] := FLocal.Fields['nick'].AsString;
    List['mail'] := FLocal.Fields['mail'].AsString;
    List['password'] := FLocal.Fields['password'].AsString;
  end;

  Sil.Trace.Leave;
end;

function TData.AccountExists(const Id: TGuid; const Password: String): Boolean;
var
  Info: IParameterList;
begin
  FLock.Locked;
  Result := FindAccount(Id, Info);

  if Result and (Info['password'] <> Password) then
  begin
    FLocal.Edit;
    FLocal.Fields['password'].AsString := Password;
    FLocal.Post;
  end;
end;

procedure TData.CreateLocalAccount(const Id: TGuid; const Nick, Password: String);
begin
  Sil.Trace.Enter('TData.CreateAccount', [Guid.ToStr(Id)]);

  FLock.Locked;

  FLocal.Append;
  FLocal.Fields['id'].AsGuid := Id;
  FLocal.Fields['nick'].AsString := Nick;
  FLocal.Fields['password'].AsString := Password;
  FLocal.Post;

  Sil.Trace.Leave;
end;

function TData.QueryUser(const Id: TGuid; out Info: IParameterList): Boolean;
var
  Values: IDataRowsetFind;
  List: IParameterList;
begin
  Sil.Trace.Enter('TData.QueryUser', [Guid.ToStr(Id)]);

  FLock.Locked;
  FUser.ActiveIndexName := 'id';

  Values := FUser.FindValues;
  Values.Add(Id);
  Result := Values.Find;

  if Result then
  begin
    List := Sil.List.Parameters;
    Info := List;

    List['id'] := FUser.Fields['id'].AsString;
    List['nick'] := FUser.Fields['nick'].AsString;
    List['mail'] := FUser.Fields['mail'].AsString;
    List['updated'] := FUser.Fields['updated'].AsDateTime;
    List['ipaddress'] := FUser.Fields['lastip'].AsLongWord;
  end;

  Sil.Trace.Leave;
end;

procedure TData.UpdateUser(const Id: TGuid; const Info: IParameters);
var
  Values: IDataRowsetFind;
begin
  Sil.Trace.Enter('TData.UpdateUser', [Guid.ToStr(Id)]);

  FLock.Locked;
  FUser.ActiveIndexName := 'id';

  Values := FUser.FindValues;
  Values.Add(Id);

  if Values.Find then
    FUser.Edit else
    FUser.Append;

  FUser.Fields['id'].AsGuid := Id;
  FUser.Fields['nick'].AsString := Info['nick'];
  FUser.Fields['mail'].AsString := Info['mail'];
  FUser.Fields['updated'].AsDateTime := Info['updated'];
  FUser.Fields['lastip'].AsLongWord := Info['ipaddress'];
  FUser.Post;

  Sil.Trace.Leave;
end;

procedure TData.AppendHistory(const MsgId: TGUID; const SndTime, RcvTime: TDateTime; const FromId, ToId: TGuid; const Kind, Status, Text: String);
var
  RecordNo: LongWord;
  ActiveIndex: String;
begin
  Sil.Trace.Enter('TData.AppendHistory', [Guid.ToStr(FromId)]);

  FLock.Locked;

  ActiveIndex := FHistory.ActiveIndexName;
  RecordNo := FHistory.CurrentRecord;

  try
    FHistory.Append;
    FHistory.Fields['msgid'].AsGuid := MsgId;
    FHistory.Fields['sndtime'].AsDateTime := SndTime;
    FHistory.Fields['rcvtime'].AsDateTime := RcvTime;
    FHistory.Fields['from'].AsGuid := FromId;
    FHistory.Fields['to'].AsGuid := ToId;
    FHistory.Fields['kind'].AsString := Kind;
    FHistory.Fields['status'].AsString := Status;
    FHistory.Fields['text'].AsString := Text;
    FHistory.Post;
  finally
    FHistory.ActiveIndexName := ActiveIndex;
    FHistory.CurrentRecord := RecordNo;
  end;

  Sil.Trace.Leave;
end;

function TData.OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
var
  Values: IDataRowsetFind;
  Cond: String;
begin
  FLock.Locked;

  FHistory.ActiveIndexName := 'from';

  Values := FHistory.FindValues;
  Values.Add(User);
  Values.Find;

  Result := Sil.GUID.IsEqual(FHistory.Fields['from'].AsGuid, User);

  if Result then
  begin
    Cond := Str.Format('from == "%s"', [Guid.ToStr(User)]);
    Table := SilData.Tk.Filter(FHistory, Cond, 'from');
  end;
end;

end.
