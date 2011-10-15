unit UmData;

interface

uses
  Sil,
  SilData,

  UiProtocol,
  UiServer;

type
  TOpenProc = procedure(const TableDef: IDataRowsetDef) of object;

  TData = class (
    TSilObject,
    IData)
  private
    FPath: String;
    FUser: IDataRowset;
    FOffline: IDataRowset;
    FLock: ICriticalSection;
    function DoAddOffline: Integer;
  private
    function DoOpenTable(const Name: String; BuildProc: TOpenProc): IDataRowset;
    procedure DoOpenOffline(const TableDef: IDataRowsetDef);
    procedure DoOpenUser(const TableDef: IDataRowsetDef);
    function DoReadStatus(const Status: String): TUserStatus;
  protected // IData
    function GetPath: String;
    procedure SetupAccount(const Nick, Password: String; var Id: TGuid; CreateAccount: Boolean);
    procedure LogonAccount(const Id: TGuid; const Password: String; IPAddress: LongWord);
    procedure LogoffAccount(const Id: TGuid);
    procedure QueryInfo(const Id: TGuid; out Data: IParameters);
    procedure QueryUsers(const Id: TGuid; Status: TUserStatus; out Users: TUserArray);
    procedure StoreMessage(const MsgId: TGUID; const UserId, FromId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure StoreResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
    function GetMessage(const Id: TGuid; out Data: IParameters): Boolean;
    procedure DeleteOffline(Id: LongWord);
    procedure Initialize;
    procedure Finalize;
    property Path: String read GetPath;
  public
    constructor Create(const Path: String);
    destructor Destroy; override;
  end;

implementation

uses
  UtProtocol, SilLtList, SilLiFiler;

{ TData }

constructor TData.Create(const Path: String);
begin
  inherited Create;
  FPath := Sil.OS.FileSystem.AddSlash(Path);
  FLock := Sil.OS.Ipc.CriticalSection;
end;

destructor TData.Destroy;
begin

  inherited;
end;

procedure TData.Initialize;
begin
  Sil.Trace.Enter('TData.Initialize');

  FUser := DoOpenTable('user.d', DoOpenUser);
  FOffline := DoOpenTable('offline.d', DoOpenOffline);

  Sil.Trace.Leave;
end;

procedure TData.Finalize;
begin
  Sil.Trace.Enter('TData.Finalize');

  FUser := nil;
  FOffline := nil;

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

procedure TData.DoOpenUser(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('id', ftGuid);
    CreateItem('password', ftString, 30);
    CreateItem('nick', ftString, 30);
    CreateItem('status', ftString, 3);
    CreateItem('updated', ftDateTime);
    CreateItem('ip', ftLongWord);
  end;

  with TableDef.Indexes do
  begin
    CreateItem('id', 'id');
    CreateItem('nick', 'nick');
    CreateItem('status', 'status');
  end;
end;

procedure TData.DoOpenOffline(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('msgid', ftGuid);
    CreateItem('kind', ftString, 3);
    CreateItem('from', ftGuid);
    CreateItem('to', ftGuid);
    CreateItem('time', ftDateTime);
    CreateItem('text', ftMemo);
  end;

  with TableDef.Indexes do
  begin
    CreateItem('msgto', 'msgid,to');
    CreateItem('msgfrom', 'msgid,from');
    CreateItem('to', 'to');
  end;
end;

procedure TData.SetupAccount(const Nick, Password: String; var Id: TGuid; CreateAccount: Boolean);
var
  Values: IDataRowsetFind;
  Lastc: Char;
  OptNick: String;
begin
  Sil.Trace.Enter('TData.SetupAccount', [Nick]);

  FLock.Locked;
  FUser.ActiveIndexName := 'nick';

  Values := FUser.FindValues;
  Values.Add(Nick);

  if Values.Find then
  begin
    if CreateAccount then
    begin
      Lastc := Str.LastChar(Nick);

      if Lastc in ['0'..'9'] then
        OptNick := Str.Copy(Nick, 1, -2) + Char(Byte(Lastc) + 1) else
        OptNick := Nick + '1';

      raise Sil.Error.Create('el nick ya fue utilizado. opcion: "%s"', [OptNick]);
    end else
    if FUser.Fields['password'].AsString <> Password then
      raise Sil.Error.Create('usuario/clave no existe');

    Id := FUser.Fields['id'].AsGuid;
  end else
  if CreateAccount then
  begin
    Id := Guid.Create;

    FUser.Append;
    FUser.Fields['id'].AsGuid := Id;
    FUser.Fields['password'].AsString := Password;
    FUser.Fields['nick'].AsString := Nick;
    FUser.Fields['status'].AsString := 'off';
    FUser.Fields['updated'].AsDateTime := DateTime.Now;
    FUser.Fields['ip'].AsLongWord := 0;
    FUser.Post;
  end else
    raise Sil.Error.Create('el nick "%s" no existe', [Nick]);

  Sil.Trace.Leave;
end;

procedure TData.LogonAccount(const Id: TGuid; const Password: String; IPAddress: LongWord);
var
  Values: IDataRowsetFind;
begin
  Sil.Trace.Enter('TData.LogonAccount', [Guid.ToStr(Id)]);

  FLock.Locked;
  FUser.ActiveIndexName := 'id';

  Values := FUser.FindValues;
  Values.Add(Id);

  if Values.Find then
  begin
    if FUser.Fields['password'].AsString = Password then
    begin
      FUser.Edit;
      FUser.Fields['status'].AsString := 'on';
      FUser.Fields['ip'].AsLongWord := IPAddress;
      FUser.Post;
    end else
      raise Sil.Error.Create('usuario/clave no existe');
  end else
    raise Sil.Error.Create('el id de usuario no existe');

  Sil.Trace.Leave;
end;

procedure TData.LogoffAccount(const Id: TGuid);
var
  Values: IDataRowsetFind;
begin
  Sil.Trace.Enter('TData.LogoffAccount', [Guid.ToStr(Id)]);

  FLock.Locked;
  FUser.ActiveIndexName := 'id';

  Values := FUser.FindValues;
  Values.Add(Id);

  if Values.Find then
  begin
    FUser.Edit;
    FUser.Fields['status'].AsString := 'off';
    FUser.Fields['ip'].AsLongWord := 0;
    FUser.Post;
  end;

  Sil.Trace.Leave;
end;

function TData.DoReadStatus(const Status: String): TUserStatus;
begin
  if Str.CompareText(Status, 'on', true) = 0 then
    Result := UsOnline else
    Result := UsOffline;
end;

procedure TData.QueryInfo(const Id: TGuid; out Data: IParameters);
var
  Values: IDataRowsetFind;
  Info: IParameterList;
begin
  Sil.Trace.Enter('TData.QueryInfo', [Guid.ToStr(Id)]);

  FLock.Locked;
  FUser.ActiveIndexName := 'id';

  Values := FUser.FindValues;
  Values.Add(Id);

  if Values.Find then
  begin
    Info := Sil.List.Parameters;
    Data := Info;

    Info['id'] := FUser.Fields['id'].AsString;
    Info['nick'] := FUser.Fields['nick'].AsString;
    Info['status'] := DoReadStatus(FUser.Fields['status'].AsString);
    Info['updated'] := FUser.Fields['updated'].AsDateTime;
    Info['ipaddress'] := FUser.Fields['ip'].AsLongWord;
  end else
    raise Sil.Error.Create('el id de usuario no existe');

  Sil.Trace.Leave;
end;

procedure TData.QueryUsers(const Id: TGuid; Status: TUserStatus; out Users: TUserArray);
const
  CStatus: array [TUserStatus] of String = ('', 'on', 'off');
var
  Values: IDataRowsetFind;
  Found: Boolean;
  UserId: TGuid;
begin
  Sil.Trace.Enter('TData.QueryUsers');

  FLock.Locked;
  Found := Status = usAny;

  if not Found then
  begin
    FUser.ActiveIndexName := 'status';

    Values := FUser.FindValues;
    Values.Add(CStatus[Status]);

    Found := Values.Find;
  end else
    FUser.First;

  if Found then
    while not FUser.IsEof and (FUser.Fields['Status'].AsString = CStatus[Status]) do
    begin
      UserId := FUser.Fields['id'].AsGuid;

      if Guid.Compare(Id, UserId) <> 0 then
        Prot.UserAdd(Users, UserId, FUser.Fields['updated'].AsDateTime);

      FUser.Next;
    end;

  Sil.Trace.Leave;
end;

function TData.DoAddOffline: Integer;
begin
  if not FOffline.IsEmpty then
  begin
    FOffline.ActiveIndexName := 'id';
    FOffline.Last;
    Result := FOffline.Fields['id'].AsInteger + 1;
  end else
    Result := 1;
end;

procedure TData.StoreMessage(const MsgId: TGUID; const UserId, FromId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
var
  i: Integer;
  RecList, Buffer: String;
  Fields: IParameterList;
begin
  Sil.Trace.Enter('TData.StoreMessage', [Guid.ToStr(UserId), Guid.ToStr(FromId)]);

  FLock.Locked;
  RecList := '';

  if not Bc then
    for i := 0 to Length(Recipients) - 1 do
      RecList := RecList + Guid.ToStr(Recipients[i]) + ';';

  FOffline.Append;
  FOffline.Fields['msgid'].AsGuid := MsgId;
  FOffline.Fields['kind'].AsString := 'ime';
  FOffline.Fields['from'].AsGuid := FromId;
  FOffline.Fields['to'].AsGuid := UserId;
  FOffline.Fields['time'].AsDateTime := Time;

  Fields := Sil.List.Parameters;
  Fields['recipients'] := RecList;
  Fields['text'] := Text;
  Sil.Serializer.SaveToString(Fields, Buffer);

  FOffline.Fields['text'].AsString := Buffer;
  FOffline.Post;

  Sil.Trace.Leave;
end;

procedure TData.StoreResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
begin
  Sil.Trace.Enter(Self, 'StoreResponse', [Guid.ToStr(MsgId), Guid.ToStr(FromId)]);

  FOffline.Append;
  FOffline.Fields['id'].AsLongWord := DoAddOffline;
  FOffline.Fields['msgid'].AsGuid := MsgId;
  FOffline.Fields['kind'].AsString := 'res';
  FOffline.Fields['from'].AsGuid := FromId;
  FOffline.Fields['to'].AsGuid := ToId;
  FOffline.Fields['time'].AsDateTime := Time;
  FOffline.Fields['text'].AsString := '';
  FOffline.Post;

  Sil.Trace.Leave;
end;

function TData.GetMessage(const Id: TGuid; out Data: IParameters): Boolean;
var
  Values: IDataRowsetFind;
  Msg: IParameterList;
begin
  Sil.Trace.Enter('TData.GetMessage', [Guid.ToStr(Id)]);

  FLock.Locked;
  FOffline.ActiveIndexName := 'to';

  Values := FOffline.FindValues;
  Values.Add(Id);

  Result := Values.Find;

  if Result then
  begin
    Msg := Sil.List.Parameters;
    Data := Msg;

    Msg['time'] := FOffline.Fields['time'].AsDateTime;
    Msg['from'] := FOffline.Fields['from'].AsString;
    Msg['msgid'] := FOffline.Fields['msgid'].AsString;
    Msg['id'] := FOffline.Fields['id'].AsLongWord;
    Msg['kind'] := FOffline.Fields['kind'].AsString;

    if Str.TextCompare(Msg['kind'], 'ime') = 0 then
      Sil.Serializer.LoadFromString(Msg, FOffline.Fields['text'].AsString);
  end;

  Sil.Trace.Leave;
end;

procedure TData.DeleteOffline(Id: LongWord);
var
  Values: IDataRowsetFind;
begin
  Sil.Trace.Enter(Self, 'DeleteOffline', [Id]);

  FLock.Locked;
  FOffline.ActiveIndexName := 'id';

  Values := FOffline.FindValues;
  Values.Add(Id);

  if Values.Find then
  begin
    FOffline.Delete;
    Sil.Trace.Log('ok');
  end;

  Sil.Trace.Leave;
end;

function TData.GetPath: String;
begin
  Result := FPath;
end;

end.
