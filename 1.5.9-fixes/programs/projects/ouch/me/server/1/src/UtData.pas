unit UtData;

interface

uses
  Sil,
  SilData,

  UiOuchProtocol,
  UiData;

type
  TBuildProc = procedure(const TableDef: IDataRowsetDef) of object;

  Db = class (Tool)
    class function CreateMgr: IDataMgr;
    class procedure BuildUser(const TableDef: IDataRowsetDef);
    class procedure BuildData(const TableDef: IDataRowsetDef);
    class procedure BuildOffline(const TableDef: IDataRowsetDef);
    class procedure BuildGroup(const TableDef: IDataRowsetDef);
    class procedure BuildGroupUser(const TableDef: IDataRowsetDef);
    class procedure BuildSession(const TableDef: IDataRowsetDef);
    class function OpenTable(const Name: String; BuildProc: TBuildProc): IDataRowset;
    class function OpenTables(const Mgr: IDataMgr): Boolean;
    // user
    class function AppendUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
    class function ChangeUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
    class function RemoveUser(const Mgr: IDataMgr; const User: TGuid): Boolean;
    class function FindUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
    // data
    class function WriteData(const Mgr: IDataMgr; const Id: TGuid; const Data: IParameters): Boolean;
    class function ReadData(const Mgr: IDataMgr; const Id: TGuid; out Data: IParameters): Boolean;
    // offline
    class function AppendOffline(const Mgr: IDataMgr; const Id, ToUser, From: TGuid; const Time: TDateTime; Kind: TOfflineKind; const Data: String): Boolean;
    class function RemoveOfflineUser(const Mgr: IDataMgr; const ToUser: TGuid): Boolean;
    class function RemoveOfflineId(const Mgr: IDataMgr; const Id, ToUser: TGuid; out Info: ROfflineInfo): Boolean;
    class function CollectOffline(const Mgr: IDataMgr; const User: TGuid; out Info: TOfflineInfoArray): Boolean;
    class function GroupAppendOffline(const Mgr: IDataMgr; const Id, From: TGuid; const Time: TDateTime; Kind: TOfflineKind; const Data: String): Boolean;
    // group
    class function AppendGroup(const Mgr: IDataMgr; const Parent: TGuid; const Name, Password: String; out Group: TGuid): Boolean;
    class function ChangeGroup(const Mgr: IDataMgr; const Group: TGuid; const Name, Password: String): Boolean;
    class function RemoveGroup(const Mgr: IDataMgr; const Group: TGuid): Boolean;
    class function FindGroup(const Mgr: IDataMgr; const Group: TGuid; const Password: String): Boolean;
    class function CollectGroup(const Mgr: IDataMgr; out Groups: TOuchGroupArray): Boolean; overload;
    class function CollectGroup(const Mgr: IDataMgr; const User: TGuid; out Groups: TGuidArray): Boolean; overload;
    // group user
    class function AppendGroupUser(const Mgr: IDataMgr; const Group, User: TGuid; const Password: String): Boolean;
    class function RemoveGroupUser(const Mgr: IDataMgr; const Group, User: TGuid): Boolean; overload;
    class function RemoveGroupUser(const Mgr: IDataMgr; const Group: TGuid): Boolean; overload;
    class function CollectGroupUser(const Mgr: IDataMgr; const Group, UserId: TGuid; out Users: TOuchUserArray): Boolean;
    // session
    class function AppendSession(const Mgr: IDataMgr; const User: TGuid; out Session: TGuid; const Address: String; const Timeout: TDateTime): Boolean;
    class function ChangeSession(const Mgr: IDataMgr; const Session: TGuid; Status: TOuchUserStatus; const Address: String; const Timeout: TDateTime): Boolean;
    class function RemoveSession(const Mgr: IDataMgr; const Session: TGuid): Boolean;
    class function FindSession(const Mgr: IDataMgr; const Session: TGuid; out User: TGuid): Boolean;
    class function FindSessionUser(const Mgr: IDataMgr; const User: TGuid; out Session: TGuid): Boolean;
    class procedure DisconnectSession(const Mgr: IDataMgr; const Time: TDateTime);
  end;

const
  dtUser = 'user.d';
  dtData = 'data.d';
  dtOffline = 'offline.d';
  dtGroup = 'group.d';
  dtGroupUser = 'groupuser.d';
  dtSession = 'session.d';

  ixUser = 'ixUser';

  ixData = 'ixId';
  ixDataName = 'ixName';

  ixOfflineId = 'ixId';
  ixOfflineIdTo = 'ixIdTo';
  ixOfflineTo = 'ixTo';
  ixOfflineTimeout = 'ixTimeout';

  ixGroup = 'ixGroup';
  ixGroupPaNa = 'ixPaNa';

  ixGroupUserGroup = 'ixGroup';
  ixGroupUserUser = 'ixUser';
  ixGroupUserGrUs = 'ixGrUs';

  ixSession = 'ixSession';
  ixSessionUser = 'ixUser';
  ixSessionTimeout = 'ixTimeout';

implementation

uses
  UmData, SilLiFiler, SilOtSocket;

{ Data }

class function Db.CreateMgr: IDataMgr;
begin
  Result := TDataMgr.Create; 
end;

class function Db.OpenTable(const Name: String; BuildProc: TBuildProc): IDataRowset;
var
  TableDef: IDataRowsetDef;
begin
  if not Sil.OS.FileSystem.Exists(Name) then
  begin
    TableDef := SilData.Tk.CreateFile(Name, fmAccessReadWrite, fmShareReadWrite);
    BuildProc(TableDef);
    TableDef.Build(Sil.OS.FileSystem.GetFileName(Name));
    Result := TableDef.Rowset;
  end else
    Result := SilData.Tk.OpenFile(Name, fmAccessReadWrite, fmShareReadWrite);
end;

class function Db.OpenTables(const Mgr: IDataMgr): Boolean;
begin
  Result := true;

  try
    Mgr.User := OpenTable(Mgr.Path + dtUser, BuildUser);
    Mgr.Data := OpenTable(Mgr.Path + dtData, BuildData);
    Mgr.Offline := OpenTable(Mgr.Path + dtOffline, BuildOffline);
    Mgr.Group := OpenTable(Mgr.Path + dtGroup, BuildGroup);
    Mgr.GroupUser := OpenTable(Mgr.Path + dtGroupUser, BuildGroupUser);
    Mgr.Session := OpenTable(Mgr.Path + dtSession, BuildSession);
  except
    Result := false;
  end
end;

class procedure Db.BuildUser(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('User', ftGuid);
    CreateItem('Password', ftString, 30);
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixUser, 'User');
  end;
end;

class procedure Db.BuildData(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('Id', ftGuid);
    CreateItem('Name', ftString, 30);
    CreateItem('Type', ftWord);
    CreateItem('Visibility', ftWord);
    CreateItem('VInteger', ftInteger);
    CreateItem('VFloat', ftFloat);
    CreateItem('VString', ftMemo);
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixData, 'Id');
    CreateItem(ixDataName, 'Id,Name');
  end;
end;

class procedure Db.BuildOffline(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('Id', ftGuid);
    CreateItem('To', ftGuid);
    CreateItem('From', ftGuid);
    CreateItem('Time', ftDateTime);
    CreateItem('Kind', ftByte);
    CreateItem('Data', ftMemo);
    CreateItem('Timeout', ftDateTime);
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixOfflineId, 'Id');
    CreateItem(ixOfflineIdTo, 'Id,To');
    CreateItem(ixOfflineTo, 'To');
    CreateItem(ixOfflineTimeout, 'Timeout');
  end;
end;

class procedure Db.BuildGroup(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('Group', ftGuid);
    CreateItem('Parent', ftGuid);
    CreateItem('Name', ftString, 30);
    CreateItem('Password', ftString, 30);
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixGroup, 'Group');
    CreateItem(ixGroupPaNa, 'Parent,Name');
  end;
end;

class procedure Db.BuildGroupUser(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('Group', ftGuid);
    CreateItem('User', ftGuid);
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixGroupUserGroup, 'Group');
    CreateItem(ixGroupUserUser, 'User');
    CreateItem(ixGroupUserGrUs, 'Group,User');
  end;
end;

class procedure Db.BuildSession(const TableDef: IDataRowsetDef);
begin
  with TableDef.Fields do
  begin
    CreateItem('User', ftGuid);
    CreateItem('Session', ftGuid);
    CreateItem('Status', ftByte);             // estado=offline/online/...
    CreateItem('Address', ftLongWord);
    CreateItem('Timeout', ftDateTime);        // se usa para testear status y uso
  end;

  with TableDef.Indexes do
  begin
    CreateItem(ixSessionUser, 'User');
    CreateItem(ixSession, 'Session');
    CreateItem(ixSessionTimeout, 'Timeout');
  end;
end;

class function Db.WriteData(const Mgr: IDataMgr; const Id: TGuid; const Data: IParameters): Boolean;
var
  Table: IDataRowset;
  e: IEnumerator;
  Item: RParameter;
  wType: Word;
begin
  Table := Mgr.Data;
  Sil.Lock.Take(Table);
  Result := Guid.NotEmpty(Id);

  Table.ActiveIndexName := ixDataName;

  if Result then
    while Data.Enumerate(e, Item) do
    begin
      if not Table.Find([Guid.ToStr(Id), Item.Name], false) then
      begin
        Table.Append;
        Table.Fields['Id'].AsGuid := Id;
        Table.Fields['Name'].AsString := Item.Name;
      end else
        Table.Edit;

      wType := Vart.VType(Item.Value);
      Table.Fields['Type'].AsLongWord := wType;
      Table.Fields['Visibility'].AsLongWord := 0;

      case wType of
        varSmallint,
        varInteger,
        varBoolean,
        varShortInt,
        varByte,
        varWord,
        varLongWord:
          Table.Fields['VInteger'].AsInteger := Item.Value;

        varSingle,
        varDouble,
        varDate:
          Table.Fields['VFloat'].AsFloat := Item.Value;

        varOleStr,
        varString:
          Table.Fields['VString'].AsString := Item.Value;
      end;

      Table.Post;
    end;
end;

class function Db.ReadData(const Mgr: IDataMgr; const Id: TGuid; out Data: IParameters): Boolean;
var
  Table: IDataRowset;
  Params: IParameterList;
begin
  Table := Mgr.Data;
  Sil.Lock.Take(Table);
  Result := Guid.NotEmpty(Id);

  Table.ActiveIndexName := ixData;
  Params := Sil.List.Parameters;
  Data := Params;

  if Result and Table.Find([Guid.ToStr(Id)], false) then
    while not Table.IsEof and (Guid.Compare(Table.Fields['Id'].AsGuid, Id) = 0) do
    begin
      // if Table.Fields['Visibility'] =

      case Table.Fields['Type'].AsLongWord of
        varSmallint,
        varInteger,
        varBoolean,
        varShortInt,
        varByte,
        varWord,
        varLongWord:
          Params[Table.Fields['Name'].AsString] := Table.Fields['VInteger'].AsInteger;

        varSingle,
        varDouble,
        varDate:
          Params[Table.Fields['Name'].AsString] := Table.Fields['VFloat'].AsFloat;

        varOleStr,
        varString:
          Params[Table.Fields['Name'].AsString] := Table.Fields['VString'].AsString;
      end;

      Table.Next;
    end;
end;

class function Db.AppendUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.User;
  Sil.Lock.Take(Table);
  Result := Str.NotEmpty(Password);

  if Result then
  begin
    Table.Append;
    Table.Fields['User'].AsGuid := User;
    Table.Fields['Password'].AsString := Password;
    Table.Post;
  end;
end;

class function Db.ChangeUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.User;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixUser;
  Result := Str.NotEmpty(Password) and Table.Find([Guid.ToStr(User)], false);

  if Result then
  begin
    Table.Edit;
    Table.Fields['Password'].AsString := Password;
    Table.Post;
  end;
end;

class function Db.RemoveUser(const Mgr: IDataMgr; const User: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.User;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixUser;
  Result := Table.Find([Guid.ToStr(User)], false);

  if Result then
  begin
    Table.Delete;
    RemoveOfflineUser(Mgr, User);
  end;
end;

class function Db.FindUser(const Mgr: IDataMgr; const User: TGuid; const Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.User;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixUser;
  Result := Table.Find([Guid.ToStr(User)], false) and
    (Str.Compare(Password, Table.Fields['Password'].AsString) = 0);
end;


class function Db.AppendOffline(const Mgr: IDataMgr; const Id, ToUser, From: TGuid; const Time: TDateTime; Kind: TOfflineKind; const Data: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Offline;
  Sil.Lock.Take(Table);

  Table.Append;
  Table.Fields['Id'].AsGuid := Id;
  Table.Fields['To'].AsGuid := ToUser;
  Table.Fields['From'].AsGuid := From;
  Table.Fields['Time'].AsDateTime := Time;
  Table.Fields['Kind'].AsByte := Ord(Kind);
  Table.Fields['Data'].AsString := Data;
  Table.Fields['Timeout'].AsDateTime := 0;
  Table.Post;

  Result := true;
end;

class function Db.RemoveOfflineId(const Mgr: IDataMgr; const Id, ToUser: TGuid; out Info: ROfflineInfo): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Offline;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixOfflineIdTo;
  Result := Table.Find([Guid.ToStr(Id), Guid.ToStr(ToUser)], false);

  if Result then
  begin
    Info.Id := Table.Fields['Id'].AsGuid;
    Info.FromUser := Table.Fields['From'].AsGuid;
    Info.ToUser := Table.Fields['To'].AsGuid;
    Info.Text := Table.Fields['Data'].AsString;
    Info.Time := Table.Fields['Time'].AsDateTime;
    Info.Kind := TOfflineKind(Table.Fields['Kind'].AsByte);

    Table.Delete;
  end;
end;

class function Db.RemoveOfflineUser(const Mgr: IDataMgr; const ToUser: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Offline;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixOfflineTo;

  repeat
    Result := Table.Find([Guid.ToStr(ToUser)], false);
    if Result then Table.Delete;
  until not Result;
end;

class function Db.CollectOffline(const Mgr: IDataMgr; const User: TGuid; out Info: TOfflineInfoArray): Boolean;
var
  Table: IDataRowset;
  i: Integer;
begin
  Table := Mgr.Offline;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixOfflineTo;
  Result := Table.Find([Guid.ToStr(User)], false);

  while not Table.IsEof and (Guid.Compare(Table.Fields['To'].AsGuid, User) = 0) do
  begin
    i := Length(Info);
    SetLength(Info, i + 1);

    Info[i].Id := Table.Fields['Id'].AsGuid;
    Info[i].FromUser := Table.Fields['From'].AsGuid;
    Info[i].ToUser := Table.Fields['To'].AsGuid;
    Info[i].Text := Table.Fields['Data'].AsString;
    Info[i].Time := Table.Fields['Time'].AsDateTime;
    Info[i].Kind := TOfflineKind(Table.Fields['Kind'].AsByte);

    Table.Next;
  end;
end;

class function Db.GroupAppendOffline(const Mgr: IDataMgr; const Id, From: TGuid; const Time: TDateTime; Kind: TOfflineKind; const Data: String): Boolean;
var
  Table, Users: IDataRowset;
  UserList, GroupList: TGuidArray;
  UserId: TGuid;
  i: Integer;
begin
  Table := Mgr.Offline;
  Sil.Lock.Take(Table);
  Users := Mgr.GroupUser;
  Sil.Lock.Take(Users);

  Result := CollectGroup(Mgr, From, GroupList);
  if not Result then Exit;

  Users.ActiveIndexName := IxGroupUserGroup;

  for i := 0 to Length(GroupList) - 1 do
  begin
    if not Users.Find([Guid.ToStr(GroupList[i])], false) then
      Continue;

    while not Users.IsEof and (Guid.Compare(Users.Fields['Group'].AsGuid, GroupList[i]) = 0) do
    begin
      UserId := Users.Fields['User'].AsGuid;

      if Guid.ArrayFind(UserList, UserId) = -1 then
      begin
        Guid.ArrayAdd(UserList, UserId);

        Table.Append;
        Table.Fields['Id'].AsGuid := Id;
        Table.Fields['To'].AsGuid := UserId;
        Table.Fields['From'].AsGuid := From;
        Table.Fields['Time'].AsDateTime := Time;
        Table.Fields['Kind'].AsByte := Ord(Kind);
        Table.Fields['Data'].AsString := Data;
        Table.Fields['Timeout'].AsDateTime := 0;
        Table.Post;
      end;

      Users.Next;
    end;
  end;
end;

class function Db.AppendGroup(const Mgr: IDataMgr; const Parent: TGuid; const Name, Password: String; out Group: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Group;
  Sil.Lock.Take(Table);
  Group := Guid.Create;

  Table.ActiveIndexName := ixGroupPaNa;
  Result := not Table.Find([Guid.ToStr(Parent), Name], false);

  if Result then
  begin
    Table.Append;
    Table.Fields['Group'].AsGuid := Group;
    Table.Fields['Parent'].AsGuid := Parent;
    Table.Fields['Name'].AsString := Name;
    Table.Fields['Password'].AsString := Password;
    Table.Post;
  end;
end;

class function Db.ChangeGroup(const Mgr: IDataMgr; const Group: TGuid; const Name, Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Group;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroup;
  Result := Table.Find([Guid.ToStr(Group)], false);

  if Result then
  begin
    Table.Edit;
    Table.Fields['Name'].AsString := Name;
    Table.Fields['Password'].AsString := Password;
    Table.Post;
  end;
end;

class function Db.RemoveGroup(const Mgr: IDataMgr; const Group: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Group;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroup;
  Result := Table.Find([Guid.ToStr(Group)], false);

  if Result then
  begin
    Table.Delete;
    RemoveGroupUser(Mgr, Group);
  end;
end;

class function Db.FindGroup(const Mgr: IDataMgr; const Group: TGuid; const Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Group;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroup;
  Result := Table.Find([Guid.ToStr(Group)], false) and
    (Str.Compare(Password, Table.Fields['Password'].AsString) = 0);
end;

class function Db.CollectGroup(const Mgr: IDataMgr; out Groups: TOuchGroupArray): Boolean;
var
  Table: IDataRowset;
  i: Integer;
begin
  Table := Mgr.Group;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroup;
  Table.First;

  while not Table.IsEof do
  begin
    i := Length(Groups);
    SetLength(Groups, i + 1);

    Groups[i].Parent := Table.Fields['Parent'].AsGuid;
    Groups[i].Id := Table.Fields['Group'].AsGuid;
    Groups[i].Name := Table.Fields['Name'].AsString;

    Table.Next;
  end;

  Result := true;
end;

class function Db.CollectGroup(const Mgr: IDataMgr; const User: TGuid; out Groups: TGuidArray): Boolean;
var
  Users: IDataRowset;
  i: Integer;
begin
  Users := Mgr.GroupUser;
  Sil.Lock.Take(Users);

  Users.ActiveIndexName := ixGroupUserUser;
  Result := Users.Find([Guid.ToStr(User)], false);
  if not Result then Exit;

  while not Users.IsEof and (Guid.Compare(Users.Fields['User'].AsGuid, User) = 0) do
  begin
    i := Length(Groups);
    SetLength(Groups, i + 1);
    Groups[i] := Users.Fields['Group'].AsGuid;
    Users.Next;
  end;
end;

class function Db.AppendGroupUser(const Mgr: IDataMgr; const Group, User: TGuid; const Password: String): Boolean;
var
  Table: IDataRowset;
begin
  Result := FindGroup(Mgr, Group, Password);
  if not Result then Exit;

  Table := Mgr.GroupUser;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroupUserGrUs;
  Result := not Table.Find([Guid.ToStr(Group), Guid.ToStr(User)], false);

  if Result then
  begin
    Table.Append;
    Table.Fields['Group'].AsGuid := Group;
    Table.Fields['User'].AsGuid := User;
    Table.Post;
  end else
    Result := true;
end;

class function Db.RemoveGroupUser(const Mgr: IDataMgr; const Group, User: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.GroupUser;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroupUserGrUs;
  Result := not Table.Find([Guid.ToStr(Group), Guid.ToStr(User)], false);

  if Result then
    Table.Delete;
end;

class function Db.RemoveGroupUser(const Mgr: IDataMgr; const Group: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.GroupUser;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroupUserGroup;

  repeat
    Result := Table.Find([Guid.ToStr(Group)], false);
    if Result then Table.Delete;
  until not Result;
end;

class function Db.CollectGroupUser(const Mgr: IDataMgr; const Group, UserId: TGuid; out Users: TOuchUserArray): Boolean;
var
  Table, UserTable, SessionTable: IDataRowset;
  i: Integer;
  User: TGuid;
begin
  Table := Mgr.GroupUser;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixGroupUserGroup;
  Result := Table.Find([Guid.ToStr(Group)], false);

  if Result then
  begin
    UserTable := Mgr.User;
    Sil.Lock.Take(UserTable);
    UserTable.ActiveIndexName := ixUser;

    SessionTable := Mgr.Session;
    Sil.Lock.Take(SessionTable);
    SessionTable.ActiveIndexName := ixSessionUser;

    while not Table.IsEof and (Guid.Compare(Table.Fields['Group'].AsGuid, Group) = 0) do
    begin
      User := Table.Fields['User'].AsGuid;

      if (Guid.Compare(User, UserId) <> 0) and
        UserTable.Find([Guid.ToStr(User)], false) and
        SessionTable.Find([Guid.ToStr(User)], false) then
      begin
        i := Length(Users);
        SetLength(Users, i + 1);

        Users[i].User := User;
        ReadData(Mgr, User, Users[i].Data);
      end;

      Table.Next;
    end;
  end;
end;

class function Db.AppendSession(const Mgr: IDataMgr; const User: TGuid; out Session: TGuid; const Address: String; const Timeout: TDateTime): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Session;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixSessionUser;
  Result := not Table.Find([Guid.ToStr(User)], false);

  if Result then
  begin
    Session := Guid.Create;

    Table.Append;
    Table.Fields['User'].AsGuid := User;
    Table.Fields['Session'].AsGuid := Session;
    Table.Fields['Status'].AsByte := Ord(usUnknown);
    Table.Fields['Address'].AsLongWord := Sil.OS.Socket.IPAddressToN(Address);
    Table.Fields['Timeout'].AsDateTime := Timeout;
    Table.Post;
  end else
    Session := Table.Fields['Session'].AsGuid;
end;

class function Db.ChangeSession(const Mgr: IDataMgr; const Session: TGuid; Status: TOuchUserStatus; const Address: String; const Timeout: TDateTime): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Session;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixSession;
  Result := Table.Find([Guid.ToStr(Session)], false);

  if Result then
  begin
    Table.Edit;
    Table.Fields['Status'].AsByte := Ord(Status);
    Table.Fields['Address'].AsLongWord := Sil.OS.Socket.IPAddressToN(Address);
    Table.Fields['Timeout'].AsDateTime := Timeout;
    Table.Post;
  end;
end;

class function Db.RemoveSession(const Mgr: IDataMgr; const Session: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Session;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixSession;
  Result := not Table.Find([Guid.ToStr(Session)], false);

  if Result then
    Table.Delete;
end;

class function Db.FindSession(const Mgr: IDataMgr; const Session: TGuid; out User: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Session;
  Sil.Lock.Take(Table);

  Table.First;
  Table.ActiveIndexName := ixSession;
  Result := Table.Find([Guid.ToStr(Session)], false);

  if Result then
    User := Table.Fields['User'].AsGuid;
end;

class function Db.FindSessionUser(const Mgr: IDataMgr; const User: TGuid; out Session: TGuid): Boolean;
var
  Table: IDataRowset;
begin
  Table := Mgr.Session;
  Sil.Lock.Take(Table);

  Table.ActiveIndexName := ixSessionUser;
  Result := Table.Find([Guid.ToStr(User)], false);

  if Result then
    Session := Table.Fields['Session'].AsGuid;
end;

class procedure Db.DisconnectSession(const Mgr: IDataMgr; const Time: TDateTime);
var
  Session, GroupUser: IDataRowset;
  UserList, GroupList: TGuidArray;
  sStatus: String;
  Id, ToUser, FromUser: TGuid;
  i1, i2: Integer;
begin
  Session := Mgr.Session;
  Sil.Lock.Take(Session);

  Session.ActiveIndexName := ixSessionTimeout;
  Session.First;

  GroupUser := Mgr.GroupUser;
  Sil.Lock.Take(GroupUser);

  sStatus := Str.Format('status="%s"', [Sil.Enum.Name(TypeInfo(TOuchUserStatus), Ord(usOffline), 'us')]);

  // carga la lista de usuarios que se desconectan
  while not Session.IsEof and (Session.Fields['Timeout'].AsDateTime <= Time) do
  begin
    Guid.ArrayAdd(UserList, Session.Fields['User'].AsGuid);
    Session.Next;
  end;

  for i1 := 0 to Length(UserList) - 1 do
  begin
    // lee la lista de grupos a los que esta loggeado el usuario
    if CollectGroup(Mgr, UserList[i1], GroupList) then
    begin
      Id := Guid.Create;

      // recorre los grupos
      for i2 := 0 to Length(GroupList) - 1 do
      begin
        GroupUser.ActiveIndexName := ixGroupUserGroup;
        GroupUser.Find([Guid.ToStr(GroupList[i2])], false);

        // recorre los usuarios del grupo
        while not GroupUser.IsEof and Guid.IsEqual(GroupUser.Fields['Group'].AsGuid, GroupList[i2]) do
        begin
          ToUser := GroupUser.Fields['User'].AsGuid;

          if Guid.ArrayFind(UserList, ToUser) = -1 then
          begin
            Guid.ArrayAdd(UserList, ToUser);
            AppendOffline(Mgr, Id, ToUser, FromUser, Time, okUserStatus, sStatus);
          end;

          GroupUser.Next;
        end;
      end;
    end;

    // borra la sesion
    Session.ActiveIndexName := ixSessionUser;
    if Session.Find([Guid.ToStr(UserList[i1])], false) then Session.Delete;
  end;
end;

end.
