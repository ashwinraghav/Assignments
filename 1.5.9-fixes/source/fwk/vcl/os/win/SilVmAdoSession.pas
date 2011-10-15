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

unit SilVmAdoSession;

interface

uses
  Windows, ADODB,

  Sil,
  SilData,
  SilClasses,
  SilProtocol, SilTool,
  SilSiSocketConnection,

  SilSiAbstractConnection,
  SilViAdoSession;

type
  TSqlSession = class (
    // extends
    TSilInterfacedObject,
    // implements
    IUnknown,
    ISqlSession)
  private
    FAppID: String;
    FClientCount: Integer;
    FConnection: IAbstractConnection;
    FDatabase: TADOConnection;
    FDBInfo: RDbConnectionInfo;
    FRemoteBuild: LongWord;
    FLock: ICriticalSection;
  protected // ISqlSession
    function GetRemoteBuild: LongWord;
    function GetConnection: IAbstractConnection;
    function GetDatabase: TADOConnection;
    function GetAppID: String;
    function GetInfo: RDbConnectionInfo;
    function GetLockable: ILockable; override; 
    function GetClientCount: Integer;
    procedure SetClientCount(Value: Integer);
    function Disconnect: Boolean;
  public
    constructor Create(RemoteBuild: LongWord; const AppID: String; const Info: RDbConnectionInfo; const Connection: IAbstractConnection = nil);
    destructor Destroy; override;
  end;

  TSqlSessionList = class (
    // extends
    TInterfaceList,
    // implements
    ISqlSessionList)
  private
    FPath: String;
  protected // ISqlSessionList
    function Connect(const User, Server: String): String;
    function CreateItem(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
    function ExistsAppID(const Value: String): Boolean;
    function First: ISqlSession;
    function Last: ISqlSession;
    function GetItem(Index: Integer): ISqlSession;
    function FindAppID(const Value: String; out Session: ISqlSession): Boolean;
    function GroupSession(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
    procedure Disconnect(const AppID: String);
    function Query(const AppID, QueryStr: String; WordAsInt: Boolean = false): IDataRowset;
    function Execute(const AppID, QueryStr: String): Integer;
    function StoredProc(const AppID, ProcName: String; var Params: IValueList; WithRecords: Boolean): IDataRowset;
    procedure QueryFields(const AppID, Command: String; var Fields: IValueList);
    procedure QueryStoredProcParams(const AppID, ProcName: String; out Params: IValueList);
  public
    constructor Create(Group: LongWord = 10);
  end;

implementation

uses
  SilVmAdoRowset;

{function SetDataSource(const FileName, InitStr: WideString): Boolean;
var
  DataInit: IDataInitialize;
begin
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  Result := DataInit.WriteStringToStorage(PWideChar(FileName), PWideChar(InitStr), CREATE_ALWAYS) = S_OK;
end;

function GetDataSource(FileName: WideString): WideString;
var
  DataInit: IDataInitialize;
  InitStr: PWideChar;
begin
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  DataInit.LoadStringFromStorage(PWideChar(FileName), InitStr);
  Result := InitStr;
end;}

{ TSqlSession }

constructor TSqlSession.Create(RemoteBuild: LongWord; const AppID: String; const Info: RDbConnectionInfo; const Connection: IAbstractConnection);
begin
  inherited Create;

  FLock := Sil.OS.IPC.CriticalSection;
  FAppID := AppID;
  FDBInfo := Info;
  FRemoteBuild := RemoteBuild;
  FConnection := Connection;
  FDatabase := TADOConnection.Create(nil);
  FDatabase.LoginPrompt := false;

  FClientCount := 1;
end;

destructor TSqlSession.Destroy;
begin
  if FDatabase <> nil then
  begin
    FDatabase.Close;
    Sil.Ref.Free(FDatabase);
  end;
  
  inherited;
end;

function TSqlSession.GetAppID: String;
begin
  Result := FAppID;
end;

procedure TSqlSession.SetClientCount(Value: Integer);
begin
  FClientCount := Value;
end;

function TSqlSession.GetClientCount: Integer;
begin
  Result := FClientCount;
end;

function TSqlSession.GetConnection: IAbstractConnection;
begin
  Result := FConnection;
end;

function TSqlSession.GetDatabase: TADOConnection;
begin
  Result := FDatabase;
end;

function TSqlSession.GetInfo: RDbConnectionInfo;
begin
  Result := FDBInfo;
end;

function TSqlSession.Disconnect: Boolean;
begin
  Dec(FClientCount);
  Result := FClientCount < 1;
  if Result then FDatabase.Close;
end;

function TSqlSession.GetRemoteBuild: LongWord;
begin
  Result := FRemoteBuild;
end;

function TSqlSession.GetLockable: ILockable;
begin
  Result := FLock;
end;

{ TSqlSessionList }

constructor TSqlSessionList.Create(Group: LongWord);
begin
  inherited Create(true);
  FPath := Sil.OS.Module.Current.Info.Path;
end;

function TSqlSessionList.GroupSession(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
begin
  Result := Info.User;
end;

function TSqlSessionList.Connect(const User, Server: String): String;
var
  Info: RDbConnectionInfo;
begin
  Info.User := User;
  Info.Server := Server;
  Result := CreateItem(0, Info);
end;

function TSqlSessionList.CreateItem(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
const
  SFileName = 'file name=';
var
  Session: ISqlSession;
  sPath: String;
begin
  Locked;
  Result := Info.User;

  if not FindAppID(Info.User, Session) then
  begin
    Session := TSqlSession.Create(RemoteBuild, Result, Info);
    Add(Session);
  end else
  if Session.Database.Connected then
    Exit;

  if Sil.Text.Compare(Info.Server, SFileName, Length(SFileName)) = 0 then
  begin
    sPath := SFileName + FPath + Str.Copy(Info.Server, Length(SFileName) + 1);
    if Str.Pos('.', sPath) = 0 then sPath := sPath + '.udl';
  end else
  if Str.Pos('=', Info.Server) = 0 then
    sPath := SFileName + FPath + Info.Server + '.udl' else
    sPath := Info.Server;

  Session.Database.ConnectionString := sPath;
  Session.Database.Open;
end;

function TSqlSessionList.ExistsAppID(const Value: String): Boolean;
var
  e: IEnumerator;
  Session: ISqlSession;
begin
  if Length(Value) > 0 then
    while Enumerate(e, Session) do
      if Sil.Text.Compare(Session.AppID, Value) = 0 then
      begin
        Result := true;
        Exit;
      end;
  Result := false;
end;

function TSqlSessionList.FindAppID(const Value: String; out Session: ISqlSession): Boolean;
var
  e: IEnumerator;
begin
  while Enumerate(e, Session) do
    if Sil.Text.Compare(Session.AppID, Value) = 0 then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
  Session := nil;
end;

function TSqlSessionList.First: ISqlSession;
begin
  Result := ISqlSession(inherited First);
end;

function TSqlSessionList.GetItem(Index: Integer): ISqlSession;
begin
  Result := ISqlSession(inherited GetItem(Index));
end;

function TSqlSessionList.Last: ISqlSession;
begin
  Result := ISqlSession(inherited Last);
end;

procedure TSqlSessionList.Disconnect(const AppID: String);
var
  Session: ISqlSession;
begin
  if FindAppID(AppID, Session) and Session.Disconnect then
    Remove(Session);
end;

function TSqlSessionList.Query(const AppID, QueryStr: String; WordAsInt: Boolean): IDataRowset;
var
  Session: ISqlSession;
begin
  Result := nil;
  if not FindAppID(AppID, Session) then Exit;

  Sil.Lock.Take(Session.Lockable);
  Result := TAdoRowset.CreateQuery(Session.Database, QueryStr, true, WordAsInt);
end;

function TSqlSessionList.Execute(const AppID, QueryStr: String): Integer;
var
  Session: ISqlSession;
  Qry: IDataRowset;
begin
  Result := 0;
  if not FindAppID(AppID, Session) then Exit;

  Sil.Lock.Take(Session.Lockable);
  Qry := TAdoRowset.CreateQuery(Session.Database, QueryStr, false);
  Result := Qry.RecordCount;
end;

function TSqlSessionList.StoredProc(const AppID, ProcName: String; var Params: IValueList; WithRecords: Boolean): IDataRowset;
var
  Session: ISqlSession;
begin
  Result := nil;
  if not FindAppID(AppID, Session) then Exit;

  Sil.Lock.Take(Session.Lockable);
  Result := TAdoRowset.CreateStoredProc(Session.Database, ProcName, Params, WithRecords);
end;

procedure TSqlSessionList.QueryFields(const AppID, Command: String; var Fields: IValueList);
var
  Session: ISqlSession;
  Item: IUnknown;
begin
  if not FindAppID(AppID, Session) then Exit;

  Sil.Lock.Take(Session.Lockable);
  Item := TAdoRowset.CreateQueryFields(Session.Database, Command, Fields);
end;

procedure TSqlSessionList.QueryStoredProcParams(const AppID, ProcName: String; out Params: IValueList);
var
  Session: ISqlSession;
  Item: IUnknown;
begin
  if not FindAppID(AppID, Session) then Exit;

  Sil.Lock.Take(Session.Lockable);
  Item := TAdoRowset.CreateStoredProcParams(Session.Database, ProcName, Params);
end;

end.
