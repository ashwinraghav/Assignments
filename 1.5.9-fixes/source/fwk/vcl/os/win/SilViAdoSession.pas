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

unit SilViAdoSession;

interface

uses
  Db, ADODB,

  Sil,
  SilData,
  SilProtocol,
  SilTool,
  SilSiAbstractConnection;

type
  ISqlSession = interface
    ['{33BE2DF2-5812-11D4-988A-00104B0FA1EF}']
    function GetClientCount: Integer;
    procedure SetClientCount(Value: Integer);
    function GetRemoteBuild: LongWord;
    function Disconnect: Boolean;
    function GetConnection: IAbstractConnection;
    function GetDatabase: TADOConnection;
    function GetAppID: String;
    function GetInfo: RDbConnectionInfo;
    function GetLockable: ILockable;
    property Lockable: ILockable read GetLockable;
    property Connection: IAbstractConnection read GetConnection;
    property Database: TADOConnection read GetDatabase;
    property AppID: String read GetAppID;
    property ClientCount: Integer read GetClientCount write SetClientCount;
    property Info: RDbConnectionInfo read GetInfo;
    property RemoteBuild: LongWord read GetRemoteBuild;
  end;

  ISqlSessionList = interface(IList)
    ['{33BE2DF3-5812-11D4-988A-00104B0FA1EF}']
    function Connect(const User, Server: String): String;
    function CreateItem(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
    function GroupSession(RemoteBuild: LongWord; const Info: RDbConnectionInfo): String;
    function Add(const Item: IUnknown): Integer;
    procedure AddList(const Source: IInterfaceList);
    function IndexOf(const Item: IUnknown): Integer;
    procedure Insert(Index: Integer; const Item: IUnknown);
    function Remove(const Item: IUnknown): Integer;
    procedure Disconnect(const AppID: String);
    function Query(const AppID, QueryStr: String; WordAsInt: Boolean = false): IDataRowset;
    function Execute(const AppID, QueryStr: String): Integer;
    function StoredProc(const AppID, ProcName: String; var Params: IValueList; WithRecords: Boolean): IDataRowset;
    procedure QueryFields(const AppID, Command: String; var Fields: IValueList);
    procedure QueryStoredProcParams(const AppID, ProcName: String; out Params: IValueList);
    function First: ISqlSession;
    function Last: ISqlSession;
    function GetItem(Index: Integer): ISqlSession;
    function FindAppID(const Value: String; out Session: ISqlSession): Boolean;
    function ExistsAppID(const Value: String): Boolean; 
    property Items[Index: Integer]: ISqlSession read GetItem; default;
  end;

implementation

end.
 
