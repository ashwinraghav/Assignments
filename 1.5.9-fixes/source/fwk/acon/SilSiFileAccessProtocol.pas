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

unit SilSiFileAccessProtocol;

interface

uses
  Sil,
  SilSiProtocolBase;

type
  TFileAccessResult = (frOk, frAccessDenied, frNotExists, frInUse);

  IFileAccessProtocol = interface (IProtocol)
    ['{80B93882-6F96-11D4-9894-00104B0FA1EF}']
    function GetUserName: String;
    function GetPassword: String;
    function Login(const UserName: String = ''; const Password: String = ''): Boolean;
    function QueryInfo(const RemoteName: String; out Info: IFileInfo): Boolean;
    function QueryFile(const Stream: IRandomStream; const RemoteName: String; const Start: LongWord = 0): Boolean;
    function SendFile(const Stream: IRandomStream; const RemoteName: String = ''; const Start: LongWord = 0): Boolean;
    function QueryFileList(out Info: IFileInfoList): Boolean;
    function MakeDir(const RemoteName: String): Boolean;
    function Remove(const RemoteName: String): Boolean;
    function Rename(const RemoteName, NewName: String): Boolean;
    function OpenDir(const RemoteName: String): Boolean;
    function GetResult: TFileAccessResult;
    property UserName: String read GetUserName;
    property Password: String read GetPassword;
  end;

  TLoginEvent = record
    Sender: IFileAccessProtocol;
    UserName: String;
    Password: String;
    Result: TFileAccessResult;
  end;

  TQueryInfoEvent = record
    Sender: IFileAccessProtocol;
    Name: String;
    Info: IFileInfo;
    Result: TFileAccessResult;
  end;

  TFileAccessEvent = record
    Sender: IFileAccessProtocol;
    Name: String;
    Start: LongWord;
    Stream: IRandomStream;
    Result: TFileAccessResult;
  end;

  TQueryFileListEvent = record
    Sender: IFileAccessProtocol;
    Path: String;
    Info: IFileInfoList;
    Result: TFileAccessResult;
  end;

  TActionEvent = record
    Sender: IFileAccessProtocol;
    Path: String;
    Handled: Boolean;
    Result: TFileAccessResult;
  end;

  TRenameEvent = record
    Sender: IFileAccessProtocol;
    OldPath: String;
    NewPath: String;
    Handled: Boolean;
    Result: TFileAccessResult;
  end;

  TOpenDirEvent = record
    Sender: IFileAccessProtocol;
    Path: String;
    Result: TFileAccessResult;
  end;

  IFileAccessProtocolEvents = interface
    ['{80B93886-6F96-11D4-9894-00104B0FA1EF}']
    procedure OnLogin(var Event: TLoginEvent);
    procedure OnQueryInfo(var Event: TQueryInfoEvent);
    procedure OnQueryFile(var Event: TFileAccessEvent);
    procedure OnQueryFileList(var Event: TQueryFileListEvent);
    procedure OnMakeDir(var Event: TActionEvent);
    procedure OnRemove(var Event: TActionEvent);
    procedure OnRename(var Event: TRenameEvent);
    procedure OnOpenDir(var Event: TOpenDirEvent);
    procedure OnReceiveFile(var Event: TFileAccessEvent);
  end;

implementation

end.
