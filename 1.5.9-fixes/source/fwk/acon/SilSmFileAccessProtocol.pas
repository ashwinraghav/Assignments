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

unit SilSmFileAccessProtocol;

{$I Defines.inc}

interface

uses
  Sil,

  SilSiAbstractConnection,
  SilSiProtocolPacket,
  SilSiFileAccessProtocol,
  SilSmProtocolBase;

const
  PROT_BUILD = 1;
  PS_BASE = $400;

  MI_LOGIN                  = PS_BASE                   + 1;
  MI_LOGIN_REPLY            = MI_LOGIN                  + 1;
  MI_QUERY_INFO             = MI_LOGIN_REPLY            + 1;
  MI_QUERY_INFO_REPLY       = MI_QUERY_INFO             + 1;
  MI_QUERY_FILE             = MI_QUERY_INFO_REPLY       + 1;
  MI_FILE_DATA              = MI_QUERY_FILE             + 1;
  MI_FILE_DATA_OK           = MI_FILE_DATA              + 1;
  MI_QUERY_FILE_LIST        = MI_FILE_DATA_OK           + 1;
  MI_QUERY_FILE_LIST_REPLY  = MI_QUERY_FILE_LIST        + 1;
  MI_MAKE_DIR               = MI_QUERY_FILE_LIST_REPLY  + 1;
  MI_MAKE_DIR_REPLY         = MI_MAKE_DIR               + 1;
  MI_REMOVE                 = MI_MAKE_DIR_REPLY         + 1;
  MI_REMOVE_REPLY           = MI_REMOVE                 + 1;
  MI_RENAME                 = MI_REMOVE_REPLY           + 1;
  MI_RENAME_REPLY           = MI_RENAME                 + 1;
  MI_OPEN_DIR               = MI_RENAME_REPLY           + 1;
  MI_OPEN_DIR_REPLY         = MI_OPEN_DIR               + 1;
  MI_SEND_FILE              = MI_OPEN_DIR_REPLY         + 1;
  MI_SEND_FILE_REPLY        = MI_SEND_FILE              + 1;

type
  TFileAccessProtocol = class (
    // extends
    TProtocolBase,
    // implements
    IFileAccessProtocol)
  private
    FResult: TFileAccessResult;
    FRoot: String;
    FCurrendDir: String;
    FUserName: String;
    FPassword: String;
  private
    function DoGetResult(const Packet: IProtocolPacket): Boolean;
    function DoSendFileList(const Info: IFileInfoList): Boolean;
    function DoSendInfo(Code: TFileAccessResult; const LocalName: String; const Info: IFileInfo): Boolean;
  private
    procedure FireLogin(var Msg: TProtocolBaseMessage); message MI_LOGIN;
    procedure FireQueryInfo(var Msg: TProtocolBaseMessage); message MI_QUERY_INFO;
    procedure FireQueryFile(var Msg: TProtocolBaseMessage); message MI_QUERY_FILE;
    procedure FireQueryFileList(var Msg: TProtocolBaseMessage); message MI_QUERY_FILE_LIST;
    procedure FireMakeDir(var Msg: TProtocolBaseMessage); message MI_MAKE_DIR;
    procedure FireRemove(var Msg: TProtocolBaseMessage); message MI_REMOVE;
    procedure FireRename(var Msg: TProtocolBaseMessage); message MI_RENAME;
    procedure FireOpenDir(var Msg: TProtocolBaseMessage); message MI_OPEN_DIR;
    //procedure FireReceiveFile(var Msg: TProtocolBaseMessage); // message MI_SEND_FILE;
  protected // IProtocolBase
    function CreatePacket(DataID: LongWord = 0): IProtocolPacket; override;
  protected // IFileAccessProtocol
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
  protected
    function GetName: String; override;
    procedure SetConnection(const Value: IAbstractConnection); override;
  public
    constructor Create(ID: LongWord; const Root: String; const Connection: IAbstractConnection);
    class function DefaultPort: LongWord; override;
  end;

implementation

uses
  SilOkFileInfo,
  SilSiPacketBuilder;

//  c:\pepe\ --> /c/pepe/
function PathToLinuxFormat(const Path: String): String;
var
  i: Integer;
begin
  Result := Path;
  if Str.IsEmpty(Path) then Exit;

  if (Length(Result) > 1) and (Result[2] = ':') then
  begin
    Result[2] := Result[1];
    Result[1] := '/';
  end;

  for i := 1 to Length(Result) do if Result[i] = '\' then Result[i] := '/';
end;

{ TFileAccessProtocol }

constructor TFileAccessProtocol.Create(ID: LongWord; const Root: String; const Connection: IAbstractConnection);
begin
  inherited Create(ID);
  SetConnection(Connection);
  FRoot := Root;
  FCurrendDir := FRoot;
end;

function TFileAccessProtocol.CreatePacket(DataID: LongWord): IProtocolPacket;
begin
  Result := inherited CreatePacket(DataID);

  Result.ProtoVer := 1;
  Result.HeaderVer := 1;
  Result.SessionID := 0;
end;

class function TFileAccessProtocol.DefaultPort: LongWord;
begin
  Result := 23355;
end;

function TFileAccessProtocol.GetResult: TFileAccessResult;
begin
  Result := FResult;
end;

function TFileAccessProtocol.DoGetResult(const Packet: IProtocolPacket): Boolean;
begin
  FResult := TFileAccessResult(Packet.Data.ReadWord);
  Result := FResult = frOk;
end;

function TFileAccessProtocol.Login(const UserName, Password: String): Boolean;
var
  Packet: IProtocolPacket;
begin
  FUserName := UserName;
  FPassword := Password;

  Packet := CreatePacket(MI_LOGIN);
  Packet.Data.WriteString(UserName);
  Packet.Data.WriteString(Password);
  Send(Packet, 'TFileAccessProtocol.Login:Send');

  Packet := WaitReply(MI_LOGIN_REPLY);
  Result := DoGetResult(Packet);
end;

procedure TFileAccessProtocol.FireLogin(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TLoginEvent;
  Packet: IProtocolPacket;
begin
  Event.Sender := Self;
  Event.UserName := Msg.Packet.Data.ReadString;
  Event.Password := Msg.Packet.Data.ReadString;
  Event.Result := frOk;

  FUserName := Event.UserName;
  FPassword := Event.Password;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnLogin(Event);

  Packet := CreatePacket(MI_LOGIN_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireLogin:Send');
end;

function TFileAccessProtocol.MakeDir(const RemoteName: String): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(MI_MAKE_DIR);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Send(Packet, 'TFileAccessProtocol.MakeDir:Send');

  Packet := WaitReply(MI_MAKE_DIR_REPLY);
  Result := DoGetResult(Packet);
end;

procedure TFileAccessProtocol.FireMakeDir(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TActionEvent;
  Packet: IProtocolPacket;
  sPath: String;
begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Path := OS.FileSystem.TranslatePath(sPath);
  Event.Handled := false;
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnMakeDir(Event);
      
  if not Event.Handled then OS.FileSystem.CreateDirectory(Event.Path);

  Packet := CreatePacket(MI_MAKE_DIR_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireMakeDir:Send');
end;

function TFileAccessProtocol.OpenDir(const RemoteName: String): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(MI_OPEN_DIR);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Send(Packet, 'TFileAccessProtocol.OpenDir:Send');

  Packet := WaitReply(MI_OPEN_DIR_REPLY);
  Result := DoGetResult(Packet);
end;

procedure TFileAccessProtocol.FireOpenDir(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TOpenDirEvent;
  Packet: IProtocolPacket;
  sPath: String;
begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Path := OS.FileSystem.TranslatePath(sPath);
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnOpenDir(Event);
    
  if Event.Result = frOk then
  begin
    FCurrendDir := PathToLinuxFormat(Event.Path);
    if not Str.IsEmpty(FCurrendDir) and (FCurrendDir[Length(FCurrendDir)] <> '/') then
      FCurrendDir := FCurrendDir + '/';
  end;

  Packet := CreatePacket(MI_OPEN_DIR_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireOpenDir:Send');
end;

function TFileAccessProtocol.QueryFile(const Stream: IRandomStream; const RemoteName: String; const Start: LongWord): Boolean;
var
  Packet: IProtocolPacket;
  Buffer: PChar;
  dwPacket, dwSize: LongWord;
begin
  Packet := CreatePacket(MI_QUERY_FILE);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Packet.Data.WriteLongWord(Start);
  Send(Packet, 'TFileAccessProtocol.QueryFile:Send');

  Packet := WaitReply(MI_SEND_FILE);
  Result := DoGetResult(Packet);

  if Result then
  begin
    Packet.Data.ReadString;
    Stream.Position := Packet.Data.ReadLongWord;
    dwSize := Packet.Data.ReadLongWord;

    Packet := CreatePacket(MI_SEND_FILE_REPLY);
    Packet.Data.WriteWord(Word(frOk));
    Send(Packet, 'TFileAccessProtocol.QueryFile:Send.2');

    while dwSize > 0 do
    begin
      Packet := WaitReply(MI_FILE_DATA);
      Send(CreatePacket(MI_FILE_DATA_OK), 'TFileAccessProtocol.QueryFile:Send.3');
      Buffer := Packet.Data.ReadPChar(dwPacket);
      if dwPacket = 0 then Break;
      Stream.Write(Buffer^, dwPacket);
      Dec(dwSize, dwPacket);
    end;
  end;
end;

procedure TFileAccessProtocol.FireQueryFile(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TFileAccessEvent;
  Packet: IProtocolPacket;
  sPath: String;
begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Name := OS.FileSystem.TranslatePath(sPath);
  Event.Start := Msg.Packet.Data.ReadLongWord;
  Event.Stream := nil;
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnQueryFile(Event);

  if Event.Result = frOk then
  begin
    if Event.Stream = nil then
    begin
      Event.Stream := OS.FileSystem.OpenFile(Event.Name, fmAccessRead, fmShareRead, true).Stream;
      Event.Stream.Position := Event.Start;
    end;
    SendFile(Event.Stream, Event.Name, Event.Stream.Position);
  end else
  begin
    Packet := CreatePacket(MI_SEND_FILE);
    Packet.Data.WriteWord(Word(Event.Result));
    Send(Packet, 'TFileAccessProtocol.FireQueryFile:Send');
  end;
end;

function TFileAccessProtocol.QueryFileList(out Info: IFileInfoList): Boolean;
var
  Packet: IProtocolPacket;
  dwCount: LongWord;
  FileInfo: IFileInfoDef;
  Attr: TFileAttributes;
begin
  Packet := CreatePacket(MI_QUERY_FILE_LIST);
  Send(Packet, 'TFileAccessProtocol.QueryFileList:Send');
  Packet := WaitReply(MI_QUERY_FILE_LIST_REPLY);
  Result := DoGetResult(Packet);

  if Result then
  begin
    dwCount := Packet.Data.ReadLongWord;
    Info := TSilFileInfoList.Create(true);

    while dwCount > 0 do
    begin
      with Packet.Data do
      begin
        FileInfo := TSilFileInfo.Create;
        FileInfo.Name := ReadString;
        FileInfo.Time := ReadFloat;
        ReadBuffer(Attr, SizeOf(TFileAttributes));
        FileInfo.Attributes := Attr;
        FileInfo.Size := ReadLongWord;
      end;
      Info.Add(FileInfo);
      Dec(dwCount);
    end;
  end;
end;

procedure TFileAccessProtocol.FireQueryFileList(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TQueryFileListEvent;
  Packet: IProtocolPacket;
begin
  Event.Sender := Self;
  Event.Path := OS.FileSystem.TranslatePath(FCurrendDir);
  Event.Info := nil;
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnQueryFileList(Event);

  if Event.Result = frOk then
  begin
    if Event.Info = nil then Event.Info := OS.FileSystem.GetList(Event.Path);
    DoSendFileList(Event.Info);
  end else
  begin
    Packet := CreatePacket(MI_QUERY_FILE_LIST_REPLY);
    Packet.Data.WriteWord(Word(Event.Result));
    Send(Packet, 'TFileAccessProtocol.FireQueryFileList:Send');
  end;
end;

function TFileAccessProtocol.DoSendFileList(const Info: IFileInfoList): Boolean;
var
  Packet: IProtocolPacket;
  e: IEnumerator;
  FileInfo: IFileInfo;
  Attr: TFileAttributes;
begin
  Packet := CreatePacket(MI_QUERY_FILE_LIST_REPLY);
  Packet.Data.WriteWord(Word(frOk));
  Packet.Data.WriteLongWord(Info.Count);

  while Info.Enumerate(e, FileInfo) do
  begin
    Packet.Data.WriteString(PathToLinuxFormat(FileInfo.Name));
    Packet.Data.WriteFloat(FileInfo.Time);
    Attr := FileInfo.Attributes;
    Packet.Data.WriteBuffer(Attr, SizeOf(TFileAttributes));
    Packet.Data.WriteLongWord(FileInfo.Size);
  end;

  Send(Packet, 'TFileAccessProtocol.DoSendFileList:Send');
  Result := true;
end;

function TFileAccessProtocol.QueryInfo(const RemoteName: String; out Info: IFileInfo): Boolean;
var
  Packet: IProtocolPacket;
  Attr: TFileAttributes;
  FInfo: IFileInfoDef;
begin
  Packet := CreatePacket(MI_QUERY_INFO);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Send(Packet, 'TFileAccessProtocol.QueryInfo:Send');

  Packet := WaitReply(MI_QUERY_INFO_REPLY);
  Result := DoGetResult(Packet);

  if Result then
    with Packet.Data do
    begin
      FInfo := TSilFileInfo.Create;
      FInfo.Name := ReadString;
      FInfo.Time := ReadFloat;
      ReadBuffer(Attr, SizeOf(TFileAttributes));
      FInfo.Attributes := Attr;
      FInfo.Size := ReadLongWord;
      Info := FInfo;
    end;
end;

procedure TFileAccessProtocol.FireQueryInfo(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TQueryInfoEvent;
  Packet: IProtocolPacket;
  sPath: String;

  procedure DoError(Code: TFileAccessResult);
  begin
    Packet := CreatePacket(MI_QUERY_INFO_REPLY);
    Packet.Data.WriteWord(Word(Code));
    Send(Packet, 'TFileAccessProtocol.FireQueryInfo:Send');
  end;

begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Name := OS.FileSystem.TranslatePath(sPath);
  Event.Info := nil;
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnQueryInfo(Event);

  if Event.Result = frOk then
  begin
    if Event.Info = nil then Event.Info := OS.FileSystem.GetInfo(Event.Name);
    if Event.Info = nil then
      DoError(frNotExists) else
      DoSendInfo(Event.Result, Event.Name, Event.Info);
  end else
    DoError(Event.Result);
end;

function TFileAccessProtocol.DoSendInfo(Code: TFileAccessResult; const LocalName: String; const Info: IFileInfo): Boolean;
var
  Packet: IProtocolPacket;
  Attr: TFileAttributes;
begin
  Packet := CreatePacket(MI_QUERY_INFO_REPLY);
  Packet.Data.WriteWord(Word(Code));

  if Info <> nil then
  begin
    Packet.Data.WriteString(PathToLinuxFormat(Info.Name));
    Packet.Data.WriteFloat(Info.Time);
    Attr := Info.Attributes;
    Packet.Data.WriteBuffer(Attr, SizeOf(TFileAttributes));
    Packet.Data.WriteLongWord(Info.Size);
  end else
  begin
    Packet.Data.WriteString('');
    Packet.Data.WriteBuffer(Attr, SizeOf(TFileAttributes));
    Packet.Data.WriteLongWord(0);
    Packet.Data.WriteLongWord(0);
  end;

  Send(Packet, 'TFileAccessProtocol.DoSendInfo:Send');
  Result := true;
end;

function TFileAccessProtocol.Remove(const RemoteName: String): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(MI_REMOVE);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Send(Packet, 'TFileAccessProtocol.Remove:Send');

  Packet := WaitReply(MI_REMOVE_REPLY);
  Result := DoGetResult(Packet);
end;

procedure TFileAccessProtocol.FireRemove(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TActionEvent;
  Packet: IProtocolPacket;
  sPath: String;
begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Path := OS.FileSystem.TranslatePath(sPath);
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnRemove(Event);

  if not Event.Handled then
  begin
    // ver si es un dir
    OS.FileSystem.DeleteFile(Event.Path);
  end;

  Packet := CreatePacket(MI_REMOVE_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireRemove:Send');
end;

function TFileAccessProtocol.Rename(const RemoteName, NewName: String): Boolean;
var
  Packet: IProtocolPacket;
begin
  Packet := CreatePacket(MI_RENAME);
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Packet.Data.WriteString(PathToLinuxFormat(NewName));
  Send(Packet, 'TFileAccessProtocol.Rename:Send');

  Packet := WaitReply(MI_RENAME_REPLY);
  Result := DoGetResult(Packet);
end;

procedure TFileAccessProtocol.FireRename(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TRenameEvent;
  Packet: IProtocolPacket;
  sPath: String;
begin
  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.NewPath := OS.FileSystem.TranslatePath(sPath);
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.OldPath := OS.FileSystem.TranslatePath(sPath);
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnRename(Event);

  if not Event.Handled then
  begin
    // ver si es un dir
    OS.FileSystem.MoveFile(Event.OldPath, Event.NewPath);
  end;

  Packet := CreatePacket(MI_RENAME_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireRename:Send');
end;

function TFileAccessProtocol.SendFile(const Stream: IRandomStream; const RemoteName: String; const Start: LongWord): Boolean;
var
  Packet: IProtocolPacket;
  dwPacket, dwSize, dwBuffer: LongWord;
  Buffer: PChar;
begin
  Packet := CreatePacket(MI_SEND_FILE);

  dwSize := Stream.Size;
  dwBuffer := 8192 - SizeOf(TProtocolHeader) - 6;

  Packet.Data.WriteWord(Word(frOk));
  Packet.Data.WriteString(PathToLinuxFormat(RemoteName));
  Packet.Data.WriteLongWord(Start);
  Packet.Data.WriteLongWord(dwSize);

  Send(Packet, 'TFileAccessProtocol.SendFile:Send');

  Packet := WaitReply(MI_SEND_FILE_REPLY);
  Result := DoGetResult(Packet);

  if Result then
  begin
    try
      Buffer := Mem.Get(dwBuffer);

      while Stream.Position < dwSize do
      begin
        dwPacket := Stream.Read(Buffer^, dwBuffer);
        if dwPacket = 0 then Break;
        Packet := CreatePacket(MI_FILE_DATA);
        Packet.Data.WritePChar(Buffer, dwPacket);
        Send(Packet, 'TFileAccessProtocol.SendFile:Send[2]');
        WaitReply(MI_FILE_DATA_OK);
      end;

      Packet := CreatePacket(MI_FILE_DATA); // aparentemente no es necesario
      Packet.Data.WritePChar(#0, 0);
      Send(Packet, 'TFileAccessProtocol.SendFile:Send[3]');
    finally
      Mem.Free(Buffer);
    end;
  end;
end;

(*)procedure TFileAccessProtocol.FireReceiveFile(var Msg: TProtocolBaseMessage);
var
  n: IEnumerator;
  Adapter: IFileAccessProtocolEvents;
  Event: TFileAccessEvent;
  Packet: IProtocolPacket;
  lwCount, wlSize: LongWord;
  PBuf: PChar;
  sPath: String;
begin
  if not DoGetResult(Msg.Packet) then Exit;

  Event.Sender := Self;
  sPath := Msg.Packet.Data.ReadString;
  if not Str.IsEmpty(sPath) and (sPath[1] <> '/') then sPath := FCurrendDir + sPath;
  Event.Name := OS.FileSystem.TranslatePath(sPath);
  Event.Start := Msg.Packet.Data.ReadLongWord;
  Event.Stream := Sil.Tk.MemoryStream();
  lwCount := Msg.Packet.Data.ReadLongWord;
  Event.Result := frOk;

  if HasConnections then
    while Events.Enumerate(n, Adapter, IFileAccessProtocolEvents) do
      Adapter.OnReceiveFile(Event);

  Packet := CreatePacket(MI_SEND_FILE_REPLY);
  Packet.Data.WriteWord(Word(Event.Result));
  Send(Packet, 'TFileAccessProtocol.FireMakeDir:Send');

  if Event.Result = frOk then
    while lwCount > 0 do
    begin
      Packet := WaitReply(MI_FILE_DATA);
      PBuf := Packet.Data.ReadPChar(wlSize);
      Event.Stream.Write(PBuf^, wlSize);
      Dec(lwCount, wlSize);
      Send(CreatePacket(MI_FILE_DATA_OK), 'TFileAccessProtocol.FireReceiveFile');
    end;
end;(*)

procedure TFileAccessProtocol.SetConnection(const Value: IAbstractConnection);
begin
  if FConnection <> nil then Sil.Sink.Disconnect(FConnection, Self);
  inherited SetConnection(Value);
  if Value <> nil then Sil.Sink.Connect(Value, Self);
end;

function TFileAccessProtocol.GetName: String;
begin
  Result := 'FileAccess';
end;

function TFileAccessProtocol.GetPassword: String;
begin
  Result := FPassword;
end;

function TFileAccessProtocol.GetUserName: String;
begin
  Result := FUserName;
end;

end.
