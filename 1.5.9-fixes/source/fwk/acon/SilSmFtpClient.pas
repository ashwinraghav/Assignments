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

unit SilSmFtpClient;

interface

uses
  SilLkInterfaced,
  SilLiStream,
  SilLiEnumerator,
  SilLiFiler,
  SilOcTypes,
  SilOeWait,
  SilOiIpc,
  SilOiThread,
  SilOiSocket,
  SilSiFtpCommand,
  SilSiFtp;

type
  TFtpDataOp = (doText, doRetrieve, doSend);
  TFtpDataMode = (dmPort, dmPassive);

  TSilFtpClient = class (
    // extends
    TSilInterfacedObject,
    // implements
    IFtpClient,
    IFtpClientConnectionEvents,
    IRunnable)
  private
    FCommand: IFtpClientCommand;
    FLastResponse: String;
    FDataStream: IStream;
    FFileStream: IRandomStream;
    FThread: IThread;
    FDataOp: TFtpDataOp;
    FDataPath: String;
    FPassiveMode: Boolean;
    FListener: IUnknown;
    FDataMode: TFtpDataMode;
  private
    function DoBeginDataConnection(const Path: String = ''): Boolean;
    function DoEndDataConnection: Boolean;
    function DoFirePassiveConnect(var Handled: Boolean): Boolean;
    function DoFirePortListen(var Handled: Boolean): Boolean;
    function DoFirePortAccept: Boolean;
    function DoGetPassiveResult(var Buffer, Address: String; var Port: Word): Boolean;
    procedure DoCheckThread;
    procedure DoReadData;
    procedure DoReadText;
    procedure DoWriteData;
    procedure DoFireList(const Line: String);
    procedure DoFireFileRetrieve;
    procedure DoFireFileSend;
  protected // IFtpClient
    function Login(const UserName, Password: String; const AccountInfo: String): Boolean;
    function ChangeDir(const Path: String): Boolean;
    function ChangeDirToParent: Boolean;
    function Quit: Boolean;
    function Reinitialize: Boolean;
    function Representation(Code: TFtpTypeCode): Boolean;
    function Structure(Code: TFtpStructCode): Boolean;
    function TransferMode(Mode: TFtpTransferMode): Boolean;
    function Retrieve(const Path: String; const Stream: IRandomStream = nil; Marker: LongWord = 0): Boolean;
    function Store(const Path: String; const Stream: IRandomStream = nil; Marker: LongWord = 0): Boolean;
    function Append(const Path: String; const Stream: IRandomStream = nil): Boolean;
    function Allocate(Size: LongWord; Pages: LongWord = 0): Boolean;
    function RenameFile(const OldPath, NewPath: String): Boolean;
    function AbortTransfer: Boolean;
    function DeleteFile(const Path: String): Boolean;
    function RemoveDir(const Path: String): Boolean;
    function MakeDir(const Path: String): Boolean;
    function CurrentDir: String;
    function FileSize(const Path: String): LongWord;
    function List(const Path: String = ''): Boolean;
    function NameList(const Path: String = ''): Boolean;
    function SiteParameters: String;
    function System: String;
    function Status(const Path: String = ''): String;
    function Help(const Command: String = ''): String;
    function Noop: Boolean;
    procedure PassiveMode(Enabled: Boolean);
  protected // IFtpClientConnectionEvents
    procedure OnPortListen(var Event: RFtpClientPortListen);
    procedure OnPortAccept(var Event: RFtpClientPortAccept);
    procedure OnPassiveConnect(var Event: RFtpClientPassiveConnect);
  protected // IRunnable
    procedure Run(const Sender: IThread);
  public
    constructor Create(const Command: IFtpClientCommand);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBtInt,
  SilLtReference,
  SilLtTool,
  SilOtTool;

type
  RWord = packed record
    case Byte of
      0: (Value: Word);
      1: (Lo: Byte; Hi: Byte);
  end;

{ TSilFtpClient }

constructor TSilFtpClient.Create(const Command: IFtpClientCommand);
begin
  inherited Create;
  FCommand := Command;
end;

destructor TSilFtpClient.Destroy;
begin
  FCommand := nil;
  inherited;
end;

function TSilFtpClient.AbortTransfer: Boolean;
begin
  Result := FCommand.AbortTransfer(FLastResponse) in [225, 226];
end;

function TSilFtpClient.Allocate(Size, Pages: LongWord): Boolean;
begin
  Result := FCommand.Allocate(Size, Pages, FLastResponse) = 200;
end;

function TSilFtpClient.ChangeDir(const Path: String): Boolean;
begin
  Result := FCommand.ChangeDir(Path, FLastResponse) = 250;
end;

function TSilFtpClient.ChangeDirToParent: Boolean;
begin
  Result := FCommand.ChangeDirToParent(FLastResponse) = 200;
end;

function TSilFtpClient.CurrentDir: String;
begin
  if FCommand.CurrentDir(FLastResponse) = 257 then
    Result := FLastResponse else
    Result := '';
end;

function TSilFtpClient.DeleteFile(const Path: String): Boolean;
begin
  Result := FCommand.DeleteFile(Path, FLastResponse) = 250;
end;

function TSilFtpClient.Login(const UserName, Password: String; const AccountInfo: String): Boolean;
begin
  Result := false;

  if FCommand.ReadLn(FLastResponse) = 220 then
    case FCommand.User(UserName, FLastResponse) of
      230:  Result := true;
      331:  Result := FCommand.Password(Password, FLastResponse) in [220, 230];
      332:  Result := FCommand.AccountInfo(AccountInfo, FLastResponse) in [220, 230];
    end;
end;

function TSilFtpClient.MakeDir(const Path: String): Boolean;
begin
  Result := FCommand.MakeDir(Path, FLastResponse) = 257;
end;

function TSilFtpClient.Noop: Boolean;
begin
  Result := FCommand.Noop(FLastResponse) = 200;
end;

procedure TSilFtpClient.PassiveMode(Enabled: Boolean);
begin
  FPassiveMode := Enabled;
end;

function TSilFtpClient.Quit: Boolean;
begin
  Result := FCommand.Quit(FLastResponse) = 221;
end;

function TSilFtpClient.Reinitialize: Boolean;
begin
  Result := FCommand.CurrentDir(FLastResponse) = 220;
end;

function TSilFtpClient.RemoveDir(const Path: String): Boolean;
begin
  Result := FCommand.RemoveDir(Path, FLastResponse) = 250;
end;

function TSilFtpClient.Representation(Code: TFtpTypeCode): Boolean;
begin
  Result := FCommand.TypeCode(Code, FLastResponse) = 200;
end;

function TSilFtpClient.SiteParameters: String;
begin
  if FCommand.SiteParameters(FLastResponse) = 200 then
    Result := FLastResponse else
    Result := '';
end;

function TSilFtpClient.Status(const Path: String): String;
begin
  if FCommand.Status(Path, FLastResponse) in [211, 212, 213] then
    Result := FLastResponse else
    Result := '';
end;

function TSilFtpClient.Structure(Code: TFtpStructCode): Boolean;
begin
  Result := FCommand.Structure(Code, FLastResponse) = 200;
end;

function TSilFtpClient.System: String;
begin
  if FCommand.System(FLastResponse) = 215 then
    Result := FLastResponse else
    Result := '';
end;

function TSilFtpClient.TransferMode(Mode: TFtpTransferMode): Boolean;
begin
  Result := FCommand.TransferMode(Mode, FLastResponse) = 200;
end;

function TSilFtpClient.FileSize(const Path: String): LongWord;
begin
  if FCommand.FileSize(Path, FLastResponse) = 213 then
    Result := Str.ToInt(FLastResponse, 0) else
    Result := 0;
end;

function TSilFtpClient.RenameFile(const OldPath, NewPath: String): Boolean;
begin
  Result :=
    (FCommand.RenameFileFrom(OldPath, FLastResponse) = 350) and
    (FCommand.RenameFileTo(NewPath, FLastResponse) = 250);
end;

function TSilFtpClient.Help(const Command: String): String;
begin
  if FCommand.Help(Command, FLastResponse) in [211, 214] then
    Result := FLastResponse else
    Result := '';
end;

function TSilFtpClient.List(const Path: String): Boolean;
begin
  try
    FDataOp := doText;
    Result :=
      DoBeginDataConnection(Path) and
      (FCommand.List(Path, FLastResponse) = 150) and
      DoEndDataConnection;
  finally
    DoCheckThread;
  end;
end;

function TSilFtpClient.NameList(const Path: String): Boolean;
begin
  try
    FDataOp := doText;
    Result :=
      DoBeginDataConnection(Path) and
      (FCommand.NameList(Path, FLastResponse) = 150) and
      DoEndDataConnection;
  finally
    DoCheckThread;
  end;
end;

function TSilFtpClient.Retrieve(const Path: String; const Stream: IRandomStream; Marker: LongWord): Boolean;
begin
  try
    FDataOp := doRetrieve;
    FFileStream := Stream;
    
    Result :=
      DoBeginDataConnection(Path) and
      ((Marker = 0) or (FCommand.Restart(Marker, FLastResponse) = 350)) and
      (FCommand.Retrieve(Path, FLastResponse) in [125, 150]) and
      DoEndDataConnection;
  finally
    DoCheckThread;
  end;
end;

function TSilFtpClient.Store(const Path: String; const Stream: IRandomStream; Marker: LongWord): Boolean;
begin
  try
    FDataOp := doSend;
    FFileStream := Stream;
    
    Result :=
      DoBeginDataConnection(Path) and
      ((Marker = 0) or (FCommand.Restart(Marker, FLastResponse) = 350)) and
      (FCommand.Store(Path, FLastResponse) in [125, 150]) and
      DoEndDataConnection;
  finally
    DoCheckThread;
  end;
end;

function TSilFtpClient.Append(const Path: String; const Stream: IRandomStream): Boolean;
begin
  try
    FDataOp := doSend;
    FFileStream := Stream;
    
    Result :=
      DoBeginDataConnection(Path) and
      (FCommand.Append(Path, FLastResponse) in [125, 150]) and
      DoEndDataConnection;
  finally
    DoCheckThread;
  end;
end;

function TSilFtpClient.DoBeginDataConnection(const Path: String): Boolean;
var
  bHandled: Boolean;
begin
  Result := false;
  bHandled := false;
  FDataStream := nil;
  FDataPath := Path;

  if not FPassiveMode and not DoFirePortListen(bHandled) then Exit else
  if not bHandled and not DoFirePassiveConnect(bHandled) then Exit;

  Result := bHandled;
  if Result then FThread := OS.Thread.Spawn('SilFtpClientThread', Self);
end;

function TSilFtpClient.DoEndDataConnection: Boolean;
begin
  Result := (FCommand.ReadLn(FLastResponse) in [226, 250]);
  FListener := nil;
end;

procedure TSilFtpClient.Run(const Sender: IThread);
begin
  if FDataMode = dmPort then
    DoFirePortAccept;

  case FDataOp of
    doText:     DoReadText;
    doRetrieve: DoReadData;
    doSend:     DoWriteData;
  end;
end;

procedure TSilFtpClient.DoReadText;
var
  Reader: IReader;
  sLine: String;
begin
  Reader := Tk.StreamReader(FDataStream);

  while Reader.ReadLn(sLine) do
    DoFireList(sLine);
end;

procedure TSilFtpClient.DoReadData;
var
  lwSize: LongWord;
  sBuf: String;
begin
  SetLength(sBuf, 8192);

  while true do
  begin
    lwSize := FDataStream.Read(sBuf[1], Length(sBuf));
    DoFireFileRetrieve;

    if lwSize > 0 then
      FFileStream.Write(sBuf[1], lwSize) else
      Break;
  end;

  FFileStream := nil;
end;

procedure TSilFtpClient.DoWriteData;
var
  lwSize: LongWord;
  sBuf: String;
begin
  SetLength(sBuf, 8192);

  while true do
  begin
    lwSize := FFileStream.Read(sBuf[1], Length(sBuf));
    DoFireFileSend;

    if lwSize > 0 then
      FDataStream.Write(sBuf[1], lwSize) else
      Break;
  end;

  FFileStream := nil;
  FDataStream := nil;
end;

procedure TSilFtpClient.DoCheckThread;
begin
  if FThread <> nil then
  begin
    FThread.Termination.WaitFor(INFINITE);
    //FThread.Detach;
    //Sil.OS.Wait.Single(FThread, INFINITE, true);
    FThread := nil;
  end;
end;

procedure TSilFtpClient.DoFireList(const Line: String);
var
  Enum: IEnumerator;
  Sink: IFtpClientEvents;
  Event: RFtpClientListEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Line := Line;
    Event.Path := FDataPath;

    while Events.Enumerate(Enum, Sink, IFtpClientEvents) do
      Sink.OnList(Event);
  end;
end;

procedure TSilFtpClient.DoFireFileRetrieve;
var
  Enum: IEnumerator;
  Sink: IFtpClientEvents;
  Event: RFtpClientFileEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Path := FDataPath;
    Event.Stream := FFileStream;

    while Events.Enumerate(Enum, Sink, IFtpClientEvents) do
      Sink.OnFileRetrieve(Event);
  end;
end;

procedure TSilFtpClient.DoFireFileSend;
var
  Enum: IEnumerator;
  Sink: IFtpClientEvents;
  Event: RFtpClientFileEvent;
begin
  if HasConnections then
  begin
    Event.Sender := Self;
    Event.Path := FDataPath;
    Event.Stream := FFileStream;

    while Events.Enumerate(Enum, Sink, IFtpClientEvents) do
      Sink.OnFileSend(Event);
  end;
end;

function TSilFtpClient.DoFirePortListen(var Handled: Boolean): Boolean;
var
  Enum: IEnumerator;
  Sink: IFtpClientConnectionEvents;
  Event: RFtpClientPortListen;
  bFired: Boolean;
begin
  bFired := false;
  Event.Sender := Self;
  Event.Address := '';
  Event.Port := 0;
  Event.Listener := nil;

  if HasConnections then
    while Events.Enumerate(Enum, Sink, IFtpClientConnectionEvents) do
    begin
      bFired := true;
      Sink.OnPortListen(Event);
    end;

  if not bFired then
    OnPortListen(Event);

  if Str.NotEmpty(Event.Address) and (Event.Port <> 0) and (Event.Listener <> nil) then
  begin
    FDataMode := dmPort;
    Result := FCommand.Port(Event.Address, Event.Port, FLastResponse) = 200;
    FListener := Event.Listener;
    Handled := true;
  end else
    Result := true;
end;

function TSilFtpClient.DoFirePortAccept: Boolean;
var
  Enum: IEnumerator;
  Sink: IFtpClientConnectionEvents;
  Event: RFtpClientPortAccept;
  bFired: Boolean;
begin
  bFired := false;
  Event.Sender := Self;
  Event.Listener := FListener;
  Event.DataStream := nil;

  if HasConnections then
    while Events.Enumerate(Enum, Sink, IFtpClientConnectionEvents) do
    begin
      bFired := true;
      Sink.OnPortAccept(Event);
    end;

  if not bFired then
    OnPortAccept(Event);

  FDataStream := Event.DataStream;
  Result := FDataStream <> nil;
end;

function TSilFtpClient.DoFirePassiveConnect(var Handled: Boolean): Boolean;
var
  Enum: IEnumerator;
  Sink: IFtpClientConnectionEvents;
  Event: RFtpClientPassiveConnect;
  sPassive: String;
  bFired: Boolean;
begin
  bFired := false;
  Result := FCommand.Passive(sPassive) = 227;

  if Result then
  begin
    Event.Sender := Self;
    Event.DataStream := nil;
    DoGetPassiveResult(sPassive, Event.Address, Event.Port);

    if HasConnections then
      while Events.Enumerate(Enum, Sink, IFtpClientConnectionEvents) do
      begin
        bFired := true;
        Sink.OnPassiveConnect(Event);
      end;

    if not bFired then
      OnPassiveConnect(Event);

    FDataStream := Event.DataStream;
    Handled := FDataStream <> nil;

    if Handled then FDataMode := dmPassive;
  end;
end;

function TSilFtpClient.DoGetPassiveResult(var Buffer, Address: String; var Port: Word): Boolean;
var
  i, iIdx: Integer;
  sLine, sItem: String;
  wPort: RWord absolute Port;
begin
  i := 0;
  iIdx := 1;
  Str.Between(Buffer, '(', ')', sLine);

  try
    repeat
      Inc(iIdx);
      sItem := Str.Trim(Str.Token(sLine, ',', i));

      if Str.NotEmpty(sItem) then
      begin
        if iIdx < 5 then Address := Address + sItem + '.' else
        if iIdx < 6 then Address := Address + sItem else
        if iIdx < 7 then wPort.Hi := Str.ToInt(sItem, 0) else
        if iIdx < 8 then wPort.Lo := Str.ToInt(sItem, 0);
      end;
    until i = 0;
  except
    Address := '';
    Port := 0;
  end;

  Result := Str.NotEmpty(Address) and (Port > 0);
end;

procedure TSilFtpClient.OnPassiveConnect(var Event: RFtpClientPassiveConnect);
var
  Client: ISocketClient;
begin
  try
    Client := OS.Socket.CreateClient(stStream, spTcp, Event.Address, Event.Port);

    if Ref.GetInterface(Client, IStream, Event.DataStream) then
      Client.Connect;
  except
    Event.DataStream := nil;
  end;
end;

procedure TSilFtpClient.OnPortListen(var Event: RFtpClientPortListen);
var
  Server: ISocketServer;
begin
  try
    Server := OS.Socket.CreateServer(stStream, spTcp, OS.Socket.Host.LocalName, 0);
    Server.Listen;

    Event.Address := OS.Socket.IP.ToStr(Server.Info.Local.Address);
    Event.Port := Server.Info.Local.Port;
    Event.Listener := Server;
  except
    Event.Listener := nil;
  end;
end;

procedure TSilFtpClient.OnPortAccept(var Event: RFtpClientPortAccept);
var
  Server: ISocketServer;
  Client: ISocketClient;
begin
  if Ref.GetInterface(Event.Listener, ISocketServer, Server) and Server.Accept(Client) then
    Ref.GetInterface(Client, IStream, Event.DataStream);
end;

end.
