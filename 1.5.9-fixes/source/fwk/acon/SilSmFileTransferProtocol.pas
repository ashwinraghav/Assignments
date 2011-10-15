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

unit SilSmFileTransferProtocol;

{$I Defines.inc}

interface

uses
  Sil,
  SilTool,
  SilSiAbstractConnection,
  SilSiSocketConnection,
  SilSiFileTransferProtocol,
  SilSmTextCommandProtocol;

// rfc959

type
  TFtpState = (
    psNone,
    psConnected,
    psPassive,
    psAbortTransfer,
    psAllocate,
    psRestart,
    psAppend,
    psChangeDir,
    psChangeDirToParent,
    psCurrentDir,
    psDeleteFile,
    psHelp,
    psList,
    psUserName,
    psPassword,
    psMakeDir,
    psNameList,
    psNoop,
    psQuit,
    psReinitialize,
    psRemoveDir,
    psRenameFileFrom,
    psRenameFileTo,
    psType,
    psRetrieve,
    psSiteParameters,
    psStatus,
    psStore,
    psStructure,
    psSystem,
    psTransferMode,
    psPort,
    psFileSize);

  TFileTransferProtocol = class (
    // extends
    TTextCommandProtocol,
    // implements
    IFtpClient,
    IFtpServer,
    ISocketServerEvents)
  private
    FDataConnection: IAbstractConnection;
    FLines: IStringList;
    FStream: IRandomStream;
    FFileName: String;
    FMarker: LongWord;
    FDataState: TFtpState;
    FDataEvent: IEvent;
    FControlEvent: IEvent;
    FListenEvent: IEvent;
    FSocketServer: IServerSocketConnection;
  private
    procedure DoClearConnection;
    procedure DoConnectToServer(var Event: TFtpConnectEvent);
    procedure DoListenToServer(var Event: TFtpListenEvent);
    function DoExtractInto(const BegStr, EndStr: String): String;
    function DoExtractText: String;
    procedure DoExtractPassive(var Passive: TFtpHostAddress);
    function DoCheckDAC: Boolean;
    procedure DoCheckServer;
    function FireDataConnection: Boolean;
    procedure DoReceiveData(Buffer: PChar; Size: LongWord);
    function GetWaitState: TFtpState;
    function DoCommand(State: TFtpState; const Buffer: String = ''): Boolean;
    function DoCheckResult(Code: Word; const Values: array of Word): Boolean;
    procedure DoLinesAdd(const Line: String);
    procedure DoLinesBegin;
    procedure DoLinesEnd(State: TFtpState);
    procedure DoRetrieveData(Buffer: PChar; Size: LongWord);
    procedure DoTransferBegin;
    procedure DoTransferSend;
    procedure DoTransferEnd(State: TFtpState);
    procedure DoStateTransfer(State: TFtpState = psNone; const Path: String = ''; const Stream: IRandomStream = nil; Marker: LongWord = 0);
    function DoPort(const Address: String; Port: LongWord): Boolean;
  protected
    procedure ReceiveLine; override;
  protected // IProtocol
    procedure SetConnection(const Value: IAbstractConnection); override;
  protected // IConnectionEvents
    procedure OnDataReceived(const Event: TConnectionDataEvent); override;
    procedure OnDisconnected(const Event: TConnectionBreakEvent); override;
  protected // ISocketServerEvents
    procedure OnListen(const Event: TSocketConnectionEvent);
    procedure OnListenFailed(const Event: TSocketConnectionEvent);
  protected // IConnectedEvents
    procedure OnDataSent(const Event: TConnectionDataEvent); override;
  protected // IFtpClient
    function GetDataConnection: IAbstractConnection;
    procedure SetDataConnection(const Value: IAbstractConnection);
    function Login(const UserName, Password: String): Boolean;
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
    function DeleteFile(const PathName: String): Boolean;
    function RemoveDir(const PathName: String): Boolean;
    function MakeDir(const PathName: String): Boolean;
    function CurrentDir: String;
    function FileSize(const PathName: String): LongWord;
    function List(const PathName: String = ''): Boolean;
    function NameList(const PathName: String = ''): Boolean;
    function SiteParameters: String;
    function System: String;
    function Status(const PathName: String = ''): String;
    function Help(const Command: String = ''): String;
    function Noop: Boolean;
    procedure WaitTransferComplete;
  public
    constructor Create(const Connection: IAbstractConnection); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilStSocketConnection,
  SilSmTextLineCompleter;

const
  ACmd: array [TFtpState] of String = ('', '',
    'PASV', 'ABOR', 'ALLO', 'REST', 'APPE', 'CWD', 'CDUP', 'PWD',
    'DELE', 'HELP', 'LIST', 'USER', 'PASS', 'MKD', 'NLST', 'NOOP',
    'QUIT', 'REIN', 'RMD', 'RNFR', 'RNTO', 'TYPE', 'RETR', 'SITE',
    'STAT', 'STOR', 'STRU', 'SYST', 'MODE', 'PORT', 'SIZE');

type
  TResultCode = (
    rc110, rc120, rc125, rc150, rc200, rc202, rc211, rc212, rc213, rc214, rc215,
    rc220, rc221, rc225, rc226, rc227, rc230, rc250, rc257, rc331, rc332, rc350,
    rc421, rc425, rc426, rc450, rc451, rc452, rc500, rc501, rc502, rc503, rc504,
    rc530, rc532, rc550, rc551, rc552, rc553);

  TResultMessage = record
    Code: Word;
    Text: String;
  end;

const
  AResult: array [TResultCode] of TResultMessage = (
    (Code: 110; Text: 'Restart marker reply.'),
    (Code: 120; Text: 'Service ready in %d minutes.'),
    (Code: 125; Text: 'Data connection already open; transfer starting.'),
    (Code: 150; Text: 'File status okay; about to open data connection.'),
    (Code: 200; Text: 'Command okay.'),
    (Code: 202; Text: 'Command not implemented, superfluous at this site.'),
    (Code: 211; Text: 'System status, or system help reply.'),
    (Code: 212; Text: 'Directory status.'),
    (Code: 213; Text: 'File status.'),
    (Code: 214; Text: 'Help message.'),
    (Code: 215; Text: '%s system type.'),
    (Code: 220; Text: '%s. Service ready.'),
    (Code: 221; Text: 'Service closing control connection.'),
    (Code: 225; Text: 'Data connection open; no transfer in progress.'),
    (Code: 226; Text: 'Closing data connection. Requested file action successful.'),
    (Code: 227; Text: 'Entering Passive Mode (%d,%d,%d,%d,%d,%d).'),
    (Code: 230; Text: 'User logged in, proceed.'),
    (Code: 250; Text: 'Requested file action okay, completed.'),
    (Code: 257; Text: '"%s" %s.'), // pathname
    (Code: 331; Text: 'User name okay, need password.'),
    (Code: 332; Text: 'Need account for login.'),
    (Code: 350; Text: 'Requested file action pending further information.'),
    (Code: 421; Text: 'Service not available, closing control connection.'),
    (Code: 425; Text: 'Can''t open data connection.'),
    (Code: 426; Text: 'Connection closed; transfer aborted.'),
    (Code: 450; Text: 'Requested file action not taken.'),
    (Code: 451; Text: 'Requested action aborted: local error in processing.'),
    (Code: 452; Text: 'Requested action not taken. Insufficient storage space in system.'),
    (Code: 500; Text: 'Syntax error, command unrecognized.'),
    (Code: 501; Text: 'Syntax error in parameters or arguments.'),
    (Code: 502; Text: 'Command not implemented.'),
    (Code: 503; Text: 'Bad sequence of commands.'),
    (Code: 504; Text: 'Command not implemented for that parameter.'),
    (Code: 530; Text: 'Not logged in.'),
    (Code: 532; Text: 'Need account for storing files.'),
    (Code: 550; Text: 'Requested action not taken. File unavailable (e.g., file not found, no access).'),
    (Code: 551; Text: 'Requested action aborted: page type unknown.'),
    (Code: 552; Text: 'Requested file action aborted. Exceeded storage allocation.'),
    (Code: 553; Text: 'Requested action not taken. File name not allowed.'));

const
  ACode: array [TFtpTypeCode] of Char = ('A', 'E', 'I', 'L');
  AStructCode: array [TFtpStructCode] of Char = ('F', 'R', 'P');
  ATransferMode: array [TFtpTransferMode] of Char = ('S', 'B', 'C');

constructor TFileTransferProtocol.Create(const Connection: IAbstractConnection);
begin
  inherited Create(Connection);

  FDataEvent := Sil.OS.Ipc.Event;
  FControlEvent := Sil.OS.Ipc.Event;
  FListenEvent := Sil.OS.Ipc.Event(true, true);
  FDataConnection := nil;
  FState := Ord(psNone);
end;

destructor TFileTransferProtocol.Destroy;
begin
  DoClearConnection;
  inherited;
end;

procedure TFileTransferProtocol.DoClearConnection;
begin
  if FDataConnection <> nil then
  begin
    FDataConnection.Disconnect;
    FDataConnection := nil;
  end;

  if FConnection <> nil then
  begin
    FConnection.Disconnect;
    FConnection := nil;
  end;

  DoCheckServer;
end;

function TFileTransferProtocol.GetWaitState: TFtpState;
begin
  Result := TFtpState(FWaitState);
end;

function TFileTransferProtocol.DoCommand(State: TFtpState; const Buffer: String): Boolean;
begin
  Result := SendCommand(ACmd[State] + Str.IIf(Str.NotEmpty(Buffer), ccSpc + Buffer), Ord(State));
end;

procedure TFileTransferProtocol.DoLinesBegin;
begin
  FLines := Sil.List.StringList;
  if FDataConnection <> nil then FDataConnection.PacketCompletion := TTextLineCompleter.Create;
end;

procedure TFileTransferProtocol.DoLinesAdd(const Line: String);
var
  e: IEnumerator;
  Adapter: IFtpClientEvents;
  Event: TFtpListEvent;
begin
  if FEvents = nil then Exit;

  Event.Sender := Self;
  Event.Line := Str.Trim(Line);

  while FEvents.Enumerate(e, Adapter, IFtpClientEvents) do
    case FDataState of
      psList:     Adapter.OnList(Event);
      psNameList: Adapter.OnListNames(Event);
    end;
end;

procedure TFileTransferProtocol.DoLinesEnd(State: TFtpState);
begin
  DoLinesAdd(Str.Null);
  DoStateTransfer;
end;

procedure TFileTransferProtocol.DoTransferBegin;
begin
  if FDataConnection <> nil then FDataConnection.PacketCompletion := nil;
end;

procedure TFileTransferProtocol.DoTransferSend;
var
  Buf: String;
  dwSize: LongWord;
  bOk: Boolean;
begin
  if (FStream <> nil) and (FDataConnection <> nil) then
  begin
    dwSize := FDataConnection.BufferSize;
    SetLength(Buf, dwSize);

    repeat
      dwSize := FStream.Read(Buf[1], dwSize);
      bOk := (dwSize > 0) and FDataConnection.IsConnected;

      if bOk then
      begin
        FDataConnection.Write(Buf[1], dwSize);
        FDataConnection.Disconnect;
      end;
    until not bOk;
  end;

  DoStateTransfer;
end;

procedure TFileTransferProtocol.DoTransferEnd(State: TFtpState);
var
  e: IEnumerator;
  Adapter: IFtpClientEvents;
  Event: TFtpFileTransferEvent;
begin
  if FEvents <> nil then
  begin
    Event.Sender := Self;
    Event.FileName := FFileName;
    Event.Marker := FMarker;
    Event.Stream := FStream;

    while FEvents.Enumerate(e, Adapter, IFtpClientEvents) do
      case State of
        psAppend,
        psStore:    Adapter.OnFileSent(Event);
        psRetrieve: Adapter.OnFileRetrieved(Event);
      end;
  end;

  DoStateTransfer;
end;

procedure TFileTransferProtocol.DoRetrieveData(Buffer: PChar; Size: LongWord);
begin
  if FStream <> nil then FStream.Write(Buffer^, Size);
end;

procedure TFileTransferProtocol.ReceiveLine;
var
  wCode: Word;
  bOk: Boolean;
begin
  wCode := Str.ToInt(Str.Left(FBuffer, 3), 0);
  FError := wCode >= 400;
  if FError then Exit;

  case GetWaitState of
    psConnected:          bOk := DoCheckResult(wCode, [220]);
    psUserName:           bOk := DoCheckResult(wCode, [230, 331]);

    psPassword:
    begin
      bOk := DoCheckResult(wCode, [230, 202]);
      FIsLogged := bOk;
    end;

    psPassive:            bOk := DoCheckResult(wCode, [227]);
    psAbortTransfer:      bOk := DoCheckResult(wCode, [225, 226]);
    psAllocate:           bOk := DoCheckResult(wCode, [200, 202]);
    psRestart:            bOk := DoCheckResult(wCode, [350]);
    psChangeDir:          bOk := DoCheckResult(wCode, [250]);
    psChangeDirToParent:  bOk := DoCheckResult(wCode, [200]);
    psCurrentDir:         bOk := DoCheckResult(wCode, [257]);
    psDeleteFile:         bOk := DoCheckResult(wCode, [250]);
    psHelp:               bOk := DoCheckResult(wCode, [211, 214]); // sysinfo, help

    psList,
    psNameList:
    begin
      bOk := DoCheckResult(wCode, [125, 150]); // start
      if not bOk then
      begin
        bOk := DoCheckResult(wCode, [226, 250]); // finish
        if bOk then
        begin
          if FDataEvent.IsSignaled then DoLinesEnd(GetWaitState);
          FControlEvent.Signal;
        end;
      end;
    end;

    psMakeDir:            bOk := DoCheckResult(wCode, [257]);
    psNoop:               bOk := DoCheckResult(wCode, [200]);

    psQuit:
    begin
      bOk := DoCheckResult(wCode, [221]);
      FIsLogged := not bOk;
    end;

    psReinitialize:       bOk := DoCheckResult(wCode, [220]);
    psRemoveDir:          bOk := DoCheckResult(wCode, [250]);
    psRenameFileFrom:     bOk := DoCheckResult(wCode, [350]);
    psRenameFileTo:       bOk := DoCheckResult(wCode, [250]);
    psType:               bOk := DoCheckResult(wCode, [200]);
    psSiteParameters:     bOk := DoCheckResult(wCode, [200, 214]);
    psStatus:             bOk := DoCheckResult(wCode, [211, 212, 213]);

    psRetrieve,
    psAppend,
    psStore:
    begin
      bOk := DoCheckResult(wCode, [125, 150]); // start
      if not bOk then
      begin
        bOk := DoCheckResult(wCode, [226, 250]); // finish
        if bOk then
        begin
          if FDataEvent.IsSignaled then DoTransferEnd(GetWaitState);
          FControlEvent.Signal;
        end;
      end else
      if FDataState = psStore then DoTransferSend;
    end;

    psStructure:          bOk := DoCheckResult(wCode, [200]);
    psSystem:             bOk := DoCheckResult(wCode, [215]);
    psTransferMode:       bOk := DoCheckResult(wCode, [200]);
    psPort:               bOk := DoCheckResult(wCode, [200]);
    psFileSize:           bOk := DoCheckResult(wCode, [213]);
    else                  bOk := false;
  end;

  if bOk then FState := FWaitState;
end;

procedure TFileTransferProtocol.DoStateTransfer(State: TFtpState; const Path: String; const Stream: IRandomStream; Marker: LongWord);
begin
  FDataState := State;
  FFileName := Path;
  FMarker := Marker;
  FStream := Stream;
  if FMarker > 0 then FStream.Position := FMarker;
  DoCheckServer;
end;

procedure TFileTransferProtocol.DoReceiveData(Buffer: PChar; Size: LongWord);
begin
  case FDataState of
    psList,
    psNameList:   DoLinesAdd(Str.Make(Buffer, Size));
    psAppend,
    psStore,
    psRetrieve:   DoRetrieveData(Buffer, Size);
  end;
end;

procedure TFileTransferProtocol.DoExtractPassive(var Passive: TFtpHostAddress);
var
  i, iIdx: Integer;
  sLine, sItem: String;
begin
  i := 0;
  iIdx := 1;
  sLine := DoExtractInto('(', ')');
  Passive.Address := '';
  Passive.Port := 0;

  try
    repeat
      Inc(iIdx);
      sItem := Str.Trim(Str.Token(sLine, ',', i));

      if Str.NotEmpty(sItem) then
      begin
        if iIdx < 5 then Passive.Address := Passive.Address + sItem + '.' else
        if iIdx < 6 then Passive.Address := Passive.Address + sItem else
        if iIdx < 7 then Passive.PortHi := Str.ToInt(sItem, 0) else
        if iIdx < 8 then Passive.PortLo := Str.ToInt(sItem, 0);
      end;
    until i = 0;
  except
    Passive.Address := '';
    Passive.Port := 0;
  end;
end;

function TFileTransferProtocol.DoExtractInto(const BegStr, EndStr: String): String;
var
  iBeg, iEnd: Integer;
begin
  iBeg := Str.Pos(BegStr, FBuffer);
  iEnd := Str.Pos(EndStr, FBuffer, iBeg + 1) - iBeg - 1;

  if (iBeg > 0) and (iEnd > 0) then
    Result := Str.Copy(FBuffer, iBeg + Length(BegStr), iEnd) else
    Result := Str.Null;
end;

function TFileTransferProtocol.DoExtractText: String;
var
  i: Integer;
  sFind: String;
begin
  if Str.Pos('214', FBuffer) > 0 then sFind := '214' else
  if Str.Pos('211', FBuffer) > 0 then sFind := '211' else sFind := Str.Null;
  if Length(sFind) > 0 then while Str.GetPos(sFind, FBuffer, 1, i) do Str.Delete(FBuffer, i, 4);
end;

function TFileTransferProtocol.DoCheckDAC: Boolean;
begin
  FDataEvent.Reset;
  FControlEvent.Reset;

  Result :=
    (FConnection <> nil) and FConnection.IsConnected and
    (((FDataConnection <> nil) and FDataConnection.IsConnected) or FireDataConnection);
end;

function TFileTransferProtocol.FireDataConnection: Boolean;
var
  e: IEnumerator;
  Adapter: IFtpClientSocketEvents;
  Event1: TFtpListenEvent;
  Event2: TFtpConnectEvent;
  Passive: TFtpHostAddress;
begin
  Event1.Sender := Self;
  Event1.Address := Str.Null;
  Event1.Port := 0;

  if FEvents <> nil then
    while FEvents.Enumerate(e, Adapter, IFtpClientSocketEvents) do Adapter.OnListenToServer(Event1) else
    DoListenToServer(Event1);

  Result := (Event1.Port <> 0) and Str.NotEmpty(Event1.Address);

  if not Result and DoCommand(psPassive) then
  begin
    DoExtractPassive(Passive);
    Event2.Address := Passive.Address;
    Event2.Port := Passive.Port;

    if FEvents <> nil then
      while FEvents.Enumerate(e, Adapter, IFtpClientSocketEvents) do Adapter.OnConnectToServer(Event2) else
      DoConnectToServer(Event2);

    Result := (Event2.Connection <> nil) and Event2.Connection.IsConnected;
    if Result then SetDataConnection(Event2.Connection);
  end else
    Result := DoPort(Event1.Address, Event1.Port);
end;

function TFileTransferProtocol.DoPort(const Address: String; Port: LongWord): Boolean;
var
  HostPort: TFtpHostAddress;
  sHostPort: String;
begin
  HostPort.Port := Port;
  sHostPort :=
    Str.Replace(Address, '.', ',') +
    ',' + Int.ToStr(HostPort.PortHi) +
    ',' + Int.ToStr(HostPort.PortLo);
  Result := DoCommand(psPort, sHostPort);
end;

function TFileTransferProtocol.AbortTransfer: Boolean;
begin
  Result := DoCommand(psAbortTransfer);
end;

function TFileTransferProtocol.Allocate(Size, Pages: LongWord): Boolean;
begin
  Result := DoCheckDAC and
    DoCommand(psAllocate, Int.ToStr(Size) + Str.IIf(Pages > 0, ' R ' + Int.ToStr(Pages)));
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.Append(const Path: String; const Stream: IRandomStream): Boolean;
begin
  DoStateTransfer(psAppend, Path, Stream, 0);
  Result := DoCheckDAC and DoCommand(psAppend, Path);
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.ChangeDir(const Path: String): Boolean;
begin
  Result := DoCommand(psChangeDir, Path);
end;

function TFileTransferProtocol.ChangeDirToParent: Boolean;
begin
  Result := DoCommand(psChangeDirToParent);
end;

function TFileTransferProtocol.CurrentDir: String;
begin
  if DoCommand(psCurrentDir) then
    Result := DoExtractInto('"', '"') else
    Result := Str.Null;
end;

function TFileTransferProtocol.DeleteFile(const PathName: String): Boolean;
begin
  Result := DoCommand(psDeleteFile, PathName);
end;

function TFileTransferProtocol.Help(const Command: String): String;
begin
  if DoCommand(psHelp) then
    Result := DoExtractText else
    Result := Str.Null;
end;

function TFileTransferProtocol.List(const PathName: String): Boolean;
begin
  DoStateTransfer(psList);
  Result := DoCheckDAC and DoCommand(psList, PathName);
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.Login(const UserName, Password: String): Boolean;
begin
  Result :=
    DoCommand(psConnected) and
    DoCommand(psUserName, UserName) and
    DoCommand(psPassword, Password);
end;

function TFileTransferProtocol.MakeDir(const PathName: String): Boolean;
begin
  Result := DoCommand(psMakeDir, PathName);
end;

function TFileTransferProtocol.FileSize(const PathName: String): LongWord;
begin
  if DoCommand(psFileSize, PathName) then
    Result := Str.ToInt(Str.Copy(FBuffer, 5, -3), 0) else
    Result := 0;
end;

function TFileTransferProtocol.NameList(const PathName: String): Boolean;
begin
  DoStateTransfer(psNameList);
  Result := DoCheckDAC and DoCommand(psNameList, PathName);
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.Noop: Boolean;
begin
  Result := DoCommand(psNoop);
end;

procedure TFileTransferProtocol.WaitTransferComplete;
begin
  while (not FControlEvent.IsSignaled) and FConnection.IsConnected do WaitDataInput;
  Sil.OS.Wait.Single(FDataEvent, INFINITE);
end;

function TFileTransferProtocol.Quit: Boolean;
begin
  Result := DoCommand(psQuit);
  DoClearConnection;
end;

function TFileTransferProtocol.Reinitialize: Boolean;
begin
  Result := DoCommand(psReinitialize);
end;

function TFileTransferProtocol.RemoveDir(const PathName: String): Boolean;
begin
  Result := DoCommand(psRemoveDir, PathName);
end;

function TFileTransferProtocol.RenameFile(const OldPath, NewPath: String): Boolean;
begin
  Result :=
    DoCommand(psRenameFileFrom, OldPath) and
    DoCommand(psRenameFileTo, NewPath);
end;

function TFileTransferProtocol.Representation(Code: TFtpTypeCode): Boolean;
begin
  Result := DoCommand(psType, ACode[Code]);
end;

function TFileTransferProtocol.Retrieve(const Path: String; const Stream: IRandomStream; Marker: LongWord): Boolean;
begin
  DoStateTransfer(psRetrieve, Path, Stream, Marker);
  Result := DoCheckDAC and ((Marker < 1) or DoCommand(psRestart, Int.ToStr(Marker))) and DoCommand(psRetrieve, Path);
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.SiteParameters: String;
begin
  if DoCommand(psSiteParameters) then
    Result := DoExtractText else
    Result := Str.Null;
end;

function TFileTransferProtocol.Status(const PathName: String): String;
begin
  if DoCommand(psStatus, PathName) then
    Result := DoExtractText else
    Result := Str.Null;
end;

function TFileTransferProtocol.Store(const Path: String; const Stream: IRandomStream; Marker: LongWord): Boolean;
begin
  DoStateTransfer(psStore, Path, Stream, Marker);
  Result := DoCheckDAC and ((Marker < 1) or DoCommand(psRestart, Int.ToStr(Marker))) and
    DoCommand(psStore, Path);
  if not Result then DoCheckServer;
end;

function TFileTransferProtocol.Structure(Code: TFtpStructCode): Boolean;
begin
  Result := DoCommand(psStructure, AStructCode[Code]);
end;

function TFileTransferProtocol.System: String;
begin
  if DoCommand(psSystem) then
    Result := DoExtractText else
    Result := Str.Null;
end;

function TFileTransferProtocol.TransferMode(Mode: TFtpTransferMode): Boolean;
begin
  Result := DoCommand(psTransferMode, ATransferMode[Mode]);
end;

{var
  debugfile: ITextFile;

procedure DoWrite(const Line: String);
begin
  if debugfile = nil then
  begin
    debugfile := Sil.OS.FileSystem.OpenTextFile('D:\mariano\src\nave7\bancal\ftp.log', fmAccessReadWrite, fmShareRead);
    debugfile.Stream.Seek(0, soFromEnd);
    debugfile.Stream.WriteLn('inicio');
  end;
  debugfile.Stream.WriteStr(DateTime.ToStr(DateTime.Now, 'dd/mm hh:nn:ss ') + Line);
end;
}
procedure TFileTransferProtocol.OnDataSent(const Event: TConnectionDataEvent);
begin
  //DoWrite(Event.Buffer);
  inherited;
end;

procedure TFileTransferProtocol.OnDataReceived(const Event: TConnectionDataEvent);
begin
  if Event.Size > 0 then
    if Reference.SameObject(Event.Sender, FConnection, IAbstractConnection) then
    begin
      //DoWrite(Event.Buffer);
      inherited;
    end else
      DoReceiveData(Event.Buffer, Event.Size);
end;

procedure TFileTransferProtocol.OnDisconnected(const Event: TConnectionBreakEvent);
begin
  if Reference.SameObject(Event.Sender, FDataConnection) then
  begin
    FDataConnection.Disconnect;

    if FControlEvent.IsSignaled then
      case FDataState of
        psList,
        psNameList: DoLinesEnd(GetWaitState);
        psRetrieve,
        psAppend,                                  
        psStore:    DoTransferEnd(GetWaitState);
      end;

    FDataEvent.Signal;
  end else
  if Reference.SameObject(Event.Sender, FConnection) then
    inherited;

  Sil.Sink.Disconnect(Event.Sender, Self);
end;

function TFileTransferProtocol.DoCheckResult(Code: Word; const Values: array of Word): Boolean;
var
  i: Word;
begin
  for i := 0 to High(Values) do
  begin
    Result := Values[i] = Code;
    if Result then Exit;
  end;

  Result := false;
end;

function TFileTransferProtocol.GetDataConnection: IAbstractConnection;
begin
  Result := FDataConnection;
end;

procedure TFileTransferProtocol.SetDataConnection(const Value: IAbstractConnection);
begin
  if not Value.IsConnected then Exit;
  FDataConnection := Value;

  if FDataConnection <> nil then
  begin
    case FDataState of
      psList,
      psNameList:   DoLinesBegin;
      psAppend,
      psStore,
      psRetrieve:   DoTransferBegin;
    end;

    Sil.Sink.Connect(FDataConnection, Self);
  end;
end;

procedure TFileTransferProtocol.SetConnection(const Value: IAbstractConnection);
begin
  inherited;
  if (Value <> nil) and Value.IsConnected then Sil.Sink.Connect(Value, Self);
end;

procedure TFileTransferProtocol.DoListenToServer(var Event: TFtpListenEvent);
var
  Server: ISocketServer;
begin
  if FSocketServer = nil then
  begin
    FListenEvent.WaitFor(INFINITE, true);
    Server := Sil.OS.Socket.CreateServer(stStream, spTcp, 0, 0);

    try
      Server.Listen;

      FListenEvent.Reset;
      FSocketServer := SilStSocketConnection.SocketConnection.CreateServer(Server, false);
      Sil.Sink.Connect(FSocketServer, Self);
      FSocketServer.SpawnThread;
    except
      // log
    end;
  end;

  Event.Port := Server.Info.Local.Port;
  Event.Address := Server.Info.Local.Host;
end;

procedure TFileTransferProtocol.DoConnectToServer(var Event: TFtpConnectEvent);
var
  Socket: ISocketClient;
begin
  Socket := OS.Socket.CreateClient(stStream, spTcp, Event.Address, Event.Port);
  Socket.Connect;
  Event.Connection := SilStSocketConnection.SocketConnection.CreateClient(Socket);
end;

procedure TFileTransferProtocol.OnListen(const Event: TSocketConnectionEvent);
var
  Connection: IClientSocketConnection;
begin
  Event.Sender.Thread.Name := 'server';

  if Event.Sender.Accept(Connection) then
  begin
    SetDataConnection(Connection);
    Connection.SpawnThread;
    Connection.Thread.Name := 'client';
  end;

  FListenEvent.Signal;
end;

procedure TFileTransferProtocol.OnListenFailed(const Event: TSocketConnectionEvent);
begin
  FListenEvent.Signal;
end;

procedure TFileTransferProtocol.DoCheckServer;
begin
  if FSocketServer <> nil then
  begin
    FSocketServer.Cancel;
    FListenEvent.WaitFor(INFINITE, true);
    FSocketServer.Cleanup;
    FSocketServer := nil;
    FDataConnection := nil;
  end;
end;

end.
