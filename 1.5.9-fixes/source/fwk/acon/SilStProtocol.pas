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

unit SilStProtocol;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilSiProtocolBase,
  SilSiAbstractConnection,
  SilSiSocks5Protocol,
  SilSiMailClient,
  SilSiPop3,
  SilSiSmtp,
  SilSiFileTransferProtocol,
  SilSiBlindProtocol,
  SilSiFileAccessProtocol,
  SilSiSqlProtocol,
  SilSiEblisProtocol,
  SilSiPop3Command,
  SilSiSmtpCommand,
  SilSiAsciiProtocol,
  SilSiAsciiCommandSink,
  SilSiSharedProtocol;

type
  ProtocolType = class of ProtocolTool;
  ProtocolTool = class(Tool)
    class function Socks5Client(const Connection: IAbstractConnection = nil): ISocks5Client;
    class function AsciiCommandSink(const Command: IUnknown; const Connection: IAbstractConnection): IAsciiCommandSink;
    class function Pop3ClientCommand: IPop3ClientCommand;
    class function SmtpClientCommand: ISmtpClientCommand;
    class function Pop3Client(const Command: IPop3ClientCommand): IPop3Client; overload;
    class function Pop3Client(const Connection: IAbstractConnection): IPop3Client; overload;
    class function FtpClient(const Connection: IAbstractConnection = nil): IFtpClient;
    class function SmtpClient(const Command: ISmtpClientCommand): ISmtpClient; overload;
    class function SmtpClient(const Connection: IAbstractConnection): ISmtpClient; overload;
    class function Blind(const Connection: IAbstractConnection): IBlindProtocol;
    class function FileAccess(ID: LongWord; const Connection: IAbstractConnection; const Root: String = ''): IFileAccessProtocol;
    class function SqlClient(ID: LongWord; const Connection: IAbstractConnection = nil): IProtSqlClient;
    class function SqlServer(ID: LongWord; const Connection: IAbstractConnection = nil): IProtSqlServer;
    class function Eblis(ID: LongWord; const Connection: IAbstractConnection = nil): IEblisProtocol;
    class function Create(
        const ClassID: TGuid;
              ID: LongWord = 0;
        const Connection: IUnknown = nil;
        const Owner: IUnknown = nil;
        const Controller: IUnknown = nil): ISharedProtocol; overload;
         
    class function Create(
        const ClassID: TGuid;
        const IID: TGuid;
          out Obj;
              ID: LongWord = 0;
        const Connection: IUnknown = nil;
        const Owner: IUnknown = nil;
        const Controller: IUnknown = nil): Boolean; overload;
        
    class function Startup(
        const Instance: IUnknown;
        const IID: TGuid;
          out Obj;
              ID: LongWord = 0;
        const Connection: IUnknown = nil): Boolean; overload;

    class function Startup(
        const Instance: IUnknown;
              ID: LongWord = 0;
        const Connection: IUnknown = nil): Boolean; overload;

    class function Shutdown(
        const Instance: IUnknown): Boolean;

    class procedure Release(var Protocol);
  end;

implementation

uses
  Sil,
  SilTool,
  SilSmAsciiProtocol,
  SilSmPop3Client,
  SilSmSocks5Protocol,
  SilSmFileTransferProtocol,
  SilSmSmtpClient,
  SilSmBlindProtocol,
  SilSmFileAccessProtocol,
  SilSmSqlProtocol,
  SilSmEblisProtocol,
  SilSmPop3Command,
  SilSmSmtpCommand,
  SilSmAsciiCommandSink;

{ ProtocolTool }

class function ProtocolTool.AsciiCommandSink(const Command: IUnknown; const Connection: IAbstractConnection): IAsciiCommandSink;
begin
  Result := TAsciiCommandSink.Create(Command, Connection);
end;

class function ProtocolTool.Blind(const Connection: IAbstractConnection): IBlindProtocol;
begin
  Result := TBlindProtocol.Create(Connection);
end;

class function ProtocolTool.Create(
  const ClassID: TGuid;
  const IID: TGuid;
    out Obj;
        ID: LongWord;
  const Connection: IUnknown;
  const Owner: IUnknown;
  const Controller: IUnknown): Boolean;
var
  Unknown: IUnknown;
begin
  Unknown := SilTool.Sv.SharedObject.CreateObject(ClassID, nil, Owner, Controller);
  try
    Result := Startup(Unknown, IID, Obj, ID, Connection);
  except
    SilTool.Sv.SharedObject.ReleaseObject(Unknown);
    raise;
  end;
end;

class function ProtocolTool.Create(
  const ClassID: TGuid;
        ID: LongWord;
  const Connection: IUnknown;
  const Owner: IUnknown;
  const Controller: IUnknown): ISharedProtocol;
begin
  if not Create(ClassID, ISharedProtocol, Result, ID, Connection, Owner, Controller) then
    raise Sil.Error.Create('No existe el protocolo solicitado: %s', [Guid.ToStr(ClassID)]);
end;

class function ProtocolTool.Startup(
  const Instance: IUnknown;
  const IID: TGuid;
    out Obj;
        ID: LongWord = 0;
  const Connection: IUnknown = nil): Boolean;
begin
  Result := Assigned(Instance) and (Instance.QueryInterface(IID, Obj) = 0);
  if Result then Startup(Instance, ID, Connection);
end;

class function ProtocolTool.Startup(
  const Instance: IInterface;
        ID: LongWord;
  const Connection: IInterface): Boolean;
var
  Shared: ISharedProtocol;
  Simple: IFormatedProtocol;
  Conn: IAbstractConnection;
begin
  if Assigned(Instance) then
  begin
    if (Instance.QueryInterface(ISharedProtocol, Shared) = 0) then
      begin
        Shared.Startup(ID, Connection);
        Result := True;
      end
    else if (Instance.QueryInterface(IFormatedProtocol, Simple) = 0)
        and (Connection.QueryInterface(IAbstractConnection, Conn) = 0) then
      begin
        if ID <> 0 then Simple.ProtocolID := ID;
        Simple.Connection := Conn;
        Result := True;
      end
    else
      Result := False;
  end else
    Result := False;
end;

class function ProtocolTool.Shutdown(const Instance: IUnknown): Boolean;
var
  Shared: ISharedProtocol;
  Simple: IFormatedProtocol;
begin
  if Assigned(Instance) and (Instance.QueryInterface(ISharedProtocol, Shared) = 0) then
    try
      Shared.Shutdown();
      Result := True;
    finally
      Shared := nil;
    end
  else if Assigned(Instance) and (Instance.QueryInterface(IFormatedProtocol, Simple) = 0) then
    try
      Simple.Connection := nil;
      Result := True;
    finally
      Simple := nil;
    end
  else
    Result := False;
end;

class procedure ProtocolTool.Release(var Protocol);
var
  Unknown: IUnknown;
begin
  Unknown := IUnknown(Protocol);
  if Assigned(Unknown) then
  try
    IUnknown(Protocol) := nil;
    Shutdown(Unknown);
  finally
    SilTool.Sv.SharedObject.ReleaseObject(Unknown);
  end;
end;

class function ProtocolTool.Eblis(ID: LongWord; const Connection: IAbstractConnection): IEblisProtocol;
begin
  Result := TEblisProtocol.Create(ID, Connection);
end;

class function ProtocolTool.FileAccess(ID: LongWord; const Connection: IAbstractConnection; const Root: String): IFileAccessProtocol;
begin
  Result := TFileAccessProtocol.Create(ID, Root, Connection);
end;

class function ProtocolTool.FtpClient(const Connection: IAbstractConnection): IFtpClient;
begin
  Result := TFileTransferProtocol.Create(Connection);
end;

class function ProtocolTool.Pop3Client(const Command: IPop3ClientCommand): IPop3Client;
begin
  Result := TPop3Client.Create(Command);
end;

class function ProtocolTool.Pop3Client(const Connection: IAbstractConnection): IPop3Client;
var
  Sink: IAsciiCommandSink;
  Command: IPop3ClientCommand;
begin
  Command := Pop3ClientCommand;
  Sink := TAsciiCommandSink.Create(Command, Connection);
  Result := Pop3Client(Command);
end;

class function ProtocolTool.Pop3ClientCommand: IPop3ClientCommand;
begin
  Result := TPop3ClientCommand.Create;
end;

class function ProtocolTool.SmtpClient(const Command: ISmtpClientCommand): ISmtpClient;
begin
  Result := TSmtpClient.Create(Command);
end;

class function ProtocolTool.SmtpClient(const Connection: IAbstractConnection): ISmtpClient;
var
  Sink: IAsciiCommandSink;
  Command: ISmtpClientCommand;
begin
  Command := SmtpClientCommand;
  Sink := TAsciiCommandSink.Create(Command, Connection);
  Result := SmtpClient(Command);
end;

class function ProtocolTool.SmtpClientCommand: ISmtpClientCommand;
var
  Command: IAsciiProtocol;
begin
  Command := TAsciiProtocol.Create;
  Result := TSmtpClientCommand.Create(Command);
end;

class function ProtocolTool.Socks5Client(const Connection: IAbstractConnection): ISocks5Client;
begin
  Result := TSocks5Protocol.Create(Connection);
end;

class function ProtocolTool.SqlClient(ID: LongWord; const Connection: IAbstractConnection): IProtSqlClient;
begin
  Result := TProtSql.CreateClient(ID, Connection);
end;

class function ProtocolTool.SqlServer(ID: LongWord; const Connection: IAbstractConnection): IProtSqlServer;
begin
  Result := TProtSql.CreateServer(ID, Connection);
end;

end.
 