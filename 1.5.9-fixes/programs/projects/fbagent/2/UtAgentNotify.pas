{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit UtAgentNotify;

interface

uses
  Windows,
  ShellApi,

  Sil,
  SilProtocol,
  SilCoder;

type
  AgentNotify = class (Tool)
  private
    class procedure DoNetSend(const ServiceName: String; const Recipients: IStringList; Subject, Body: String);
    class procedure DoSmtp(const ServiceName: String; const Recipients: IStringList; Subject, Body: String);
  public
    class procedure Run(const ServiceName: String; const Recipients: IStringList; const Subject, Body: String);
  end;

implementation

uses
  UfConfig, SilLiKey;

type
  ISendMail = interface
    ['{248A26FF-B81C-4FD3-8A27-3F1EDC0DAFCA}']
    procedure Send(const Domain, Server: String; Port: Integer; const From: String; const Recipient: IStringList; const Subject, Text: String);
  end;

  TSendMail = class (TSilInterfacedObject, IAsciiProtocolEvents, ISendMail)
  private
    FSocket: ISocketClient;
  protected // IAsciiProtocolEvents
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  protected // ISendMail
    procedure Send(const Domain, Server: String; Port: Integer; const From: String; const Recipient: IStringList; const Subject, Text: String);
  end;

{ TSendMail }

procedure TSendMail.OnReadLine(var Event: RReadLineEvent);
var
  Buf: array [0..1024] of Char;
  lwSize: LongWord;
begin
  lwSize := FSocket.Stream.Read(Buf, SizeOf(Buf));
  SetString(Event.Text, Buf, lwSize);
  Event.Result := lwSize > 0;
end;

procedure TSendMail.OnWriteLine(var Event: RWriteLineEvent);
begin
  Event.Result := FSocket.Stream.Write(Event.Text[1], Length(Event.Text)) > 0;
end;

procedure TSendMail.Send(const Domain, Server: String; Port: Integer; const From: String; const Recipient: IStringList; const Subject, Text: String);
var
  Command: ISmtpClientCommand;
  Client: ISmtpClient;
  Msg: IMailMessage;
  ToRcpt, CC, Item: String;
  Enum: IEnumerator;
begin
  FSocket := Sil.OS.Socket.CreateClient(stStream, spTCP, Server, Port);
  FSocket.Parameters.WriteTimeout := 10000;
  FSocket.Parameters.ReadTimeout := 10000;
  FSocket.Connect;

  Command := SilProtocol.Tk.SmtpClientCommand;
  Sil.Sink.Connect(Command, Self);
  Client := SilProtocol.Tk.SmtpClient(Command);

  if not Client.Login(Str.IIf(Str.IsEmpty(Domain), 'fbagent', Domain)) then
    raise Sil.Error.Create('login error at %s:%d', [Server, Port]);

  ToRcpt := '';
  CC := '';

  while Recipient.Enumerate(Enum, Item) do
  begin
    Client.Recipients.Add(Item);

    if Enum.Iteration = 0 then
      ToRcpt := Item else
      Str.Add(CC, Item, ', ');
  end;

  Msg := Client.Messages.CreateNew;

  with Msg.Header.Labels do
  begin
    CreateNew('From', From);
    CreateNew('To', ToRcpt);
    if Str.NotEmpty(CC) then CreateNew('CC', CC);
    CreateNew('Subject', Subject);
    CreateNew('Date', SilCoder.Mime.DateTimeToStr(DateTime.Now));
  end;

  Msg.Header.Body.Text := Text;

  if Client.Send(From) then
    Client.Logout else
    raise Sil.Error.Create('send message failed');

  Sil.Sink.Disconnect(Command, Self);
end;

{ AgentNotify }

class procedure AgentNotify.Run(const ServiceName: String; const Recipients: IStringList; const Subject, Body: String);
begin
  if Sil.Text.Compare(ServiceName, 'smtp') = 0 then DoSmtp(ServiceName, Recipients, Subject, Body) else
  if Sil.Text.Compare(ServiceName, 'netsend') = 0 then DoNetSend(ServiceName, Recipients, Subject, Body);
end;

class procedure AgentNotify.DoSmtp(const ServiceName: String; const Recipients: IStringList; Subject, Body: String);
var
  Sender: ISendMail;
  Host: String;
  Port: Integer;
begin
  try
    with Config.GetTag('config/smtp', true) do
    begin
      Host := Childs.ReadString('host');
      Port := Childs.ReadInteger('port', 25, true);
    end;

    if Str.NotEmpty(Host) and (Port > 0) then
    begin
      Sender := TSendMail.Create;
      Sender.Send('fbagent', Host, Port, 'fbagent@noreply.org', Recipients, Subject, Body);
      Sender := nil;
    end else
      raise Sil.Error.Create('mail server not configured.');
  except
    Sil.Trace.Exception('AgentNotify.DoSmtp');
  end;
end;

class procedure AgentNotify.DoNetSend(const ServiceName: String; const Recipients: IStringList; Subject, Body: String);
var
  Enum: IEnumerator;
  Item: String;
begin
  while Recipients.Enumerate(Enum, Item) do
    ShellExecute(0, nil, 'net.exe', PChar('send ' + Item + ' ' + Subject), nil, SW_HIDE);
end;

end.
