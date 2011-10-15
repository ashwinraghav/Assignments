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

unit SilSmMailNotify;

interface

{$include Defines.inc}

uses
  Sil,
  SilEMail,
  SilCoder,
  SilSiNotify,
  SilSiAsciiProtocol;

type
  TMailNotifier = class (
    // extends
    TSilInterfacedObject,
    // implements
    IEventServer,
    IAsciiProtocolEvents)
  private
    FConfig: IEventConfig;
    FHost: String;
    FPort: Integer;
    FDomain: String;
    FDefaultSender: String;
    FDefaultSubject: String;
    FConnection: ISocketClient;
  private
    function DoSend(const Event: IEventData): Boolean;
  protected // IEventServer
    function GetName: String;
    procedure Initialize(const Config: IEventConfig);
    procedure Finalize;
    procedure Notify(const Event: IEventData);
  protected // IAsciiProtocolEvents
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMailNotifier }

constructor TMailNotifier.Create;
begin
  inherited Create;
end;

destructor TMailNotifier.Destroy;
begin
  inherited;
end;

function TMailNotifier.GetName: String;
begin
  Result := 'sil.mailnotifier';
end;

procedure TMailNotifier.Initialize(const Config: IEventConfig);
begin
  FConfig := Config;
  FHost := FConfig.DefParams['mail.server'];
  FPort := FConfig.DefParams['mail.port'];
  FDomain := FConfig.DefParams['mail.domain'];
  FDefaultSender := FConfig.DefParams['default.sender'];
  FDefaultSubject := FConfig.DefParams['default.subject'];
end;

procedure TMailNotifier.Finalize;
begin
end;

procedure TMailNotifier.Notify(const Event: IEventData);
var
  i: integer;
begin
  if Str.IsEmpty(Event.Sender) then Event.Sender := FDefaultSender;
  if Str.IsEmpty(Event.Subject) then Event.Subject := FDefaultSubject;

  i := sil.str.pos('.', fhost);

  if (sil.str.pos('.', event.sender) = 0) and (i > 0) then
    event.sender := event.sender + sil.str.copy(FHost, i);

  if FConfig.Configure(Event, Self) then DoSend(Event);
end;

function TMailNotifier.DoSend(const Event: IEventData): Boolean;
var
  Command: ISmtpClientCommand;
  Client: ISmtpClient;
  Msg: IMailMessage;
  i1, i2: Integer;
  sRecipients, sItem1, sItem2, buf: String;
  Params: IParameterList;
  Enum1, Enum2: IEnumerator;
  BodyPart, FilePart: IMailPart;
  Attachment: IAttachment;
  Lbl: IMimeLabel;
begin
  try
    FConnection := Sil.OS.Socket.CreateClient(stStream, spTCP, FHost, FPort);
    FConnection.Parameters.WriteTimeout := 5000;
    FConnection.Parameters.ReadTimeout := 5000;
    FConnection.Connect;

    Params := Sil.List.Parameters;
    sRecipients := '';

    try
      Command := SilEMail.SmtpClientCommand;
      Client := SilEMail.SmtpClient(Command);

      Sil.Sink.Connect(Command, Self);
      Client.Login(FDomain);

      while Event.Recipients.Enumerate(Enum1, sItem1) do
      begin
        i1 := 0;
        Params.Clear;

        while Str.Enumerate(sItem1, ';', sItem2, i1) do
        begin
          sItem2 := Str.Trim(sItem2);
          i2 := Str.Pos('=', sItem2);
          if i2 > 0 then Params[Str.Left(sItem2, i2 - 1)] := Str.Copy(sItem2, i2 + 1);
        end;

        if Vart.IsOk(Params['mail']) and Vart.IsOk(Params['name']) then
        begin
          Client.Recipients.Add(Params['mail']);
          Str.Add(sRecipients, Str.Format('"%s" <%s>, ', [Params['name'], Params['mail']]));
        end;
      end;

      Str.Delete(sRecipients, -2);
      Msg := Client.Messages.CreateNew;

      with Msg.Header.Labels do
      begin
        CreateNew('From', Event.Sender);
        CreateNew('To', sRecipients);
        CreateNew('Subject', Event.Subject);
        CreateNew('Date', SilCoder.Mime.DateTimeToStr(DateTime.Now));
      end;

      BodyPart := Msg.Parts.CreateNew;
      BodyPart.Body.Text := Event.Text;

      if Event.Attachments.Count > 0 then
        while Event.Attachments.Enumerate(Enum2, Attachment) do
        begin
          Attachment.Stream.Position := 0;
          FilePart := Msg.Parts.CreateNew;
          FilePart.AttachFile(Attachment.Name, Attachment.Stream);

          if not Assigned(Lbl) then
          begin
            Lbl := Msg.Header.Labels.CreateNew('Content-Type', 'multipart/mixed');
            Lbl.Params.AddNew('boundary', FilePart.Boundary);
            BodyPart.Boundary := FilePart.Boundary;
          end;
        end;

      Event.Received := Client.Send(Event.Sender);

      if Client.GetLastError(buf) then
        Event.Response := buf;

      Client.Logout;
    finally
      Sil.Sink.Disconnect(Command, Self);
    end;

    Result := true;
  except
    Result := false;
  end;
end;

procedure TMailNotifier.OnReadLine(var Event: RReadLineEvent);
var
  Buf: array [0..1024] of Char;
  lwSize: LongWord;
begin
  lwSize := FConnection.Stream.Read(Buf, SizeOf(Buf));
  SetString(Event.Text, Buf, lwSize);
  Event.Result := lwSize > 0;
end;

procedure TMailNotifier.OnWriteLine(var Event: RWriteLineEvent);
begin
  Event.Result := FConnection.Stream.Write(Event.Text[1], Length(Event.Text)) > 0;
end;

end.

