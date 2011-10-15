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

unit SilSmSmtpClient;

interface

uses
  Sil,
  SilLkAggregated,
  SilSiSmtpCommand,
  SilSiMailClient,
  SilSiAsciiProtocol,
  SilSiSmtp,
  SilLiStringList,
  SilLiEnumerator;

type
  TSmtpClient = class (
    // extends
    TSilAggregatedObject,
    // implements
    ISmtpClient,
    IAsciiProtocolCommand)
  private
    FCommand: ISmtpClientCommand;
    FMessages: IMailMessageList;
    FCoders: IStringList;
    FIsLogged: Boolean;
    FRecipients: IStringList;
    FRecipientsFailed: IStringList;
    FLastResult: String;
  private
    procedure DoCreate(const Command: ISmtpClientCommand);
  protected // IAsciiProtocolCommand
    function GetCommand: IUnknown;
    procedure SetCommand(const Value: IUnknown);
  protected // ISmtpClient
    function GetMessages: IMailMessageList;
    function GetCoders: IStringList;
    function GetRecipients: IStringList;
    function Login(const Domain: String = ''): Boolean;
    function Verify(const UserName: String): Boolean;
    function Logout: Boolean;
    procedure Clear;
    function Send(const FromAccount: String): Boolean;
    function GetLastError(out ErrorStr: String): Boolean;
  public
    constructor Create(const Command: ISmtpClientCommand); reintroduce;
    constructor CreateAggregated(const Controller: IUnknown; const Command: ISmtpClientCommand = nil); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBcChr,
  SilLmStringList,
  SilSmMimeCoder,
  SilSmMime, SilLtStream;

constructor TSmtpClient.Create(const Command: ISmtpClientCommand);
begin
  inherited Create;
  DoCreate(Command);
end;

constructor TSmtpClient.CreateAggregated(const Controller: IUnknown; const Command: ISmtpClientCommand);
begin
  inherited Create(Controller);
  DoCreate(Command);
end;

procedure TSmtpClient.DoCreate(const Command: ISmtpClientCommand);
begin
  FCommand := Command;
  FMessages := TMailMessageList.Create;
  FIsLogged := false;
  FRecipients := Sil.List.StringList;
  FRecipientsFailed := Sil.List.StringList;

  FCoders := Sil.List.StringList;
  FCoders.IgnoreCase := true;
  FCoders.Add(TBase64Coder.Name, TBase64Coder);
  FCoders.Add(TUUCoder.Name, TUUCoder);
  FCoders.Add(TISO8859_1Coder.Name, TISO8859_1Coder);
  FCoders.Add(TQuotedPrintableCoder.Name, TQuotedPrintableCoder);
  FCoders.Add(TWindows1252Coder.Name, TWindows1252Coder);
  FCoders.Add(TDefaultCoder.Name, TDefaultCoder);
end;

destructor TSmtpClient.Destroy;
begin
  FMessages := nil;
  FCoders := nil;
  inherited;
end;

procedure TSmtpClient.Clear;
begin
  FMessages.Clear;
  FRecipients.Clear;
  FRecipientsFailed.Clear;
end;

function TSmtpClient.GetCoders: IStringList;
begin
  Result := FCoders;
end;

function TSmtpClient.GetMessages: IMailMessageList;
begin
  Result := FMessages;
end;

function TSmtpClient.Login(const Domain: String): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.WaitGreetings(sResponse) and FCommand.Hello(Domain, sResponse);
  FLastResult := FLastResult + ccCRLF + sResponse;
end;

function TSmtpClient.Verify(const UserName: String): Boolean;
begin
  Result := FCommand.Verify(UserName, FLastResult);
end;

function TSmtpClient.Logout: Boolean;
begin
  Result := FCommand.Quit(FLastResult);
end;

function TSmtpClient.Send(const FromAccount: String): Boolean;
var
  e1, e2: IEnumerator;
  sItem, sHeader, sLines, sBody, sBoundary, dummy: String;
  MailMsg: IMailMessage;
  Part: IMailPart;
  Content: IWriteOnlyStream;

  procedure DoWrite(const Text: String);
  begin
    if Str.NotEmpty(Text) then
      Content.Write(Text[1], Length(Text));
  end;

begin
  Result := false;

  while FMessages.Enumerate(e1, MailMsg) do
  begin
    if FRecipients.Count = 0 then Continue;
    FRecipientsFailed.Clear;

    Result := FCommand.MailFrom(FromAccount, FLastResult);

    if Result then
    begin
      while FRecipients.Enumerate(e2, sItem) do
        if not FCommand.Recipient(sItem, FLastResult) then
          FRecipientsFailed.Add(sItem);

      Content := Sil.Stream.WriteOnly;
      sHeader := MailMsg.Header.Lines.Text + ccCRLF + ccCRLF;
      DoWrite(MailMsg.Header.Body.Text);
      sBoundary := '';

      while MailMsg.Parts.Enumerate(e2, Part) do
      begin
        sLines := Part.Lines.Text;
        sBody := Part.Body.Text;

        if Length(Part.Boundary) > 0 then
        begin
          sBoundary := Part.Boundary;
          sLines := ccCRLF + '--' + sBoundary + ccCRLF + Str.IIf(Length(sLines) > 0, sLines + ccCRLF, '');
        end;

        DoWrite(sLines + ccCRLF + sBody);
      end;

      if Length(sBoundary) > 0 then
        DoWrite(ccCRLF + ccCRLF + '--' + sBoundary + '--' + ccCRLF);

      Result :=
        (FRecipientsFailed.Count < FRecipients.Count) and
        FCommand.Data(dummy) and
        FCommand.WriteLn(sHeader + Content.Buffer + ccCRLF + '.') and
        FCommand.WaitDelivery(FLastResult);
    end;

    if not Result then Break;
  end;
end;

function TSmtpClient.GetRecipients: IStringList;
begin
  Result := FRecipients;
end;

function TSmtpClient.GetCommand: IUnknown;
begin
  Result := FCommand;
end;

procedure TSmtpClient.SetCommand(const Value: IInterface);
begin
  FCommand := Value as ISmtpClientCommand;
end;

function TSmtpClient.GetLastError(out ErrorStr: String): Boolean;
begin
  Result := Length(FLastResult) > 0;
  ErrorStr := FLastResult;
end;

end.

