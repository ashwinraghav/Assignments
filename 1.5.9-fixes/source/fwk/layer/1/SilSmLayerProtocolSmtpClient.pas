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

unit SilSmLayerProtocolSmtpClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiSmtp,
  SilSiSmtpCommand,
  SilSiMailClient,
  SilSiAsciiProtocol,
  SilSiLayer,
  SilSmLayer;

type
  TSilSmtpClientLayer = class (
    // extends
    TSilLayer,
    // implements
    ISmtpClient,
    IAsciiProtocol)
  private
    function GetLink: ILayerLink;
  private
    FLink: Pointer;
    FQueue: IInterfaceQueue;
    FCommand: ISmtpClientCommand;
    FMessages: IMailMessageList;
    FCoders: IStringList;
    FIsLogged: Boolean;
    FRecipients: IStringList;
    FRecipientsFailed: IStringList;
    FLastResult: String;
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
  protected // IAsciiProtocol
    function WriteLn(const Command: String): Boolean;
    function ReadLn(out Response: String): Boolean;
  protected
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
    procedure Receive(const Command: ILayerCommand); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilStLayer,
  SilSmSmtpCommand,
  SilSmLayerCommand,
  SilBtStr,
  SilBcChr,
  SilLmStringList,
  SilSmMimeCoder,
  SilSmMime;

{ TSilSmtpClientLayer }

constructor TSilSmtpClientLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited;
end;

destructor TSilSmtpClientLayer.Destroy;
begin
  inherited;
end;

function TSilSmtpClientLayer.GetLink: ILayerLink;
begin
  Result := ILayerLink(FLink);
end;

procedure TSilSmtpClientLayer.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
  MakeRef(Link, @FLink);

  FQueue := Sil.List.InterfaceQueue;

  FCommand := TSmtpClientCommand.Create(Self);
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

procedure TSilSmtpClientLayer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  if Assigned(FQueue) then
  begin
    FQueue.Cancel;
    FCommand := nil;
    FMessages := nil;
    FCoders := nil;
    FQueue := nil;
    DropRef(@FLink);
  end;
end;

procedure TSilSmtpClientLayer.Receive(const Command: ILayerCommand);
begin
  if Assigned(FQueue) then FQueue.Put(Command.Packet);
end;

function TSilSmtpClientLayer.ReadLn(out Response: String): Boolean;
var
  Packet: IPacket;
begin
  Result := Assigned(FQueue) and FQueue.Get(IPacket, Packet, INFINITE, true) and (Packet.Buffer.Size > 0);

  if Result then
  begin
    SetLength(Response, Packet.Buffer.Size);
    Packet.Buffer.Read(Response[1], Length(Response));
  end;
end;

function TSilSmtpClientLayer.WriteLn(const Command: String): Boolean;
var
  Packet: IPacket;
begin
  Packet := Sil.Stream.Raw.Packet;
  Packet.Buffer.Write(Command[1], Length(Command));
  Packet.Buffer.Position := 0;
  Write(Cmd.Create(GetLink, Packet));
  Result := Packet.Buffer.Position > 0;
end;

procedure TSilSmtpClientLayer.Clear;
begin
  FMessages.Clear;
  FRecipients.Clear;
  FRecipientsFailed.Clear;
end;

function TSilSmtpClientLayer.GetCoders: IStringList;
begin
  Result := FCoders;
end;

function TSilSmtpClientLayer.GetMessages: IMailMessageList;
begin
  Result := FMessages;
end;

function TSilSmtpClientLayer.GetRecipients: IStringList;
begin
  Result := FRecipients;
end;

function TSilSmtpClientLayer.Login(const Domain: String): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.WaitGreetings(sResponse) and FCommand.Hello(Domain, sResponse);
  FLastResult := FLastResult + ccCRLF + sResponse;
end;

function TSilSmtpClientLayer.Logout: Boolean;
begin
  Result := FCommand.Quit(FLastResult);
end;

function TSilSmtpClientLayer.Send(const FromAccount: String): Boolean;
var
  e1, e2: IEnumerator;
  sItem, sHeader, sContent, sLines, sBody, sBoundary: String;
  MailMsg: IMailMessage;
  Part: IMailPart;
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

      sHeader := MailMsg.Header.Lines.Text + ccCRLF + ccCRLF;
      sContent := MailMsg.Header.Body.Text;
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

        sContent := sContent + sLines + ccCRLF + sBody;
      end;

      if Length(sBoundary) > 0 then
        sContent := sContent + ccCRLF + ccCRLF + '--' + sBoundary + '--' + ccCRLF;

      Result :=
        (FRecipientsFailed.Count < FRecipients.Count) and
        FCommand.Data(FLastResult) and
        FCommand.WriteLn(sHeader + sContent + ccCRLF + '.') and
        FCommand.WaitDelivery(FLastResult);
    end;

    if not Result then Break;
  end;
end;

function TSilSmtpClientLayer.Verify(const UserName: String): Boolean;
begin
  Result := FCommand.Verify(UserName, FLastResult);
end;

function TSilSmtpClientLayer.GetLastError(out ErrorStr: String): Boolean;
begin
  Result := Str.NotEmpty(FLastResult);
  ErrorStr := FLastResult;
end;

end.
