program sendmail;

{$include Defines.inc}

uses
  Sil,
  SilCoder,
  SilProtocol;

{$R *.res}

type
  ISendMail = interface
    ['{248A26FF-B81C-4FD3-8A27-3F1EDC0DAFCA}']
    procedure Send(const Domain, Server, From, Recipient, Subject, Text: String);
  end;

  TSendMail = class (TSilInterfacedObject, IAsciiProtocolEvents, ISendMail)
  private
    FSocket: ISocketClient;
  protected // IAsciiProtocolEvents
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  protected // ISendMail
    procedure Send(const Domain, Server, From, Recipient, Subject, Text: String);
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

procedure TSendMail.Send(const Domain, Server, From, Recipient, Subject, Text: String);
var
  Command: ISmtpClientCommand;
  Client: ISmtpClient;
  Msg: IMailMessage;
  sRecipient, sCC, sItem: String;
  i: Integer;
  bCC: Boolean;
begin
  FSocket := Sil.OS.Socket.CreateClient(stStream, spTCP, Server, 25);
  FSocket.Parameters.WriteTimeout := 5000;
  FSocket.Parameters.ReadTimeout := 5000;
  FSocket.Connect;

  Command := SilProtocol.Tk.SmtpClientCommand;
  Sil.Sink.Connect(Command, Self);
  Client := SilProtocol.Tk.SmtpClient(Command);

  Client.Login(Str.IIf(Str.IsEmpty(Domain), 'sendmail', Domain));

  i := 0;
  sRecipient := '';
  sCC := '';
  bCC := false;

  while Str.Enumerate(Recipient, ';', sItem, i) do
  begin
    Client.Recipients.Add(sItem);
    if not bCC then
    begin
      bCC := true;
      sRecipient := sItem;
    end else
      Str.Add(sCC, sItem, Str.IIf(i > 0, ', ', ''));
  end;

  Msg := Client.Messages.CreateNew;

  with Msg.Header.Labels do
  begin
    CreateNew('From', From);
    CreateNew('To', sRecipient);
    if Str.NotEmpty(sCC) then CreateNew('CC', sCC);
    CreateNew('Subject', Subject);
    CreateNew('Date', SilCoder.Mime.DateTimeToStr(DateTime.Now));
  end;

  Msg.Header.Body.Text := Text;
  Client.Send(From);
  Client.Logout;

  Sil.Sink.Disconnect(Command, Self);
end;

var
  Tokens: TStringArray;

// /server smtp.apre.siderca.ot /from mariano /to sidtmpo@siderca.com /subject sendmail /text hola mundo

function ReadParams(const ParamStr: String; out ParamArray: TStringArray): Boolean;
var
  sItem, sSymbol: String;
  i, iPos: Integer;
begin
  i := 0;
  SetLength(ParamArray, Length(Tokens));
  Result := true;

  while Str.Enumerate(ParamStr, '/', sItem, i) do
  begin
    sItem := Str.Trim(sItem);
    iPos := Str.Pos(ccSPC, sItem);

    if iPos > 0 then
    begin
      sSymbol := Str.Copy(sItem, 1, iPos - 1);
      sItem := Str.Copy(sItem, iPos + 1);
      iPos := Str.ArrayFind(Tokens, sSymbol);
      if iPos >= 0 then ParamArray[iPos] := sItem;
    end;
  end;

  for i := 1 to Length(ParamArray) - 1 do
    if Str.IsEmpty(ParamArray[i]) then
    begin
      Result := false;
      Break;
    end;
end;

var
  i: Integer;
  Sender: ISendMail;
  Params: TStringArray;
  sCmd: String;
begin
  sCmd := Sil.OS.Process.CommandLine;
  i := Str.Pos('/', sCmd);
  if i > 0 then sCmd := Str.Copy(sCmd, i);

  Str.ArraySet(Tokens, ['domain', 'server', 'from', 'to', 'subject', 'text']);

  if ReadParams(sCmd, Params) then
  begin
    Sender := TSendMail.Create;
    Sender.Send(Params[0], Params[1], Params[2], Params[3], Params[4], Params[5]);
  end;
end.
