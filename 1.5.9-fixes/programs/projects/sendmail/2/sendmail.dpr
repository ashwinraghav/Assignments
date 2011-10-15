program sendmail;

{$include Defines.inc}

(*)
  uso:

  sendmail
    /server smtp.apre.siderca.ot
    /from peperino
    /to fulano@mail.com
    /subject sendmail
    /text hola mundo
    /attach c:\back\*.fbk
(*)

uses
  Sil,
  SilCoder,
  SilProtocol;

{$R *.res}

type
  ISendMail = interface
    ['{248A26FF-B81C-4FD3-8A27-3F1EDC0DAFCA}']
    procedure Send(const Domain, Server, From, Recipient, Subject, Text, Attach: String);
    function IsOk(out Error: String): Boolean;
  end;

  TSendMail = class (TSilInterfacedObject, IAsciiProtocolEvents, ISendMail)
  private
    FSocket: ISocketClient;
    FErrorStr: String;
    FSuccess: Boolean;
  protected // IAsciiProtocolEvents
    procedure OnWriteLine(var Event: RWriteLineEvent);
    procedure OnReadLine(var Event: RReadLineEvent);
  protected // ISendMail
    procedure Send(const Domain, Server, From, Recipient, Subject, Text, Attach: String);
    function IsOk(out Message: String): Boolean;
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

function DoGetFile(const Path: String): String;
var
  Temp: string;
begin
  Temp := Sil.Str.Trim(Path);
  if Str.DelimiterPos('*?', Temp) > 0 then
    Sil.OS.FileSystem.StampLast(Temp, Result) else
    Result := Temp;
end;

function DoGetContent(const Path: String): String;
var
  f: IFile;
begin
  Result := '';
  f := Sil.Os.FileSystem.OpenFile(DoGetFile(Path), fmAccessRead, fmShareReadWrite, True);
  if f.Stream.Size > 0 then
  begin
    SetLength(Result, f.Stream.Size + 1);
    f.Stream.Read(Result[1], Length(Result));
    Result[length(Result)] := #0;
  end;
end;

procedure DoLog(const Msg: String);
var
  List: IStringList;
  LogFile: String;
begin
  try
    LogFile := Sil.OS.FileSystem.ChangeFileExt(Sil.OS.Process.Current.Info.FullName, 'log');
    List := Sil.List.StringList;

    if Sil.OS.FileSystem.Exists(LogFile) then
      Sil.Serializer.LoadFromFile(List, LogFile);

    while List.Count >= 100 do
      List.Delete(0);

    List.Add(Str.Format('%s %s', [DateTime.ToStr(DateTime.Now), Msg]));

    Sil.Serializer.SaveToFile(List, LogFile);
  except end;
end;

procedure TSendMail.Send(const Domain, Server, From, Recipient, Subject, Text, Attach: String);
var
  Command: ISmtpClientCommand;
  Client: ISmtpClient;
  Msg: IMailMessage;
  Part1, Part2: IMailPart;
  Lbl: IMimeLabel;
  sRecipient, sCC, sItem: String;
  i: Integer;
  bCC: Boolean;
  bBoundaryAdded: Boolean;
begin
  try
    FSocket := Sil.OS.Socket.CreateClient(stStream, spTCP, Server, 25);
    FSocket.Parameters.WriteTimeout := 5000;
    FSocket.Parameters.ReadTimeout := 5000;
    FSocket.Connect;
  except
    on e: Exception do
    begin
      DoLog('error: ' + e.Message);
      exit;
    end;
  end;

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

  Part1 := Msg.Parts.CreateNew;

  if Sil.Str.StartsWith('@', Text) then
    Part1.Body.Text := DoGetContent(Sil.Str.TrimLeft(Text, '@', 1)) else
    Part1.Body.Text := Text;

  if Str.NotEmpty(Attach) then
  begin
    Lbl := Msg.Header.Labels.CreateNew('Content-Type', 'multipart/mixed');
    i := 0;
    bBoundaryAdded := False;
    while Sil.Str.Enumerate(Attach, ',', sItem, i) do
    begin
      Part2 := Msg.Parts.CreateNew;
      Part2.AttachFile(DoGetFile(sItem));

      Part1.Boundary := Part2.Boundary;

      if not bBoundaryAdded then
      begin
        Lbl.Params.AddNew('boundary', Part2.Boundary);
        bBoundaryAdded := True;
      end else
        Lbl.Params.Find('boundary').Value := Part2.Boundary;

      Part1 := Part2;
      Part2 := nil;
    end;
  end;

  FSuccess := Client.Send(From);
  Client.GetLastError(FErrorStr);
  Client.Logout;

  Sil.Sink.Disconnect(Command, Self);
end;

function TSendMail.IsOk(out Message: String): Boolean;
begin
  Result := FSuccess;
  Message := FErrorStr;
end;

var
  Tokens: TStringArray;

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
    if (i < 6) and Str.IsEmpty(ParamArray[i]) then
    begin
      DoLog(Str.Format('missing param: %s', [Tokens[i]]));
      Result := false;
      Break;
    end;
end;

var
  i: Integer;
  Sender: ISendMail;
  Params: TStringArray;
  sCmd, Msg: String;
begin
  sCmd := Sil.OS.Process.CommandLine;
  i := Str.Pos('/', sCmd);
  if i > 0 then sCmd := Str.Copy(sCmd, i);

  Str.ArraySet(Tokens, ['domain', 'server', 'from', 'to', 'subject', 'text', 'attach']);

  if ReadParams(sCmd, Params) then
  begin
    Sender := TSendMail.Create;
    Sender.Send(Params[0], Params[1], Params[2], Params[3], Params[4], Params[5], Params[6]);

    ExitCode := Ord(Sender.IsOk(Msg));
    DoLog(Msg);
  end;
end.
