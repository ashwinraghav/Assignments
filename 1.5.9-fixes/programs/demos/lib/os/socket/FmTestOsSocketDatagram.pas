unit FmTestOsSocketDatagram;

interface

uses
  Windows, WinSock, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

const
  LOG_SIZE      = 1024 * 1000 * 2;
  PING_TIMEOUT  = 1000;

type
  TForm1 = class(
    TForm,
    IUnknown,
    IRunnable,
    ITimerEvents)
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FSource: Boolean;
    FSequence: LongWord;
    FReader: IThread;
    FWriter: ITimer;
    FSocket: ISocketClient;
    FHost: String;
    FBindPort: Word;
    FPort: Word;
    FPingTime: TDateTime;
    FLog: ITextFile;
    FNext: Boolean;
    FPerf: IPerformanceCounter;
    procedure DoPack(out Buffer: String; Source: Boolean; Sequence: LongWord; const dtStamp: TDateTime);
    procedure DoUnpack(Buffer: PChar; Size: LongWord; out lwSeq: LongWord; out dtStamp: TDateTime);
    procedure DoSend(Sequence: LongWord; const dtStamp: TDateTime; Source: Boolean; const Address: ISocketAddress);
    procedure DoStamp(const Sender: IInterface; Ref: Pointer);
    procedure DoLog(const Buffer: String);
  protected // IRunnable
    procedure Run(const Sender: IThread);
  protected // ITimerEvents
    procedure OnTick(const Sender: RTimerEvent);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetSwitch(const Switch: String; out Value: String; const Default: String = ''): Boolean;
var
  sCmd: String;
  i1, i2, iLen: Integer;
begin
  sCmd := Sil.OS.Process.CommandLine;
  iLen := Length(Switch);
  i1 := Str.Pos(Switch, sCmd);
  Result := i1 > 0;

  if (i1 > 0) and (Length(sCmd) > i1 + iLen) and (sCmd[i1 + iLen] = '=') then
  begin
    i2 := Str.Pos(' ', sCmd, i1);
    if i2 > 0 then
      Value := Str.Copy(sCmd, i1 + iLen + 1, i2 - i1 - iLen - 1) else
      Value := Str.Copy(sCmd, i1 + iLen + 1);
  end;

  if Str.IsEmpty(Value) then Value := Default;
end;

function HasSwitch(const Switch: String): Boolean;
var
  Dummy: String;
begin
  Result := GetSwitch(Switch, Dummy);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sPort, sBind: String;
begin
  FLog := Sil.OS.FileSystem.CreateTextFile(Sil.OS.Process.Current.Info.Path + 'test.log', fmAccessReadWrite, fmShareReadWrite);

  GetSwitch('/host', FHost, 'localhost');
  GetSwitch('/port', sPort, '12345');

  GetSwitch('/source', sBind, sPort);
  FBindPort := Str.ToInt(sBind, 0);

  FSource := HasSwitch('/source');
  FSequence := 0;
  FPort := Str.ToInt(sPort, 0);
  FNext := false;

  FReader := Sil.OS.Thread.Spawn('reader', Self);

  FPerf := Sil.OS.Performance.Create;

  if FSource then
  begin
    FWriter := Sil.OS.Timer.Create(0, PING_TIMEOUT, Self);
    FWriter.TickCount := 1;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FWriter <> nil then FWriter.Enabled := false;
  if FSocket <> nil then FSocket.Disconnect;
  if FReader <> nil then Sil.OS.Wait.Single(FReader, INFINITE);
  FPerf := nil;
end;

procedure TForm1.OnTick(const Sender: RTimerEvent);
var
  Address: ISocketAddress;
  dtStamp: TDateTime;
begin
  dtStamp := DateTime.Now;
  if FNext then DoLog('error');

  FWriter.Interval := PING_TIMEOUT * 5;
  FWriter.Restart;

  Inc(FSequence);
  Address := Sil.OS.Socket.IP.Create(stDatagram, spUDP, FHost, FPort);
  FPingTime := dtStamp;
  DoSend(FSequence, FPingTime, FSource, Address);
  FNext := true;
end;

procedure TForm1.Run(const Sender: IThread);
var
  Address: ISocketAddress;
  sBuffer: String;
  lwSize, lwSeq: LongWord;
  dtStamp: TDateTime;
  Stamp: PDouble;
begin
  FSocket := Sil.OS.Socket.CreateClient;
  FSocket.Bind(stDatagram, spUDP, 0, FBindPort);
  SetLength(sBuffer, 8192);

  while true do
  begin
    lwSize := FSocket.Stream.ReadFrom(sBuffer[1], Length(sBuffer), Address);

    if Sil.OS.Socket.LastError = ECONNRESET then
      Continue;

    if lwSize < 1 then
      Break;

    label1.caption := Address.Format(CAddressLong);

    FNext := false;
    DoUnpack(PChar(sBuffer), lwSize, lwSeq, dtStamp);

    if FSource then
    begin
      New(Stamp);
      Stamp^ := FPerf.ToMilliseconds;

      Sil.OS.Thread.AsyncCall(DoStamp, Stamp);
    end else
      DoSend(lwSeq, dtStamp, false, Address);
  end;
end;

procedure TForm1.DoStamp(const Sender: IUnknown; Ref: Pointer);
var
  Lap: PDouble absolute Ref;
begin
  label4.Caption := Float.ToStr(Lap^);
  Dispose(Lap);
end;

procedure TForm1.DoSend(Sequence: LongWord; const dtStamp: TDateTime; Source: Boolean; const Address: ISocketAddress);
var
  sBuffer: String;
begin
  DoPack(sBuffer, Source, Sequence, dtStamp);
  FPerf.GetTime(true);
  FSocket.Stream.WriteTo(sBuffer[1], Length(sBuffer), Address);
end;

procedure TForm1.DoPack(out Buffer: String; Source: Boolean; Sequence: LongWord; const dtStamp: TDateTime);
var
  Stream: IMemoryStream;
  Writer: IWriter;
begin
  Stream := Sil.Stream.Memory;
  Writer := Sil.Stream.Raw.Writer(Stream);

  Writer.WriteLongWord(Sequence);
  Writer.WriteBoolean(Source);
  Writer.WriteDate(dtStamp);

  DoLog(Str.Format('seq:%.10d stamp:%s tipo:%s', [
    Sequence, DateTime.ToStr(dtStamp, 'hh:nn:ss.zzz'), '-->']));

  SetString(Buffer, Stream.Memory, Stream.Size);
end;

procedure TForm1.DoUnpack(Buffer: PChar; Size: LongWord; out lwSeq: LongWord; out dtStamp: TDateTime);
var
  Stream: IMemoryStream;
  Reader: IReader;
  bLog: Boolean;
begin
  Stream := Sil.Tk.MemoryStream(Buffer, Size);
  Reader := Sil.Stream.Raw.Reader(Stream);

  lwSeq := Reader.ReadLongWord;
  Reader.ReadBoolean;
  bLog := true;

  if FSource then
  begin
    bLog := lwSeq = FSequence;
    dtStamp := DateTime.Now - Reader.ReadDate;
    FWriter.Interval := PING_TIMEOUT;
    FWriter.Restart;
  end else
    dtStamp := Reader.ReadDate;

  if bLog then
    DoLog(Str.Format('seq:%.10d stamp:%s tipo:%s', [
      lwSeq, DateTime.ToStr(dtStamp, 'hh:nn:ss.zzz'), '<--']));
end;

procedure TForm1.DoLog(const Buffer: String);
begin
  if FLog.Stream.Size > LOG_SIZE then
  begin
    FLog.Stream.Position := 0;
    FLog.Stream.Truncate;
  end;

  FLog.Stream.WriteLn(Buffer);
end;

end.

