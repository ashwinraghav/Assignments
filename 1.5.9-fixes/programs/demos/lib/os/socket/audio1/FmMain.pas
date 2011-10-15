unit FmMain;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  Sil,
  SilMM,

  UiStreamingProtocol,
  UmStreamingProtocol;

type
  TfoStreamer = class(TForm, IWaveAudioInHook, IRunnable, IStreamingProtocolEvents)
    GroupBox1: TGroupBox;
    edHost: TEdit;
    Label1: TLabel;
    btAdd: TButton;
    lbHosts: TListBox;
    btRemove: TButton;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbSamples: TComboBox;
    cbBits: TComboBox;
    cbChannels: TComboBox;
    btCapture: TButton;
    btStop: TButton;
    GroupBox3: TGroupBox;
    btClose: TButton;
    Label5: TLabel;
    laTransmit: TLabel;
    Label7: TLabel;
    laReceive: TLabel;
    cbPropagar: TCheckBox;
    edBuffer: TEdit;
    Label6: TLabel;
    Label8: TLabel;
    edMSecs: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btCaptureClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure DoCalcBufferSize(Sender: TObject);
  private
    FWaveIn: IWaveAudio;
    FStreamer: IStreamingProtocol;
    FReceiver: ISocketClient;
    FPort: Word;
    FThread: IThread;
    FMixer: IMixerDevice;
    procedure DoInit;
    procedure DoFin;
    procedure BufferFull(const Sender: IWaveAudio; const Buffer: IWaveAudioBuffer);
    procedure StopCapture(const Sender: IWaveAudio);
    procedure Run(const Thread: IThread);
    procedure DoAddSuscription(const Sender: IInterface; Param: Pointer);
    function DoGetParams: IParameterList;
    procedure DoSetParam;
  protected
    procedure OnSubscriberReceived(var Event: RStreamingSuscriberEvent);
    procedure OnSubscriptionRevoked(var Event: RStreamingSuscriberEvent);
    procedure OnDataReceived(var Event: RStreamingDataEvent);
    procedure OnParamsReceived(var Event: RStreamingParamsEvent);
  public
    { Public declarations }
  end;

var
  foStreamer: TfoStreamer;

implementation

uses SilOiSocket, SilOjSocket, SilOjThread, SilOiFile, SilOtFile, SilLiKey,
  SilLtList, SilOiWaveAudio, SilOtMixerDevice;

{$R *.dfm}

procedure TfoStreamer.FormCreate(Sender: TObject);
begin
  DoInit;
end;

procedure TfoStreamer.DoInit;
var
  i: Integer;
  FileName: String;
  Key: IValueKeys;
begin
  FMixer := SilMM.Mixer.Device;
  FWaveIn := SilMM.WaveAudio.Create;

  for i := 0 to Ord(High(TSamplesPerSec)) do
    cbSamples.Items.Add(Sil.Enum.Name(TypeInfo(TSamplesPerSec), i, 'sp'));

  for i := 0 to Ord(High(TBitsPerSample)) do
    cbBits.Items.Add(Sil.Enum.Name(TypeInfo(TBitsPerSample), i, 'bs'));

  for i := 0 to Ord(High(TChannels)) do
    cbChannels.Items.Add(Sil.Enum.Name(TypeInfo(TChannels), i, 'ch'));

  FileName := Sil.OS.FileSystem.ChangeFileExt(Sil.OS.Process.Current.Info.FullName, '.ini');
  Key := Sil.Sv.Configuration.Open(FileName, true);

  with Key.Get('quality', true) do
  begin
    cbSamples.ItemIndex := ReadInteger('SamplesPerSec', 0, true);
    cbBits.ItemIndex := ReadInteger('BitsPerSample', 0, true);
    cbChannels.ItemIndex := ReadInteger('Channels', 0, true);
  end;

  with Key.Get('streaming', true) do
  begin
    edMSecs.Text := ReadString('MSecs', '200', true);
    FPort := ReadInteger('Port', 27940, true);
  end;

  FThread := Sil.OS.Thread.Spawn(Self);
  DoCalcBufferSize(nil);
end;

procedure TfoStreamer.DoFin;
var
  FileName: String;
  Key: IValueKeys;
begin
  FileName := Sil.OS.FileSystem.ChangeFileExt(Sil.OS.Process.Current.Info.FullName, '.ini');
  Key := Sil.Sv.Configuration.Open(FileName, true);

  with Key.Get('quality', true) do
  begin
    WriteInteger('SamplesPerSec', cbSamples.ItemIndex);
    WriteInteger('BitsPerSample', cbBits.ItemIndex);
    WriteInteger('Channels', cbChannels.ItemIndex);
  end;

  with Key.Get('streaming', true) do
  begin
    WriteString('MSecs', edMSecs.Text);
    WriteInteger('Port', FPort);
  end;

  Sil.OS.Wait.Single(FThread, INFINITE, true);
  FThread := nil;
end;

procedure TfoStreamer.FormDestroy(Sender: TObject);
begin
  if Assigned(FReceiver) then FReceiver.Disconnect;
  if Assigned(FWaveIn) then FWaveIn.Finalize;

  DoFin;
end;

procedure TfoStreamer.BufferFull(const Sender: IWaveAudio; const Buffer: IWaveAudioBuffer);
begin
  FStreamer.SendBuffer(Buffer.Data.Memory^, Buffer.BytesRecorded);
end;

procedure TfoStreamer.StopCapture(const Sender: IWaveAudio);
begin
end;

procedure TfoStreamer.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfoStreamer.DoSetParam;
begin
  FWaveIn.Format.Channels := TChannels(cbChannels.ItemIndex);
  FWaveIn.Format.SamplesPerSec := TSamplesPerSec(cbSamples.ItemIndex);
  FWaveIn.Format.BitsPerSample := TBitsPerSample(cbBits.ItemIndex);
end;

procedure TfoStreamer.DoCalcBufferSize(Sender: TObject);
var
  MSecs: LongWord;
begin
  DoSetParam;
  MSecs := Str.ToInt(edMSecs.Text, 1000);
  edBuffer.Text := Int.ToStr(Trunc(FWaveIn.Format.AvgBytesPerSec / 1000 * MSecs));
end;

procedure TfoStreamer.btCaptureClick(Sender: TObject);

  procedure DoAddBuffer;
  var
    Buffer: IWaveAudioBuffer;
    Size: Integer;
  begin
    Buffer := SilMM.WaveAudio.Buffer;
    Size := Str.ToInt(edBuffer.Text, 0);
    if Size < 1 then Size := FWaveIn.Format.AvgBytesPerSec div 5;
    Buffer.Data.Size := Size;
    FWaveIn.Buffers.Add(Buffer);
  end;

begin
  FMixer.Recording.Microphone.Select;

  DoSetParam;
  FStreamer.SendParams(DoGetParams);

  FWaveIn.IsLoopEnabled := true;
  FWaveIn.Initialize(wmIn, Self);

  FWaveIn.Buffers.Clear;
  DoAddBuffer;
  DoAddBuffer;

  FWaveIn.Capture;

  btCapture.Enabled := false;
  btStop.Enabled := true;
end;

procedure TfoStreamer.btStopClick(Sender: TObject);
begin
  try
    btCapture.Enabled := true;
    btStop.Enabled := false;
    FWaveIn.Stop;
  except end;
end;

procedure TfoStreamer.btAddClick(Sender: TObject);
var
  Host: String;
  Address: ISocketAddress;
begin
  Host := Str.Trim(edHost.Text);

  if Str.NotEmpty(Host) and (lbHosts.Items.IndexOf(Host) = -1) then
  begin
    Address := Sil.OS.Socket.IP.Create(Host, FPort);
    FStreamer.Subscribe(Address);
    lbHosts.Items.AddObject(Host, Pointer(Address));
  end;

  edHost.Clear;
end;

procedure TfoStreamer.btRemoveClick(Sender: TObject);
var
  Address: ISocketAddress;
begin
  if (lbHosts.Items.Count > 0) and (lbHosts.ItemIndex > -1) then
  begin
    Address := ISocketAddress(Pointer(lbHosts.Items.Objects[lbHosts.ItemIndex]));
    FStreamer.UnSubscribe(Address);
    FStreamer.RemoveSubscriber(Address);
    lbHosts.Items.Delete(lbHosts.ItemIndex);
  end;
end;

function TfoStreamer.DoGetParams: IParameterList;
begin
  Result := Sil.List.Parameters;
  Result['Channels'] := FWaveIn.Format.Channels;
  Result['SamplesPerSec'] := FWaveIn.Format.SamplesPerSec;
  Result['BitsPerSample'] := FWaveIn.Format.BitsPerSample;
end;

procedure TfoStreamer.OnSubscriberReceived(var Event: RStreamingSuscriberEvent);
begin
  if Assigned(FWaveIn) then
  begin
    FStreamer.AddSubscriber(Event.Id, Event.Host);

    if FWaveIn.IsRunning then
      Event.Sender.SendParams(Event.Host, DoGetParams);

    if Event.Sender.Subscribe(Event.Host) then
      Sil.OS.Thread.SyncCall(DoAddSuscription, Event.Host);
  end;
end;

procedure TfoStreamer.DoAddSuscription(const Sender: IUnknown; Param: Pointer);
var
  Host: ISocketAddress;
  Ptr: Pointer;
begin
  Host := Sender as ISocketAddress;
  Ptr := Pointer(Host);

  if lbHosts.Items.IndexOfObject(Ptr) = -1 then
    lbHosts.Items.AddObject(Host.Format, Ptr);
end;

procedure TfoStreamer.OnParamsReceived(var Event: RStreamingParamsEvent);
var
  WaveOut: IWaveAudio;
begin
  WaveOut := SilMM.WaveAudio.Create;
  WaveOut.IsDiscardEnabled := true;
  WaveOut.Format.Channels := Event.Params['Channels'];
  WaveOut.Format.SamplesPerSec := Event.Params['SamplesPerSec'];
  WaveOut.Format.BitsPerSample := Event.Params['BitsPerSample'];
  WaveOut.Initialize(wmOut);

  Event.Subscription.Data := WaveOut;
end;

procedure TfoStreamer.OnSubscriptionRevoked(var Event: RStreamingSuscriberEvent);
var
  Index: Integer;
begin
  FStreamer.UnSubscribe(Event.Host);
  FStreamer.RemoveSubscriber(Event.Host);

  Index := lbHosts.Items.IndexOfObject(Pointer(Event.Host));
  if Index >= 0 then lbHosts.Items.Delete(Index);
end;

procedure TfoStreamer.OnDataReceived(var Event: RStreamingDataEvent);
var
  Buffer: IWaveAudioBuffer;
  WaveOut: IWaveAudio;
begin
  WaveOut := IWaveAudio(Event.Subscription.Data);

  if Assigned(WaveOut) then
  begin
    Buffer := SilMM.WaveAudio.Buffer;
    Buffer.Data.Write(Event.Buffer^, Event.Size);

    if WaveOut.Buffers.Add(Buffer) > 0 then
      WaveOut.Reproduce;
  end;
end;

procedure TfoStreamer.Run(const Thread: IThread);
var
  Address: ISocketAddress;
  Buffer: String;
  Size: LongWord;
begin
  FReceiver := Sil.OS.Socket.CreateClient;
  FReceiver.Bind(stDatagram, spUDP, 0, FPort);

  try
    FStreamer := TStreamingProtocol.Create(FReceiver, Self);
    SetLength(Buffer, FReceiver.Parameters.SendBufferSize);

    repeat
      Size := FReceiver.Stream.ReadFrom(Buffer[1], Length(Buffer), Address);
      FStreamer.Receive(Address, Buffer[1], Size);
    until Size = 0;
  except
    // log
  end;
end;

end.
