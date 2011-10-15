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

unit SilOmWaveAudio;

{$I Defines.inc}

interface

uses
  MMSystem,

  SilLiStream,
  SilOiWaveAudio,
  SilLkObject,
  SilLkInterfaced,
  SilOkWaveAudio;

type
  TSilWindowsWaveAudioFormat = class (TSilObject, IWaveAudioFormat)
  private
    FFormat: TWaveFormatEx;
  private
    procedure DoCalcBlockAlign;
    procedure DoCalcAvgBytesPerSec;
  protected
    function GetTag: Word;
    function GetExtraSize: Word;
    function GetChannels: TChannels;
    function GetSamplesPerSec: TSamplesPerSec;
    function GetAvgBytesPerSec: LongWord;
    function GetBlockAlign: Word;
    function GetBitsPerSample: TBitsPerSample;
    procedure SetTag(Value: Word);
    procedure SetChannels(Value: TChannels);
    procedure SetSamplesPerSec(Value: TSamplesPerSec);
    procedure SetAvgBytesPerSec(Value: LongWord);
    procedure SetBlockAlign(Value: Word);
    procedure SetBitsPerSample(Value: TBitsPerSample);
    procedure SetExtraSize(Value: Word);
    function GetPtr: Pointer;
    property Tag: Word read GetTag write SetTag;
    property Channels: TChannels read GetChannels write SetChannels;
    property SamplesPerSec: TSamplesPerSec read GetSamplesPerSec write SetSamplesPerSec;
    property AvgBytesPerSec: LongWord read GetAvgBytesPerSec write SetAvgBytesPerSec;
    property BlockAlign: Word read GetBlockAlign write SetBlockAlign;
    property BitsPerSample: TBitsPerSample read GetBitsPerSample write SetBitsPerSample;
    property Ptr: Pointer read GetPtr;
  public
    constructor Create;
  end;

  TSilWindowsWaveAudioDeviceClass = class of TSilWindowsWaveAudioDevice;

  TSilWindowsWaveAudioDevice = class (TSilObject, IWaveAudioDevice)
  protected
    FId: LongWord;
    FMode: TWaveAudioMode;
  protected
    function GetMode: TWaveAudioMode;
    function GetId: LongWord;
    function GetName: String; virtual; abstract;
    function Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean; virtual; abstract;
    property Id: LongWord read GetId;
    property Name: String read GetName;
  public
    constructor Create(Id: LongWord); virtual;
  end;

  TSilWindowsWaveOutAudioDevice = class (TSilWindowsWaveAudioDevice)
  private
    FCaps: TWaveOutCaps;
  protected
    function GetName: String; override;
    function Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean; override;
  public
    constructor Create(Id: LongWord); override;
  end;

  TSilWindowsWaveInAudioDevice = class (TSilWindowsWaveAudioDevice)
  private
    FCaps: TWaveInCaps;
  protected
    function GetName: String; override;
    function Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean; override;
  public
    constructor Create(Id: LongWord); override;
  end;

  TSilWindowsWaveAudioBuffer = class (TSilInterfacedObject, IWaveAudioBuffer)
  private
    FHeader: TWaveHdr;
    FBuffer: IMemoryStream;
    FIsPrepared: Boolean;
    FWaveAudio: Pointer;
  protected
    function GetData: IMemoryStream;
    function GetBytesRecorded: LongWord;
    function GetFlags: LongWord;
    function GetLoops: LongWord;
    function GetPtr: Pointer;
    function GetIsPrepared: Boolean;
    function GetWaveAudio: IWaveAudio;
    procedure SetBytesRecorded(Value: LongWord);
    procedure SetFlags(Value: LongWord);
    procedure SetLoops(Value: LongWord);
    procedure SetIsPrepared(Value: Boolean);
    procedure SetWaveAudio(const Value: IWaveAudio);
    property Data: IMemoryStream read GetData;
    property BytesRecorded: LongWord read GetBytesRecorded write SetBytesRecorded;
    property Flags: LongWord read GetFlags write SetFlags;
    property Loops: LongWord read GetLoops write SetLoops;
    property IsPrepared: Boolean read GetIsPrepared write SetIsPrepared;
    property Ptr: Pointer read GetPtr;
  public
    constructor Create(const Stream: IRandomStream);
    destructor Destroy; override;
  end;

  TSilWindowsWaveAudio = class (TSilWaveAudio)
  private
    FHandle: Integer;
    FUsable: Boolean;
  protected
    function PrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean; override;
    function UnPrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean; override;
    function DoCreateWaveFormat: IWaveAudioFormat; override;
    function GetPosition: LongWord; override;
    function GetUsable: Boolean; override;
    function Initialize(Mode: TWaveAudioMode; const Hook: IUnknown): Boolean; override;
    procedure Finalize; override;
    function Capture: Boolean; override;
    function Reproduce: Boolean; override;
    function Reset: Boolean; override;
    function Stop: Boolean; override;
  public
    destructor Destroy; override;
  end;

function GetFormat(const Buffer: IRandomStream; var Format: IWaveAudioFormat): LongWord;

implementation

uses
  SilLtStream,
  SilLtSerializer;

type
  TWaveFileHeader = packed record
    szRiff: array[0..3] of Char;    // 'RIFF'
    lFileSize: Integer;
    szWave: array[0..7] of Char;    // 'WAVEfmt '
    lUnknown: Integer;              // 16
    nChannels: Word;
    nBlockAlign: Word;
    nSamplesPerSec: Integer;
    nAvgBytesPerSec: Integer;
    wFormatTag: Word;               // WAVE_FORMAT_PCM
    wBitsPerSample: Word;
    szID3: array[0..3] of Char;     // 'data'
    lDataSize: Integer;
  end;

function ActivateBuffer(Handle: Integer; const Audio: IWaveAudioControl): Boolean;
var
  i, Count: Integer;
begin
  Audio.Buffers.Locked;
  Count := Audio.Buffers.Count;
  Result := false;

  try
    if Count > 0 then
      for i := Audio.BufferIndex to Audio.BufferIndex + 1 do
        if i >= Count then
        begin
          if Count > 1 then
          begin
            Result := true;
            Audio.Buffers[0].IsPrepared := true;
          end else
            Break;
        end else
        if i >= 0 then
        begin
          Audio.Buffers[i].IsPrepared := true;
          Result := true;
        end;
  except
    Result := false;
  end;
end;

procedure WaveProc(hdrvr: HDRVR; uMsg: LongWord; dwUser: LongWord; dwParam1, dwParam2: LongWord); stdcall;
var
  Audio: IWaveAudioControl;
  Buffer: IWaveAudioBuffer;
begin
  case uMsg of
    MM_WIM_DATA,
    MM_WOM_DONE:
    begin
      Audio := IWaveAudioControl(Pointer(dwUser));
      if not Audio.IsRunning then Exit;

      Buffer := IWaveAudioBuffer(PWaveHdr(dwParam1).dwUser);

      if Audio.IsUsable then
        try
          case Audio.Mode of
            wmIn:   if Assigned(Audio.HookIn) then Audio.HookIn.BufferFull(Audio, Buffer);
            wmOut:  if Assigned(Audio.HookOut) then Audio.HookOut.BufferPlayed(Audio, Buffer);
          end;

          Buffer.IsPrepared := false;
          Buffer := nil;

          Audio.NextBuffer;

          if not ActivateBuffer(hdrvr, Audio) then
            case Audio.Mode of
              wmIn:   if Assigned(Audio.HookIn) then Audio.HookIn.StopCapture(Audio);
              wmOut:  if Assigned(Audio.HookOut) then Audio.HookOut.StopReproduce(Audio);
            end;
        except end;
    end;
	end;
end;

{ TSilWindowsWaveAudio }

destructor TSilWindowsWaveAudio.Destroy;
begin
  Finalize;
  inherited;
end;

function TSilWindowsWaveAudio.PrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
var
  Ptr: Pointer;
begin
  Buffers.Locked;
  Result := false;

  if FUsable then
  begin
    Ptr := Buffer.Ptr;

    case FMode of
      wmIn:
        Result := Buffer.IsPrepared or
          ((waveInPrepareHeader(FHandle, Ptr, SizeOf(TWavehdr)) = MMSYSERR_NOERROR) and
          (waveInAddBuffer(FHandle, Ptr, SizeOf(TWavehdr)) = MMSYSERR_NOERROR));

      wmOut:
        Result := Buffer.IsPrepared or
          ((waveOutPrepareHeader(FHandle, Ptr, SizeOf(TWavehdr)) = MMSYSERR_NOERROR) and
          (waveOutWrite(FHandle, Ptr, SizeOf(TWavehdr)) = MMSYSERR_NOERROR));
    end;
  end;
end;

function TSilWindowsWaveAudio.UnPrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
var
  Ptr: Pointer;
begin
  Buffers.Locked;
  Result := false;

  if FUsable then
  begin
    Ptr := Buffer.Ptr;

    case FMode of
      wmIn:
        Result := not Buffer.IsPrepared or
          (waveInUnPrepareHeader(FHandle, Ptr, SizeOf(TWaveHdr)) = MMSYSERR_NOERROR);

      wmOut:
        Result := not Buffer.IsPrepared or
          (waveOutUnPrepareHeader(FHandle, Ptr, SizeOf(TWaveHdr)) = MMSYSERR_NOERROR);
    end;
  end;
end;

function TSilWindowsWaveAudio.Initialize(Mode: TWaveAudioMode; const Hook: IUnknown): Boolean;
var
  WaveFormat: PWaveFormatEx;
  Control: IWaveAudioControl;
  DeviceID: LongWord;
begin
  if FHandle <> 0 then Finalize;

  FMode := Mode;
  FUsable := true;

  if Assigned(FDevice) then
    DeviceID := FDevice.Id else
    DeviceID := WAVE_MAPPER;

  WaveFormat := Format.Ptr;
  Control := Self;

  case FMode of
    wmIn:
    begin
      if Assigned(Hook) then FHookIn := Hook as IWaveAudioInHook;
      Result := waveInOpen(@FHandle, DeviceID, WaveFormat, Integer(@WaveProc), Integer(Control), CALLBACK_FUNCTION) = MMSYSERR_NOERROR;
    end;

    wmOut:
    begin
      if Assigned(Hook) then FHookOut := Hook as IWaveAudioOutHook;
      Result := waveOutOpen(@FHandle, DeviceID, WaveFormat, Integer(@WaveProc), Integer(Control), CALLBACK_FUNCTION) = MMSYSERR_NOERROR;
    end;

    else
      Result := false;
  end;
end;

procedure TSilWindowsWaveAudio.Finalize;
begin
  if FHandle = 0 then Exit;

  Stop;
  FIsRunning := false;
  FBufferIndex := -1;

  case FMode of
    wmIn:
    begin
      waveInReset(FHandle);
      Buffers.Clear;
      waveInClose(FHandle);
    end;

    wmOut:
    begin
      waveOutReset(FHandle);
      Buffers.Clear;
      waveOutClose(FHandle);
    end;
  end;

  FHandle := 0;
end;

function TSilWindowsWaveAudio.Capture: Boolean;
var
  Buffer: IWaveAudioBuffer;
  i: Integer;
begin
  Buffers.Locked;

  if FUsable and not FIsRunning and (FMode = wmIn) then
  begin
    if Buffers.Count = 0 then
      for i := 1 to 2 do
      begin
        Buffer := TSilWindowsWaveAudioBuffer.Create(nil);
        Buffer.Data.Size := Format.AvgBytesPerSec * 2;
        Buffers.Add(Buffer);
      end;

    FBufferIndex := 0;
    Result := ActivateBuffer(FHandle, Self) and (waveInStart(FHandle) = MMSYSERR_NOERROR);
    FIsRunning := Result;
  end else
    Result := false;
end;

function TSilWindowsWaveAudio.Reproduce: Boolean;
begin
  Buffers.Locked;

  if FUsable and not FIsRunning and (FMode = wmOut) and (Buffers.Count > 0) then
  begin
    FBufferIndex := 0;
    Result := ActivateBuffer(FHandle, Self);
    FIsRunning := Result;
  end else
    Result := false;
end;

function TSilWindowsWaveAudio.DoCreateWaveFormat: IWaveAudioFormat;
begin
  Result := TSilWindowsWaveAudioFormat.Create;
end;

function TSilWindowsWaveAudio.GetPosition: LongWord;
begin
  Result := 0;
end;

function TSilWindowsWaveAudio.Reset: Boolean;
begin
  Result := false;

  if FUsable then
    case FMode of
      wmIn:   Result := waveInReset(FHandle) = MMSYSERR_NOERROR;
      wmOut:  Result := waveOutReset(FHandle) = MMSYSERR_NOERROR;
    end;
end;

function TSilWindowsWaveAudio.Stop: Boolean;
begin
  Result := false;

  if FUsable then
    case FMode of
      wmIn:   Result := waveInStop(FHandle) = MMSYSERR_NOERROR;
      wmOut:  Result := waveOutPause(FHandle) = MMSYSERR_NOERROR;
    end;
end;

function TSilWindowsWaveAudio.GetUsable: Boolean;
begin
  Result := FUsable;
end;

{ TSilWindowsWaveAudioBuffer }

constructor TSilWindowsWaveAudioBuffer.Create(const Stream: IRandomStream);
begin
  inherited Create;

  FBuffer := MemoryStream.Create;
  FHeader.dwUser := LongWord(Self as IWaveAudioBuffer);

  if Assigned(Stream) then
    Serializer.WriteStream(Stream, FBuffer);
end;

destructor TSilWindowsWaveAudioBuffer.Destroy;
begin
  SetIsPrepared(false);
  DropRef(@FWaveAudio);
  FBuffer := nil;
  
  inherited;
end;

function TSilWindowsWaveAudioBuffer.GetBytesRecorded: LongWord;
begin
  Result := FHeader.dwBytesRecorded;
end;

function TSilWindowsWaveAudioBuffer.GetData: IMemoryStream;
begin
  Result := FBuffer;
end;

function TSilWindowsWaveAudioBuffer.GetFlags: LongWord;
begin
  Result := FHeader.dwFlags;
end;

function TSilWindowsWaveAudioBuffer.GetIsPrepared: Boolean;
begin
  Result := FIsPrepared;
end;

function TSilWindowsWaveAudioBuffer.GetLoops: LongWord;
begin
  Result := FHeader.dwLoops;
end;

function TSilWindowsWaveAudioBuffer.GetWaveAudio: IWaveAudio;
begin
  Result := IWaveAudio(FWaveAudio);
end;

function TSilWindowsWaveAudioBuffer.GetPtr: Pointer;
begin
  FHeader.lpData := FBuffer.Memory;
  FHeader.dwBufferLength := FBuffer.Size;
  Result := @FHeader;
end;

procedure TSilWindowsWaveAudioBuffer.SetBytesRecorded(Value: LongWord);
begin
  FHeader.dwBytesRecorded := Value;
end;

procedure TSilWindowsWaveAudioBuffer.SetFlags(Value: LongWord);
begin
  FHeader.dwFlags := Value;
end;

procedure TSilWindowsWaveAudioBuffer.SetIsPrepared(Value: Boolean);
var
  Audio: IWaveAudio;
begin
  Audio := GetWaveAudio;

  if Assigned(Audio) then
  begin
    if Value then
      FIsPrepared := Audio.PrepareBuffer(Self) else
      FIsPrepared := not Audio.UnPrepareBuffer(Self);
  end else
    FIsPrepared := Value;
end;

procedure TSilWindowsWaveAudioBuffer.SetLoops(Value: LongWord);
begin
  FHeader.dwLoops := Value;
end;

procedure TSilWindowsWaveAudioBuffer.SetWaveAudio(const Value: IWaveAudio);
begin
  ChangeRef(Value, @FWaveAudio);
end;

{ TSilWindowsWaveAudioFormat }

const
  ABitsPerSample: array [0..1] of Word = (8, 16);
  AChannels: array [0..1] of Word = (1, 2);
  ASamplesPerSec: array [0..2] of Word = (11025, 22050, 44100);

constructor TSilWindowsWaveAudioFormat.Create;
begin
  inherited Create;

  FFormat.cbSize := 0;
  FFormat.wFormatTag := WAVE_FORMAT_PCM;

  Channels := chStereo;
  SamplesPerSec := sp44100;
  BitsPerSample := bp16;
end;

function TSilWindowsWaveAudioFormat.GetExtraSize: Word;
begin
  Result := FFormat.cbSize;
end;

procedure TSilWindowsWaveAudioFormat.SetExtraSize(Value: Word);
begin
  FFormat.cbSize := Value;
end;

function TSilWindowsWaveAudioFormat.GetAvgBytesPerSec: LongWord;
begin
  Result := FFormat.nAvgBytesPerSec;
end;

function DoFind(const List: array of Word; Value: Word): Word;
var
  i: Integer;
begin
  for i := 0 to High(List) do
    if List[i] = Value then
    begin
      Result := i;
      Exit;
    end;

  Result := 0;
end;

function TSilWindowsWaveAudioFormat.GetBitsPerSample: TBitsPerSample;
begin
  Result := TBitsPerSample(DoFind(ABitsPerSample, FFormat.wBitsPerSample));
end;

function TSilWindowsWaveAudioFormat.GetBlockAlign: Word;
begin
  Result := FFormat.nBlockAlign;
end;

function TSilWindowsWaveAudioFormat.GetChannels: TChannels;
begin
  Result := TChannels(DoFind(AChannels, FFormat.nChannels));
end;

function TSilWindowsWaveAudioFormat.GetPtr: Pointer;
begin
  Result := @FFormat;
end;

function TSilWindowsWaveAudioFormat.GetSamplesPerSec: TSamplesPerSec;
begin
  Result := TSamplesPerSec(DoFind(ASamplesPerSec, FFormat.nSamplesPerSec));
end;

function TSilWindowsWaveAudioFormat.GetTag: Word;
begin
  Result := FFormat.wFormatTag;
end;

procedure TSilWindowsWaveAudioFormat.SetAvgBytesPerSec(Value: LongWord);
begin
  FFormat.nAvgBytesPerSec := Value;
end;

procedure TSilWindowsWaveAudioFormat.SetBitsPerSample(Value: TBitsPerSample);
begin
  FFormat.wBitsPerSample := ABitsPerSample[Ord(Value)];
  DoCalcBlockAlign;
end;

procedure TSilWindowsWaveAudioFormat.SetBlockAlign(Value: Word);
begin
  FFormat.nBlockAlign := Value;
  DoCalcAvgBytesPerSec;
end;

procedure TSilWindowsWaveAudioFormat.SetChannels(Value: TChannels);
begin
  FFormat.nChannels := AChannels[Ord(Value)];
  DoCalcBlockAlign;
end;

procedure TSilWindowsWaveAudioFormat.SetSamplesPerSec(Value: TSamplesPerSec);
begin
  FFormat.nSamplesPerSec := ASamplesPerSec[Ord(Value)];
  DoCalcBlockAlign;
end;

procedure TSilWindowsWaveAudioFormat.SetTag(Value: Word);
begin
  FFormat.wFormatTag := Value;
end;

procedure TSilWindowsWaveAudioFormat.DoCalcBlockAlign;
begin
  try
    SetBlockAlign(FFormat.nChannels * (FFormat.wBitsPerSample div 8));
  except end;
end;

procedure TSilWindowsWaveAudioFormat.DoCalcAvgBytesPerSec;
begin
  try
    SetAvgBytesPerSec(FFormat.nBlockAlign * FFormat.nSamplesPerSec);
  except end;
end;

type
  RWaveFormatRec = packed record
    SamplesPerSec: TSamplesPerSec;
    BitsPerSample: TBitsPerSample;
    Channels: TChannels;
    Format: Word;
  end;

const
  AWaveFormats: array [0..11] of RWaveFormatRec = (
    (SamplesPerSec: sp11025; BitsPerSample: bp8;  Channels: chMono;   Format: WAVE_FORMAT_1M08),
    (SamplesPerSec: sp11025; BitsPerSample: bp8;  Channels: chStereo; Format: WAVE_FORMAT_1S08),
    (SamplesPerSec: sp11025; BitsPerSample: bp16; Channels: chMono;   Format: WAVE_FORMAT_1M16),
    (SamplesPerSec: sp11025; BitsPerSample: bp16; Channels: chStereo; Format: WAVE_FORMAT_1S16),
    (SamplesPerSec: sp22050; BitsPerSample: bp8;  Channels: chMono;   Format: WAVE_FORMAT_2M08),
    (SamplesPerSec: sp22050; BitsPerSample: bp8;  Channels: chStereo; Format: WAVE_FORMAT_2S08),
    (SamplesPerSec: sp22050; BitsPerSample: bp16; Channels: chMono;   Format: WAVE_FORMAT_2M16),
    (SamplesPerSec: sp22050; BitsPerSample: bp16; Channels: chStereo; Format: WAVE_FORMAT_2S16),
    (SamplesPerSec: sp44100; BitsPerSample: bp8;  Channels: chMono;   Format: WAVE_FORMAT_4M08),
    (SamplesPerSec: sp44100; BitsPerSample: bp8;  Channels: chStereo; Format: WAVE_FORMAT_4S08),
    (SamplesPerSec: sp44100; BitsPerSample: bp16; Channels: chMono;   Format: WAVE_FORMAT_4M16),
    (SamplesPerSec: sp44100; BitsPerSample: bp16; Channels: chStereo; Format: WAVE_FORMAT_4S16));

{ TSilWindowsWaveAudioDevice }

constructor TSilWindowsWaveAudioDevice.Create(Id: LongWord);
begin
  inherited Create;
  FId := Id;
end;

function TSilWindowsWaveAudioDevice.GetId: LongWord;
begin
  Result := FId;
end;

function TSilWindowsWaveAudioDevice.GetMode: TWaveAudioMode;
begin
  Result := FMode;
end;

{ TSilWindowsWaveOutAudioDevice }

constructor TSilWindowsWaveOutAudioDevice.Create(Id: LongWord);
begin
  inherited Create(Id);
  FMode := wmOut;
  waveOutGetDevCaps(FId, @FCaps, SizeOf(FCaps));
end;

function TSilWindowsWaveOutAudioDevice.GetName: String;
begin
  Result := FCaps.szPname;
end;

function TSilWindowsWaveOutAudioDevice.Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean; 
var
  i: Integer;
begin
  for i := 0 to High(AWaveFormats) do
    if (AWaveFormats[i].SamplesPerSec = SamplesPerSec) and
      (AWaveFormats[i].BitsPerSample = BitsPerSample) and
      (AWaveFormats[i].Channels = Channels) then
    begin
      Result := FCaps.dwFormats and AWaveFormats[i].Format = AWaveFormats[i].Format;
      Exit;
    end;

  Result := false;
end;

{ TSilWindowsWaveInAudioDevice }

constructor TSilWindowsWaveInAudioDevice.Create(Id: LongWord);
begin
  inherited Create(Id);
  FMode := wmIn;
  waveInGetDevCaps(FId, @FCaps, SizeOf(FCaps));
end;

function TSilWindowsWaveInAudioDevice.GetName: String;
begin
  Result := FCaps.szPname;
end;

function TSilWindowsWaveInAudioDevice.Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(AWaveFormats) do
    if (AWaveFormats[i].SamplesPerSec = SamplesPerSec) and
      (AWaveFormats[i].BitsPerSample = BitsPerSample) and
      (AWaveFormats[i].Channels = Channels) then
    begin
      Result := FCaps.dwFormats and AWaveFormats[i].Format = AWaveFormats[i].Format;
      Exit;
    end;

  Result := false;
end;

function GetFormat(const Buffer: IRandomStream; var Format: IWaveAudioFormat): LongWord;
var
  FileHdr: TWaveFileHeader;
begin
  if Buffer.Size >= SizeOf(TWaveFileHeader) then
  begin
    if not Assigned(Format) then Format := TSilWindowsWaveAudioFormat.Create;
    Buffer.Read(FileHdr, SizeOf(TWaveFileHeader));

    Format.Tag := FileHdr.wFormatTag;

    if Format.Tag = 2 then
      Format.Tag := 1;

    Format.SamplesPerSec := TSamplesPerSec(DoFind(ASamplesPerSec, FileHdr.nSamplesPerSec));

    Format.BlockAlign := FileHdr.nBlockAlign;

    if Format.BlockAlign = 2 then
      Format.Channels := chStereo else
      Format.Channels := TChannels(DoFind(AChannels, FileHdr.nChannels));

    Format.BitsPerSample := TBitsPerSample(DoFind(ABitsPerSample, FileHdr.wBitsPerSample));
    Format.ExtraSize := FileHdr.lUnknown;

    Result := FileHdr.lDataSize;
  end else
    Result := 0;
end;

end.
