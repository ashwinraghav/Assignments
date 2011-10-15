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

unit SilOiWaveAudio;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLiStream,
  SilLiEnumerator;

type
  IWaveAudio = interface;
  IWaveAudioInHook = interface;
  IWaveAudioOutHook = interface;
  IWaveAudioBuffer = interface;
  IWaveAudioFormat = interface;
  IWaveAudioBufferList = interface;
  IWaveAudioDevice = interface;
  IWaveAudioDeviceList = interface;
  IWaveAudioControl = interface;

  TWaveAudioMode = (wmUnknown, wmIn, wmOut);
  TSamplesPerSec = (sp11025, sp22050, sp44100);
  TBitsPerSample = (bp8, bp16);
  TChannels = (chMono, chStereo);

  IWaveAudioBuffer = interface
    ['{FD0F60C2-6812-457F-97D8-3F4E58FB7F1D}']
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
    property WaveAudio: IWaveAudio read GetWaveAudio write SetWaveAudio;
  end;

  IWaveAudioBufferList = interface
    ['{C5ED82D7-D85D-44A0-BB2C-D9ED9EFFE3B4}']
    function GetCount: Integer;
    function Get(Index: Integer): IWaveAudioBuffer;
    function Add(const Value: IWaveAudioBuffer): Integer;
    procedure Put(Index: Integer; const Value: IWaveAudioBuffer);
    procedure Delete(Index: Integer);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    function First: IWaveAudioBuffer;
    function Last: IWaveAudioBuffer;
    procedure Clear;
    function Locked: ILock;
    function Enumerate(var Enum: IEnumerator; out Item: IWaveAudioBuffer): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IWaveAudioBuffer read Get write Put; default;
  end;

  IWaveAudioFormat = interface
    ['{98191704-C4FB-48BC-9EAC-23188835B780}']
    function GetTag: Word;
    function GetChannels: TChannels;
    function GetSamplesPerSec: TSamplesPerSec;
    function GetAvgBytesPerSec: LongWord;
    function GetBlockAlign: Word;
    function GetBitsPerSample: TBitsPerSample;
    function GetExtraSize: Word;
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
    property ExtraSize: Word read GetExtraSize write SetExtraSize;
  end;

  IWaveAudio = interface
    ['{090C1E4D-98F8-402B-839B-109884CDED10}']
    function GetFormat: IWaveAudioFormat;
    function GetBuffers: IWaveAudioBufferList;
    function GetPosition: LongWord;
    function GetMode: TWaveAudioMode;
    function GetIsRunning: Boolean;
    function GetIsLoopEnabled: Boolean;
    function GetDevice: IWaveAudioDevice;
    procedure SetIsLoopEnabled(Value: Boolean);
    function GetIsDiscardEnabled: Boolean;
    procedure SetIsDiscardEnabled(Value: Boolean);
    function Initialize(Mode: TWaveAudioMode; const Hook: IUnknown = nil): Boolean;
    function PrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
    function UnPrepareBuffer(const Buffer: IWaveAudioBuffer): Boolean;
    procedure Finalize;
    function Capture: Boolean;
    function Reproduce: Boolean;
    function Reset: Boolean;
    function Stop: Boolean;
    property Mode: TWaveAudioMode read GetMode;
    property Format: IWaveAudioFormat read GetFormat;
    property Buffers: IWaveAudioBufferList read GetBuffers;
    property Position: LongWord read GetPosition;
    property IsRunning: Boolean read GetIsRunning;
    property IsLoopEnabled: Boolean read GetIsLoopEnabled write SetIsLoopEnabled;
    property IsDiscardEnabled: Boolean read GetIsDiscardEnabled write SetIsDiscardEnabled;
    property Device: IWaveAudioDevice read GetDevice;
  end;

  IWaveAudioControl = interface (IWaveAudio)
    ['{2680B734-213F-47F6-B6C0-C43A5DA597EA}']
    function GetInHook: IWaveAudioInHook;
    function GetOutHook: IWaveAudioOutHook;
    function GetBufferIndex: Integer;
    function Locked: ILock;
    function GetUsable: Boolean;
    procedure NextBuffer;
    property BufferIndex: Integer read GetBufferIndex;
    property HookIn: IWaveAudioInHook read GetInHook;
    property HookOut: IWaveAudioOutHook read GetOutHook;
    property IsUsable: Boolean read GetUsable;
  end;

  IWaveAudioInHook = interface
    ['{6507EB5E-31DC-435A-B94C-FA6099F6DA0F}']
    procedure BufferFull(const Sender: IWaveAudio; const Buffer: IWaveAudioBuffer);
    procedure StopCapture(const Sender: IWaveAudio);
  end;

  IWaveAudioOutHook = interface
    ['{A29E7D3A-8E3C-4EF3-85FF-956881652515}']
    procedure BufferPlayed(const Sender: IWaveAudio; const Buffer: IWaveAudioBuffer);
    procedure StopReproduce(const Sender: IWaveAudio);
  end;

  IWaveAudioDevice = interface
    ['{65C93EA9-9D4C-4A6B-BB49-31FFB968ED2A}']
    function GetId: LongWord;
    function GetName: String;
    function GetMode: TWaveAudioMode;
    function Supports(SamplesPerSec: TSamplesPerSec; BitsPerSample: TBitsPerSample; Channels: TChannels): Boolean;
    property Id: LongWord read GetId;
    property Name: String read GetName;
    property Mode: TWaveAudioMode read GetMode;
  end;

  IWaveAudioDeviceList = interface
    ['{F6C4279F-F837-42D7-8CCC-94593AD73FC5}']
    function GetCount: Integer;
    function Get(Index: Integer): IWaveAudioDevice;
    function Add(const Value: IWaveAudioDevice): Integer;
    procedure Put(Index: Integer; const Value: IWaveAudioDevice);
    procedure Delete(Index: Integer);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    function First: IWaveAudioDevice;
    function Last: IWaveAudioDevice;
    procedure Clear;
    function Locked: ILock;
    function Enumerate(var Enum: IEnumerator; out Item: IWaveAudioDevice): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IWaveAudioDevice read Get write Put; default;
  end;

implementation

end.
