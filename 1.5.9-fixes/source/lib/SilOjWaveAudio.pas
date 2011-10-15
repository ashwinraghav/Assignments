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

unit SilOjWaveAudio;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiStream,
  SilOiFile,
  SilOiWaveAudio;

type
  WaveAudio = class (Tool)
    class function Create(const Device: IWaveAudioDevice = nil): IWaveAudio; overload; virtual;
    class function Create(const FileName: String; const Device: IWaveAudioDevice = nil): IWaveAudio; overload; virtual; 
    class function GetDeviceList(Mode: TWaveAudioMode; Locked: Boolean = false): IWaveAudioDeviceList; virtual; abstract;
    class function DeviceList(Locked: Boolean = false): IWaveAudioDeviceList; virtual;
    class function Format: IWaveAudioFormat; virtual;
    class function Buffer(const Stream: IRandomStream = nil): IWaveAudioBuffer; virtual;
    class function BufferList(const WaveAudio: IWaveAudio = nil): IWaveAudioBufferList; virtual;
    class function GetFormat(const Buffer: IRandomStream; var Format: IWaveAudioFormat): LongWord; virtual; abstract;
    class procedure PlayFile(const FileName: String); virtual; abstract;
  end;

implementation

uses
  SilOtTool,
  SilOkWaveAudio,
  SilOsWaveAudio;

{ WaveAudio }

class function WaveAudio.Create(const FileName: String; const Device: IWaveAudioDevice): IWaveAudio;
var
  Source: IFile;
  Size: LongWord;
  Format: IWaveAudioFormat;
  Buffer: IWaveAudioBuffer;
begin
  Result := Create(Device);
  Source := OS.FileSystem.OpenFile(FileName, fmAccessRead, fmShareRead);
  Format := Result.Format;
  Size := OSWaveAudio.GetFormat(Source.Stream, Format);

  if Size > 0 then
  begin
    Buffer := WaveAudio.Buffer;
    Buffer.Data.Size := Size;
    Source.Stream.Read(Buffer.Data.Memory^, Size);
    Result.Buffers.Add(Buffer);
  end;
end;

class function WaveAudio.Buffer(const Stream: IRandomStream): IWaveAudioBuffer;
begin
  Result := TSilOsWaveAudioBuffer.Create(Stream);
end;

class function WaveAudio.BufferList(const WaveAudio: IWaveAudio = nil): IWaveAudioBufferList;
begin
  Result := TSilWaveAudioBufferList.Create(WaveAudio);
end;

class function WaveAudio.Create(const Device: IWaveAudioDevice): IWaveAudio;
begin
  Result := TSilOsWaveAudio.Create(Device);
end;

class function WaveAudio.DeviceList(Locked: Boolean): IWaveAudioDeviceList;
begin
  Result := TSilWaveAudioDeviceList.Create(Locked);
end;

class function WaveAudio.Format: IWaveAudioFormat;
begin
  Result := TSilOsWaveAudioFormat.Create;
end;

end.
