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

unit SilOtWaveAudio;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiWaveAudio,
  SilOjWaveAudio;

type
  WaveAudio = class (SilOjWaveAudio.WaveAudio)
    class function GetDeviceList(Mode: TWaveAudioMode; Locked: Boolean = false): IWaveAudioDeviceList; override;
    class function GetFormat(const Buffer: IRandomStream; var Format: IWaveAudioFormat): LongWord; override;
    class procedure PlayFile(const FileName: String); override;
  end;

implementation

uses
  MMSystem,
  SilOkWaveAudio,
  SilOmWaveAudio;

{ WindowsWaveAudio }

class function WaveAudio.GetDeviceList(Mode: TWaveAudioMode; Locked: Boolean): IWaveAudioDeviceList;
var
  i, Count: Integer;
  DeviceClass: TSilWindowsWaveAudioDeviceClass;
  Device: IWaveAudioDevice;
begin
  Result := TSilWaveAudioDeviceList.Create(Locked);
  DeviceClass := nil;
  Count := 0;

  case Mode of
    wmIn:
    begin
      Count := waveInGetNumDevs;
      DeviceClass := TSilWindowsWaveInAudioDevice;
    end;

    wmOut:
    begin
      Count := waveOutGetNumDevs;
      DeviceClass := TSilWindowsWaveOutAudioDevice;
    end;
  end;

  if Assigned(DeviceClass) then
    for i := 0 to Count - 1 do
    begin
      Device := DeviceClass.Create(i);
      Result.Add(Device);
    end;
end;

class function WaveAudio.GetFormat(const Buffer: IRandomStream; var Format: IWaveAudioFormat): LongWord;
begin
  Result := SilOmWaveAudio.GetFormat(Buffer, Format);
end;

class procedure WaveAudio.PlayFile(const FileName: String);
begin
  PlaySound(PChar(FileName), 0, SND_FILENAME);
end;

end.
