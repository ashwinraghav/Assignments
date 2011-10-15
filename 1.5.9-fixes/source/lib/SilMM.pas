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

unit SilMM;

{$I Defines.inc}

interface

uses
  SilOiWaveAudio,
  SilOiMixerDevice,

  SilOtWaveAudio,
  SilOtMixerDevice;

type
  Mixer                         = SilOtMixerDevice.Mixer;
  WaveAudio                     = SilOtWaveAudio.WaveAudio;

type
  IWaveAudio                    = SilOiWaveAudio.IWaveAudio;
  IWaveAudioInHook              = SilOiWaveAudio.IWaveAudioInHook;
  IWaveAudioOutHook             = SilOiWaveAudio.IWaveAudioOutHook;
  IWaveAudioBuffer              = SilOiWaveAudio.IWaveAudioBuffer;
  IWaveAudioFormat              = SilOiWaveAudio.IWaveAudioFormat;
  IWaveAudioBufferList          = SilOiWaveAudio.IWaveAudioBufferList;
  IWaveAudioDevice              = SilOiWaveAudio.IWaveAudioDevice;
  IWaveAudioDeviceList          = SilOiWaveAudio.IWaveAudioDeviceList;
  IWaveAudioControl             = SilOiWaveAudio.IWaveAudioControl;

  TWaveAudioMode                = SilOiWaveAudio.TWaveAudioMode;
  TSamplesPerSec                = SilOiWaveAudio.TSamplesPerSec;
  TBitsPerSample                = SilOiWaveAudio.TBitsPerSample;
  TChannels                     = SilOiWaveAudio.TChannels;

const
  wmIn                          = SilOiWaveAudio.wmIn;
  wmOut                         = SilOiWaveAudio.wmOut;

const
  sp11025                       = SilOiWaveAudio.sp11025;
  sp22050                       = SilOiWaveAudio.sp22050;
  sp44100                       = SilOiWaveAudio.sp44100;

  bp8                           = SilOiWaveAudio.bp8;
  bp16                          = SilOiWaveAudio.bp16;

  chMono                        = SilOiWaveAudio.chMono;
  chStereo                      = SilOiWaveAudio.chStereo;

type
  IMixerDevice                  = SilOiMixerDevice.IMixerDevice;
  IMixerComponent               = SilOiMixerDevice.IMixerComponent;
  IMixerComponentList           = SilOiMixerDevice.IMixerComponentList;
  IMixerControl                 = SilOiMixerDevice.IMixerControl;
  IMixerControlList             = SilOiMixerDevice.IMixerControlList;
  IMixerMasterPlaybackComponent = SilOiMixerDevice.IMixerMasterPlaybackComponent;
  IMixerMasterRecordingComponent= SilOiMixerDevice.IMixerMasterRecordingComponent;
  IMixerStandardComponent       = SilOiMixerDevice.IMixerStandardComponent;
  IMixerRecordingComponent      = SilOiMixerDevice.IMixerRecordingComponent;
  IMixerVolumeComponent         = SilOiMixerDevice.IMixerVolumeComponent;
  IMixerRecordingComponentList  = SilOiMixerDevice.IMixerRecordingComponentList;
  IMixerVolumeComponentList     = SilOiMixerDevice.IMixerVolumeComponentList;

  TMixerControlClass            = SilOiMixerDevice.TMixerControlClass;
  TMixerControlSubClass         = SilOiMixerDevice.TMixerControlSubClass;
  TMixerControlUnits            = SilOiMixerDevice.TMixerControlUnits;
  TMixerComponentType           = SilOiMixerDevice.TMixerComponentType;
  TMixerControlType             = SilOiMixerDevice.TMixerControlType;

const
  ccUnknown                     = SilOiMixerDevice.ccUnknown;
  ccCustom                      = SilOiMixerDevice.ccCustom;
  ccMeter                       = SilOiMixerDevice.ccMeter;
  ccSwitch                      = SilOiMixerDevice.ccSwitch;
  ccNumber                      = SilOiMixerDevice.ccNumber;
  ccSlider                      = SilOiMixerDevice.ccSlider;
  ccFader                       = SilOiMixerDevice.ccFader;
  ccTime                        = SilOiMixerDevice.ccTime;
  ccList                        = SilOiMixerDevice.ccList;

const
  scUnknown                     = SilOiMixerDevice.scUnknown;
  scPolled                      = SilOiMixerDevice.scPolled;
  scBoolean                     = SilOiMixerDevice.scBoolean;
  scButton                      = SilOiMixerDevice.scButton;
  scMicrosecs                   = SilOiMixerDevice.scMicrosecs;
  scMillisecs                   = SilOiMixerDevice.scMillisecs;
  scSingle                      = SilOiMixerDevice.scSingle;
  scMultiple                    = SilOiMixerDevice.scMultiple;

const
  cuUnknown                     = SilOiMixerDevice.cuUnknown;
  cuCustom                      = SilOiMixerDevice.cuCustom;
  cuBoolean                     = SilOiMixerDevice.cuBoolean;
  cuSigned                      = SilOiMixerDevice.cuSigned;
  cuUnsigned                    = SilOiMixerDevice.cuUnsigned;
  cuDecibels                    = SilOiMixerDevice.cuDecibels;
  cuPercent                     = SilOiMixerDevice.cuPercent;

const
  ctUnknown                     = SilOiMixerDevice.ctUnknown;
  ctDigital                     = SilOiMixerDevice.ctDigital;
  ctLine                        = SilOiMixerDevice.ctLine;
  ctMonitor                     = SilOiMixerDevice.ctMonitor;
  ctSpeakers                    = SilOiMixerDevice.ctSpeakers;
  ctHeadphones                  = SilOiMixerDevice.ctHeadphones;
  ctTelephone                   = SilOiMixerDevice.ctTelephone;
  ctWaveIn                      = SilOiMixerDevice.ctWaveIn;
  ctVoiceIn                     = SilOiMixerDevice.ctVoiceIn;
  ctMicrophone                  = SilOiMixerDevice.ctMicrophone;
  ctSynthesizer                 = SilOiMixerDevice.ctSynthesizer;
  ctCompactDisc                 = SilOiMixerDevice.ctCompactDisc;
  ctPcSpeaker                   = SilOiMixerDevice.ctPcSpeaker;
  ctWaveOut                     = SilOiMixerDevice.ctWaveOut;
  ctAuxiliary                   = SilOiMixerDevice.ctAuxiliary;
  ctAnalog                      = SilOiMixerDevice.ctAnalog;

const
  ntUnknown                     = SilOiMixerDevice.ntUnknown;
  ntCustom                      = SilOiMixerDevice.ntCustom;
  ntBooleanMeter                = SilOiMixerDevice.ntBooleanMeter;
  ntSignedMeter                 = SilOiMixerDevice.ntSignedMeter;
  ntPeakMeter                   = SilOiMixerDevice.ntPeakMeter;
  ntUnsignedMeter               = SilOiMixerDevice.ntUnsignedMeter;
  ntBoolean                     = SilOiMixerDevice.ntBoolean;
  ntOnOff                       = SilOiMixerDevice.ntOnOff;
  ntMute                        = SilOiMixerDevice.ntMute;
  ntMono                        = SilOiMixerDevice.ntMono;
  ntLoudness                    = SilOiMixerDevice.ntLoudness;
  ntStereoEnhacer               = SilOiMixerDevice.ntStereoEnhacer;
  ntButton                      = SilOiMixerDevice.ntButton;
  ntDecibels                    = SilOiMixerDevice.ntDecibels;
  ntSigned                      = SilOiMixerDevice.ntSigned;
  ntUnsigned                    = SilOiMixerDevice.ntUnsigned;
  ntPercent                     = SilOiMixerDevice.ntPercent;
  ntSlider                      = SilOiMixerDevice.ntSlider;
  ntPan                         = SilOiMixerDevice.ntPan;
  ntQSoundPan                   = SilOiMixerDevice.ntQSoundPan;
  ntFader                       = SilOiMixerDevice.ntFader;
  ntVolume                      = SilOiMixerDevice.ntVolume;
  ntBass                        = SilOiMixerDevice.ntBass;
  ntTreble                      = SilOiMixerDevice.ntTreble;
  ntEqualizer                   = SilOiMixerDevice.ntEqualizer;
  ntSingleSelect                = SilOiMixerDevice.ntSingleSelect;
  ntMux                         = SilOiMixerDevice.ntMux;
  ntMultipleSelect              = SilOiMixerDevice.ntMultipleSelect;
  ntMixer                       = SilOiMixerDevice.ntMixer;
  ntMicroTime                   = SilOiMixerDevice.ntMicroTime;
  ntMilliTime                   = SilOiMixerDevice.ntMilliTime;

implementation

end.
 