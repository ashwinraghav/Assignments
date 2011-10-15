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

unit SilOiMixerDevice;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLiEnumerator;

type
  IMixerDevice = interface;
  IMixerComponent = interface;
  IMixerComponentList = interface;
  IMixerControl = interface;
  IMixerControlList = interface;
  IMixerMasterPlaybackComponent = interface;
  IMixerMasterRecordingComponent = interface;
  IMixerStandardComponent = interface;
  IMixerRecordingComponent = interface;
  IMixerVolumeComponent = interface;
  IMixerRecordingComponentList = interface;
  IMixerVolumeComponentList = interface;

  TMixerControlClass = (
    ccUnknown, ccCustom, ccMeter, ccSwitch, ccNumber, ccSlider, ccFader, ccTime, ccList);

  TMixerControlSubClass = (
    scUnknown, scPolled, scBoolean, scButton, scMicrosecs, scMillisecs, scSingle, scMultiple);

  TMixerControlUnits = (
    cuUnknown, cuCustom, cuBoolean, cuSigned, cuUnsigned, cuDecibels, cuPercent);

  TMixerControlType = (
    ntUnknown, ntCustom, ntBooleanMeter, ntSignedMeter, ntPeakMeter, ntUnsignedMeter, ntBoolean,
    ntOnOff, ntMute, ntMono, ntLoudness, ntStereoEnhacer, ntButton, ntDecibels, ntSigned,
    ntUnsigned, ntPercent, ntSlider, ntPan, ntQSoundPan, ntFader, ntVolume, ntBass, ntTreble,
    ntEqualizer, ntSingleSelect, ntMux, ntMultipleSelect, ntMixer, ntMicroTime, ntMilliTime);

  TMixerComponentType = (
    ctUnknown, ctDigital, ctLine, ctMonitor, ctSpeakers, ctHeadphones, ctTelephone, ctWaveIn,
    ctVoiceIn, ctMicrophone, ctSynthesizer, ctCompactDisc, ctPcSpeaker, ctWaveOut, ctAuxiliary,
    ctAnalog);

  IMixerDevice = interface
    ['{0F822454-EC6F-4299-8D52-E426D8352CDB}']
    function GetPlayback: IMixerMasterPlaybackComponent;
    function GetRecording: IMixerMasterRecordingComponent;
    function GetName: String;
    function GetManufacturerId: Word;
    function GetProductId: Word;
    function GetDriverVersion: LongWord;
    property Playback: IMixerMasterPlaybackComponent read GetPlayback;
    property Recording: IMixerMasterRecordingComponent read GetRecording;
    property Name: String read GetName;
    property ManufacturerId: Word read GetManufacturerId;
    property ProductId: Word read GetProductId;
    property DriverVersion: LongWord read GetDriverVersion;
  end;

  IMixerComponent = interface
    ['{363F3A3E-154A-481A-BD5D-C1B524B75535}']
    function GetDevice: IMixerDevice;
    function GetId: LongWord;
    function GetComponentType: TMixerComponentType;
    function GetChannelCount: Word;
    function GetControls: IMixerControlList;
    function GetName: String;
    property Device: IMixerDevice read GetDevice;
    property Id: LongWord read GetId;
    property ComponentType: TMixerComponentType read GetComponentType;
    property ChannelCount: Word read GetChannelCount;
    property Controls: IMixerControlList read GetControls;
    property Name: String read GetName;
  end;

  IMixerStandardComponent = interface (IMixerComponent)
    ['{92593A0A-5156-4542-97C8-6D34ADDE48BB}']
    function GetVolume: Word;
    function GetChannelVolume(Index: Word): Word;
    procedure SetVolume(Value: Word);
    procedure SetChannelVolume(Index, Value: Word);
    property Volume: Word read GetVolume write SetVolume;
    property ChannelVolume[Index: Word]: Word read GetChannelVolume write SetChannelVolume;
  end;

  IMixerRecordingComponent = interface (IMixerStandardComponent)
    ['{83FB3C2C-EE6E-4DBA-BD20-A67A37035F9A}']
    function GetIsSelected: Boolean;
    procedure Select;
    property IsSelected: Boolean read GetIsSelected;
  end;
  
  IMixerVolumeComponent = interface (IMixerStandardComponent)
    ['{83FB3C2C-EE6E-4DBA-BD20-A67A37035F9A}']
    function GetIsMuted: Boolean;
    procedure Mute;
    procedure UnMute;
    property IsMuted: Boolean read GetIsMuted;
  end;

  IMixerMasterPlaybackComponent = interface (IMixerComponent)
    ['{505520AB-6B14-44E1-8A8C-4D5A99BC8FC9}']
    function GetComponents: IMixerVolumeComponentList;
    function GetMaster: IMixerVolumeComponent;
    function GetWave: IMixerVolumeComponent;
    function GetMidi: IMixerVolumeComponent;
    function GetCDAudio: IMixerVolumeComponent;
    property Components: IMixerVolumeComponentList read GetComponents;
    property Master: IMixerVolumeComponent read GetMaster;
    property Wave: IMixerVolumeComponent read GetWave;
    property Midi: IMixerVolumeComponent read GetMidi;
    property CDAudio: IMixerVolumeComponent read GetCDAudio;
  end;

  IMixerMasterRecordingComponent = interface (IMixerComponent)
    ['{45B509A6-A262-4430-B565-67D7178DA586}']
    function GetComponents: IMixerRecordingComponentList;
    function GetCDAudio: IMixerRecordingComponent;
    function GetMicrophone: IMixerRecordingComponent;
    procedure Select(const Component: IMixerComponent; Value: Boolean);
    function IsSelected(const Component: IMixerComponent): Boolean;
    property Components: IMixerRecordingComponentList read GetComponents;
    property CDAudio: IMixerRecordingComponent read GetCDAudio;
    property Microphone: IMixerRecordingComponent read GetMicrophone;
  end;

  IMixerComponentList = interface
    ['{7DF3657F-FFC3-43BF-B15F-8D2E75FAC40D}']
    function GetCount: Integer;
    procedure Delete(Index: Integer);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Clear;
    function Locked: ILock;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    function Enumerate(var Enum: IEnumerator; out Item: IMixerComponent): Boolean;
    property Count: Integer read GetCount;
  end;

  IMixerRecordingComponentList = interface (IMixerComponentList)
    ['{3676D901-0612-44AF-9912-912899F311E9}']
    function Get(Index: Integer): IMixerRecordingComponent;
    function Add(const Value: IMixerRecordingComponent): Integer;
    procedure Put(Index: Integer; const Value: IMixerRecordingComponent);
    function First: IMixerRecordingComponent;
    function Last: IMixerRecordingComponent;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerRecordingComponent): Boolean;
    property Items[Index: Integer]: IMixerRecordingComponent read Get write Put; default;
  end;

  IMixerVolumeComponentList = interface (IMixerComponentList)
    ['{3676D901-0612-44AF-9912-912899F311E9}']
    function Get(Index: Integer): IMixerVolumeComponent;
    function Add(const Value: IMixerVolumeComponent): Integer;
    procedure Put(Index: Integer; const Value: IMixerVolumeComponent);
    function First: IMixerVolumeComponent;
    function Last: IMixerVolumeComponent;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerVolumeComponent): Boolean;
    property Items[Index: Integer]: IMixerVolumeComponent read Get write Put; default;
  end;

  IMixerControl = interface
    ['{5C710CFC-F488-4FED-9E03-A7C88F2F928E}']
    function GetComponent: IMixerComponent;
    function GetControlClass: TMixerControlClass;
    function GetControlSubClass: TMixerControlSubClass;
    function GetControlUnits: TMixerControlUnits;
    function GetControlType: TMixerControlType;
    function GetValue: Integer;
    function GetChannelValue(Index: Word): Integer;
    function GetName: String;
    function GetId: LongWord;
    function GetUpperBound: Integer;
    function GetLowerBound: Integer;
    function GetSteps: Integer;
    procedure SetComponent(const Value: IMixerComponent);
    procedure SetValue(Value: Integer);
    procedure SetChannelValue(Index: Word; Value: Integer);
    property Component: IMixerComponent read GetComponent write SetComponent;
    property ControlClass: TMixerControlClass read GetControlClass;
    property ControlSubClass: TMixerControlSubClass read GetControlSubClass;
    property ControlUnits: TMixerControlUnits read GetControlUnits;
    property ControlType: TMixerControlType read GetControlType;
    property Value: Integer read GetValue write SetValue;
    property ChannelValue[Index: Word]: Integer read GetChannelValue write SetChannelValue;
    property Name: String read GetName;
    property Id: LongWord read GetId;
    property UpperBound: Integer read GetLowerBound;
    property LowerBound: Integer read GetUpperBound;
    property Steps: Integer read GetSteps;
  end;

  IMixerControlList = interface
    ['{7DF3657F-FFC3-43BF-B15F-8D2E75FAC40D}']
    function GetCount: Integer;
    function Get(Index: Integer): IMixerControl;
    function Add(const Value: IMixerControl): Integer;
    procedure Put(Index: Integer; const Value: IMixerControl);
    procedure Delete(Index: Integer);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    function First: IMixerControl;
    function Last: IMixerControl;
    procedure Clear;
    function Locked: ILock;
    function Enumerate(var Enum: IEnumerator; out Item: IMixerControl): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    function Find(const Name: String; out Control: IMixerControl): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IMixerControl read Get write Put; default;
  end;

implementation

end.
