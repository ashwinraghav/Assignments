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

unit SilOmMixerDevice;

{$I Defines.inc}

interface

uses
  MMSystem,

  SilBeTypes,
  SilLiEnumerator,
  SilOiMixerDevice,
  SilLkObject,
  SilLkInterfaced,
  SilOkMixerDevice;

type
  IWindowsMixerSelect = interface
    ['{D999F91F-A991-41E5-B893-956617D31955}']
    procedure Select(const Component: IMixerComponent; Value: Boolean);
    function IsSelected(const Component: IMixerComponent): Boolean;
  end;

  IWindowsMixerComponent = interface
    ['{8B717D48-1D94-4987-8BAC-E5AC5BB1F353}']
    function GetMixerLine: TMixerLine;
  end;

  TSilWindowsMixerDevice = class (TSilMixerDevice)
  private
    FHandle: Integer;
    FMixerCaps: TMixerCaps;
  private
    procedure DoInit;
  protected
    procedure DoCheckComponents; override;
  protected
    function GetName: String; override;
    function GetManufacturerId: Word; override;
    function GetProductId: Word; override;
    function GetDriverVersion: LongWord; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSilWindowsMixerComponent = class (
    // extends
    TSilInterfacedObject,
    // implements
    IMixerComponent,
    IWindowsMixerComponent)
  private
    FHandle: Integer;
    FMixerLine: TMixerLine;
    FDevice: Pointer;
    FControls: IMixerControlList;
  private
    function GetWinDevice: IMixerDevice;
  protected
    procedure DoCheckControls; virtual;
  protected // IWindowsMixerComponent
    function GetMixerLine: TMixerLine;
  protected // IMixerComponent
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
  public
    constructor Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine); virtual;
    destructor Destroy; override;
  end;

  TSilWindowsMixerStandardComponent = class (
    // extends
    TSilWindowsMixerComponent,
    // implements
    IMixerStandardComponent,
    IMixerVolumeComponent,
    IMixerRecordingComponent)
  private
    FVolume: IMixerControl;
    FMute: IMixerControl;
    FMux: IWindowsMixerSelect;
    procedure DoInitControl;
  protected // IMixerStandardComponent
    function GetVolume: Word;
    function GetChannelVolume(Index: Word): Word;
    procedure SetVolume(Value: Word);
    procedure SetChannelVolume(Index, Value: Word);
    property Volume: Word read GetVolume write SetVolume;
    property ChannelVolume[Index: Word]: Word read GetChannelVolume write SetChannelVolume;
  protected // IMixerVolumeComponent
    function GetIsMuted: Boolean;
    procedure Mute;
    procedure UnMute;
    property IsMuted: Boolean read GetIsMuted;
  protected // IMixerRecordingComponent
    function GetIsSelected: Boolean;
    procedure Select;
    property IsSelected: Boolean read GetIsSelected;
  public
    constructor Create(Handle: Integer; const Device: IMixerDevice; const Mux: IWindowsMixerSelect; const MixerLine: TMixerLine); reintroduce;
    destructor Destroy; override;
  end;

  TSilWindowsMixerMasterPlaybackComponent = class (
    // extends
    TSilWindowsMixerComponent,
    // implements
    IMixerMasterPlaybackComponent)
  private
    FComponents: IMixerVolumeComponentList;
    FMaster: IMixerVolumeComponent;
    FWave: IMixerVolumeComponent;
    FMidi: IMixerVolumeComponent;
    FCDAudio: IMixerVolumeComponent;
    procedure DoGetComponents;
    procedure DoFindComponent(const ComponentType: TMixerComponentType; var Component: IMixerVolumeComponent);
    procedure DoGetMaster;
  protected // IMixerMasterPlaybackComponent
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
  public
    constructor Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine); override;
    destructor Destroy; override;
  end;

  TSilWindowsMixerMasterRecordingComponent = class (
    // extends
    TSilWindowsMixerComponent,
    // implements
    IMixerMasterRecordingComponent)
  private
    FComponents: IMixerRecordingComponentList;
    FMux: IWindowsMixerSelect;
    FCDAudio: IMixerRecordingComponent;
    FMicrophone: IMixerRecordingComponent;
    procedure DoGetComponents;
    procedure DoFindComponent(const ComponentType: TMixerComponentType; var Component: IMixerRecordingComponent);
    procedure DoGetMux;
  protected
    function GetComponents: IMixerRecordingComponentList;
    function GetCDAudio: IMixerRecordingComponent;
    function GetMicrophone: IMixerRecordingComponent;
    procedure Select(const Component: IMixerComponent; Value: Boolean);
    function IsSelected(const Component: IMixerComponent): Boolean;
    property Components: IMixerRecordingComponentList read GetComponents;
    property CDAudio: IMixerRecordingComponent read GetCDAudio;
    property Microphone: IMixerRecordingComponent read GetMicrophone;
  public
    constructor Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine); override;
    destructor Destroy; override;
  end;

  TSilWindowsMixerControl = class (
    // extends
    TSilInterfacedObject,
    // implements
    IMixerControl,
    IWindowsMixerSelect)
  protected
    FHandle: Integer;
    FComponent: Pointer;
    FMixerControl: TMixerControl;
    function DoControlValues(MustSet: Boolean; Index: Integer; var Values: TIntegerArray): Boolean;
  protected // IMixerControl
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
    property ChannelValue[Index: Word]: Integer read GetChannelValue;
    property Name: String read GetName;
    property Id: LongWord read GetId;
    property UpperBound: Integer read GetLowerBound;
    property LowerBound: Integer read GetUpperBound;
    property Steps: Integer read GetSteps;
  protected // IWindowsMixerSelect
    procedure Select(const Component: IMixerComponent; Value: Boolean);
    function IsSelected(const Component: IMixerComponent): Boolean;
  public
    constructor Create(Handle: Integer; const Component: IMixerComponent; const MixerControl: TMixerControl);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBtVart;

procedure DoInitValues(var Values: TIntegerArray; Size: Integer);
begin
  SetLength(Values, Size);
  FillChar(Values[0], High(Values) * SizeOf(Values[0]), 0);
end;

{ TSilWindowsMixerDevice }

constructor TSilWindowsMixerDevice.Create;
begin
  inherited Create;

  mixerOpen(@FHandle, 0, 0, 0, MIXER_OBJECTF_MIXER);
  DoInit;
end;

destructor TSilWindowsMixerDevice.Destroy;
begin
  mixerClose(FHandle);
  inherited;
end;

procedure TSilWindowsMixerDevice.DoInit;
begin
  mixerGetDevCaps(FHandle, @FMixerCaps, SizeOf(FMixerCaps));
end;

procedure TSilWindowsMixerDevice.DoCheckComponents;
var
  i{, Count}: Integer;
  Line: TMixerLine;
begin
  //Count := 0;

  for i := MIXERLINE_COMPONENTTYPE_DST_FIRST to MIXERLINE_COMPONENTTYPE_DST_LAST do
  begin
    FillChar(Line, SizeOf(Line), 0);
    Line.cbStruct := SizeOf(Line);
    Line.dwComponentType := i;

    if mixerGetLineInfo(FHandle, @Line, MIXER_GETLINEINFOF_COMPONENTTYPE) = MMSYSERR_NOERROR then
    begin
      //Inc(Count);

      case Line.dwDestination of
        0:  DoInitPlayback(TSilWindowsMixerMasterPlaybackComponent.Create(FHandle, Self, Line));
        1:  DoInitRecording(TSilWindowsMixerMasterRecordingComponent.Create(FHandle, Self, Line));
      end;
    end;

    //if Count >= 2 then Break;
  end;
end;

function TSilWindowsMixerDevice.GetName: String;
begin
  Result := FMixerCaps.szPname;
end;

function TSilWindowsMixerDevice.GetDriverVersion: LongWord;
begin
  Result := FMixerCaps.vDriverVersion;
end;

function TSilWindowsMixerDevice.GetManufacturerId: Word;
begin
  Result := FMixerCaps.wMid;
end;

function TSilWindowsMixerDevice.GetProductId: Word;
begin
  Result := FMixerCaps.wPid;
end;

{ TSilWindowsMixerComponent }

type
  PMIXERCONTROLS = ^TMIXERCONTROLS;
  TMIXERCONTROLS = array [0..0] of TMIXERCONTROL;

  PMixerControlDetailsListTexts = ^PMixerControlDetailsListTextList;
  PMixerControlDetailsListTextList = array [0..0] of TMixerControlDetailsListText;

  PMixerControlDetailsBooleans = ^TMixerControlDetailsBooleanList;
  TMixerControlDetailsBooleanList = array [0..0] of TMixerControlDetailsBoolean;

constructor TSilWindowsMixerComponent.Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine);
begin
  inherited Create;

  FHandle := Handle;
  MakeRef(Device, @FDevice);
  FMixerLine := MixerLine;
  FControls := TSilMixerControlList.Create;
end;

destructor TSilWindowsMixerComponent.Destroy;
begin
  DropRef(@FDevice);
  FControls := nil;
  inherited;
end;

procedure TSilWindowsMixerComponent.DoCheckControls;
var
  pmxctrl: PMIXERCONTROLS;
  mxlctrl: TMixerLineControls;
  i: LongWord;
  Control: IMixerControl;
begin
  GetMem(pmxctrl, FMixerLine.cControls * Sizeof(TMixerControl));

  try
    mxlctrl.cbStruct := SizeOf(mxlctrl);
    mxlctrl.dwLineID := FMixerLine.dwLineID;
    mxlctrl.dwControlID := 0;
    mxlctrl.cControls := FMixerLine.cControls;
    mxlctrl.cbmxctrl := SizeOf(TMixerControl);
    mxlctrl.pamxctrl := Pointer(pmxctrl);

    if mixerGetLineControls(FHandle, @mxlctrl, MIXER_GETLINECONTROLSF_ALL) = 0 then
      for i := 0 to FMixerLine.cControls - 1 do
      begin
        Control := TSilWindowsMixerControl.Create(FHandle, Self, pmxctrl[i]);
        FControls.Add(Control);
      end;
  finally
    FreeMem(pmxctrl);
  end;
end;

function TSilWindowsMixerComponent.GetChannelCount: Word;
begin
  Result := FMixerLine.cChannels;
end;

function TSilWindowsMixerComponent.GetComponentType: TMixerComponentType;
begin
  case FMixerLine.dwComponentType of
    MIXERLINE_COMPONENTTYPE_DST_DIGITAL:      Result := ctDigital;
    MIXERLINE_COMPONENTTYPE_DST_LINE:         Result := ctLine;
    MIXERLINE_COMPONENTTYPE_DST_MONITOR:      Result := ctMonitor;
    MIXERLINE_COMPONENTTYPE_DST_SPEAKERS:     Result := ctSpeakers;
    MIXERLINE_COMPONENTTYPE_DST_HEADPHONES:   Result := ctHeadphones;
    MIXERLINE_COMPONENTTYPE_DST_TELEPHONE:    Result := ctTelephone;
    MIXERLINE_COMPONENTTYPE_DST_WAVEIN:       Result := ctWaveIn;
    MIXERLINE_COMPONENTTYPE_DST_VOICEIN:      Result := ctVoiceIn;

    MIXERLINE_COMPONENTTYPE_SRC_DIGITAL:      Result := ctDigital;
    MIXERLINE_COMPONENTTYPE_SRC_LINE:         Result := ctLine;
    MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE:   Result := ctMicrophone;
    MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER:  Result := ctSynthesizer;
    MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC:  Result := ctCompactDisc;
    MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE:    Result := ctTelephone;
    MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER:    Result := ctPcSpeaker;
    MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT:      Result := ctWaveOut;
    MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY:    Result := ctAuxiliary;
    MIXERLINE_COMPONENTTYPE_SRC_ANALOG:       Result := ctAnalog;

  else                                      Result := ctUnknown;
  end;
end;

function TSilWindowsMixerComponent.GetControls: IMixerControlList;
begin
  if FControls.Count = 0 then DoCheckControls;
  Result := FControls;
end;

function TSilWindowsMixerComponent.GetDevice: IMixerDevice;
begin
  Result := GetWinDevice;
end;

function TSilWindowsMixerComponent.GetId: LongWord;
begin
  Result := FMixerLine.dwLineID;
end;

function TSilWindowsMixerComponent.GetMixerLine: TMixerLine;
begin
  Result := FMixerLine;
end;

function TSilWindowsMixerComponent.GetName: String;
begin
  Result := FMixerLine.szName;
end;

function TSilWindowsMixerComponent.GetWinDevice: IMixerDevice;
begin
  Result := IMixerDevice(FDevice);
end;

{ TSilWindowsMixerControl }

constructor TSilWindowsMixerControl.Create(Handle: Integer; const Component: IMixerComponent; const MixerControl: TMixerControl);
begin
  inherited Create;

  FHandle := Handle;
  MakeRef(Component, @FComponent);
  FMixerControl := MixerControl;
end;

destructor TSilWindowsMixerControl.Destroy;
begin
  DropRef(@FComponent);
  inherited;
end;

function TSilWindowsMixerControl.GetComponent: IMixerComponent;
begin
  Result := IMixerComponent(FComponent);
end;

function TSilWindowsMixerControl.GetControlClass: TMixerControlClass;
begin
  case FMixerControl.dwControlType and MIXERCONTROL_CT_CLASS_MASK of
    MIXERCONTROL_CT_CLASS_METER:  Result := ccMeter;
    MIXERCONTROL_CT_CLASS_SWITCH: Result := ccSwitch;
    MIXERCONTROL_CT_CLASS_NUMBER: Result := ccNumber;
    MIXERCONTROL_CT_CLASS_SLIDER: Result := ccSlider;
    MIXERCONTROL_CT_CLASS_FADER:  Result := ccFader;
    MIXERCONTROL_CT_CLASS_TIME:   Result := ccTime;
    MIXERCONTROL_CT_CLASS_LIST:   Result := ccList;
    else                          Result := ccUnknown;
  end;
end;

function TSilWindowsMixerControl.GetControlSubClass: TMixerControlSubClass;
begin
  Result := scUnknown;
  
  case FMixerControl.dwControlType and MIXERCONTROL_CT_CLASS_MASK of
    MIXERCONTROL_CT_CLASS_METER:            Result := scPolled;

    MIXERCONTROL_CT_CLASS_SWITCH:
      case FMixerControl.dwControlType and MIXERCONTROL_CT_SUBCLASS_MASK of
        MIXERCONTROL_CT_SC_SWITCH_BOOLEAN:  Result := scBoolean;
        MIXERCONTROL_CT_SC_SWITCH_BUTTON:   Result := scButton;
      end;

    MIXERCONTROL_CT_CLASS_TIME:
      case FMixerControl.dwControlType and MIXERCONTROL_CT_SUBCLASS_MASK of
        MIXERCONTROL_CT_SC_TIME_MICROSECS:  Result := scMicrosecs;
        MIXERCONTROL_CT_SC_TIME_MILLISECS:  Result := scMillisecs;
      end;

    MIXERCONTROL_CT_CLASS_LIST:
      case FMixerControl.dwControlType and MIXERCONTROL_CT_SUBCLASS_MASK of
        MIXERCONTROL_CT_SC_LIST_SINGLE:     Result := scSingle;
        MIXERCONTROL_CT_SC_LIST_MULTIPLE:   Result := scMultiple;
      end;
  end;
end;

function TSilWindowsMixerControl.GetControlUnits: TMixerControlUnits;
begin
  case FMixerControl.dwControlType and MIXERCONTROL_CT_UNITS_MASK of
    MIXERCONTROL_CT_UNITS_CUSTOM:   Result := cuCustom;
    MIXERCONTROL_CT_UNITS_BOOLEAN:  Result := cuBoolean;
    MIXERCONTROL_CT_UNITS_SIGNED:   Result := cuSigned;
    MIXERCONTROL_CT_UNITS_UNSIGNED: Result := cuUnsigned;
    MIXERCONTROL_CT_UNITS_DECIBELS: Result := cuDecibels;
    MIXERCONTROL_CT_UNITS_PERCENT:  Result := cuPercent;
    else                            Result := cuUnknown;
  end;
end;

const
  AControlType: array [TMixerControlType] of LongWord = (
    0,
    MIXERCONTROL_CONTROLTYPE_CUSTOM,
    MIXERCONTROL_CONTROLTYPE_BOOLEANMETER,
    MIXERCONTROL_CONTROLTYPE_SIGNEDMETER,
    MIXERCONTROL_CONTROLTYPE_PEAKMETER,
    MIXERCONTROL_CONTROLTYPE_UNSIGNEDMETER,
    MIXERCONTROL_CONTROLTYPE_BOOLEAN,
    MIXERCONTROL_CONTROLTYPE_ONOFF,
    MIXERCONTROL_CONTROLTYPE_MUTE,
    MIXERCONTROL_CONTROLTYPE_MONO,
    MIXERCONTROL_CONTROLTYPE_LOUDNESS,
    MIXERCONTROL_CONTROLTYPE_STEREOENH,
    MIXERCONTROL_CONTROLTYPE_BUTTON,
    MIXERCONTROL_CONTROLTYPE_DECIBELS,
    MIXERCONTROL_CONTROLTYPE_SIGNED,
    MIXERCONTROL_CONTROLTYPE_UNSIGNED,
    MIXERCONTROL_CONTROLTYPE_PERCENT,
    MIXERCONTROL_CONTROLTYPE_SLIDER,
    MIXERCONTROL_CONTROLTYPE_PAN,
    MIXERCONTROL_CONTROLTYPE_QSOUNDPAN,
    MIXERCONTROL_CONTROLTYPE_FADER,
    MIXERCONTROL_CONTROLTYPE_VOLUME,
    MIXERCONTROL_CONTROLTYPE_BASS,
    MIXERCONTROL_CONTROLTYPE_TREBLE,
    MIXERCONTROL_CONTROLTYPE_EQUALIZER,
    MIXERCONTROL_CONTROLTYPE_SINGLESELECT,
    MIXERCONTROL_CONTROLTYPE_MUX,
    MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT,
    MIXERCONTROL_CONTROLTYPE_MIXER,
    MIXERCONTROL_CONTROLTYPE_MICROTIME,
    MIXERCONTROL_CONTROLTYPE_MILLITIME);

function TSilWindowsMixerControl.GetControlType: TMixerControlType;
begin
  for Result := Low(TMixerControlType) to High(TMixerControlType) do
    if AControlType[Result] = FMixerControl.dwControlType then
      Exit;

  Result := ntUnknown;
end;

function TSilWindowsMixerControl.GetId: LongWord;
begin
  Result := FMixerControl.dwControlID;
end;

function TSilWindowsMixerControl.GetLowerBound: Integer;
begin
  Result := FMixerControl.Bounds.lMinimum;
end;

function TSilWindowsMixerControl.GetName: String;
begin
  Result := FMixerControl.szName;
end;

function TSilWindowsMixerControl.GetSteps: Integer;
begin
  Result := FMixerControl.Metrics.cSteps;
end;

function TSilWindowsMixerControl.GetUpperBound: Integer;
begin
  Result := FMixerControl.Bounds.lMaximum;
end;

procedure TSilWindowsMixerControl.SetComponent(const Value: IMixerComponent);
begin
  ChangeRef(Value, @FComponent);
end;

function TSilWindowsMixerControl.GetValue: Integer;
begin
  Result := GetChannelValue(0);
end;

function TSilWindowsMixerControl.GetChannelValue(Index: Word): Integer;
var
  Values: TIntegerArray;
begin
  DoInitValues(Values, Component.ChannelCount);

  if DoControlValues(false, Index, Values) then
    Result := Values[Index] else
    Result := Vart.Unassigned;
end;

procedure TSilWindowsMixerControl.SetChannelValue(Index: Word; Value: Integer);
var
  Values: TIntegerArray;
  Component: IMixerComponent;
begin
  Component := GetComponent;

  if Assigned(Component) then
  begin
    DoInitValues(Values, Component.ChannelCount);

    if High(Values) >= Index then
    begin
      Values[Index] := Value;
      DoControlValues(true, Index, Values);
    end;
  end;
end;

procedure TSilWindowsMixerControl.SetValue(Value: Integer);
var
  Values: TIntegerArray;
  Component: IMixerComponent;
  i: Integer;
begin
  Component := GetComponent;

  if Assigned(Component) then
  begin
    DoInitValues(Values, Component.ChannelCount);

    for i := 0 to Length(Values) - 1 do
      Values[i] := Value;

    DoControlValues(true, -1, Values);
  end;
end;

function TSilWindowsMixerControl.DoControlValues(MustSet: Boolean; Index: Integer; var Values: TIntegerArray): Boolean;
var
  Component: IMixerComponent;
  Details: TMixerControlDetails;
  cChannels, cMultipleItems: LongWord;
  pListBool: PMixerControlDetailsBooleans;
  SubClass: TMixerControlSubClass;
  i1, i2: Integer;
begin
  Result := false;
  Component := GetComponent;
  if not Assigned(Component) then Exit;

  if (MIXERCONTROL_CONTROLF_UNIFORM and FMixerControl.fdwControl) > 0 then
    cChannels := 1 else
    cChannels := Component.ChannelCount;

  if (MIXERCONTROL_CONTROLF_MULTIPLE and FMixerControl.fdwControl) > 0 then
    cMultipleItems := FMixerControl.cMultipleItems else
    cMultipleItems := 0;

  Details.cbStruct := SizeOf(Details);
  Details.dwControlID := FMixerControl.dwControlID;
  Details.cChannels := cChannels;
  Details.cMultipleItems := cMultipleItems;

  if cMultipleItems = 0 then
    cMultipleItems := 1;

  GetMem(pListBool, cChannels * cMultipleItems * SizeOf(TMixerControlDetailsBoolean));

  try
    Details.cbDetails := SizeOf(TMixerControlDetailsBoolean);
    Details.paDetails := Pointer(pListBool);

    if mixerGetControlDetails(FHandle, @Details, MIXER_GETCONTROLDETAILSF_VALUE) = MMSYSERR_NOERROR then
    begin
      i1 := 0;
      SubClass := ControlSubClass;

      while i1 < Integer(cMultipleItems) do
      begin
        if SubClass <> scSingle then
        begin
          for i2 := 0 to cChannels - 1 do
            if (Index = -1) or (i1 + i2 = Index) then
              if MustSet then
                pListBool[i1 + i2].fValue := Values[i2] else
                Values[i2] := pListBool[i1 + i2].fValue;
        end else
        begin
          if MustSet then
            pListBool[i1].fValue := Values[i1] else
            Values[i1] := pListBool[i1].fValue;
        end;

        Inc(i1, cChannels);
      end;

      Result := not MustSet or
        (mixerSetControlDetails(FHandle, @Details, MIXER_GETCONTROLDETAILSF_VALUE) = MMSYSERR_NOERROR);
    end;
  finally
    FreeMem(pListBool);
  end;
end;

function TSilWindowsMixerControl.IsSelected(const Component: IMixerComponent): Boolean;
var
  Values: TIntegerArray;
  WinComp: IWindowsMixerComponent;
begin
  WinComp := Component as IWindowsMixerComponent;

  DoInitValues(Values, FMixerControl.cMultipleItems);
  DoControlValues(false, 0, Values);
  Result := Boolean(Values[WinComp.GetMixerLine.dwSource]);
end;

procedure TSilWindowsMixerControl.Select(const Component: IMixerComponent; Value: Boolean);
var
  Values: TIntegerArray;
  WinComp: IWindowsMixerComponent;
begin
  WinComp := Component as IWindowsMixerComponent;

  DoInitValues(Values, FMixerControl.cMultipleItems);
  Values[WinComp.GetMixerLine.dwSource] := Ord(Value);
  DoControlValues(true, 0, Values);
end;

{ TSilWindowsMixerMasterPlaybackComponent }

constructor TSilWindowsMixerMasterPlaybackComponent.Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine);
begin
  inherited;
  FComponents := TSilMixerVolumeComponentList.Create;
end;

destructor TSilWindowsMixerMasterPlaybackComponent.Destroy;
begin
  FComponents := nil;
  inherited;
end;

function TSilWindowsMixerMasterPlaybackComponent.GetMaster: IMixerVolumeComponent;
begin
  if not Assigned(FMaster) then DoGetMaster;
  Result := FMaster;
end;

function TSilWindowsMixerMasterPlaybackComponent.GetCDAudio: IMixerVolumeComponent;
begin
  if not Assigned(FCDAudio) then DoFindComponent(ctCompactDisc, FCDAudio);
  Result := FCDAudio;
end;

function TSilWindowsMixerMasterPlaybackComponent.GetMidi: IMixerVolumeComponent;
begin
  if not Assigned(FMidi) then DoFindComponent(ctSynthesizer, FMidi);
  Result := FMidi;
end;

function TSilWindowsMixerMasterPlaybackComponent.GetWave: IMixerVolumeComponent;
begin
  if not Assigned(FWave) then DoFindComponent(ctWaveOut, FWave);
  Result := FWave;
end;

function TSilWindowsMixerMasterPlaybackComponent.GetComponents: IMixerVolumeComponentList;
begin
  if FComponents.Count = 0 then DoGetComponents;
  Result := FComponents;
end;

procedure TSilWindowsMixerMasterPlaybackComponent.DoFindComponent(const ComponentType: TMixerComponentType; var Component: IMixerVolumeComponent);
var
  Enum: IEnumerator;
begin
  while Components.Enumerate(Enum, Component) do
    if Component.ComponentType = ComponentType then
      Exit;

  Component := nil;
end;

procedure TSilWindowsMixerMasterPlaybackComponent.DoGetMaster;
var
  Line: TMixerLine;
begin
  Line := FMixerLine;

  if mixerGetLineInfo(FHandle, @Line, MIXER_GETLINEINFOF_SOURCE) = 0 then
    FMaster := TSilWindowsMixerStandardComponent.Create(FHandle, Device, nil, Line);
end;

procedure TSilWindowsMixerMasterPlaybackComponent.DoGetComponents;
var
  Line: TMixerLine;
  i: Integer;
  Component: IMixerVolumeComponent;
begin
  Line := FMixerLine;
  FComponents.Add(GetMaster);

  for i := 0 to FMixerLine.cConnections - 1 do
  begin
    Line.dwSource := i;

    if mixerGetLineInfo(FHandle, @Line, MIXER_GETLINEINFOF_SOURCE) = 0 then
    begin
      Component := TSilWindowsMixerStandardComponent.Create(FHandle, Device, nil, Line);
      FComponents.Add(Component);
    end;
  end;
end;

{ TSilWindowsMixerMasterRecordingComponent }

constructor TSilWindowsMixerMasterRecordingComponent.Create(Handle: Integer; const Device: IMixerDevice; const MixerLine: TMixerLine);
begin
  inherited;

  FComponents := TSilMixerRecordingComponentList.Create;
  DoGetMux;
end;

destructor TSilWindowsMixerMasterRecordingComponent.Destroy;
begin
  FComponents := nil;
  inherited;
end;

function TSilWindowsMixerMasterRecordingComponent.GetComponents: IMixerRecordingComponentList;
begin
  if FComponents.Count = 0 then DoGetComponents;
  Result := FComponents;
end;

function TSilWindowsMixerMasterRecordingComponent.GetCDAudio: IMixerRecordingComponent;
begin
  if not Assigned(FCDAudio) then DoFindComponent(ctCompactDisc, FCDAudio);
  Result := FCDAudio;
end;

function TSilWindowsMixerMasterRecordingComponent.GetMicrophone: IMixerRecordingComponent;
begin
  if not Assigned(FMicrophone) then DoFindComponent(ctMicrophone, FMicrophone);
  Result := FMicrophone;
end;

procedure TSilWindowsMixerMasterRecordingComponent.DoGetComponents;
var
  Line: TMixerLine;
  i: Integer;
  Component: IMixerRecordingComponent;
begin
  Line := FMixerLine;

  for i := 0 to FMixerLine.cConnections - 1 do
  begin
    Line.dwSource := i;

    if mixerGetLineInfo(FHandle, @Line, MIXER_GETLINEINFOF_SOURCE) = 0 then
    begin
      Component := TSilWindowsMixerStandardComponent.Create(FHandle, Device, FMux, Line);
      FComponents.Add(Component);
    end;
  end;
end;

procedure TSilWindowsMixerMasterRecordingComponent.DoFindComponent(const ComponentType: TMixerComponentType; var Component: IMixerRecordingComponent);
var
  Enum: IEnumerator;
begin
  while Components.Enumerate(Enum, Component) do
    if Component.ComponentType = ComponentType then
      Exit;

  Component := nil;
end;

procedure TSilWindowsMixerMasterRecordingComponent.DoGetMux;
var
  Line: TMixerLine;
  pmxctrl: PMIXERCONTROLS;
  mxlctrl: TMixerLineControls;
begin
  Line := FMixerLine;

  if mixerGetLineInfo(FHandle, @Line, MIXER_GETLINEINFOF_SOURCE) = 0 then
  begin
    GetMem(pmxctrl, FMixerLine.cControls * Sizeof(TMixerControl));

    try
      mxlctrl.cbStruct := SizeOf(mxlctrl);
      mxlctrl.dwLineID := Line.dwLineID;
      mxlctrl.dwControlID := 0;
      mxlctrl.cControls := Line.cControls;
      mxlctrl.cbmxctrl := SizeOf(TMixerControl);
      mxlctrl.pamxctrl := Pointer(pmxctrl);

      if mixerGetLineControls(FHandle, @mxlctrl, MIXER_GETLINECONTROLSF_ALL) = 0 then
        if Line.cControls > 0 then
          FMux := TSilWindowsMixerControl.Create(FHandle, Self, pmxctrl[0]);
    finally
      FreeMem(pmxctrl);
    end;
  end;
end;

procedure TSilWindowsMixerMasterRecordingComponent.Select(const Component: IMixerComponent; Value: Boolean);
begin
  FMux.Select(Component, Value);
end;

function TSilWindowsMixerMasterRecordingComponent.IsSelected(const Component: IMixerComponent): Boolean;
begin
  Result := FMux.IsSelected(Component);
end;

{ TSilWindowsMixerStandardComponent }

constructor TSilWindowsMixerStandardComponent.Create(Handle: Integer; const Device: IMixerDevice; const Mux: IWindowsMixerSelect; const MixerLine: TMixerLine);
begin
  inherited Create(Handle, Device, MixerLine);

  FMux := Mux;
  DoInitControl;
end;

destructor TSilWindowsMixerStandardComponent.Destroy;
begin
  FVolume := nil;
  FMute := nil;
  FMux := nil;

  inherited;
end;

procedure TSilWindowsMixerStandardComponent.DoInitControl;
var
  Enum: IEnumerator;
  Control: IMixerControl;
begin
  DoCheckControls;

  while FControls.Enumerate(Enum, Control) do
    case Control.ControlType of
      ntVolume: FVolume := Control;
      ntMute:   FMute := Control;
    end;
end;

function TSilWindowsMixerStandardComponent.GetChannelVolume(Index: Word): Word;
begin
  Result := FVolume.ChannelValue[Index];
end;

function TSilWindowsMixerStandardComponent.GetIsMuted: Boolean;
begin
  Result := FMute.Value = 1;
end;

function TSilWindowsMixerStandardComponent.GetVolume: Word;
begin
  Result := FVolume.Value;
end;

procedure TSilWindowsMixerStandardComponent.Mute;
begin
  FMute.Value := 1;
end;

procedure TSilWindowsMixerStandardComponent.UnMute;
begin
  FMute.Value := 0;
end;

procedure TSilWindowsMixerStandardComponent.SetChannelVolume(Index, Value: Word);
begin
  FVolume.ChannelValue[Index] := Value;
end;

procedure TSilWindowsMixerStandardComponent.SetVolume(Value: Word);
begin
  FVolume.Value := Value;
end;

procedure TSilWindowsMixerStandardComponent.Select;
begin
  FMux.Select(Self, true);
end;

function TSilWindowsMixerStandardComponent.GetIsSelected: Boolean;
begin
  Result := FMux.IsSelected(Self);
end;

end.
