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

unit SilVCL;            

interface

uses
  Classes,
  Sil,
//SilViProfiler,
  SilViDatasetEnumerator,
  SilViDrag,
//SilVmProfiler,
  SilVmProperties,
  SilVmStreamWrapper,
//SilVmStringProfile,
  SilVmListWrapper,
  SilVmStringsWrapper,
  SilVmDatasetEnumerator,
  SilVtEnumerator,
  SilVtComponents,
  SilVtLockers,
  SilVtComObj,
  SilVtBind,
  SilVtDrag,
  SilVtStream,
  SilViControls,
  SilVkControl,
  SilVtControls,
  SilVkCustomControl,
  SilVmControlProperty,
  SilVmCustomButton,
  SilVmButton,
  SilVmImgButton,
  SilVm3dShape,
  SilVm3dButton,
  SilVmFlowThread,
  SilVmFlowNode,
  SilVmCustomPanel,
  SilVmGradientPanel;

//type
//  IProfileSource      = SilViProfiler.IProfileSource;
//  IProfiler           = SilViProfiler.IProfiler;

type
  IDatasetEnumerator  = SilViDatasetEnumerator.IDatasetEnumerator;

type
  IDragState          = SilViDrag.IDragState;
  IDragEvents         = SilViDrag.IDragEvents;

type
  TListWrapper        = SilVmListWrapper.TListWrapper;

type
  TStringsWrapper     = SilVmStringsWrapper.TStringsWrapper;

//type
//  TProfiler           = SilVmProfiler.TProfiler;

type
  TStreamWrapper      = SilVmStreamWrapper.TStreamWrapper;

//type
//  TStringProfile      = SilVmStringProfile.TStringProfile;

type
  TPropertySet        = SilVmProperties.TPropertySet;

type
  ComponentClass      = SilVtComponents.ComponentClass;
  ComponentTool       = SilVtComponents.ComponentTool;

type
  Lock                = SilVtLockers.Lock;

type
//TProfilerClass = class of TProfiler;
  TPropertySetClass = class of TPropertySet;
  TLockClass = class of Lock;

type
  TStreamConvertionClass = SilVtStream.TStreamConvertionClass;

type
  ISilCustomControl = SilViControls.ISilCustomControl;
  ISilControl = SilViControls.ISilControl;
  ISilControlPainter = SilViControls.ISilControlPainter;
  ISilCustomButton = SilViControls.ISilCustomButton;
  ISilButton = SilViControls.ISilButton;
  ISilImageSource = SilViControls.ISilImageSource;
  ISil3dImageSource = SilViControls.ISil3dImageSource;

type
  ISilControlProperty = SilViControls.ISilControlProperty;
  ISilPositionProperty = SilViControls.ISilPositionProperty;
  ISilCaptionProperty = SilViControls.ISilCaptionProperty;
  ISilImageProperty = SilViControls.ISilImageProperty;
  ISilGroupingProperty = SilViControls.ISilGroupingProperty;
  ISilHighlightProperty = SilViControls.ISilHighlightProperty;

type
  TSilControlState = SilViControls.TSilControlState;
const
  acsDefault = SilViControls.acsDefault;
  acsFocus = SilViControls.acsFocus;
  acsMouseOver = SilViControls.acsMouseOver;

type
  TSil3dShape = SilViControls.TSil3dShape;
const
  ksNone = SilViControls.ksNone;
  ksEllipse = SilViControls.ksEllipse;
  ksPipe = SilViControls.ksPipe;
  ksRoundRect = SilViControls.ksRoundRect;
  ksHorSwitch = SilViControls.ksHorSwitch;
  ksRhombus = SilViControls.ksRhombus;
  ksPool = SilViControls.ksPool;

type
  TSilRelativePosition = SilViControls.TSilRelativePosition;
const
  rpFixed = SilViControls.rpFixed;
  rpTopLeft = SilViControls.rpTopLeft;
  rpTopCenter = SilViControls.rpTopCenter;
  rpTopRight = SilViControls.rpTopRight;
  rpCenterLeft = SilViControls.rpCenterLeft;
  rpCenter = SilViControls.rpCenter;
  rpCenterRight = SilViControls.rpCenterRight;
  rpBottomLeft = SilViControls.rpBottomLeft;
  rpBottomCenter = SilViControls.rpBottomCenter;
  rpBottomRight = SilViControls.rpBottomRight;

type
  TSilCustomControl = SilVkCustomControl.TSilCustomControl;

type
  TSilControl = SilVkControl.TSilControl;

type
  TSilCustomButton = SilVmCustomButton.TSilCustomButton;

const
  absPressed = SilViControls.absPressed;
  absRepeating = SilViControls.absRepeating;
  
type
  TSilButton = SilVmButton.TSilButton;
  TSilImageButton = SilVmImgButton.TSilImageButton;
  //ISilButtonImageSource = SilVmImgButton.ISilButtonImageSource;

type
  TSil3dButton = SilVm3dButton.TSil3dButton;

type
  TSilImageProperty = SilVmControlProperty.TSilImageProperty;

type
  ImageTool = SilVm3dShape.ImageTool;

type
  TSilFlowNode = SilVmFlowNode.TSilFlowNode;
  TFlowActivateEvent = SilVmFlowNode.TFlowActivateEvent;

type
  TSilCustomPanel = SilVmCustomPanel.TSilCustomPanel;

type
  TSilGradientPanel = SilVmGradientPanel.TSilGradientPanel;
  
const
  nsInactive = SilVmFlowNode.nsInactive;
  nsWorking = SilVmFlowNode.nsWorking;
  nsActive = SilVmFlowNode.nsActive;
  nsError = SilVmFlowNode.nsError;

  nkDesition = SilVmFlowNode.nkDesition;
  nkThrow = SilVmFlowNode.nkThrow;
  nkEnd = SilVmFlowNode.nkEnd;

  tmNone = SilVmFlowNode.tmNone;
  tmParent = SilVmFlowNode.tmParent;
  tmSelf = SilVmFlowNode.tmSelf;
  
type
  TFlowThreadEvent = SilVmFlowThread.TFlowThreadEvent;
  IFlowThread = SilVmFlowThread.IFlowThread;
  FlowThread = SilVmFlowThread.FlowThread;

type
  Vcl = class(Tool)
    class function List(List: TList; FreeOnDestroy: Boolean = False; Locked: Boolean = False): IPointerList; overload;
    class function List(List: TStrings; FreeOnDestroy: Boolean = False; Locked: Boolean = False): IStringList; overload;
    //class function Profiler: TProfilerClass;
    class function PropertySet: TPropertySetClass;
    class function SetProp: TPropertySet;
    class function Lock: TLockClass;
    class function Comp: ComponentClass;
    class function Enumerator: EnumeratorClass;
    class function ComObj: ComObjectClass;
    class function Bind: PropertyBindClass;
    class function Drag: DragType;
    class function Stream: TStreamConvertionClass;
  end;

const
  pamDefaultClear = SilVtBind.pamDefaultClear;
  pamHideZeros = SilVtBind.pamHideZeros;

implementation

{ Vcl }

class function Vcl.Bind: PropertyBindClass;
begin
  Result := SilVtBind.PropertyBindTool;
end;

class function Vcl.ComObj: ComObjectClass;
begin
  Result := SilVtComObj.ComObjectTool;
end;

class function Vcl.Comp: ComponentClass;
begin
  Result := SilVtComponents.ComponentTool;
end;

class function Vcl.Drag: DragType;
begin
  Result := SilVtDrag.DragTool;
end;

class function Vcl.Enumerator: EnumeratorClass;
begin
  Result := SilVtEnumerator.EnumeratorTool;
end;

class function Vcl.List(List: TList; FreeOnDestroy, Locked: Boolean): IPointerList;
begin
  Result := TListWrapper.Create(List, Locked, FreeOnDestroy);
end;

class function Vcl.List(List: TStrings; FreeOnDestroy, Locked: Boolean): IStringList;
begin
  Result := TStringsWrapper.Create(List, Locked, FreeOnDestroy);
end;

class function Vcl.Lock: TLockClass;
begin
  Result := SilVtLockers.Lock;
end;

//class function Vcl.Profiler: TProfilerClass;
//begin
//  Result := TProfiler;
//end;

class function Vcl.PropertySet: TPropertySetClass;
begin
  Result := TPropertySet;
end;

class function Vcl.SetProp: TPropertySet;
begin
  Result := TPropertySet.App;
end;

class function Vcl.Stream: TStreamConvertionClass;
begin
  Result := TStreamConvertion;
end;

end.

