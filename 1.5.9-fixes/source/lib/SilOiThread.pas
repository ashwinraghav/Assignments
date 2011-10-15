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

unit SilOiThread;

{$I Defines.inc}

interface

uses                             
  SilBeError,

  SilLiEnumerator,
  SilLiLock,
  SilLiParameters,

  SilOiHandle,
  SilOiIpc;

type
  TThreadMethod = procedure(const Sender: IUnknown; const Ref) of object;
  TThreadCallMethod = procedure(const Sender: IUnknown; Param: Pointer) of object;
  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical);

type
  IThread = interface
    ['{FD203303-0B70-11D4-9155-00C0261013CD}']
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function GetSuspended: Boolean;
    procedure SetSuspended(Value: Boolean);
    function GetReturnValue: Integer;
    procedure SetReturnValue(Value: Integer);
    function GetThreadID: Cardinal;
    function GetTermination: IIpcObject;
    function GetTerminated: Boolean;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetMain: Boolean;
    function GetExitCode: LongWord;
    function GetIsCurrent: Boolean;
    function GetIsSpawned: Boolean;
    function GetHandle: IHandle;
    function GetData: IParameterList;
    procedure Resume;
    procedure Suspend;
    procedure SyncCall(Method: TThreadMethod; const Ref); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    procedure SyncCall(Method: TThreadMethod); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    procedure AsyncCall(Method: TThreadMethod; const Ref); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    procedure AsyncCall(Method: TThreadMethod); overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    procedure Detach;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property ThreadID: Cardinal read GetThreadID;
    property Termination: IIpcObject read GetTermination;
    property IsTerminated: Boolean read GetTerminated;
    property IsSuspended: Boolean read GetSuspended write SetSuspended;
    property ReturnValue: Integer read GetReturnValue write SetReturnValue;
    property Name: String read GetName write SetName;
    property Handle: IHandle read GetHandle;
    property Data: IParameterList read GetData;
    property IsMain: Boolean read GetMain;
    property ExitCode: LongWord read GetExitCode;
    property IsCurrent: Boolean read GetIsCurrent;
    property IsSpawned: Boolean read GetIsSpawned;
  end;

  RThreadRunMessage = record
    Id: Word;
    Thread: IThread;
    Tags: IParameterList;
    Ptr: Pointer;
  end;

  IRunnable = interface
    ['{FD203302-0B70-11D4-9155-00C0261013CD}']
    procedure Run(const Thread: IThread);
  end;

  ICachedThread = interface(IThread)
    ['{C044EA25-96AB-4443-88D7-3D8A06DEA0C8}']
    function GetActivation: IEvent;
    function GetRunnable: IRunnable;
    procedure SetRunnable(const Value: IRunnable);
    procedure Kill;
    property Activation: IEvent read GetActivation;
    property Runnable: IRunnable read GetRunnable write SetRunnable;
  end;

  IThreadHook = interface
    ['{D7AA6461-F694-11D3-9871-00104B0FA1EF}']
    function Initialize(const Thread: IThread): Boolean;
    procedure Finalize(const Thread: IThread);
    procedure Suspended(const Thread: IThread);
    procedure Resumed(const Thread: IThread);
  end;

  IThreadEvents = interface
    ['{A4FE2D05-71F8-11D4-9894-00104B0FA1EF}']
    procedure OnEnter(const Thread: IThread);
    procedure OnExit(const Thread: IThread);
    procedure OnUnhandledException(const Thread: IThread; Error: Exception);
  end;

  TThreadCallMode = (tcSync, tcAsync);

  IThreads = interface(IItemization)
    ['{D20CC3A4-0D01-11D4-9158-00C0261013CD}']
    function GetCount: Integer;
    function GetDestroying: Boolean;
    function GetItem(const Index: Variant): IThread;
    property Count: Integer read GetCount;
    function First: IThread;
    function Last: IThread;
    procedure Lock;
    procedure Unlock;
    function IndexOf(const Item: IThread): Integer;
    function FindThreadID(ID: LongWord; out Thread: IThread): Boolean;
    property Items[const Index: Variant]: IThread read GetItem; default;
    property Destroying: Boolean read GetDestroying;
  end;

  IThreadList = interface(IThreads)
    ['{D20CC3A4-0D01-11D4-9158-00C0261013CD}']
    function Add(const Item: IThread): Integer;
    function Remove(const Item: IThread): Integer;
    procedure Call(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadMethod; const Ref);
    procedure ThreadCall(const Obj: IUnknown; Mode: TThreadCallMode; Method: TThreadCallMethod; const Param: Pointer = nil);
    procedure RemoveForeign;
  end;

  IThreadManager = interface
    ['{16528BD1-F4FE-11D3-9870-00104B0FA1EF}']
    function GetCacheSize: LongWord;
    procedure SetCacheSize(Value: LongWord);
    function GetRemoveTimeout: LongWord;
    function GetThreads: IThreads;
    procedure SetRemoveTimeout(Value: LongWord);
    function Spawn(const Runnable: IRunnable; Suspended: Boolean = false): IThread; overload;
    function Spawn(const Name: String; const Runnable: IRunnable; Suspended: Boolean = false): IThread; overload;
    property RemoveTimeout: LongWord read GetRemoveTimeout write SetRemoveTimeout;
    property CacheSize: LongWord read GetCacheSize write SetCacheSize;
    property Threads: IThreads read GetThreads;
  end;

const
  GsThreadList: TGUID = '{0FBF0F15-8C4A-4F9C-BD98-FFA89656032F}';

implementation
end.
