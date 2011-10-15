{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit SilOxThread;

interface

{$INCLUDE Defines.inc}

uses
  SilOsTypes,
  
  SilOhInterface,

  SilLiParameters,
  SilLiConnection,
  SilLiEventList,
  SilLiReference,
  
  SilOiHandle,
  SilOiWait,
  SilOiIpc,
  SilOiThread,

  SilLkInterfaced,
  SilOkHandled;

type
  TSilThread = class (
    TSilHandledObject,
    IWaitable,
    IThread )
  private
    FThreadID: Cardinal;
    FSuspended: Boolean;
    FReturnValue: Integer;
    FPriority: TThreadPriority;
    FName: String;
  protected
    FRunnable: IUnknown;
    FTermination: IEvent;
    FFinished: IEvent;
    FExitCode: LongWord;
    FData: IParameterList;
    FIsSpawned: Boolean;
  protected
    function DoGetHook(out Hook: IThreadHook): Boolean;
    function DoGetInstance: IOsThreadInstance;
    function GetRunnable: IRunnable;
    procedure SetRunnable(const Value: IRunnable);
  protected // IWaitable
    function WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean = false): TSyncWaitResult; virtual; abstract;
  protected // IThread
    function GetPriority: TThreadPriority; virtual;
    procedure SetPriority(Value: TThreadPriority); virtual;
    function GetSuspended: Boolean; virtual;
    procedure SetSuspended(Value: Boolean); virtual;
    function GetThreadID: Cardinal; virtual;
    function GetTermination: IIpcObject; virtual;
    function GetTerminated: Boolean; virtual;
    function GetReturnValue: Integer; virtual;
    function GetName: String; virtual;
    procedure SetName(const Value: String); virtual;
    function GetMain: Boolean; virtual;
    function GetExitCode: LongWord; virtual;
    function GetIsCurrent: Boolean; virtual;
    function GetIsSpawned: Boolean;
    function GetData: IParameterList; virtual;
    procedure SetReturnValue(Value: Integer); virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure Resume; virtual;
    procedure Suspend; virtual;
    procedure SyncCall(Method: TThreadMethod; const Ref); overload; virtual;
    procedure SyncCall(Method: TThreadMethod); overload; virtual;
    procedure AsyncCall(Method: TThreadMethod; const Ref); overload; virtual;
    procedure AsyncCall(Method: TThreadMethod); overload; virtual;
    procedure Detach; virtual;
  protected
    function DoResume: Boolean; virtual; abstract;
    function DoSuspend: Boolean; virtual; abstract;
    procedure DoApplyPriority; virtual; abstract;
    function DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle; virtual; abstract;
  protected
    procedure DoExecute; virtual;
  protected 
    procedure DoFireEnter;
    procedure DoFireExit;
    procedure DoFireUnhandledException(Error: TObject);
  protected
    property Runnable: IRunnable read GetRunnable write SetRunnable;
    property Priority: TThreadPriority read FPriority write FPriority;
    property ThreadID: Cardinal read FThreadID write FThreadID;
    property ReturnValue: Integer read FReturnValue;
    property Instance: IOsThreadInstance read DoGetInstance;
  public
    class function GetCurrent: IThread; virtual; abstract;
  public
    constructor Create(const Handle: IHandle; const Runnable: IRunnable = nil; const Name: String = ''); overload;
    constructor Create(const Runnable: IRunnable; const Name: String = ''; CreateSuspended: Boolean = false); overload; virtual;
    constructor Create(const Dispatchable: IDispatchable; Id: LongWord; const Name: String = ''; CreateSuspended: Boolean = false); overload; virtual;
    destructor Destroy; override;
    procedure BeforeConstruction; override;
    procedure AfterDestruction; override;
  end;

implementation

uses
  SysUtils,

  SilBtInt,
  SilBtStr,
  SilLiTraits,
  SilLiEnumerator,
  SilLtList,
  SilLtReference,
  SilOfThreadList,
  SilOtTool,

  SilOmDispatchRunnable,
  SilOmAdoptRunnable,
  SilLfTraits;

{ TSilThread }

constructor TSilThread.Create(const Handle: IHandle; const Runnable: IRunnable; const Name: String);
begin
  inherited Create(Handle, False, True);
  FFinished := Os.Ipc.Event();
  FTermination := Os.Ipc.Event();
  FPriority := tpNormal;
  SetName(Name);
  SetRunnable(Runnable);
end;

constructor TSilThread.Create(const Runnable: IRunnable; const Name: String; CreateSuspended: Boolean);
begin
  SilOfThreadList.CheckList;

  FSuspended := CreateSuspended;
  FIsSpawned := true;
  Create(DoCreateThread(True, FThreadID), Runnable, Name);

  if not CreateSuspended then Resume;
end;

constructor TSilThread.Create(const Dispatchable: IDispatchable; Id: LongWord; const Name: String; CreateSuspended: Boolean);
begin
  Create(TSilAdaptDispatchRunnable.Create(Self, Dispatchable, Id), Name, CreateSuspended);
end;

destructor TSilThread.Destroy;
begin
  //nunca se podria destruir si no salio de ThreadProc

  FTermination.Signal;
  Detach;

  inherited;
end;

procedure TSilThread.BeforeConstruction;
begin
  inherited;
  SilOfThreadList.AddThread(Self);
end;

procedure TSilThread.AfterDestruction;
begin
  SilOfThreadList.RemoveThread(Self);
  inherited;
end;

procedure TSilThread.Detach;
begin
  SetRunnable(nil);
end;

function TSilThread.GetPriority: TThreadPriority;
begin
  Result := FPriority;
end;

procedure TSilThread.SetPriority(Value: TThreadPriority);
begin
  if FPriority = Value then Exit;
  FPriority := Value;
  DoApplyPriority;
end;

function TSilThread.GetSuspended: Boolean;
begin
  Result := FSuspended;
end;

procedure TSilThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend else
      Resume;
end;

function TSilThread.GetThreadID: Cardinal;
begin
  Result := FThreadID;
end;

function TSilThread.GetTermination: IIpcObject;
begin
  Result := FTermination;
end;

function TSilThread.GetTerminated: Boolean;
begin
  Result := FTermination.IsSignaled;
end;

function TSilThread.GetReturnValue: Integer;
begin
  Result := FReturnValue;
end;

function TSilThread.GetName: String;
begin
  Result := FName;
end;

procedure TSilThread.SetName(const Value: String);
begin
  if Str.Len(Value) > 0 then
    FName := Value else
    FName := Int.ToHex(FThreadID, 8);
end;

function TSilThread.GetMain: Boolean;
begin
  Result := FThreadID = MainThreadID;
end;

function TSilThread.GetIsSpawned: Boolean;
begin
  Result := FIsSpawned;
end;

function TSilThread.GetExitCode: LongWord;
begin
  Result := FExitCode;
end;

function TSilThread.GetIsCurrent: Boolean;
begin
  Result := Self.FThreadID = Os.Thread.ID;
end;

function TSilThread.GetData: IParameterList;
begin
  if FData = nil then FData := ListTool.Parameters(True);
  Result := FData;
end;

function TSilThread.DoGetHook(out Hook: IThreadHook): Boolean;
begin
  Result := Ref.GetInterface(FRunnable, IThreadHook, Hook);
end;

procedure TSilThread.Initialize;
var
  Hook: IThreadHook;
begin
  if DoGetHook(Hook) then
    Hook.Initialize(Self);
end;

procedure TSilThread.Finalize;
var
  Hook: IThreadHook;
begin
  if DoGetHook(Hook) then
    Hook.Finalize(Self);
end;

procedure TSilThread.Resume;
var
  Hook: IThreadHook;
begin
  if Instance.Resume() then
  begin
    FSuspended := false;
    if DoGetHook(Hook) then
      Hook.Resumed(Self);
  end;
end;

procedure TSilThread.Suspend;
var
  Hook: IThreadHook;
begin
  if Instance.Suspend() then
  begin
    if DoGetHook(Hook) then
      Hook.Suspended(Self);
    FSuspended := true;
  end;
end;

procedure TSilThread.SetReturnValue(Value: Integer);
begin
  FReturnValue := Value;
end;

procedure TSilThread.DoExecute;
begin
  Initialize;
  try
    if Assigned(FRunnable) then
      Runnable.Run(Self);
  finally
    Finalize;
  end;
end;

procedure TSilThread.SetRunnable(const Value: IRunnable);
begin
  FRunnable := TSilAdoptRunnable.Create(Self, Runnable);
end;

function TSilThread.GetRunnable: IRunnable;
begin
  Result := FRunnable as IRunnable;
end;

procedure TSilThread.DoFireEnter;
var
  n: IEnumerator;
  l: IThreadEvents;
begin
  try
    if HasConnections then
      with Events do
        while Enumerate(n, l, IThreadEvents) do l.OnEnter(Self);
  except
  end;
end;

procedure TSilThread.DoFireExit;
var
  n: IEnumerator;
  l: IThreadEvents;
begin
  try
    if HasConnections then
      with Events do
      begin
        while Enumerate(n, l, IThreadEvents) do l.OnExit(Self);
        Clear;
      end;
  except
  end;
end;

procedure TSilThread.DoFireUnhandledException(Error: TObject);
var
  n: IEnumerator;
  l: IThreadEvents;
begin
  try
    if HasConnections then
      with Events do
      begin
        while Enumerate(n, l, IThreadEvents) do l.OnUnhandledException(Self, Exception(Error));
        Clear;
      end;
  except
  end;
end;

procedure TSilThread.AsyncCall(Method: TThreadMethod);
begin
end;

procedure TSilThread.AsyncCall(Method: TThreadMethod; const Ref);
begin
end;

procedure TSilThread.SyncCall(Method: TThreadMethod);
begin
end;

procedure TSilThread.SyncCall(Method: TThreadMethod; const Ref);
begin
end;

function TSilThread.DoGetInstance: IOsThreadInstance;
begin
  Result := IOsThreadInstance(FHandle);
end;

end.
