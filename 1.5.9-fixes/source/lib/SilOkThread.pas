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

unit SilOkThread;

interface

{$INCLUDE Defines.inc}

uses
  SilLiParameters,
  SilLiConnection,
  SilLiEventList,

  SilLiReference,
  SilOiHandle,
  SilOiWait,
  SilOiIpc,
  SilOiThread,
  SilLiTraits,

  SilLkInterfaced;

type
  TSilThread = class (
    // extends
    TSilInterfacedObject,
    // implements
    IHandledObject,
    IThread,
    IRunnable,
    IWaitable)
  private
    FHandle: IHandle;
    FThreadID: Cardinal;
    FSuspended: Boolean;
    FReturnValue: Integer;
    FPriority: TThreadPriority;
    FName: String;
    FAdoptionTrait: IAdoptionTrait;
  protected
    FId: LongWord;
    FTermination: IEvent;
    FFinished: IEvent;
    FRunnable: Pointer;
    FDispatchable: Pointer;
    FExitCode: LongWord;
    FData: IParameterList;
    FIsSpawned: Boolean;
  protected
    function DoGetHook(out Hook: IThreadHook): Boolean;
    function GetRunnable: IRunnable;
    procedure SetRunnable(const Value: IRunnable);  
    function GetDispatchable: IDispatchable;
    property Runnable: IRunnable read GetRunnable write SetRunnable;
    property Dispatchable: IDispatchable read GetDispatchable;
    property Handle: IHandle read FHandle;
    property Priority: TThreadPriority read FPriority write FPriority;
    property ThreadID: Cardinal read FThreadID write FThreadID;
    property ReturnValue: Integer read FReturnValue;
  protected // IHandledObject
    function GetHandle: IHandle; virtual;
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
    procedure Resume; virtual;
    procedure Suspend; virtual;
    procedure SyncCall(Method: TThreadMethod; const Ref); overload; virtual;
    procedure SyncCall(Method: TThreadMethod); overload; virtual;
    procedure AsyncCall(Method: TThreadMethod; const Ref); overload; virtual;
    procedure AsyncCall(Method: TThreadMethod); overload; virtual;
    procedure Detach; virtual;
  protected // IRunnable
    procedure Run(const Thread: IThread); virtual;
  protected // IWaitable
    function WaitFor(const Timeout: Cardinal; AvoidMsgLock: Boolean = false): TSyncWaitResult; virtual; abstract;
  protected
    function DoResume: Boolean; virtual; abstract;
    function DoSuspend: Boolean; virtual; abstract;
    procedure DoApplyPriority; virtual; abstract;
    function DoCreateThread(CreateSuspended: Boolean; var ThreadID: Cardinal): IHandle; virtual; abstract;
  protected
    procedure DoExecute; virtual;
  public
    procedure DoFireEnter;
    procedure DoFireExit;
    procedure DoFireUnhandledException(Error: TObject);
  public
    class function GetCurrent: IThread; virtual; abstract;
  public
    constructor Create(const Name: String; const Handle: IHandle; const Runnable: IRunnable = nil); overload;
    constructor Create(const Runnable: IRunnable; CreateSuspended: Boolean = false); overload;
    constructor Create(const Name: String; const Runnable: IRunnable; CreateSuspended: Boolean = false); overload; virtual;
    constructor Create(Id: LongWord; const Name: String; const Dispatchable: IDispatchable; CreateSuspended: Boolean); overload; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  SilBtInt,
  SilLiEnumerator,
  SilLtList,
  SilLtReference,
  SilOsTypes,
  SilOfThreadList,
  SilOtTool;

{ TSilThread }

constructor TSilThread.Create(const Name: String; const Runnable: IRunnable; CreateSuspended: Boolean);
begin
  SilOfThreadList.CheckList;

  FReturnValue := 0;
  FSuspended := CreateSuspended;
  FIsSpawned := true;
  Create(Name, DoCreateThread(True, FThreadID), Runnable);

  if not CreateSuspended then Resume;
end;

constructor TSilThread.Create(const Runnable: IRunnable; CreateSuspended: Boolean);
begin
  Create('', Runnable, CreateSuspended);
end;

constructor TSilThread.Create(const Name: String; const Handle: IHandle; const Runnable: IRunnable);
begin
  SilOfThreadList.AddThread(Self);
  inherited Create;

  FHandle := Handle;
  SetRunnable(Runnable);

  FFinished := OS.IPC.Event();
  FTermination := OS.IPC.Event();
  FPriority := tpNormal;

  if Length(Name) > 0 then
    FName := Name
  else
    FName := Int.ToHex(FThreadID, 8);
end;

constructor TSilThread.Create(Id: LongWord; const Name: String; const Dispatchable: IDispatchable; CreateSuspended: Boolean);
begin
  FId := Id;
  FDispatchable := Pointer(Dispatchable);
  Create(Name, nil, CreateSuspended);
end;

destructor TSilThread.Destroy;
begin
  FData := nil;

  if Assigned(FTermination) then
    FTermination.Signal;
    
  Detach;
  FTermination := nil;

  inherited;
end;

procedure TSilThread.SyncCall(Method: TThreadMethod);
begin
  GetList.Call(Self, tcSync, Method, nil^);
end;

procedure TSilThread.SyncCall(Method: TThreadMethod; const Ref);
begin
  GetList.Call(Self, tcSync, Method, Ref);
end;

procedure TSilThread.AsyncCall(Method: TThreadMethod);
begin
  GetList.Call(Self, tcAsync, Method, nil^);
end;

procedure TSilThread.AsyncCall(Method: TThreadMethod; const Ref);
begin
  GetList.Call(Self, tcAsync, Method, Ref);
end;

procedure TSilThread.Detach;
begin
  SetRunnable(nil);
end;

function TSilThread.GetHandle: IHandle;
begin
  Result := FHandle;
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
      Suspend
    else
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
  if Length(Value) > 0 then FName := Value;
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
  Result := Self.FThreadID = OS.Thread.ID;
end;

function TSilThread.GetData: IParameterList;
begin
  if FData = nil then FData := ListTool.Parameters(True);
  Result := FData;
end;

function TSilThread.DoGetHook(out Hook: IThreadHook): Boolean;
begin
  Result := Ref.GetInterface(Runnable, IThreadHook, Hook)
        or Ref.GetInterface(IUnknown(FDispatchable), IThreadHook, Hook);
end;

procedure TSilThread.Resume;
var
  Hook: IThreadHook;
begin
  if DoResume then
  begin
    FSuspended := false; 
    if DoGetHook(Hook) then Hook.Resumed(Self);
  end;
end;

procedure TSilThread.Suspend;
var
  Hook: IThreadHook;
begin
  if DoSuspend then
  begin
    if DoGetHook(Hook) then Hook.Suspended(Self);      
    FSuspended := true;
  end;
end;

procedure TSilThread.Run(const Thread: IThread);
begin
end;

procedure TSilThread.SetReturnValue(Value: Integer);
begin
  FReturnValue := Value;
end;

procedure TSilThread.DoExecute;
var
  Hook: IThreadHook;
  Msg: RThreadRunMessage;
begin
  try
    if not DoGetHook(Hook) or Hook.Initialize(Self) then
    begin
      Hook := nil;

      if Assigned(FDispatchable) and (FId <> 0) then
      begin
        Msg.Id := FId;
        Msg.Thread := Self;
        IDispatchable(FDispatchable).Dispatch(Msg);
      end else
      if Assigned(FRunnable) then
        IRunnable(FRunnable).Run(Self);
    end;
  finally
    if DoGetHook(Hook) then Hook.Finalize(Self);
  end;
end;

procedure TSilThread.SetRunnable(const Value: IRunnable);
begin
  // es necesario manterner la referencia a IAdoptionTrait, de lo contrario,
  // si no la soporta, cuando se llama con nil, no se puede asegurar que el objeto
  // exista para hacer un Reg.Supports(IUnknown(FRunnable), IAdoptionTrait). marianop

  if Assigned(FAdoptionTrait) and not Ref.SameObject(Value, FAdoptionTrait) then
    FAdoptionTrait := nil;

  FRunnable := Pointer(Value);

  // si esta asignado FAdoptionTrait, quiere decir que es el mismo objeto que estaba,
  // por eso no se pide de nuevo para evitar la posibilidad de destruirlo

  if Assigned(Value) and not Assigned(FAdoptionTrait) then
    Ref.GetInterface(Value, IAdoptionTrait, FAdoptionTrait);
end;

function TSilThread.GetRunnable: IRunnable;
begin
  Result := IRunnable(FRunnable);
end;

function TSilThread.GetDispatchable: IDispatchable;
begin
  Result := IDispatchable(FDispatchable);
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
  except end;
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
  except end;
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
  except end;
end;

end.
