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

unit SilLmTrace;

{$I Defines.inc}

{$IMPORTEDDATA ON}

interface

uses
  SilBeDataType,
  SilLiReference,
  SilLiTrace,
  SilLkObject,
  SilLkLogger,
  SilLeTrace,
  SilLtTrace,
  SilLmPointerList,
  SilOiModule,
  SilOiThread,
  SilOiPerformance,
  SilBeMemMgr,
  SilBeError,
  SilLiParameters,
  SilLiLoggers;

type
  TTraceStack = class(
    TSilPointerList,
    IReferenceable,
    ITraceStack,
    IThreadEvents )
  private
    FThread: Pointer;
    FTrace: ITrace;
  protected
    function Thread: IThread;
    function IsEmpty: Boolean;
    function Level: Integer;
    function Top: ITrace;
    procedure Push(const Item: ITrace);
    procedure Pop;
  protected // IThreadEvents
    procedure OnEnter(const Thread: IThread);
    procedure OnExit(const Thread: IThread);
    procedure OnUnhandledException(const Thread: IThread; Error: Exception);
  public
    class function Create(const Thread: IThread): ITraceStack; reintroduce;
    constructor CreateNew(const Thread: IThread); reintroduce;
    destructor Destroy; override;
  end;

  TTraceData = class(
    TSilObject,
    IReferenceable,
    ITraceData,
    IThreadData )
  private
    FName: string;
    FPrototype: string;
    FTime, FElapsed: TDateTime;
    FLevel: Integer;
    FMemory: RMemoryInfo;
    FThreadID: LongWord;
    FThreadName: string;
    FModule: string;
  protected // ITraceData
    function GetLevel: Integer;
    function GetName: string;
    function GetPrototype: string;
    function GetTime: TDateTime;
    function GetElapsed: TDateTime;
    function GetMemory: RMemoryInfo;
    function GetThread: IThreadData;
    function GetModule: string;
    function Refresh: ITraceData;
  protected // IThreadData
    function IThreadData.ID = GetThreadID;
    function IThreadData.Name = GetThreadName;
    function GetThreadID: LongWord;
    function GetThreadName: string;
  public
    constructor Create(Level: Integer; const Thread: IThread; const Module: string; const Name: string; const Args: array of Variant; const Sender, Category: string);
    destructor Destroy; override;
  end;

  TTrace = class(
    TSilObject,
    IReferenceable,
    ITrace )
  private
    FStack: TTraceStack;
    FParent: TTrace;
    FData: ITraceData;
    FSender: string;
    FCategory: string;
    FExit: string;
    FFrame: Pointer;
    //FHandler: Pointer;
    FEntered: Boolean;
    FFailed: Boolean;
    FCounter: IPerformanceCounter;
    FInitial: Double;
    FCurrent: Double;
  private
    procedure DoHandleException(Ex: Exception);
    //procedure DoUnhook;
    procedure DoEnter(Force: Boolean = False);
    procedure DoLeave(Stamp: TDateTime; Force: Boolean = False);
    procedure DoLog(Stamp: TDateTime; Kind: TTraceKind; const Message: string; const Sender: string = ''; const Category: string = ''); overload;
    procedure DoLog(Stamp: TDateTime; Kind: TTraceKind; const Message: string; const Args: array of const; const Sender: string = ''; const Category: string = ''); overload;
  protected // ITrace
    procedure Log(const Message: string; const Args: array of const; const Sender, Category: string); overload;
    procedure Log(const Message: string; const Sender, Category: string); overload;
    procedure Error(const Message: string; const Args: array of const; const Sender, Category: string); overload;
    procedure Error(const Message: string; const Sender, Category: string); overload;
    procedure Exit(const Message: string); overload;
    procedure Exit(const Message: string; const Args: array of const); overload;
    function GetData: ITraceData;
    function GetTimeInitial: Double;
    function GetTimeCurrent: Double;
    function TimeElapsed(Reset: Boolean = False): Double; overload; 
    function TimeElapsed(From: Double): Double; overload; 
  protected
    property TimeInitial: Double read GetTimeInitial;
    property TimeCurrent: Double read GetTimeCurrent;
  public
    constructor Create(const Stack: ITraceStack; const Name: string; const Args: array of Variant; const ExceptionFrame: Pointer; const Sender, Category: string);
    destructor Destroy; override;
  end;

{ exports }

procedure LogginManager(const Manager: ILogginManager);
procedure Define(const Logger: LoggerType; const Format: FormatterType; const Params: IParameters);
procedure Undefine(const Logger: LoggerType);
function Current: ITrace;
function Enter(const Name: string; const Args: array of Variant; const ExceptionFrame: Pointer; const Sender, Category: string): ITrace;
procedure Leave(const Message: string); overload;
procedure Leave(const Message: string; const Args: array of const); overload;
procedure Log(const Message: string; const Args: array of const; const Sender, Category: string); overload;
procedure Log(const Message: string; const Sender, Category: string); overload;
procedure Error(const Message: string; const Args: array of const; const Sender, Category: string);
function GetStack: ITraceStack;

implementation

uses
  SilBcDebug,
  SilBgDebug,
  SilBtDateTime,
  SilBtVart,
  SilBtStr,
  SilBtFloat,
  SilBtMem,
  SilLiField,
  SilLtConnection,
  SilLdTrace,
  SilLtLoggers,
  SilOeExceptFrame,
  SilOfExceptFrame,
  SilOtTool,
  SilLtReference,
  SilLiFiler;

//-----------------------------------------------------------------------------------------

function GetStack: ITraceStack;
var
  Thread: IThread;
  Item: Variant;
begin
  Thread := OS.Thread.Current;
  Item := Thread.Data['TraceStack'];
  if not Vart.ToInterface(Item, ITraceStack, Result) then
    Result := TTraceStack.Create(Thread);
end;

procedure LogginManager(const Manager: ILogginManager);
begin
  SilLtLoggers.LogginManager(Manager);
end;

procedure Define(const Logger: LoggerType; const Format: FormatterType; const Params: IParameters);
begin
  SilLtLoggers.Add(Logger, Format, Params);
end;

procedure Undefine(const Logger: LoggerType);
begin
  SilLtLoggers.Remove(Logger);
end;

function Current: ITrace;
begin
  Result := GetStack().Top;
  if not Assigned(Result) then
    Result := Trace.Enter('[' + Os.Thread.Current.Name + ']');
end;

function Enter(const Name: string; const Args: array of Variant; const ExceptionFrame: Pointer; const Sender, Category: string): ITrace;
begin
  Result := TTrace.Create(GetStack, Name, Args, ExceptionFrame, Sender, Category);
end;

procedure Leave(const Message: string);
begin
  Current.Exit(Message);
end;

procedure Leave(const Message: string; const Args: array of const);
begin
  Current.Exit(Message, Args);
end;

procedure Log(const Message: string; const Sender, Category: string);
begin
  Current.Log(Message, Sender, Category);
end;

procedure Log(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  Current.Log(Message, Args, Sender, Category);
end;

procedure Error(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  Current.Error(Message, Args, Sender, Category);
end;

function Lookup: TTrace;
begin
  Result := Reference.GetInstance(Current);
  ASSERT(Assigned(Result));
end;

const
  cDelphiException    = $0EEDFADE;

type
  TExceptMapper = function(const Rec: TExceptRecord): Exception;

function IsDelphiExcept(const Rec: TExceptRecord): Boolean;
begin
  Result := Rec.ExceptCode = cDelphiException;
end;

function GetExcept(const Rec: TExceptRecord): Exception;
begin
  Result := Exception(Rec.ExceptObject);
end;

function CreateExcept(const Rec: TExceptRecord): Exception;
begin
{$IFNDEF LINUX}
  Result := TExceptMapper(ExceptObjProc)(Rec);
{$ENDIF}
end;

procedure HandleExcept(const Rec: TExceptRecord; Item: TTrace);
var
  Ex: Exception;
  Created: Boolean;
begin
  Created := not IsDelphiExcept(Rec);
  if Created then
    Ex := CreateExcept(Rec) else
    Ex := GetExcept(Rec);
  try
    Item.DoHandleException(Ex);
  finally
    if Created then Ex.Free;
  end;
end;

function DefCW: Word;
begin
  Result := System.Default8087CW;
end;

(*)
function TraceException(Rec: PExceptRecord; Frame, ThreadCtx, DispatherCtx: Pointer): LongWord; stdcall; assembler;
var
  CW: Word;
asm
         call     Lookup
         mov      edx, eax
         mov      eax, Rec
         test     TExceptRecord[eax].ExceptFlags, 6
         jnz      @@leave
         push     eax
         call     DefCW
         mov      CW, ax
         pop      eax
         FNINIT
         FWAIT
         FLDCW    CW
         push     edx
         call     HandleExcept
         pop      edx
@@leave: mov      eax, edx
         push     TTrace[eax].FHandler
         call     TTrace.DoUnhook
         pop      eax
         pop      ebp
         jmp      eax
end;
(*)

{ TTraceStack }

class function TTraceStack.Create(const Thread: IThread): ITraceStack;
var
  Stack: TTraceStack;
begin
  Stack := CreateNew(Thread);
  Result := Stack;

  Thread.Data['TraceStack'] := Result;
  
  if not Thread.IsMain then
    Stack.FTrace := Trace.Enter('Thread: ' + Thread.Name);
end;

constructor TTraceStack.CreateNew(const Thread: IThread);
begin
  inherited Create;
  FThread := Pointer(Thread);
  if Thread.IsSpawned then
    Sink.Connect(Thread, Self, false);
end;

destructor TTraceStack.Destroy;
begin
  FThread := nil;
  FTrace := nil;
  inherited;
end;

function TTraceStack.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

function TTraceStack.Level: Integer;
begin
  Result := Count - 1;
end;

function TTraceStack.Top: ITrace;
begin
  if not IsEmpty then
    Result := ITrace(Last) else
    Result := nil;
end;

procedure TTraceStack.Push(const Item: ITrace);
begin
  Add(Pointer(Item));
end;

procedure TTraceStack.Pop;
begin
  if not IsEmpty then
    Delete(Level);
end;

function TTraceStack.Thread: IThread;
begin
  Result := IThread(FThread);
end;

procedure TTraceStack.OnEnter(const Thread: IThread);
begin
end;

procedure TTraceStack.OnExit(const Thread: IThread);
begin
  if Assigned(FTrace) then
  begin
    FTrace.Exit(SThreadTerminated);
    FTrace := nil;
  end;

  Sink.Disconnect(Thread, Self);
end;

procedure TTraceStack.OnUnhandledException(const Thread: IThread; Error: Exception);
begin
  if Assigned(FTrace) then FTrace.Error('UnhandledException: ' + Error.Message);
end;

{ TTraceData }

constructor TTraceData.Create(Level: Integer; const Thread: IThread; const Module: string; const Name: string; const Args: array of Variant; const Sender, Category: string);
begin
  inherited Create;
  FLevel := Level;
  FModule := Module;
  FName := Name;
  FTime := DateTime.Now;
  FElapsed := FTime;
  FPrototype := Trace.Prototype(Name, Args);
  FMemory := Mem.Info;
  FThreadID := Thread.ThreadID;
  FThreadName := Thread.Name;
end;

destructor TTraceData.Destroy;
begin
  inherited;
end;

function TTraceData.GetLevel: Integer;
begin
  Result := FLevel;
end;

function TTraceData.GetName: string;
begin
  Result := FName;
end;

function TTraceData.GetPrototype: string;
begin
  Result := FPrototype;
end;

function TTraceData.GetTime: TDateTime;
begin
  Result := FTime;
end;

function TTraceData.GetElapsed: TDateTime;
begin
  Result := FElapsed - FTime;
end;

function TTraceData.GetMemory: RMemoryInfo;
begin
  Result := FMemory;
end;

function TTraceData.GetThread: IThreadData;
begin
  Result := Self;
end;

function TTraceData.GetModule: string;
begin
  Result := FModule;
end;

function TTraceData.GetThreadID: LongWord;
begin
  Result := FThreadID;
end;

function TTraceData.GetThreadName: string;
begin
  Result := FThreadName;
end;

function TTraceData.Refresh: ITraceData;
begin
  FMemory := Mem.Info;
  FElapsed := DateTime.Now();
  Result := Self;
end;

{ TTrace }

constructor TTrace.Create(const Stack: ITraceStack; const Name: string; const Args: array of Variant; const ExceptionFrame: Pointer; const Sender, Category: string);
begin
  inherited Create;
  FParent := Reference.GetInstance(Stack.Top);
  FStack := Reference.GetInstance(Stack);
  ASSERT(Assigned(FStack));
  FStack.Push(Self);
  FData := TTraceData.Create(FStack.Level, FStack.Thread, Os.Module.Name(Self.ClassType), Name, Args, Sender, Category);
  FSender := Sender;
  FCategory := Category;
  FFrame := ExceptionFrame;
  FFailed := True;

  FCounter := OS.Performance.Create;
  if Assigned(FParent) then
    FInitial := FParent.TimeCurrent else
    Finitial := 0;
  FCurrent := 0;
  
  DoEnter();
end;

destructor TTrace.Destroy;
begin
  DoLeave(DateTime.Now);

  FData := nil;
  FStack.Pop;
  FStack := nil;
  FParent := nil;
  FCounter := nil;
  inherited;
end;

procedure TTrace.Log(const Message: string; const Sender, Category: string);
begin
  if (CDebugTracing and GDebug.Flags <> 0) then
  begin
    if not FEntered then DoEnter(CDebugOnTraceForce and GDebug.Flags <> 0);
    DoLog(DateTime.Now, tkLog, Message, Sender, Category);
  end;
end;

procedure TTrace.Log(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  if (CDebugTracing and GDebug.Flags <> 0) then
  begin
    if not FEntered then DoEnter(CDebugOnTraceForce and GDebug.Flags <> 0);
    DoLog(DateTime.Now, tkLog, Message, Args, Sender, Category);
  end;
end;

procedure TTrace.Error(const Message: string; const Sender, Category: string);
begin
  if (CDebugError and GDebug.Flags <> 0) then
  begin
    if not FEntered then DoEnter(CDebugOnErrorForce and GDebug.Flags <> 0);
    DoLog(DateTime.Now, tkError, Message, Sender, Category);
  end;
end;

procedure TTrace.Error(const Message: string; const Args: array of const; const Sender, Category: string);
begin
  if (CDebugError and GDebug.Flags <> 0) then
  begin
    if not FEntered then DoEnter(CDebugOnErrorForce and GDebug.Flags <> 0);
    DoLog(DateTime.Now, tkError, Message, Args, Sender, Category);
  end;
end;

procedure TTrace.Exit(const Message: string);
begin
  FFailed := False;
  FExit := Message;
end;

procedure TTrace.Exit(const Message: string; const Args: array of const);
begin
  Exit(Str.Format(Message, Args));
end;

function TTrace.GetData: ITraceData;
begin
  Result := FData;
end;

function TTrace.GetTimeCurrent: Double;
begin
  Result := FCounter.ToMicroseconds(False);
end;

function TTrace.GetTimeInitial: Double;
begin
  Result := FInitial;
end;

function TTrace.TimeElapsed(Reset: Boolean): Double;
var
  Current: Double;
begin
  Current := GetTimeCurrent;
  Result := Current - FCurrent;
  if Reset then FCurrent := Current;
end;

function TTrace.TimeElapsed(From: Double): Double;
begin
  Result := TimeCurrent - From;
end;

procedure TTrace.DoHandleException(Ex: Exception);
begin
  FFailed := True;
  Trace.Error(Ex);
end;

(*)
procedure TTrace.DoUnhook;
begin
  // SilOfExceptFrame.Unhook(FFrame, FHandler);
end;
(*)

procedure TTrace.DoEnter(Force: Boolean);
begin
  if not FEntered and (Force or (CDebugCalls and GDebug.Flags <> 0)) then
  begin
    FEntered := True;
    if Assigned(FParent) then FParent.DoEnter(Force);
    DoLog(FData.Time, tkEnter, FData.Prototype);
  end;
end;

procedure TTrace.DoLeave(Stamp: TDateTime; Force: Boolean);
var
  Message: string;
begin
  if not FEntered then DoEnter(Force);

  if FEntered then
  begin
    Message := FData.Name;

    if FFailed then DoLog(Stamp, tkError, SNoTraceExit, [Message]);

    if Str.IsAssigned(FExit) then
      Str.Add(Message, FExit, ': ');

    DoLog(Stamp, tkLeave, Message);
  end;
end;

procedure TTrace.DoLog(Stamp: TDateTime; Kind: TTraceKind; const Message: string; const Sender, Category: string);
var
  Name, Sign: string;
  Time: Double;
  Elapsed: string;
begin
  try
    case Kind of
      tkLeave:
        begin
          Sign := 'T:';
          Time := TimeCurrent;
        end;
      tkLog, tkError:
        begin
          Sign := 'L:';
          Time := TimeElapsed(True);
        end;

      else
        begin
          Sign := '  ';
          if Assigned(FParent) then
            Time := FParent.TimeElapsed(FInitial) else
            Time := 0;
        end;
    end;

    Elapsed := Str.Format('%12.2f', [Time]);

    Name := '[' + Sign + Elapsed + '] ';
    if not FEntered then
      Name := Name + FData.Name + ': ';

    SilLtLoggers.Log(Stamp, Kind, FData.Refresh, Name + Message, Str.Iif(Str.IsAssigned(Sender), Sender, FSender), Str.Iif(Str.IsAssigned(Category), Category, FCategory));
  except
  end;
end;

procedure TTrace.DoLog(Stamp: TDateTime; Kind: TTraceKind; const Message: string; const Args: array of const; const Sender, Category: string);
begin
  DoLog(Stamp, Kind, Str.Format(Message, Args), Sender, Category);
end;

end.
