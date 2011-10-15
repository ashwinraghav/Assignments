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

unit UmAgentDispatch;

interface

{$include Defines.inc}

uses
  SysUtils,
  
  Sil,
  SilXml,
  
  UiAgent,
  UmSchedule;

type
  TAgentDispatch = class (TSilObject, IAgentDispatch, IRunnable)
  private
    FQueue: IInterfaceQueue;
    FThread: IThread;
    FRunning: IInterfaceList;
  protected // IAgentDispatch
    procedure Start;
    procedure Stop;
    procedure Put(const Data: ISchedule);
  protected // IRunnable
    procedure Run(const Sender: IThread);
  end;

  TRunningStep = class (TSilObject, IRunningStep, IRunnable)
  private
    FList: IInterfaceList;
    FThread: IThread;
    FStop: IEvent;
    FSchedule: ISchedule;
    FFinished: IEvent;
    FStartTime: TDateTime;
    FControlEnabled: Boolean;
  private
    function DoRun(const Step: IStep; const Source: IAgentSource): Integer;
    function DoRunApplication(const Params: IParameters; out ErrStr: String): Boolean;
    function DoRunSql(const Params: IParameters; out ErrStr: String): Boolean;
    function DoRunBackup(const Params: IParameters; out ErrStr: String): Boolean;
    procedure DoCalcNext(const Data: ISchedule);
    procedure DoNotify(const Status: String; const EndTime: TDateTime);
  protected // IRunningStep
    function GetFinished: Boolean;
    function GetSchedule: ISchedule;
    function GetStartTime: TDateTime;
    procedure Start;
    procedure Stop;
  protected // IRunnable
    procedure Run(const Sender: IThread);
  public
    constructor Create(const List: IInterfaceList; const Schedule: ISchedule);
    destructor Destroy; override;
  end;

implementation

uses
  UtXml,
  DmAgentDispatch, SilOjFile, SilBtStr, UtAgentNotify,
  UiRecurrence, SilOiFile;

{ TAgentDispatch }

procedure TAgentDispatch.Start;
begin
  Stop;
  Sil.Trace.Enter(Self, 'Start');

  FRunning := Sil.List.InterfaceList(true);
  FQueue := Sil.List.InterfaceQueue;
  FThread := Sil.OS.Thread.Spawn('dispatch', Self);

  Sil.Trace.Leave;
end;

procedure TAgentDispatch.Stop;
var
  Enum: IEnumerator;
  Item: IRunningStep;
begin
  Sil.Trace.Enter(Self, 'Stop');

  if Assigned(FQueue) then
  begin
    FQueue.Cancel;
    FThread.Termination.WaitFor;

    FQueue := nil;
    FThread := nil;
  end;

  if Assigned(FRunning) then
  begin
    while FRunning.Enumerate(Enum, Item) do
      Item.Stop;

    FRunning.Clear;
    FRunning := nil;
  end;

  Sil.Trace.Leave;
end;

procedure TAgentDispatch.Put(const Data: ISchedule);
begin
  FQueue.Put(Data);
end;

procedure TAgentDispatch.Run(const Sender: IThread);
var
  Schedule: ISchedule;
  Enum: IEnumerator;
  RunningStep: IRunningStep;
  Found: Boolean;
begin
  Sil.Trace.Enter(Self, 'Run');

  while FQueue.Get(ISchedule, Schedule) do
  begin
    Found := false;

    try
      while FRunning.Enumerate(Enum, RunningStep) do
        if RunningStep.Finished then
        begin
          Sil.Trace.Log('trying to stop (%d) %s, started at %s', [RunningStep.Schedule.Id, RunningStep.Schedule.Name, DateTime.ToStr(RunningStep.StartTime)]);
          RunningStep.Stop;
        end;
    except
      Sil.Trace.Exception('RunningStep.Stop');
    end;

    while FRunning.Enumerate(Enum, RunningStep) do
      if Schedule.Id = RunningStep.Schedule.Id then
      begin
        Sil.Trace.Log('schedule (%d) "%s" already running', [Schedule.Id, Schedule.Name]);

        Found := true;
        Enum := nil;
        Break;
      end;

    if not Found then
      try
        RunningStep := TRunningStep.Create(FRunning, Schedule);
        RunningStep.Start;
      except
        Sil.Trace.Exception('RunningStep.Start');
      end;
  end;

  Sil.Trace.Leave;
end;

{ TRunningStep }

constructor TRunningStep.Create(const List: IInterfaceList; const Schedule: ISchedule);
begin
  Sil.Trace.Enter(Self, 'Create');

  inherited Create;

  FControlEnabled := Str.Pos('app', Sil.OS.Process.Current.Info.Name) = 0;

  FStartTime := DateTime.Now;
  FFinished := Sil.OS.Ipc.Event;
  FList := List;
  FSchedule := Schedule;
  FList.Add(IRunningStep(Self));

  Sil.Trace.Leave;
end;

destructor TRunningStep.Destroy;
begin
  Sil.Trace.Enter(Self, 'Destroy');

  Stop;
  FSchedule := nil;

  inherited;

  Sil.Trace.Leave;
end;

function TRunningStep.GetSchedule: ISchedule;
begin
  Result := FSchedule;
end;

procedure TRunningStep.Start;
begin
  Sil.Trace.Enter(Self, 'Start');

  if not Assigned(FThread) then
  begin
    FStop := Sil.OS.Ipc.Event;
    FThread := Sil.OS.Thread.Spawn('task', Self);
  end;

  Sil.Trace.Leave;
end;

procedure TRunningStep.Stop;
var
  Stopped: Boolean;
begin
  if Assigned(FStop) then
  begin
    Sil.Trace.Enter(Self, 'Stop', FSchedule.Name);

    Stopped := true;
    FStop.Signal;

    if FThread.Termination.WaitFor(500) = wrSignaled then
    begin
      FStop := nil;
      FThread := nil;
    end else
      Stopped := false;

    if Stopped then
    begin
      if Assigned(FList) then
      begin
        if FList.Remove(Self) = -1 then
          Sil.Trace.Error('step not found in FList');

        FList := nil;
      end else
        Sil.Trace.Error('not Assigned(FList)');
    end;

    Sil.Trace.Leave;
  end;
end;

procedure TRunningStep.Run(const Sender: IThread);
var
  Step: IStep;
  List: IStepList;
  HistoryId, Next: Integer;
  Status: String;
  EndTime: TDateTime;
  CalcOnly: Boolean;
begin
  Sil.Trace.Enter(Self, 'Run');

  try
    try
      List := FSchedule.StepList;

      if ((FSchedule.LastRun > 0) and (List.Count > 0)) or (FSchedule.Recurrence.Increment = 0) then
      begin
        CalcOnly := false;
        FSchedule.Source.History(HistoryId, 0, FSchedule.TaskId, 0, DateTime.Now, 0, 'run');
        FSchedule.HistoryId := HistoryId;

        Step := List.First;

        repeat
          Sil.Trace.Log('task="%s" step="%s" running', [FSchedule.Name, Step.Name]);

          Next := DoRun(Step, FSchedule.Source);

          if List.ValidIndex(Next) then
            Step := List[Next] else
            Step := nil;
        until not Assigned(Step);

        EndTime := DateTime.Now;
        Status := Str.IIf(Next = -1, 'err', 'ok');
        FSchedule.Source.History(HistoryId, 0, 0, 0, 0, EndTime, Status);

        if Str.NotEmpty(FSchedule.Notification, true) then
          DoNotify(Status, EndTime);

        Sil.Trace.Log('task="%s" result=%s', [FSchedule.Name, Status]);
      end else
      begin
        CalcOnly := true;
        Sil.Trace.Log('task="%s" next_run calc', [FSchedule.Name]);
      end;

      DoCalcNext(FSchedule);
      FSchedule.Source.Update(FSchedule, CalcOnly);
    finally
      FFinished.Signal;
    end;
  except
    Sil.Trace.Exception;
  end;

  Sil.Trace.Leave;
end;

procedure TRunningStep.DoNotify(const Status: String; const EndTime: TDateTime);
var
  Tree: IXmlTree;
  Enum: IEnumerator;
  ChildNode: IXmlNode;
  ChildTag: IXmlTag;
  ServiceName, Event, Subject, Body: String;
begin
  Sil.Trace.Enter(Self, 'Run');

  try
    Tree := SilXml.Tool.FromStr(FSchedule.Notification);

    with Tree.Root.AsTag do
      while Childs.Enumerate(Enum, ChildNode) do
        if ChildNode.NodeKind = nkTag then
        begin
          ChildTag := ChildNode.AsTag;
          ServiceName := ChildTag.Name;
          Event := ChildTag.Childs.ReadString('event');

          if
            (Sil.Text.Compare(Event, 'all') = 0) or
            ((Sil.Text.Compare(Event, 'failed') = 0) and (Status = 'err')) or
            ((Sil.Text.Compare(Event, 'succeeded') = 0) and (Status = 'ok')) then
          begin
            if Sil.Text.Compare(Event, 'all') = 0 then
            begin
              if Status = 'err' then
                Event := 'failed' else
                Event := 'succeeded';
            end;

            Subject := Str.Format('[FbSqla] Execution of task "%s", has %s.', [FSchedule.Name, Event]);

            Body := Str.Format('Task: "%s"' + ccCRLF + 'Started at: %s' + ccCRLF + 'Duration: %s' + ccCRLF + 'Status: %s', [
              FSchedule.Name, DateTime.ToStr(FStartTime), Time.ToStr(EndTime - FStartTime), Status]);

            AgentNotify.Run(ChildTag.Name, ChildTag.Childs.ReadStrings('recipients'), Subject, Body);
          end;
        end;
  except
    Sil.Trace.Exception;
  end;

  Sil.Trace.Leave;
end;

procedure TRunningStep.DoCalcNext(const Data: ISchedule);
var
  Recurrence: IRecurrence;

  procedure DoInc(Element: TTimeElement);
  var
    Stamp: TDateTime;
  begin
    Stamp := Data.NextRun;
    DateTime.Inc(Stamp, Element, 1);
    Data.NextRun := Date.Extract(Stamp) + Time.Extract(Recurrence.StartTime);
  end;

var
  DayOfWeek: TDayOfWeek;
  DayPos: TDayPosition;
  Found: Boolean;
  MonthFilter: TMonthFilter;
  DayOfWeekFilter: TDaysOfWeek;
  DayPositionFilter: TDayPositions;
  DayDayNumberFilter: TDayNumberFilter;
  Stamp, Control: TDateTime;
begin
  Sil.Trace.Enter(Self, 'DoCalcNext', [Data.Name]);

  Stamp := DateTime.Now;
  Control := Stamp + Time.EncodeParts(0, 0, 10);
  Recurrence := Data.Recurrence;

  if (Data.LastRun = 0) or (Data.NextRun = 0) or (Data.NextRun + Time.EncodeParts(0, 5) < Stamp) then
  begin
    Data.LastRun := Stamp;
    Data.NextRun := Date.Extract(Data.LastRun) + Time.Extract(Recurrence.StartTime);
  end;

  while true do
  begin
    if (FControlEnabled and (Control < DateTime.Now)) or (Recurrence.Increment <= 0) then
    begin
      if Recurrence.Increment <> 0 then
        Sil.Trace.Error('internal error: unable to schedule task');

      Data.NextRun := 0;
      Break;
    end;

    if (Recurrence.DurationKind = dkEnd) and (Data.NextRun > Recurrence.EndDate) then
    begin
      Data.NextRun := 0;
      Break;
    end;

    MonthFilter := Recurrence.GetMonthFilter;

    if (MonthFilter <> []) and not (Date.Decode(Data.NextRun).Month in MonthFilter) then
    begin
      DoInc(teMonth);
      Continue;
    end;

    DayDayNumberFilter := Recurrence.GetDayNumberFilter;

    if DayDayNumberFilter = [] then
    begin
      DayOfWeekFilter := Recurrence.GetDayOfWeekFilter;
      DayPositionFilter := Recurrence.GetDayPositionFilter;

      if DayOfWeekFilter <> [] then
      begin
        DayOfWeek := Date.DayOfWeek(Data.NextRun);

        if not (DayOfWeek in DayOfWeekFilter) then
        begin
          DoInc(teDay);
          Continue;
        end;

        if DayPositionFilter <> [] then
        begin
          Found := false;

          for DayPos := Low(TDayPosition) + 1 to High(TDayPosition) do
            if DayPos in DayPositionFilter then
            begin
              Found := Date.Compare(Data.NextRun, DateTime.Find(Data.NextRun, DayPos, DayOfWeek)) = 0;
              if Found then Break;
            end;

          if not Found then
          begin
            DoInc(teDay);
            Continue;
          end;
        end;
      end;
    end else
    if not (Date.Decode(Data.NextRun).Day in DayDayNumberFilter) then
    begin
      DoInc(teDay);
      Continue;
    end;

    if (Recurrence.OccursKind = okEvery) and (Recurrence.EndTime > 0) and (Time.Extract(Data.NextRun) > Recurrence.EndTime) then
    begin
      DoInc(teDay);
      Continue;
    end;

    if Data.NextRun >= Stamp then
      Break;

    Data.NextRun := Data.NextRun + Recurrence.Increment;
  end;

  if Data.NextRun <> 0 then
    Sil.Trace.Log('next run %s', [DateTime.ToStr(Data.NextRun)]) else
    Sil.Trace.Log('end of task scope');

  Sil.Trace.Leave;
end;

function TRunningStep.DoRun(const Step: IStep; const Source: IAgentSource): Integer;
var
  HistoryId: Integer;
  ErrStr: String;

  function DoGetNext(Index: Integer; const Status: String): Integer;
  begin
    Result := Int.IIf((-1 * Index in [1..3]) or (Index > Step.Order), Index, -1);

    case Index of
      -3: // Goto the next step
        Result := Step.Order + 1;
      -2: // Quit the job reporting success
        Result := -2;
      -1: // Quit the job reporting failure
        Result := -1;
    end;

    Source.History(HistoryId, 0, 0, 0, 0, DateTime.Now, Status, ErrStr);
    Sil.Trace.Log('task="%s" step="%s" result=%s', [FSchedule.Name, Step.Name, Status]);
  end;

var
  Kind: String;
  Params: IParameterList;
  IsOk: Boolean;
begin
  Sil.Trace.Enter(Self, 'DoRun');

  Result := -1;
  IsOk := false;
  HistoryId := 0;

  if AgentXml.Command(Step.Data, Kind, Params) then
  begin
    FSchedule.LastRun := DateTime.Now;
    Source.History(HistoryId, FSchedule.HistoryId, FSchedule.TaskId, Step.Id, FSchedule.LastRun, 0, 'run');

    if Sil.Text.Compare(Kind, 'sql') = 0 then
      IsOk := DoRunSql(Params, ErrStr) else
    if Sil.Text.Compare(Kind, 'application') = 0 then
      IsOk := DoRunApplication(Params, ErrStr) else
    if Sil.Text.Compare(Kind, 'backup') = 0 then
      IsOk := DoRunBackup(Params, ErrStr);

    if IsOk then
      Result := DoGetNext(Step.OnSuccess, 'ok') else
      Result := DoGetNext(Step.OnFailure, 'err');
  end;

  Sil.Trace.Leave;
end;

function TRunningStep.DoRunSql(const Params: IParameters; out ErrStr: String): Boolean;
var
  Dm: TdaDispatch;
  Isolation: String;
begin
  Sil.Trace.Enter(Self, 'DoRunSql');

  try
    Dm := TdaDispatch.Create(nil);

    try
      Dm.coDispatch.DatabaseName := Vart.ToStr(Params['connection']);

      Sil.Trace.Log('database %s', [Dm.coDispatch.DatabaseName]);

      Dm.coDispatch.Params.Values['user_name'] := Vart.ToStr(Params['user']);
      Dm.coDispatch.Params.Values['password'] := Vart.ToStr(Params['password']);
      Dm.coDispatch.Open;

      try
        try
          Isolation := Vart.ToStr(Params['isolation']);

          if Str.NotEmpty(Isolation) then // IBX conversion
            Dm.trDispatchStat.Params.Text := Str.Replace(Isolation, 'CommittedVersion', 'rec_version', true);
        except
          Sil.Trace.Exception;
        end;

        Dm.trDispatchStat.StartTransaction;

        Dm.sqAction.SQL.Text := Vart.ToStr(Params['command']);
        Dm.sqAction.ExecQuery;

        Dm.trDispatchStat.Commit;
      except
        Dm.trDispatchStat.Rollback;
        raise;
      end;
    finally
      Dm.Free;
    end;

    Result := true;
  except
    on e: Exception do
    begin
      ErrStr := e.Message;
      Sil.Trace.Error(ErrStr, 'DoRunSql');
      Result := false;
    end;
  end;

  Sil.Trace.Leave;
end;

function TRunningStep.DoRunApplication(const Params: IParameters; out ErrStr: String): Boolean;
var
  Proc: IProcess;
  Signaled: Integer;
  Cmd: String;
begin
  Sil.Trace.Enter(Self, 'DoRunApplication');

  try
    Cmd := Vart.ToStr(Params['command']);
    Sil.Trace.Log('command %s', [Cmd]);

    Proc := Sil.OS.Process.Execute(Cmd, pvHidden, 0);

    Sil.OS.Wait.Any([Proc, FStop], INFINITE, Signaled);

    if FStop.IsSignaled then
    begin
      Proc.Terminate;
      Result := false;
    end else
      Result := Proc.ExitCode = LongWord(Vart.ToInt(Params['exitcode'], 0));
  except
    on e: Exception do
    begin
      ErrStr := e.Message;
      Sil.Trace.Error(ErrStr, 'DoRunApplication');
      Result := false;
    end;
  end;

  Sil.Trace.Leave;
end;

function TRunningStep.DoRunBackup(const Params: IParameters; out ErrStr: String): Boolean;
var
  Dm: TdaDispatch;
  LogFileName, LogLine: String;
  LogFile: ITextFile;
  MustLog: Boolean;
begin
  Sil.Trace.Enter(Self, 'DoRunBackup');

  try
    Dm := TdaDispatch.Create(nil);

    try
      Dm.SetBackupServer(Params);

      Sil.Trace.Log('database %s', [Dm.bsBackup.DatabaseName]);

      Dm.bsBackup.Active := true;

      try
        LogFileName := Str.Trim(Vart.ToStr(Params['logfile']));

        Dm.bsBackup.Verbose := Str.NotEmpty(LogFileName);
        Dm.bsBackup.ServiceStart;

        MustLog := Str.NotEmpty(LogFileName);

        if MustLog then
          LogFile := Sil.OS.FileSystem.CreateTextFile(LogFileName, fmAccessReadWrite, fmShareRead, true);

        while not Dm.bsBackup.Eof do
        begin
          LogLine := Dm.bsBackup.GetNextLine;
          if Assigned(LogFile) then LogFile.Stream.WriteLn(LogLine);
        end;

      finally
        Dm.bsBackup.Active := false;
      end;

      Dm.DeleteOldFiles;
    finally
      Dm.Free;
    end;

    Result := true;
  except
    on e: Exception do
    begin
      ErrStr := e.Message;
      Sil.Trace.Error(ErrStr, 'DoRunBackup');
      Result := false;
    end;
  end;

  Sil.Trace.Leave;
end;

function TRunningStep.GetFinished: Boolean;
begin
  Result := FFinished.IsSignaled;
end;

function TRunningStep.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

end.
