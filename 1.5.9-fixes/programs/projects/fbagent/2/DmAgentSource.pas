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

unit DmAgentSource;

interface

{$include Defines.inc}

uses
  SysUtils, Classes, IBQuery, DB, IBCustomDataSet, IBStoredProc, IBDatabase,

  Sil,
  UiAgent, ExtCtrls;

type
  TdaAgentSource = class(TDataModule, IAgentSource, ITimerEvents)
    coAgent: TIBDatabase;
    trRunStat: TIBTransaction;
    doRunStatUpd: TIBStoredProc;
    quRunStat: TIBQuery;
    doHistoryCheck: TIBStoredProc;
    trAction: TIBTransaction;
    doHistoryDel: TIBStoredProc;
    quTaskNotification: TIBQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    FSync: ICriticalSection;
    FInitialized: Boolean;
    FConnectTimer: ITimer;
    FHistoryLimit: Integer;
    procedure DoReadConfig;
    procedure DoReadConnection;
    procedure DoReadSettings;
  protected // ITimerEvents
    procedure OnTick(const Event: RTimerEvent);
  protected // IAgentSource
    function Get(out Data: ISchedule): Boolean;
    procedure Update(const Data: ISchedule; CalcOnly: Boolean);
    procedure History(var HistoryId: Integer; ParentId, TaskId, StepId: Integer; const StartTime, EndTime: TDateTime; const Status, Text: String);
  end;

var
  daAgentSource: TdaAgentSource;

implementation

uses
  UfConfig,
  UtCoder,
  UmSchedule, SilLiKey, UmRecurrence;

{$R *.dfm}

{ TdaAgentSource }

procedure TdaAgentSource.DataModuleCreate(Sender: TObject);
begin
  FInitialized := false;
  FSync := Sil.OS.Ipc.CriticalSection;
  DoReadConfig;
end;

procedure TdaAgentSource.DoReadConnection;
begin
  with Config.GetTag('config/database', true) do
  begin
    coAgent.DatabaseName := Childs.ReadString('name');
    coAgent.Params.Values['user_name'] := Childs.ReadString('username');
    coAgent.Params.Values['password'] := Coder.Decrypt(Childs.ReadString('password'));
  end;
end;

procedure TdaAgentSource.DoReadSettings;
begin
  with Config.GetTag('config/service', true) do
  begin
    FHistoryLimit := Childs.ReadInteger('historylimit', 100);
  end;
end;

procedure TdaAgentSource.DoReadConfig;
begin
  Sil.Trace.Enter(Self, 'DoReadConfig');

  try
    DoReadConnection;
    DoReadSettings;

    coAgent.Open;
    FInitialized := true;
  except
    Sil.Trace.Log('unable to connect to %s', [coAgent.DatabaseName]);

    FConnectTimer := Sil.OS.Timer.Create(0, 5000, nil, false);
    Sil.Sink.Connect(FConnectTimer, Self);

    FConnectTimer.TickCount := 1;
    FConnectTimer.Enabled := true;
  end;

  Sil.Trace.Leave;
end;

procedure TdaAgentSource.OnTick(const Event: RTimerEvent);
begin
  Sil.Sink.Disconnect(FConnectTimer, Self);
  FConnectTimer := nil;
  DoReadConfig;
end;

function TdaAgentSource.Get(out Data: ISchedule): Boolean;
var
  RunCount: Integer;
begin
  Sil.Trace.Enter(Self, 'Get');
  FSync.Locked;

  if FInitialized then
  begin
    if not quRunStat.Active then
    begin
      trRunStat.StartTransaction;
      quRunStat.Open;
    end;

    Result := not quRunStat.Eof;

    if Result then
    begin
      Data := TSchedule.Create(Self, quRunStat.FieldByName('task_name').AsString);
      Sil.Trace.Log('processing task "%s"', [Data.Name]);

      RunCount := quRunStat.FieldByName('repeat').AsInteger;

      Data.Recurrence.Configure(
        quRunStat.FieldByName('start_date').AsDateTime,
        quRunStat.FieldByName('end_date').AsDateTime,
        quRunStat.FieldByName('start_time').AsDateTime,
        quRunStat.FieldByName('end_time').AsDateTime,
        quRunStat.FieldByName('increment').AsDatetime,
        Int.IIf(RunCount > 0, RunCount, -1),
        quRunStat.FieldByName('day_filter_kind').AsInteger,
        quRunStat.FieldByName('dow_pos_filter').AsInteger,
        quRunStat.FieldByName('day_filter').AsInteger,
        quRunStat.FieldByName('month_filter').AsInteger);

      Data.Id := quRunStat.FieldByName('run_id').AsInteger;
      Data.TaskId := quRunStat.FieldByName('task_id').AsInteger;

      Data.LastRun := quRunStat.FieldByName('last_run').AsDateTime;
      Data.NextRun := quRunStat.FieldByName('next_run').AsDateTime;

      quTaskNotification.ParamByName('task_id').AsInteger := Data.TaskId;
      quTaskNotification.Open;

      Data.Notification := quTaskNotification.FieldByName('notification').AsString;
      quTaskNotification.Close;

      repeat
        Data.StepList.AddStep(
          quRunStat.FieldByName('step_id').AsInteger,
          quRunStat.FieldByName('step_name').AsString,
          quRunStat.FieldByName('Step_data').AsString,
          quRunStat.FieldByName('step_order').AsInteger,
          quRunStat.FieldByName('on_success').AsInteger,
          quRunStat.FieldByName('on_failure').AsInteger);

        quRunStat.Next;
      until quRunStat.Eof or (Data.Id <> quRunStat.FieldByName('run_id').AsInteger);
    end else
      trRunStat.Commit;
  end else
    Result := false;

  Sil.Trace.Leave;
end;

procedure TdaAgentSource.Update(const Data: ISchedule; CalcOnly: Boolean);
begin
  Sil.Trace.Enter(Self, 'Update', [Data.Name]);
  FSync.Locked;

  try
    doRunStatUpd.Transaction.StartTransaction;

    doRunStatUpd.ParamByName('run_id').AsInteger := Data.Id;
    doRunStatUpd.ParamByName('last_run').AsDateTime := Data.LastRun;
    doRunStatUpd.ParamByName('next_run').AsDateTime := Data.NextRun;

    if Data.Recurrence.Increment = 0 then
      doRunStatUpd.ParamByName('repeat').AsInteger := 0 else
    if CalcOnly then
      doRunStatUpd.ParamByName('repeat').AsInteger := Data.Recurrence.EndCount else
      doRunStatUpd.ParamByName('repeat').AsInteger := Int.IIf(Data.Recurrence.EndCount > 0, Data.Recurrence.EndCount - 1, -1);

    doRunStatUpd.ExecProc;
    doRunStatUpd.Transaction.Commit;
  except
    Sil.Trace.Exception('TdaAgentSource.Update');
    doRunStatUpd.Transaction.Rollback;
  end;

  Sil.Trace.Leave;
end;

procedure TdaAgentSource.History(var HistoryId: Integer; ParentId, TaskId, StepId: Integer; const StartTime, EndTime: TDateTime; const Status, Text: String);
begin
  Sil.Trace.Enter(Self, 'History');
  FSync.Locked;

  try
    doHistoryCheck.Transaction.StartTransaction;

    doHistoryCheck.ParamByName('history_id').AsInteger := HistoryId;
    doHistoryCheck.ParamByName('parent_id').AsInteger := ParentId;
    doHistoryCheck.ParamByName('task_id').AsInteger := TaskId;
    doHistoryCheck.ParamByName('step_id').AsInteger := StepId;
    doHistoryCheck.ParamByName('start_time').AsDateTime := StartTime;
    doHistoryCheck.ParamByName('end_time').AsDateTime := EndTime;
    doHistoryCheck.ParamByName('status').AsString := Status;
    doHistoryCheck.ParamByName('text_message').Value := Vart.IIf(Str.NotEmpty(Text), Text);
    doHistoryCheck.ExecProc;

    HistoryId := doHistoryCheck.ParamByName('history_id_out').AsInteger;

    if ParentId = 0 then
      try
        doHistoryDel.ParamByName('task_id').AsInteger := TaskId;
        doHistoryDel.ParamByName('max_size').AsInteger := FHistoryLimit;
        doHistoryDel.ExecProc;
      except
        Sil.Trace.Exception('TdaAgentSource.History:del');
      end;

    doHistoryCheck.Transaction.Commit;
  except
    Sil.Trace.Exception('TdaAgentSource.History');
    doHistoryCheck.Transaction.Rollback;
  end;

  Sil.Trace.Leave;
end;

end.
